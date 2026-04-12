# ============================================================
#  COCA Corpus Extractor — Shiny Application
#  Extracts tagged patterns from the Corpus of Contemporary
#  American English (COCA, inline/WLP format, CLAWS7 tagset)
# ============================================================
# Dependencies: shiny, shinydashboard, shinyWidgets, DT,
#               stringi, data.table, future, promises, shinyjs
# Install once (run this in R before launching):
#   install.packages(c("shiny","shinydashboard","shinyWidgets",
#     "DT","stringi","data.table","future","promises","shinyjs"))
# Run: shiny::runApp("coca_extractor_app.R")
# ============================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(stringi)
library(data.table)
library(future)
library(promises)
library(shinyjs)

# Use multisession so the extraction runs in a background R process
# and does NOT block the Shiny UI thread.
plan(multisession)

# ── Helpers ────────────────────────────────────────────────────────────────────

#' Parse genre, file_id, and year from a COCA inline filename.
#' Handles:
#'   wlp_acad_1990.inline.txt  → genre=acad, year=1990
#'   wlp_blog_01.inline.txt    → genre=blog, year=NA
#'   wlp_web_29.inline.txt     → genre=web,  year=NA
parse_filename <- function(fname) {
  base <- gsub("(\\.inline)?\\.txt$", "", basename(fname))
  m    <- stri_match_first_regex(base, "^wlp_([a-z]+)_(\\d+)$")
  if (is.na(m[1, 1])) {
    return(list(file_id = base, genre = NA_character_, year = NA_integer_))
  }
  genre   <- m[1, 2]
  id_part <- m[1, 3]
  year    <- if (nchar(id_part) == 4L && as.integer(id_part) >= 1900L)
               as.integer(id_part) else NA_integer_
  list(file_id = base, genre = genre, year = year)
}

#' Read a COCA corpus file safely (latin-1 → UTF-8, collapse to one string).
read_corpus_file <- function(file_path) {
  raw <- readLines(file_path, encoding = "latin1", warn = FALSE)
  raw <- stri_encode(raw, from = "ISO-8859-1", to = "UTF-8")
  tolower(paste(raw, collapse = " "))
}

#' Remove POS tags and stray punctuation from a tagged string.
#' Handles simple tags (_jj), ambiguous/double tags (_jj@_nn1),
#' and cleans up residual symbols used as separators in COCA.
strip_tags <- function(x) {
  # Remove ditto-tag extensions first (e.g. _ii21, _vvg@_jj@)
  x <- stri_replace_all_regex(x, "_[a-z][a-z0-9]*(?:@_[a-z][a-z0-9]*)*", "")
  # Remove stray COCA symbols
  x <- stri_replace_all_regex(x, "[<>@#%$*]", "")
  # Collapse extra whitespace
  x <- stri_replace_all_regex(x, "\\s{2,}", " ")
  stri_trim_both(x)
}

last_n_words <- function(s, n) {
  toks <- stri_split_regex(stri_trim_both(s), "\\s+")[[1]]
  toks <- toks[nchar(toks) > 0]
  paste(tail(toks, n), collapse = " ")
}

first_n_words <- function(s, n) {
  toks <- stri_split_regex(stri_trim_both(s), "\\s+")[[1]]
  toks <- toks[nchar(toks) > 0]
  paste(head(toks, n), collapse = " ")
}

# ── Build a regex pattern from user-supplied tag tokens ────────────────────────
#
# Token grammar (one per line in the UI text box):
#   WORD/TAG        literal word with exact tag          e.g. how/rgq
#   */TAG           any word with that tag               e.g. */jj
#   WORD/*          exact word, any tag                  e.g. how/*
#   [TAG1|TAG2]/*   one of several tags (alternatives)
#   TAG?            tag is optional  (attach ? after tag token)
#   ...             gap of 0-3 words (fuzzy)
#
# Each token produces a regex fragment that matches the inline
# COCA format:  word_tag  (possibly ambiguous: word_tag@_tag2)
#
build_pattern <- function(tokens) {
  # Each element: list(word=, tag=, optional=)
  frags <- lapply(tokens, function(tok) {
    tok  <- stri_trim_both(tok)
    if (tok == "...") return("(?:\\S+_\\S+\\s+){0,3}")
    optional <- stri_endswith_fixed(tok, "?")
    if (optional) tok <- stri_sub(tok, 1, nchar(tok) - 1L)

    parts <- stri_split_fixed(tok, "/")[[1]]
    word_pat <- if (parts[1] == "*" || parts[1] == "") "\\w+" else
                  stri_replace_all_regex(parts[1], "([.*+?^${}()|\\[\\]\\\\])", "\\\\$1")
    tag_raw  <- if (length(parts) < 2 || parts[2] == "*" || parts[2] == "") "[a-z][a-z0-9]*" else {
      # Strip surrounding brackets if present
      t <- stri_replace_all_fixed(parts[2], c("[", "]"), c("", ""), vectorise_all = FALSE)
      alts <- stri_split_fixed(t, "|")[[1]]
      if (length(alts) > 1) paste0("(?:", paste(alts, collapse = "|"), ")")
      else alts[1]
    }
    # Ambiguous-tag suffix  (_tag@_tag2@_tag3 ...) is tolerated but not required
    # We anchor only on the *first* tag component
    tag_pat <- paste0(tag_raw, "(?:@_[a-z][a-z0-9]*)*")
    frag    <- paste0(word_pat, "_", tag_pat)
    if (optional) paste0("(?:", frag, "\\s+)?") else paste0(frag, "\\s+")
  })
  # Strip trailing \s+ from last non-optional fragment and collapse
  pat <- paste(frags, collapse = "")
  # Trim the trailing \s+ that the last fragment always adds.
  # The built string ends in the literal characters \s+ so the regex
  # to match them is "\\s\\+$" (one level of R string escaping).
  pat <- stri_replace_last_regex(pat, "\\s\\+$", "")
  pat
}

# ── Per-file extraction ────────────────────────────────────────────────────────
process_file <- function(file_path, pattern, ctx_words = 15L, ctx_chars = 600L) {
  tryCatch({
    tagged <- read_corpus_file(file_path)
    meta   <- parse_filename(file_path)

    locs <- stri_locate_all_regex(tagged, pattern, case_insensitive = TRUE)[[1]]
    if (nrow(locs) == 0L || is.na(locs[1L, 1L])) return(NULL)

    rows <- lapply(seq_len(nrow(locs)), function(i) {
      ms <- locs[i, "start"]
      me <- locs[i, "end"]

      left_tagged  <- stri_sub(tagged, max(1L, ms - ctx_chars), ms - 1L)
      right_tagged <- stri_sub(tagged, me + 1L,
                               min(stri_length(tagged), me + ctx_chars))

      left_ctx   <- last_n_words(strip_tags(left_tagged),  ctx_words)
      right_ctx  <- first_n_words(strip_tags(right_tagged), ctx_words)
      match_str  <- strip_tags(stri_sub(tagged, ms, me))
      ctx_full   <- paste(left_ctx, match_str, right_ctx)

      data.frame(
        file_id       = meta$file_id,
        genre         = meta$genre,
        year          = meta$year,
        left_context  = left_ctx,
        match         = match_str,
        right_context = right_ctx,
        context_full  = ctx_full,
        stringsAsFactors = FALSE
      )
    })

    rbindlist(rows, fill = TRUE)

  }, error = function(e) {
    warning("Error in ", basename(file_path), ": ", e$message)
    NULL
  })
}

# ── Genre metadata ─────────────────────────────────────────────────────────────
GENRE_MAP <- list(
  "Academic"     = "acad",
  "Blog"         = "blog",
  "Fiction"      = "fic",
  "Magazine"     = "mag",
  "News"         = "news",
  "Spoken"       = "spok",
  "TV & Movies"  = "tvm",
  "Web"          = "web"
)
GENRES_WITH_YEARS <- c("acad","fic","mag","news","spok","tvm")

# ── CLAWS7 tag reference (for the help panel) ──────────────────────────────────
CLAWS7_TAGS <- c(
  "APPGE – possessive pronoun (my, your)",
  "AT – article (the, no)",
  "AT1 – singular article (a, an)",
  "CC – coordinating conjunction (and, or)",
  "CCB – adversative conj. (but)",
  "CS – subordinating conjunction (if, because)",
  "CST – that (conjunction)",
  "CSW – whether (conjunction)",
  "DA – after-determiner (such, former)",
  "DA1 – singular after-determiner (little, much)",
  "DA2 – plural after-determiner (few, many)",
  "DAR – comparative after-determiner (more, less)",
  "DAT – superlative after-determiner (most, least)",
  "DD – determiner (any, some)",
  "DD1 – singular determiner (this, that)",
  "DD2 – plural determiner (these, those)",
  "DDQ – wh-determiner (which, what)",
  "EX – existential there",
  "IF – for (preposition)",
  "II – general preposition",
  "IO – of (preposition)",
  "IW – with/without",
  "JJ – general adjective",
  "JJR – comparative adjective (older, better)",
  "JJT – superlative adjective (oldest, best)",
  "JK – catenative adjective (able, willing)",
  "MC – cardinal number (two, three)",
  "MC1 – singular cardinal (one)",
  "MD – ordinal number (first, next, last)",
  "ND1 – noun of direction (north, east)",
  "NN – common noun, neutral (sheep, cod)",
  "NN1 – singular common noun (book, girl)",
  "NN2 – plural common noun (books, girls)",
  "NNB – preceding title noun (Mr., Prof.)",
  "NNT1 – temporal noun sg. (day, week)",
  "NNT2 – temporal noun pl. (days, weeks)",
  "NP – proper noun, neutral (IBM, Andes)",
  "NP1 – singular proper noun (London, Jane)",
  "NP2 – plural proper noun (Browns, Koreas)",
  "NPM1 – singular month (October)",
  "PN – indefinite pronoun, neutral (none)",
  "PN1 – indefinite pronoun sg. (anyone, nobody)",
  "PNQS – subjective wh-pronoun (who)",
  "PPH1 – 3sg neuter pronoun (it)",
  "PPHO1 – 3sg objective pronoun (him, her)",
  "PPHO2 – 3pl objective pronoun (them)",
  "PPHS1 – 3sg subjective pronoun (he, she)",
  "PPHS2 – 3pl subjective pronoun (they)",
  "PPIO1 – 1sg objective pronoun (me)",
  "PPIO2 – 1pl objective pronoun (us)",
  "PPIS1 – 1sg subjective pronoun (I)",
  "PPIS2 – 1pl subjective pronoun (we)",
  "PPY – 2nd person pronoun (you)",
  "RG – degree adverb (very, so, too)",
  "RGQ – wh- degree adverb (how)",
  "RGR – comparative degree adverb (more, less)",
  "RL – locative adverb (alongside, forward)",
  "RP – prep. adverb / particle (about, in)",
  "RR – general adverb",
  "RRQ – wh- general adverb (where, when, why, how)",
  "RT – quasi-nominal adverb of time (now, tomorrow)",
  "TO – infinitive marker (to)",
  "UH – interjection (oh, yes, um)",
  "VB0 – be, base form",
  "VBDR – were",
  "VBDZ – was",
  "VBG – being",
  "VBI – be, infinitive",
  "VBM – am",
  "VBN – been",
  "VBR – are",
  "VBZ – is",
  "VD0 – do, base form",
  "VDD – did",
  "VDI – do, infinitive",
  "VDN – done",
  "VDZ – does",
  "VH0 – have, base form",
  "VHD – had (past tense)",
  "VHG – having",
  "VHI – have, infinitive",
  "VHN – had (past participle)",
  "VHZ – has",
  "VM – modal auxiliary (can, will, would)",
  "VMK – modal catenative (ought, used)",
  "VV0 – base form of lexical verb (give, work)",
  "VVD – past tense of lexical verb (gave, worked)",
  "VVG – -ing participle (giving, working)",
  "VVI – infinitive (to give)",
  "VVN – past participle (given, worked)",
  "VVZ – -s form of lexical verb (gives, works)",
  "XX – not, n't",
  "ZZ1 – singular letter (A, b)"
)

# ── Quick-pattern presets ──────────────────────────────────────────────────────
PRESETS <- list(
  "how + adjective (how_rgq + *_jj)"        = "how/rgq\n*/jj",
  "modal + verb infinitive (*_vm + *_vvi)"   = "*/vm\n*/vvi",
  "adjective + noun (*_jj + *_nn1)"          = "*/jj\n*/nn1",
  "be + adjective (*_vbz/*_vbr + *_jj)"      = "*/[vbz|vbr]\n*/jj",
  "verb + preposition (*_vv0 + *_ii)"        = "*/vv0\n*/ii",
  "determiner + adjective + noun"            = "*/dd1\n*/jj\n*/nn1",
  "Custom (type below)"                      = ""
)

# ══════════════════════════════════════════════════════════════════════════════
#  UI
# ══════════════════════════════════════════════════════════════════════════════

ui <- dashboardPage(
  skin = "black",

  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://www.r-project.org/logo/Rlogo.svg",
               height = "22px", style = "margin-right:8px;"),
      "COCA Extractor"
    ),
    titleWidth = 220
  ),

  dashboardSidebar(
    width = 220,
    sidebarMenu(
      id = "sidebar",
      menuItem("⚙️  Setup",      tabName = "setup",   icon = icon("folder-open")),
      menuItem("🔍  Pattern",    tabName = "pattern",  icon = icon("code")),
      menuItem("📂  Corpus",     tabName = "corpus",   icon = icon("database")),
      menuItem("▶️  Extract",    tabName = "run",      icon = icon("play")),
      menuItem("📊  Results",    tabName = "results",  icon = icon("table")),
      menuItem("📖  Tag Guide",  tabName = "tags",     icon = icon("book"))
    )
  ),

  dashboardBody(
    useShinyjs(),

    # ── Custom CSS ──────────────────────────────────────────────────────────
    tags$head(tags$style(HTML("
      body, .content-wrapper, .main-footer { background:#1a1a2e; }
      .skin-black .main-header .logo { background:#0f0f23; font-weight:700;
        letter-spacing:.5px; font-size:15px; }
      .skin-black .main-header .navbar { background:#0f0f23; }
      .skin-black .main-sidebar { background:#0f0f23; }
      .skin-black .sidebar-menu > li.active > a,
      .skin-black .sidebar-menu > li > a:hover { background:#16213e;
        border-left:3px solid #e94560; }
      .skin-black .sidebar-menu > li > a { color:#b0b8d4; }
      .box { border-radius:8px; border:none !important;
             box-shadow:0 2px 16px rgba(0,0,0,.4); }
      .box-header { border-radius:8px 8px 0 0; }
      .box.box-primary { border-top:3px solid #e94560 !important; }
      .box.box-info    { border-top:3px solid #0f3460 !important; }
      .box.box-success { border-top:3px solid #16a085 !important; }
      .box.box-warning { border-top:3px solid #f39c12 !important; }
      .box-body { background:#16213e; color:#d4daf0; }
      .box-header { background:#0f3460 !important; color:#fff !important; }
      .box-header .box-title { color:#fff !important; }
      h4.section-title { color:#e94560; font-size:13px; text-transform:uppercase;
        letter-spacing:1.5px; margin-top:18px; border-bottom:1px solid #2a2a4a;
        padding-bottom:6px; }
      .shiny-input-container label { color:#b0b8d4; font-size:13px; }
      .form-control, .selectize-input { background:#0f0f23 !important;
        color:#d4daf0 !important; border:1px solid #2a2a4a !important; }
      .selectize-dropdown { background:#0f0f23; color:#d4daf0; }
      .selectize-dropdown .active { background:#e94560; }
      textarea.form-control { font-family: 'Fira Code', monospace;
        font-size:13px; min-height:120px; }
      .btn-primary { background:#e94560; border-color:#e94560; font-weight:600; }
      .btn-primary:hover { background:#c73652; border-color:#c73652; }
      .btn-success { background:#16a085; border-color:#16a085; font-weight:600; }
      .btn-success:hover { background:#128069; border-color:#128069; }
      .btn-info    { background:#0f3460; border-color:#0f3460; font-weight:600; }
      .btn-warning { background:#f39c12; border-color:#f39c12; font-weight:600; }
      .info-box { border-radius:8px; }
      .info-box-icon { border-radius:8px 0 0 8px; }
      .callout { border-radius:8px; }
      .callout-info  { border-left:4px solid #3498db;
        background:#0f3460; color:#d4daf0; }
      .callout-warning { border-left:4px solid #f39c12;
        background:#2d2000; color:#ffd180; }
      .callout-success { border-left:4px solid #16a085;
        background:#00261c; color:#a8eddc; }
      #pattern_preview { background:#0f0f23; color:#7fdbca;
        font-family:'Fira Code',monospace; font-size:12px; padding:10px;
        border-radius:6px; border:1px solid #2a2a4a; word-break:break-all; }
      .progress-log { background:#0f0f23; color:#a8eddc;
        font-family:'Fira Code',monospace; font-size:12px;
        padding:12px; border-radius:6px; min-height:60px;
        border:1px solid #2a2a4a; white-space:pre-wrap; }
      .tag-grid { column-count:2; column-gap:16px; }
      .tag-item { font-size:12px; color:#b0b8d4; padding:2px 0;
        font-family:'Fira Code',monospace; break-inside:avoid; }
      .tag-item b { color:#e94560; }
      #results_table .dataTable { background:#0f0f23 !important;
        color:#d4daf0 !important; }
      .dataTables_wrapper { color:#b0b8d4; }
      .dataTables_filter input,
      .dataTables_length select { background:#0f0f23;
        color:#d4daf0; border:1px solid #2a2a4a; }
      table.dataTable thead th { background:#0f3460; color:#fff; border:none; }
      table.dataTable tbody tr:hover { background:#16213e !important; }
      .year-range-note { font-size:11px; color:#7f8cbc; margin-top:4px; }
    "))),

    tabItems(

      # ════════════════════════════════════════════════
      # TAB 1 — SETUP
      # ════════════════════════════════════════════════
      tabItem(tabName = "setup",
        fluidRow(
          box(title = "📁 Corpus & Output Paths", width = 12,
              status = "primary", solidHeader = TRUE,
            fluidRow(
              column(6,
                h4("Corpus folder", class = "section-title"),
                textInput("corpus_path",
                  label = "Full path to the folder containing the .inline.txt files",
                  value = "",
                  placeholder = "/path/to/coca-wlp/inline"),
                div(class = "callout callout-info",
                  "The folder should contain files named like",
                  tags$code("wlp_acad_1990.inline.txt"),
                  " — both year-based (acad, fic, etc.) and numeric (blog, web)."
                ),
                br(),
                actionButton("validate_path", "✔ Validate path",
                             class = "btn-info", width = "180px"),
                br(), br(),
                uiOutput("path_status")
              ),
              column(6,
                h4("Output folder", class = "section-title"),
                textInput("output_path",
                  label = "Full path to the folder where the CSV will be saved",
                  value = "",
                  placeholder = "/path/to/output/folder"),
                textInput("output_filename",
                  label = "CSV filename",
                  value = "coca_extraction.csv",
                  placeholder = "my_extraction.csv"),
                br(),
                div(class = "callout callout-info",
                  "The file will be written as UTF-8 with BOM for Excel compatibility."
                )
              )
            )
          )
        ),
        fluidRow(
          box(title = "⚙️ Context & Processing Options", width = 12,
              status = "info", solidHeader = TRUE,
            fluidRow(
              column(4,
                h4("Context window", class = "section-title"),
                sliderInput("ctx_words", "Words shown left/right of match",
                            min = 5, max = 30, value = 15, step = 1),
                sliderInput("ctx_chars", "Character buffer for tag search (internal)",
                            min = 200, max = 1200, value = 600, step = 50)
              ),
              column(4,
                h4("Parallel processing", class = "section-title"),
                div(class = "callout callout-info",
                  "Extraction runs in a dedicated background R process via ",
                  tags$code("future::multisession"), " so the UI stays",
                  " fully responsive throughout. No configuration needed."
                )
              ),
              column(4,
                h4("Case sensitivity", class = "section-title"),
                materialSwitch("case_insensitive",
                  label = "Case-insensitive matching",
                  value = TRUE, status = "danger"),
                br(),
                h4("Output columns", class = "section-title"),
                checkboxGroupInput("extra_cols",
                  label = NULL,
                  choices = c(
                    "Full context (combined)"      = "context_full",
                    "Char offset (start position)" = "char_offset"
                  ),
                  selected = "context_full"
                )
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # TAB 2 — PATTERN BUILDER
      # ════════════════════════════════════════════════
      tabItem(tabName = "pattern",
        fluidRow(
          box(title = "🔍 Pattern Builder", width = 7,
              status = "primary", solidHeader = TRUE,
            h4("Quick presets", class = "section-title"),
            selectInput("preset", label = NULL,
                        choices = names(PRESETS), width = "100%"),
            hr(),
            h4("Pattern tokens — one per line", class = "section-title"),
            div(class = "callout callout-info",
              tags$b("Token syntax:"), tags$br(),
              tags$code("WORD/TAG"), "  – exact word + tag (e.g. ", tags$code("how/rgq"), ")", tags$br(),
              tags$code("*/TAG"),    "  – any word with that tag (e.g. ", tags$code("*/jj"), ")", tags$br(),
              tags$code("WORD/*"),   "  – exact word, any tag", tags$br(),
              tags$code("*/[TAG1|TAG2]"), "  – alternative tags (e.g. ", tags$code("*/[vbz|vbr]"), ")", tags$br(),
              tags$code("TOKEN?"),   "  – optional token (add ? at end)", tags$br(),
              tags$code("..."),      "  – flexible gap of 0–3 words", tags$br(),
              "Tags are case-insensitive. Ambiguous COCA tags (e.g. ",
              tags$code("_jj@_nn1"), ") are matched automatically."
            ),
            br(),
            textAreaInput("pattern_tokens",
              label = "Enter one token per line:",
              value = "how/rgq\n*/jj",
              rows = 8, width = "100%"),
            br(),
            h4("Generated regex pattern", class = "section-title"),
            verbatimTextOutput("pattern_preview"),
            br(),
            actionButton("test_pattern", "🧪 Test on first matched file",
                         class = "btn-warning", width = "240px"),
            br(), br(),
            uiOutput("test_result")
          ),

          box(title = "📋 Examples", width = 5,
              status = "info", solidHeader = TRUE,
            h4("Common patterns", class = "section-title"),
            tags$table(class = "table table-condensed",
              style = "color:#b0b8d4; font-size:13px;",
              tags$thead(tags$tr(
                tags$th("Goal"), tags$th("Tokens")
              )),
              tags$tbody(
                tags$tr(tags$td("how + adj"),
                        tags$td(tags$code("how/rgq"), br(), tags$code("*/jj"))),
                tags$tr(tags$td("modal + VVI"),
                        tags$td(tags$code("*/vm"), br(), tags$code("*/vvi"))),
                tags$tr(tags$td("be + adj (is/are)"),
                        tags$td(tags$code("*/[vbz|vbr]"), br(), tags$code("*/jj"))),
                tags$tr(tags$td("adj + noun (with opt. intensifier)"),
                        tags$td(tags$code("*/rg?"), br(), tags$code("*/jj"), br(), tags$code("*/nn1"))),
                tags$tr(tags$td("gap example: verb … noun"),
                        tags$td(tags$code("*/vv0"), br(), tags$code("..."), br(), tags$code("*/nn1"))),
                tags$tr(tags$td("as … as (comparisons)"),
                        tags$td(tags$code("as/*"), br(), tags$code("*/jj"), br(), tags$code("as/*")))
              )
            ),
            br(),
            div(class = "callout callout-warning",
              tags$b("Ditto tags:"), " COCA uses ditto tags for multi-word units",
              " (e.g. ", tags$code("in_ii31 terms_ii32 of_ii33"), "). The extractor",
              " handles these automatically — just match on the tag base (", 
              tags$code("ii"), ")."
            ),
            br(),
            div(class = "callout callout-success",
              tags$b("Ambiguous tags:"), " Some tokens carry multiple tags separated",
              " by ", tags$code("@"), " (e.g. ", tags$code("how_rgq@_rrq"), ").",
              " The extractor matches on the first tag and ignores the rest."
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # TAB 3 — CORPUS SELECTION
      # ════════════════════════════════════════════════
      tabItem(tabName = "corpus",
        fluidRow(
          box(title = "📂 Select Corpus Files", width = 12,
              status = "primary", solidHeader = TRUE,
            fluidRow(
              column(5,
                h4("Genre", class = "section-title"),
                checkboxGroupInput("genres",
                  label = NULL,
                  choices = names(GENRE_MAP),
                  selected = names(GENRE_MAP),
                  inline = FALSE),
                br(),
                div(
                  actionButton("sel_all",  "Select all",  class = "btn-info",
                               style = "margin-right:6px"),
                  actionButton("sel_none", "Clear all",   class = "btn-warning")
                )
              ),
              column(7,
                h4("Year range (for dated genres: acad, fic, mag, news, spok, tvm)",
                   class = "section-title"),
                div(class = "year-range-note",
                  "Blog and Web files have no year in their filename and are",
                  " always included in full when their genre is selected."
                ),
                br(),
                sliderInput("year_range",
                  label = NULL,
                  min = 1990, max = 2019,
                  value = c(1990, 2019),
                  step = 1, sep = "",
                  width = "100%"),
                br(),
                hr(),
                h4("Summary of selected files", class = "section-title"),
                uiOutput("file_summary")
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # TAB 4 — EXTRACT
      # ════════════════════════════════════════════════
      tabItem(tabName = "run",
        fluidRow(
          box(title = "▶️ Run Extraction", width = 12,
              status = "success", solidHeader = TRUE,
            fluidRow(
              column(6,
                h4("Pre-flight checklist", class = "section-title"),
                uiOutput("preflight"),
                br(),
                actionButton("run_extraction", "▶ Start Extraction",
                             class = "btn-success btn-lg",
                             width = "220px"),
                br(), br(),
                actionButton("abort_extraction", "⏹ Abort",
                             class = "btn-danger",
                             width = "120px")
              ),
              column(6,
                h4("Progress", class = "section-title"),
                uiOutput("progress_ui"),
                br(),
                div(class = "progress-log", id = "log_area",
                    textOutput("progress_log"))
              )
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # TAB 5 — RESULTS
      # ════════════════════════════════════════════════
      tabItem(tabName = "results",
        fluidRow(
          valueBoxOutput("vbox_total",   width = 3),
          valueBoxOutput("vbox_genres",  width = 3),
          valueBoxOutput("vbox_years",   width = 3),
          valueBoxOutput("vbox_files",   width = 3)
        ),
        fluidRow(
          box(title = "📊 Extracted Matches", width = 12,
              status = "primary", solidHeader = TRUE,
            fluidRow(
              column(3,
                selectInput("filter_genre", "Filter by genre",
                            choices = c("All genres" = ""),
                            width = "100%")
              ),
              column(3,
                selectInput("filter_year", "Filter by year",
                            choices = c("All years" = ""),
                            width = "100%")
              ),
              column(3,
                textInput("filter_match", "Search in match",
                          placeholder = "e.g. old, big")
              ),
              column(3,
                br(),
                downloadButton("download_csv", "⬇ Download CSV",
                               class = "btn-success", style = "width:100%")
              )
            ),
            hr(),
            DTOutput("results_table")
          )
        )
      ),

      # ════════════════════════════════════════════════
      # TAB 6 — TAG GUIDE
      # ════════════════════════════════════════════════
      tabItem(tabName = "tags",
        fluidRow(
          box(title = "📖 CLAWS7 Tag Reference (COCA)", width = 12,
              status = "info", solidHeader = TRUE,
            p(style = "color:#7fdbca; font-size:13px;",
              "Tags appear after an underscore in the corpus:",
              tags$code("word_TAG"), ". Ambiguous tokens carry multiple tags:",
              tags$code("word_TAG1@_TAG2"), ". The extractor matches on the first tag."),
            br(),
            div(class = "tag-grid",
              lapply(CLAWS7_TAGS, function(t) {
                parts <- stri_split_fixed(t, " – ", n = 2)[[1]]
                div(class = "tag-item",
                    tags$b(parts[1]),
                    if (length(parts) > 1) paste0(" – ", parts[2]) else "")
              })
            )
          )
        )
      )

    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage


# ══════════════════════════════════════════════════════════════════════════════
#  SERVER
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # ── Reactive state ───────────────────────────────────────────────────────────
  rv <- reactiveValues(
    corpus_files = character(0),
    selected_files = character(0),
    results = NULL,
    log_lines = character(0),
    running = FALSE
  )

  # ── Preset → tokens ─────────────────────────────────────────────────────────
  observeEvent(input$preset, {
    val <- PRESETS[[input$preset]]
    if (nchar(val) > 0) {
      updateTextAreaInput(session, "pattern_tokens", value = val)
    }
  })

  # ── Validate corpus path ─────────────────────────────────────────────────────
  observeEvent(input$validate_path, {
    path <- stri_trim_both(input$corpus_path)
    if (!dir.exists(path)) {
      rv$corpus_files <- character(0)
    } else {
      all_f <- list.files(path, pattern = "\\.inline\\.txt$",
                          full.names = TRUE)
      rv$corpus_files <- all_f
    }
  })

  output$path_status <- renderUI({
    n <- length(rv$corpus_files)
    if (n == 0) {
      div(class = "callout callout-warning",
          "⚠ No .inline.txt files found. Check the path and click Validate.")
    } else {
      div(class = "callout callout-success",
          sprintf("✔ Found %d corpus files.", n))
    }
  })

  # ── Selected files (genre + year filter) ────────────────────────────────────
  selected_files <- reactive({
    if (length(rv$corpus_files) == 0) return(character(0))
    chosen_codes <- unlist(GENRE_MAP[input$genres], use.names = FALSE)
    yr <- input$year_range

    Filter(function(f) {
      meta <- parse_filename(f)
      if (is.na(meta$genre) || !meta$genre %in% chosen_codes) return(FALSE)
      if (!is.na(meta$year)) {
        meta$year >= yr[1] && meta$year <= yr[2]
      } else {
        TRUE  # blog/web: always include
      }
    }, rv$corpus_files)
  })

  observeEvent(input$sel_all,  updateCheckboxGroupInput(session, "genres",
    selected = names(GENRE_MAP)))
  observeEvent(input$sel_none, updateCheckboxGroupInput(session, "genres",
    selected = character(0)))

  output$file_summary <- renderUI({
    files <- selected_files()
    if (length(files) == 0) {
      return(div(class = "callout callout-warning",
                 "No files match the current selection."))
    }
    metas <- lapply(files, parse_filename)
    genres_sel <- unique(sapply(metas, `[[`, "genre"))
    years_sel  <- unique(na.omit(sapply(metas, `[[`, "year")))
    div(class = "callout callout-success",
      tags$b(sprintf("%d files selected", length(files))), tags$br(),
      sprintf("Genres: %s", paste(sort(genres_sel), collapse = ", ")),
      tags$br(),
      if (length(years_sel) > 0)
        sprintf("Years: %d – %d", min(years_sel), max(years_sel))
      else
        "Years: N/A (blog/web only)"
    )
  })

  # ── Pattern preview ─────────────────────────────────────────────────────────
  current_pattern <- reactive({
    tokens <- stri_split_lines(stri_trim_both(input$pattern_tokens))[[1]]
    tokens <- tokens[nchar(stri_trim_both(tokens)) > 0]
    if (length(tokens) == 0) return("")
    tryCatch(build_pattern(tokens), error = function(e) paste("ERROR:", e$message))
  })

  output$pattern_preview <- renderText(current_pattern())

  # ── Test pattern on first matching file ──────────────────────────────────────
  observeEvent(input$test_pattern, {
    files <- selected_files()
    pat   <- current_pattern()
    if (length(files) == 0 || nchar(pat) == 0) return()

    found <- FALSE
    for (f in head(files, 20L)) {
      tagged <- tryCatch(read_corpus_file(f), error = function(e) NULL)
      if (is.null(tagged)) next
      locs <- stri_locate_all_regex(tagged, pat,
                                    case_insensitive = input$case_insensitive)[[1]]
      if (!is.na(locs[1, 1]) && nrow(locs) > 0) {
        ms <- locs[1, "start"]; me <- locs[1, "end"]
        snippet <- stri_sub(tagged, max(1L, ms - 120L), min(stri_length(tagged), me + 120L))
        output$test_result <- renderUI({
          div(class = "callout callout-success",
            tags$b(sprintf("✔ %d match(es) found in %s", nrow(locs), basename(f))),
            br(),
            tags$details(
              tags$summary("Show tagged snippet"),
              tags$pre(style = "font-size:11px; color:#7fdbca; background:#0f0f23;
                                padding:8px; border-radius:4px; overflow-x:auto;",
                       snippet)
            )
          )
        })
        found <- TRUE
        break
      }
    }
    if (!found) {
      output$test_result <- renderUI({
        div(class = "callout callout-warning",
            "⚠ No matches found in the first 20 files. Try adjusting your pattern.")
      })
    }
  })

  # ── Pre-flight checklist ─────────────────────────────────────────────────────
  output$preflight <- renderUI({
    items <- list()
    ok <- function(msg) tags$li(style = "color:#16a085", paste("✔", msg))
    err <- function(msg) tags$li(style = "color:#e94560", paste("✗", msg))

    n_files <- length(selected_files())
    items[[1]] <- if (n_files > 0) ok(sprintf("%d corpus files selected", n_files)) else
      err("No corpus files selected")

    pat <- current_pattern()
    items[[2]] <- if (nchar(pat) > 0 && !stri_startswith_fixed(pat, "ERROR")) ok("Pattern is valid") else
      err("Pattern is empty or invalid")

    out_dir <- stri_trim_both(input$output_path)
    items[[3]] <- if (dir.exists(out_dir)) ok("Output folder exists") else
      err("Output folder not found")

    fn <- stri_trim_both(input$output_filename)
    items[[4]] <- if (nchar(fn) > 0 && stri_endswith_fixed(fn, ".csv")) ok("Filename looks good") else
      err("Filename must end in .csv")

    tags$ul(style = "list-style:none; padding-left:0;", items)
  })

  # ── Extraction ───────────────────────────────────────────────────────────────
  #
  # Architecture: the heavy file loop runs inside a future (background process).
  # Progress is written to a temp file line-by-line; a reactiveTimer polls that
  # file every 800 ms and pushes updates to the UI — keeping Shiny fully
  # responsive throughout.  The promise %...>% chain fires when the future
  # resolves and handles success/error in the main session.
  #
  # Shared state across the async boundary:
  rv$log_file  <- NULL   # path to the temp progress log
  rv$n_files   <- 0L     # total files in this run (for the progress bar)
  rv$job       <- NULL   # the future object (for abort detection)

  # Polling timer — active only while a job is running
  log_timer <- reactiveTimer(800)

  observe({
    log_timer()
    req(rv$running, !is.null(rv$log_file), file.exists(rv$log_file))
    lines <- tryCatch(readLines(rv$log_file, warn = FALSE, encoding = "UTF-8"),
                      error = function(e) character(0))
    if (length(lines) > 0) rv$log_lines <- lines
  })

  output$progress_log <- renderText({
    paste(tail(rv$log_lines, 40), collapse = "\n")
  })

  output$progress_ui <- renderUI({
    req(rv$running)
    n   <- rv$n_files
    done <- sum(grepl("^\\[", rv$log_lines))   # count timestamped file lines
    tags$div(
      tags$progress(
        value = done, max = max(n, 1L),
        style = paste0("width:100%; height:18px; border-radius:4px;",
                       " accent-color:#16a085;")
      ),
      tags$small(style = "color:#7fdbca;",
                 sprintf("%d / %d files processed", done, n))
    )
  })

  observeEvent(input$run_extraction, {
    files   <- selected_files()
    pat     <- current_pattern()
    out_dir <- stri_trim_both(input$output_path)
    fn      <- stri_trim_both(input$output_filename)

    # Guards
    if (length(files) == 0)
      { showNotification("No files selected.",          type = "error"); return() }
    if (nchar(pat) == 0 || stri_startswith_fixed(pat, "ERROR"))
      { showNotification("Pattern is empty or invalid.", type = "error"); return() }
    if (!dir.exists(out_dir))
      { showNotification("Output folder not found.",    type = "error"); return() }
    if (!stri_endswith_fixed(fn, ".csv"))
      { showNotification("Filename must end in .csv.",  type = "error"); return() }

    # Snapshot all inputs before handing off to the future
    ctx_words  <- as.integer(input$ctx_words)
    ctx_chars  <- as.integer(input$ctx_chars)
    case_ins   <- isTRUE(input$case_insensitive)
    extra_cols <- input$extra_cols
    out_file   <- file.path(out_dir, fn)
    log_path   <- tempfile(fileext = ".log")

    rv$log_file  <- log_path
    rv$n_files   <- length(files)
    rv$log_lines <- character(0)
    rv$results   <- NULL
    rv$running   <- TRUE

    disable("run_extraction")

    # ── Worker function (runs entirely in background process) ─────────────
    # Must be self-contained: no references to session, rv, or input.
    run_extraction_worker <- function(files, pat, ctx_words, ctx_chars,
                                      case_ins, extra_cols, out_file, log_path) {
      library(stringi)
      library(data.table)

      ts <- function() format(Sys.time(), "%H:%M:%S")
      wlog <- function(...) {
        msg <- paste0("[", ts(), "] ", ...)
        cat(msg, "\n", file = log_path, append = TRUE)
      }

      # ── inline helpers (must be redefined inside the worker) ─────────────
      parse_filename_w <- function(fname) {
        base <- gsub("(\\.inline)?\\.txt$", "", basename(fname))
        m    <- stri_match_first_regex(base, "^wlp_([a-z]+)_(\\d+)$")
        if (is.na(m[1,1]))
          return(list(file_id=base, genre=NA_character_, year=NA_integer_))
        genre   <- m[1,2]
        id_part <- m[1,3]
        year    <- if (nchar(id_part)==4L && as.integer(id_part)>=1900L)
                     as.integer(id_part) else NA_integer_
        list(file_id=base, genre=genre, year=year)
      }
      read_corpus_file_w <- function(fp) {
        raw <- readLines(fp, encoding="latin1", warn=FALSE)
        raw <- stri_encode(raw, from="ISO-8859-1", to="UTF-8")
        tolower(paste(raw, collapse=" "))
      }
      strip_tags_w <- function(x) {
        x <- stri_replace_all_regex(x, "_[a-z][a-z0-9]*(?:@_[a-z][a-z0-9]*)*", "")
        x <- stri_replace_all_regex(x, "[<>@#%$*]", "")
        x <- stri_replace_all_regex(x, "\\s{2,}", " ")
        stri_trim_both(x)
      }
      last_n  <- function(s,n){ toks<-stri_split_regex(stri_trim_both(s),"\\s+")[[1]]; toks<-toks[nchar(toks)>0]; paste(tail(toks,n),collapse=" ") }
      first_n <- function(s,n){ toks<-stri_split_regex(stri_trim_both(s),"\\s+")[[1]]; toks<-toks[nchar(toks)>0]; paste(head(toks,n),collapse=" ") }

      process_file_w <- function(fp, pat, ctx_words, ctx_chars, case_ins) {
        tryCatch({
          tagged <- read_corpus_file_w(fp)
          meta   <- parse_filename_w(fp)
          locs   <- stri_locate_all_regex(tagged, pat,
                                          case_insensitive=case_ins)[[1]]
          if (nrow(locs)==0L || is.na(locs[1L,1L])) return(NULL)
          rows <- lapply(seq_len(nrow(locs)), function(i) {
            ms <- locs[i,"start"]; me <- locs[i,"end"]
            lt <- stri_sub(tagged, max(1L, ms-ctx_chars), ms-1L)
            rt <- stri_sub(tagged, me+1L, min(stri_length(tagged), me+ctx_chars))
            lc <- last_n(strip_tags_w(lt), ctx_words)
            rc <- first_n(strip_tags_w(rt), ctx_words)
            ms_str <- strip_tags_w(stri_sub(tagged, ms, me))
            data.frame(
              file_id=meta$file_id, genre=meta$genre, year=meta$year,
              left_context=lc, match=ms_str, right_context=rc,
              context_full=paste(lc, ms_str, rc),
              stringsAsFactors=FALSE
            )
          })
          rbindlist(rows, fill=TRUE)
        }, error=function(e){ wlog("WARNING in ", basename(fp), ": ", e$message); NULL })
      }

      # ── Main loop ────────────────────────────────────────────────────────
      wlog(sprintf("Starting extraction of %d files…", length(files)))
      wlog(sprintf("Pattern: %s", pat))

      results_list <- vector("list", length(files))
      for (idx in seq_along(files)) {
        f  <- files[idx]
        res <- process_file_w(f, pat, ctx_words, ctx_chars, case_ins)
        results_list[[idx]] <- res
        hits <- if (is.null(res)) 0L else nrow(res)
        wlog(sprintf("[%d/%d] %s — %d hit(s)", idx, length(files), basename(f), hits))
      }

      # ── Combine & write ──────────────────────────────────────────────────
      non_null <- Filter(Negate(is.null), results_list)
      if (length(non_null) == 0) {
        results <- data.table(file_id=character(), genre=character(),
                              year=integer(), left_context=character(),
                              match=character(), right_context=character(),
                              context_full=character())
      } else {
        results <- rbindlist(non_null, fill=TRUE)
      }

      if (!"context_full" %in% extra_cols && "context_full" %in% names(results))
        results[, context_full := NULL]

      fwrite(results, file=out_file, bom=TRUE, quote=TRUE)
      wlog(sprintf("DONE — %d matches written to %s", nrow(results), out_file))

      results   # return value of the future
    }

    # ── Launch the future ─────────────────────────────────────────────────
    rv$job <- future(
      run_extraction_worker(files, pat, ctx_words, ctx_chars,
                            case_ins, extra_cols, out_file, log_path),
      packages = c("stringi", "data.table"),
      seed     = TRUE
    )

    # ── Promise callbacks (fire in the main session when future resolves) ──
    rv$job %...>% (function(results) {
      rv$results <- results
      rv$running <- FALSE
      enable("run_extraction")

      n <- nrow(results)
      showNotification(
        sprintf("✔ Extraction complete — %d matches saved.", n),
        type = "message", duration = 10
      )
      updateTabItems(session, "sidebar", selected = "results")

      genres_avail <- c("All genres" = "", sort(unique(na.omit(results$genre))))
      years_avail  <- c("All years"  = "", as.character(sort(unique(na.omit(results$year)))))
      updateSelectInput(session, "filter_genre", choices = genres_avail)
      updateSelectInput(session, "filter_year",  choices = years_avail)
    }) %...!% (function(err) {
      rv$running <- FALSE
      enable("run_extraction")
      showNotification(paste("Extraction error:", conditionMessage(err)),
                       type = "error", duration = 15)
    })

    NULL  # observeEvent must return NULL; the promise chain is detached
  })

  observeEvent(input$abort_extraction, {
    rv$running <- FALSE
    enable("run_extraction")
    showNotification("Abort requested. The background worker will finish its current file then stop.",
                     type = "warning")
  })

  # ── Results display ──────────────────────────────────────────────────────────
  filtered_results <- reactive({
    req(!is.null(rv$results))
    dt <- copy(rv$results)
    if (nchar(input$filter_genre) > 0)
      dt <- dt[genre == input$filter_genre]
    if (nchar(input$filter_year) > 0)
      dt <- dt[year == as.integer(input$filter_year)]
    if (nchar(stri_trim_both(input$filter_match)) > 0)
      dt <- dt[stri_detect_regex(match, input$filter_match, case_insensitive = TRUE)]
    dt
  })

  output$vbox_total  <- renderValueBox(valueBox(
    if (is.null(rv$results)) "–" else nrow(rv$results),
    "Total matches", icon = icon("list"), color = "red"))

  output$vbox_genres <- renderValueBox(valueBox(
    if (is.null(rv$results)) "–"
    else length(unique(na.omit(rv$results$genre))),
    "Genres", icon = icon("layer-group"), color = "blue"))

  output$vbox_years  <- renderValueBox(valueBox(
    if (is.null(rv$results)) "–"
    else {
      yr <- na.omit(rv$results$year)
      if (length(yr) == 0) "N/A"
      else sprintf("%d–%d", min(yr), max(yr))
    },
    "Year span", icon = icon("calendar"), color = "green"))

  output$vbox_files  <- renderValueBox(valueBox(
    if (is.null(rv$results)) "–"
    else length(unique(rv$results$file_id)),
    "Files with hits", icon = icon("file-alt"), color = "yellow"))

  output$results_table <- renderDT({
    req(!is.null(rv$results))
    dt <- filtered_results()
    datatable(dt,
      options = list(
        pageLength = 20,
        scrollX    = TRUE,
        columnDefs = list(
          list(width = "80px",  targets = c(0, 1, 2)),  # file_id, genre, year
          list(width = "220px", targets = c(3, 5)),     # left/right ctx
          list(width = "140px", targets = 4)            # match
        ),
        dom = "lrtip"
      ),
      rownames   = FALSE,
      class      = "compact stripe"
    ) |>
      formatStyle("match",
        backgroundColor = "#1a1a2e",
        color = "#f9ca24", fontWeight = "bold") |>
      formatStyle("genre",
        backgroundColor = "#0f3460", color = "#fff")
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      fn <- stri_trim_both(input$output_filename)
      if (nchar(fn) == 0) fn <- "coca_extraction.csv"
      fn
    },
    content = function(file) {
      req(!is.null(rv$results))
      fwrite(filtered_results(), file = file, bom = TRUE, quote = TRUE)
    }
  )

} # end server

shinyApp(ui, server)
