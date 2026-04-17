# COCA Extractor

A Shiny application for extracting POS-tagged patterns from the **Corpus of Contemporary American English (COCA)** — WLP inline format, CLAWS7 tagset — with left/right context, genre and year filtering, and CSV export.

> **Corpus required.** This app works with the raw WLP files purchased from Mark Davies at [corpusdata.org](https://www.corpusdata.org/). The corpus is not bundled with this repository and cannot be.

---

## Table of contents

- [Requirements](#requirements)
- [Installation](#installation)
- [Launching the app](#launching-the-app)
- [Quick start](#quick-start)
- [Interface guide](#interface-guide)
  - [Setup](#setup)
  - [Pattern Builder](#pattern-builder)
  - [Corpus](#corpus)
  - [Extract](#extract)
  - [Results](#results)
  - [Tag Guide](#tag-guide)
- [Token grammar reference](#token-grammar-reference)
- [Output format](#output-format)
- [How the extraction works](#how-the-extraction-works)
- [Known limitations](#known-limitations)
- [Adapting to other corpora](#adapting-to-other-corpora)
- [Licence](#licence)

---

## Requirements

### R packages

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "DT",
  "stringi",
  "data.table",
  "future",
  "promises",
  "shinyjs"
))
```

All packages are available from CRAN. Tested on R ≥ 4.1.

### Corpus files

You need the **COCA WLP inline** files — the format in which each token appears as `word_TAG` with words separated by spaces, distributed across plain text files named:

```
wlp_acad_1990.inline.txt   ← academic genre, year 1990
wlp_blog_01.inline.txt     ← blog genre, file 01 (no year)
wlp_web_29.inline.txt      ← web genre, file 29 (no year)
```

All files for your purchased genres should live in a **single flat folder** — no subfolders. That path is all the app needs.

---

## Installation

No installation in the traditional sense. Download or clone this repository:

```bash
git clone https://github.com/YOUR-USERNAME/coca-extractor.git
```

The app is a single self-contained file: `coca_extractor_app.R`.

---

## Launching the app

From R or RStudio:

```r
shiny::runApp("path/to/coca_extractor_app.R")
```

Or, if your working directory is already the repository root:

```r
shiny::runApp("coca_extractor_app.R")
```

The app opens in your default browser. If it opens in the RStudio viewer pane instead, click **Open in Browser** — the results table and tag guide are more comfortable at full width.

---

## Quick start

1. **Setup tab** → enter your corpus folder path → click *Validate path* → confirm the file count → enter an output folder and filename.
2. **Pattern tab** → choose a preset or type your own tokens (one per line) → check the regex preview → optionally click *Test on first matched file*.
3. **Corpus tab** → tick the genres you want → set the year range.
4. **Extract tab** → verify the preflight checklist is all green → click *Start Extraction*.
5. **Results tab** → filter, browse, download.

---

## Interface guide

### Setup

| Field | Description |
|---|---|
| Corpus folder | Full path to the directory containing your `.inline.txt` files. Click *Validate* to confirm the app can find them. |
| Output folder | Directory where the CSV will be written. Must already exist. |
| CSV filename | Must end in `.csv`. Default: `coca_extraction.csv`. |
| Context words | Number of words shown to the left and right of each match in the output (5–30). |
| Character buffer | Internal window (in characters) used when slicing the raw tagged string for context. Increase if your context words are being cut off by very long tokens. |
| Case-insensitive | Whether the pattern match ignores case. On by default; COCA files are lowercased on read regardless. |
| Full context column | Include a `context_full` column combining left context, match, and right context in a single field. |

### Pattern Builder

The core of the app. Enter one **token** per line. Each token specifies one position in the sequence you are looking for. The app assembles your tokens into a regular expression (shown in the *Generated regex* box) and searches the corpus with it.

See [Token grammar reference](#token-grammar-reference) for the full syntax.

Six **presets** are available from the dropdown as starting points:

| Preset | Tokens |
|---|---|
| how + adjective | `how/rgq` · `*/jj` |
| modal + verb infinitive | `*/vm` · `*/vvi` |
| adjective + noun | `*/jj` · `*/nn1` |
| be + adjective | `*/[vbz\|vbr]` · `*/jj` |
| verb + preposition | `*/vv0` · `*/ii` |
| determiner + adjective + noun | `*/dd1` · `*/jj` · `*/nn1` |

The **Test on first matched file** button runs the pattern against up to 20 files and shows you a raw tagged snippet around the first hit. Use this before committing to a full extraction.

### Corpus

| Control | Description |
|---|---|
| Genre checkboxes | Select any combination of the eight COCA genres. *Select all* and *Clear all* buttons available. |
| Year slider | Filters files for the six dated genres (academic, fiction, magazine, news, spoken, TV & movies). Range: 1990–2019. Blog and web files have no year and are always included in full when their genre is selected. |
| File summary | Shows the number of selected files, genres, and year span. Updates live as you adjust the controls. |

### Extract

The **preflight checklist** runs before every extraction and checks four conditions:

- At least one corpus file is selected
- The pattern is non-empty and valid
- The output folder exists
- The filename ends in `.csv`

All four must show ✔ before the Start button will do anything useful.

**During extraction**, a live log updates every 800 ms, showing each file as it is processed and its hit count. The extraction runs in a background R process (`future::multisession`) so the interface remains fully responsive. You can browse other tabs while waiting.

The **Abort** button sets a flag that stops the loop after the current file completes. It does not kill the background process mid-file.

### Results

Displays the extraction output in a searchable DataTable with three filters:

- **Genre** — filter to a single genre
- **Year** — filter to a single year
- **Search in match** — substring or regex match against the `match` column

Summary boxes at the top show: total matches, number of genres represented, year span, and number of files that returned at least one hit.

The **Download CSV** button writes the *currently filtered* view (not the full results) to a UTF-8 file with byte-order mark, which opens without encoding issues in Excel on all platforms.

### Tag Guide

A two-column reference listing all CLAWS7 tags used in COCA with brief descriptions and examples. Available at any time without leaving the app.

---

## Token grammar reference

Each line in the Pattern Builder text box is one **token**, representing one word-position in the sequence. Tokens are combined in order with a whitespace separator between positions.

### Basic syntax

| Token | Meaning | Example |
|---|---|---|
| `WORD/TAG` | Exact word, exact tag | `how/rgq` |
| `*/TAG` | Any word, exact tag | `*/jj` |
| `WORD/*` | Exact word, any tag | `how/*` |
| `*/*` or `*/` | Any word, any tag | `*/*` |

### Alternative tags

Use `[TAG1|TAG2]` in the tag position to match either tag:

```
*/[vbz|vbr]     # is or are
*/[jj|jjr]      # adjective or comparative adjective
*/[nn1|nn2]     # singular or plural common noun
```

### Optional tokens

Append `?` to make a token optional (zero or one occurrence):

```
*/rg?           # optional degree adverb (very, so, too)
*/jj?           # optional adjective
```

### Fuzzy gap

Use `...` on its own line to allow a gap of zero to three words between adjacent tokens:

```
*/vv0
...
*/nn1
```

This matches a base-form verb followed by a noun with up to three intervening words.

### Worked examples

**`how` + adjective:**
```
how/rgq
*/jj
```

**`be` + optional adverb + adjective (`is very tall`, `are quite old`, `is tall`):**
```
*/[vbz|vbr]
*/rg?
*/jj
```

**Modal + optional adverb + verb infinitive:**
```
*/vm
*/rr?
*/vvi
```

**`as` + adjective + `as` (equative construction):**
```
as/*
*/jj
as/*
```

**Verb + short gap + noun:**
```
*/vv0
...
*/nn1
```

### Notes on COCA-specific tagging

- **Ambiguous tokens** carry multiple tags separated by `@`, e.g. `how_rgq@_rrq`. The pattern matches on the *first* tag only; secondary tags are ignored.
- **Ditto tags** mark multi-word units with a two-digit suffix, e.g. `in_ii31 terms_ii32 of_ii33`. Match on the tag base (`ii`) and the suffix is ignored.
- The COCA files are lowercased on read, so all matching is effectively case-insensitive regardless of the setting.

---

## Output format

The CSV file is **UTF-8 with BOM** (readable by Excel without encoding issues). Each row is one match.

| Column | Description |
|---|---|
| `file_id` | Corpus filename without extension, e.g. `wlp_acad_1990.inline` |
| `genre` | Genre code: `acad`, `blog`, `fic`, `mag`, `news`, `spok`, `tvm`, `web` |
| `year` | Four-digit year, or `NA` for blog and web files |
| `left_context` | Up to N words immediately preceding the match (tags stripped) |
| `match` | The matched sequence (tags stripped) |
| `right_context` | Up to N words immediately following the match (tags stripped) |
| `context_full` | *(optional)* Concatenation of left context, match, and right context |

N is set by the *Context words* slider in Setup (default: 15).

---

## How the extraction works

1. Each selected corpus file is read with `latin-1` encoding and immediately re-encoded to `UTF-8`. This is not optional: the raw COCA files contain characters that corrupt silently if read as UTF-8 directly. The entire file is then collapsed to a single lowercase string.

2. `stringi::stri_locate_all_regex()` searches the string for all non-overlapping occurrences of the assembled pattern.

3. For each match, the app slices a buffer of 600 characters on each side (adjustable in Setup), strips all POS tags from those buffers using a second regex pass, and takes the outermost N words as left and right context.

4. Tag stripping handles: simple tags (`_jj`), ambiguous/double tags (`_jj@_nn1`), ditto-tag suffixes (`_ii21`), and the stray `@`, `<`, `>`, `#`, `%`, `$`, `*` symbols used as separators in the COCA files.

5. Metadata (genre, year, file identifier) is parsed from the filename. Both naming conventions are recognised: `wlp_genre_YYYY.inline.txt` (dated) and `wlp_genre_NN.inline.txt` (undated).

6. The extraction loop runs inside a `future::future()` call — a true background R process — so the Shiny UI remains responsive throughout. Progress is written line-by-line to a temporary file; a `reactiveTimer` in the main session polls it every 800 ms and pushes updates to the log panel and progress bar.

7. When the future resolves, a `promises` callback fires in the main session, stores the results in a reactive value, and navigates the UI to the Results tab.

---

## Known limitations

**Speed.** Files are processed sequentially in a single background worker. On a typical laptop, a simple two-token pattern across all genres and all years (≈ 230 files) takes between 5 and 15 minutes depending on pattern complexity and hardware. Per-file parallelisation is planned but not yet implemented.

**Pattern expressiveness.** The token grammar handles the large majority of POS-sequence queries, but it is not a full query language. There is no support for:
- Distance constraints beyond the fuzzy `...` gap (0–3 words)
- Agreement or co-reference constraints across positions
- Lookahead or lookbehind assertions

The generated regex is shown in the Pattern Builder tab. Advanced users can inspect it and adapt it for use directly in R with `stringi` if the grammar is insufficient.

**Memory.** Large result sets are held in memory until downloaded. This is rarely an issue in practice, but bear it in mind for very frequent patterns on large genre selections.

**Regex edge cases.** The pattern builder escapes literal word characters, but unusual token inputs (e.g. words containing regex metacharacters) may produce unexpected results. The *Test* button is your first line of defence.

---

## Adapting to other corpora

The extraction logic is largely corpus-agnostic. The assumptions baked in are:

1. Annotation format: `word_TAG` tokens separated by whitespace
2. File encoding: `latin-1` (re-encoded to UTF-8 on read)
3. Filename convention: `wlp_genre_identifier.inline.txt`

If your corpus uses a different annotation format or filename convention, you will need to modify:

- `read_corpus_file()` — encoding and collapsing strategy
- `parse_filename()` — genre and year extraction from filename
- `strip_tags()` — the tag-stripping regex (currently tuned for CLAWS7 and COCA symbols)
- `GENRE_MAP` — the genre labels and codes shown in the UI

The CLAWS7 tagset and `word_TAG` format are shared with several other corpora, including the British National Corpus (BNC) in its flat-text distribution. Adapting the app for BNC-style files would require only a new `parse_filename()` function and a minor adjustment to `strip_tags()`. Adapting it for corpora in CoNLL-U (dependency annotation) or XML formats would require more substantial changes to the read and search pipeline, as those formats are not amenable to flat-string regex search.

---

## Licence

CC-BY-NC-ND-4.0. See [`LICENSE`](LICENSE) for details.

The COCA corpus data itself is **not covered by this licence** and is subject to the terms of your individual purchase agreement with Mark Davies / Brigham Young University.

---

## Citation

If you use this app in research or teaching, a citation would be appreciated:

```
Desagulier, Guillaume (2025). COCA Extractor: a Shiny application for POS-tagged
pattern extraction from COCA. GitHub: https://github.com/YOUR-USERNAME/coca-extractor
```

---

## Acknowledgements

The token grammar and tag-stripping logic were developed for research on constructional patterns in American English. The CLAWS7 tagset documentation is due to the UCREL group at Lancaster University.
