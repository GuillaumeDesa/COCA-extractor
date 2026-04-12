# Changelog

All notable changes to COCA Extractor are documented here.

## [1.0.1] — 2026

### Added
- Six-tab Shiny dashboard: Setup, Pattern Builder, Corpus, Extract, Results, Tag Guide
- Token grammar builder with support for `WORD/TAG`, `*/TAG`, `WORD/*`, alternative tags `[TAG1|TAG2]`, optional tokens `TOKEN?`, and fuzzy gaps `...`
- Live regex preview in the Pattern Builder tab
- Pattern tester: runs the current pattern against the first matching file and shows a raw tagged snippet
- Six built-in presets (how + adjective, modal + VVI, adjective + noun, be + adjective, verb + preposition, determiner + adjective + noun)
- Genre selection: Academic, Blog, Fiction, Magazine, News, Spoken, TV & Movies, Web
- Year range slider (1990–2019) for the six dated genres; blog and web files included in full
- Async extraction via `future::future()` + `promises`, keeping the UI responsive throughout
- Live progress log polled every 800 ms from a temp file written by the background worker
- Pre-flight checklist on the Extract tab
- Tag stripping handling simple tags, ambiguous/double tags (`_jj@_nn1`), ditto tags (`_ii21`), and COCA separator symbols
- Latin-1 → UTF-8 re-encoding on file read
- Metadata columns: `file_id`, `genre`, `year`
- Context columns: `left_context`, `match`, `right_context`, optional `context_full`
- Results DataTable with genre, year, and match-text filters
- CSV download (UTF-8 with BOM) of the current filtered view
- Full CLAWS7 tag reference in the Tag Guide tab
- `shinyjs`-powered button disable/enable during extraction to prevent double-firing
