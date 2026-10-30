# Changelog

## Unreleased

### Added

- `etaf-md-preview` opens the preview in a window beside the
  source and keeps the point in the source text, so one command from a
  Markdown buffer sets up editing with a live preview.
- A preview opened from a file follows that file: saving the source, or
  changing it on disk from another program, repaints the document after the
  write settles, so an editor buffer and its preview sit side by side.  `r` still reloads on
  demand.  A preview closed with `q` or killed stops following, so a watch and
  a repaint never outlive the buffer that shows them.

### Changed

- `etaf-md-preview-close` frees the preview's window when the frame has another
  window, so a preview opened beside its source closes back onto the source
  instead of leaving a second window on the same file.

- The preview opens as an ETAF application: its keys are declared on the
  root Host, so `n`, `p` and `q` reach heading navigation and closing
  instead of the renderer's line scrolling and `quit-window`, and the page
  background follows a palette change without a separate repaint.
  `etaf-md-preview-mode-map` is gone; the keys are listed in `?` help.
  Requires ETAF 0.3.0 and ETAF UI 0.2.0.

### Fixed

- Headings keep a color in a theme that paints its own heading color as its
  body color (`outline-1` within 12% contrast of the body ink), which several
  dark themes do: the palette then derives the heading color from the accent,
  mixed toward the page background, so headings stay distinguishable without
  competing with links.
- The default reading measure is 96 cells instead of 110. At 110 a paragraph of
  Chinese runs past fifty characters per line, which is a long way back for the
  eye; the measure stays adjustable with `[` and `]`.
- The face-derived palette follows an Emacs theme switch. It is derived from the
  faces of the frame that shows the preview and reads `etaf-appearance' inside
  the derivation, so switching from a dark to a light theme repaints the
  document instead of leaving the old theme's ink on the new page.
- A window that fills the reading measure keeps a two-cell gutter on each side,
  so text no longer starts on the window edge. The column also clips at that
  gutter, which stops a table's right border from being extended across the
  window's last column as a thick bar. A window narrower than the minimum
  measure plus the gutters keeps its margins and clips a block too wide to
  shrink, such as a table that must keep its words whole.
- Selecting the palette the `auto` theme had already resolved to applies the
  named palette's surfaces and page background; the palette source is now
  reactive, so the selection repaints even when the palette name is unchanged.
- The entry resolves its own `lisp/` directory while byte-compiling as well as at
  load time. Installing the archive compiled the entry with `load-file-name`
  unset, so `etaf-md-preview-theme` was not found and `package.el` reported
  "Cannot open load file".

## Unreleased

### Added

- `etaf-md-preview-doctor` reports the optional ffmpeg capability in the same
  shape as `etaf-install-doctor`; reading Markdown needs no external program.

All notable user-visible changes to ETAF Markdown Preview. This project follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

- `?` and `C-c h` share searchable, themed help inside the preview, with
  concise labels from the effective bindings. Escape exits one layer at a
  time; clicking outside closes help and restores the reading focus.

- Local videos play inside the preview through ETAF's native video Component:
  the card shows a still poster until RET (or a click) mounts the player in
  place; further presses toggle play/pause. Without ffmpeg the card keeps the
  poster and opens in the system player as before.
- Media cards reuse the measured pixel size of an unchanged file instead of
  re-probing it on every preview. The probe records path, size and
  modification time, so an updated image or video is still measured from its
  new bytes. Terminal frames render the media placeholder without attempting
  a pixel probe that needs a graphic frame.

### Fixed

- Note, tip, important, warning and caution callouts end at their last body
  line without an extra padded blank row; normal block spacing is unchanged.
- Heading links override the heading color without duplicate properties, keep
  the heading typography, and remain inline with surrounding heading text.
- The video play caption activates the same desktop-player action as its poster;
  pressing RET on the caption no longer leaves the video unopened.

## [0.1.0] - 2026-10-03

### Added

- Render a local Markdown document as a modern reading view:
  `M-x etaf-md-preview-open`.
- CommonMark blocks and inlines: headings (ATX and Setext), paragraphs,
  emphasis, strong emphasis, strikethrough, inline code, links, autolinks,
  images, fenced and indented code, blockquotes, nested lists, thematic
  breaks, character references, backslash escapes and hard line breaks.
- GitHub extensions: pipe tables drawn as a full grid with an outer frame,
  task list items, bare URL autolinks and numbered footnotes; GitHub alerts
  (`> [!NOTE]` and friends) render as titled callout blocks.
- A `:flavor` argument selects `gfm` (the default), `commonmark`, or a feature
  plist that toggles individual extensions.
- YAML front matter is stripped; its `title` becomes the document title and
  its author, date, version, license and status keys render in a title area
  with tag chips.
- Local images render inline through the ETAF image Host; local videos render
  a poster frame with a play caption; remote and missing media render labeled
  placeholder cards that open on activation.
- Dark, light and sepia palettes, published as ETAF UI theme tokens, with
  `t` cycling and `etaf-md-preview-set-theme` pinning. The default
  `etaf-md-preview-theme` value `auto` derives the document colors from the
  active Emacs theme's faces and paints no page background, so panels, tables
  and code blocks match whatever theme is in use.
- User options for the initial palette, Markdown dialect and reading-measure
  bounds (`etaf-md-preview-theme`, `etaf-md-preview-flavor`,
  `etaf-md-preview-width-min`, `etaf-md-preview-width-max`).
- Outline navigation (`n`/`p`, `TAB`), imenu integration, reloading, an
  adjustable maximum reading measure and whole-document native scrolling; a
  window resize reflows through the renderer's viewport update without
  rebuilding the view.
- Headless sessions through `etaf-md-preview-create`/`-view`/`-state` for
  scripts and tests.
