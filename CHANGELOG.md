# Changelog

All notable user-visible changes to ETAF Markdown Preview. This project follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

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
