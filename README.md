# ETAF Markdown Preview

ETAF Markdown Preview renders a local Markdown document as a modern reading
view inside Emacs. It draws the document with [ETAF](https://gitea.vhkd.top/geekinney/etaf)
components and the [Ebox](https://gitea.vhkd.top/geekinney/ebox) renderer: a
centered reading measure, a typographic heading scale, fenced code blocks with
language tags, blockquotes, nested lists, task markers, pipe tables, thematic
breaks, numbered footnotes, clickable links, and local image and video cards.
The whole document publishes into one ordinary Emacs buffer, so native
scrolling, isearch and window splitting keep working.

![The previewer at a wide frame](examples/etaf-md-preview-wide.png)

## Capabilities

- CommonMark blocks and inlines: ATX and Setext headings, paragraphs, emphasis
  and strong emphasis, strikethrough, inline code, links (inline, reference and
  shortcut), autolinks, images, fenced and indented code, blockquotes, nested
  ordered/unordered lists, thematic breaks and character references.
- GitHub extensions: pipe tables with alignment, task list items, bare URL
  autolinks and numbered footnotes.
- GitHub alerts (`> [!NOTE]` and friends) render as titled callout blocks.
- A `:flavor` argument selects `gfm` (default), `commonmark`, or a feature
  plist that toggles individual extensions.
- YAML front matter is stripped from the body and feeds the document title.
- Local images render through ETAF's image Host (graphics in a graphical
  frame, terminal conversion otherwise); local videos render a poster frame
  through the same pipeline, with a play caption that opens the video.
- Links open in the browser, jump to in-document headings, or open local files;
  a missing media file becomes a labeled card instead of an error.
- Three fixed palettes (dark, light and sepia) plus a face-derived default
  (`etaf-md-preview-theme' = `auto') that follows the active Emacs theme and
  paints no page background; all colors are published as `:ui-*` theme tokens
  so shared ETAF UI components follow the preview.
- Outline navigation (`n`/`p`, `TAB`), imenu integration, reloading (`r`), an
  adjustable maximum reading measure (`[`/`]`) and native scrolling. A window
  resize reflows through the framework's viewport update without rebuilding
  the view.
- Four user options: `etaf-md-preview-theme`, `etaf-md-preview-flavor`,
  `etaf-md-preview-width-min` and `etaf-md-preview-width-max`.

## Prerequisites

| Requirement | Why | Check |
| --- | --- | --- |
| Emacs 29.1 or newer | the ETAF runtime and Ebox | `emacs --version` |
| ETAF, ETAF UI, Ebox and ECSS on `load-path` | rendering and the component catalog | see Installation |
| ffmpeg (optional) | video poster frames and terminal image conversion | `ffmpeg -version` |

## Installation

Clone this repository next to the framework checkouts so the directories are
siblings, then add each repository root to `load-path` and require the entry:

```elisp
(add-to-list 'load-path "/path/to/emacs-taf/ecss")
(add-to-list 'load-path "/path/to/emacs-taf/ebox")
(add-to-list 'load-path "/path/to/emacs-taf/etaf-ui")
(add-to-list 'load-path "/path/to/emacs-taf/etaf")
(add-to-list 'load-path "/path/to/emacs-taf/etaf-apps/etaf-md-preview")
(require 'etaf-md-preview)
```

`M-x etaf-md-preview-open` then prompts for a file. A package installation that
exposes the root entry through package.el does not need the manual
`add-to-list` for this repository.

## Usage

```
M-x etaf-md-preview-open RET README.md RET
```

| Key | Action |
| --- | --- |
| `n` / `p` | next / previous heading |
| `TAB` | choose a heading |
| `RET` or mouse-1 | activate the link, image or video under point |
| `r` | re-read and repaint the document |
| `t` | cycle dark / light / sepia |
| `[` / `]` | narrow / widen the reading measure |
| `q` | close the preview |
| `C-s`, `C-v`, wheel | native isearch and scrolling |

## Development

```sh
make check            # structure, compilation, API bindings, acceptance
make benchmark        # batch render baseline for a generated large document
```

See [docs/manual.md](docs/manual.md) for workflows, the supported syntax and
scripting, and [docs/architecture.md](docs/architecture.md) for the module
layout and the rendering decisions.
