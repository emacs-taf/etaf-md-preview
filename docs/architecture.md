# ETAF Markdown Preview architecture

## Module layout

| File | Responsibility |
| --- | --- |
| `etaf-md-preview.el` | Package entry: the public API inventory, the feature closure and the `lisp/` load-path bootstrap. |
| `lisp/etaf-md-preview-parser.el` | Pure Elisp markdown parser: text to a semantic block/inline tree. |
| `lisp/etaf-md-preview-theme.el` | Palettes, semantic color lookup, typography helpers and the ETAF UI token plist. |
| `lisp/etaf-md-preview-model.el` | Session state, source reading, refresh, reading measure, outline navigation and target opening. |
| `lisp/etaf-md-preview-views.el` | Presentation: markdown tree plus environment to ETAF views. |
| `lisp/etaf-md-preview-app.el` | The Component, the buffer, the keymap and the interactive commands. |

Only ETAF and ETAF UI public interfaces are used by the application; the
buffer is an ordinary Emacs buffer rendered by Ebox through ETAF. The
previewer never calls an `ebox-` function directly.

## Data flow

```
file or text → parser → document plist → session refs → views → ETAF mount → Ebox publication
                                         ↑
                          theme / width / errors (reactive refs)
```

- **Parser.** `etaf-md-preview--parse` is a pure function over a string. It
  strips YAML front matter, reads block structure line by line, collects link
  reference and footnote definitions, then parses inline content. The output
  carries no presentation facts.
- **Model.** A session is a plist of `etaf-ref`s: document, title, theme,
  reading measure and error text. Every displayed value is read through
  `etaf-md-preview--read` while rendering, so a refresh, a theme change or a
  measure change repaints the mounted preview through the ordinary ETAF
  reactive path; no code calls the renderer or the buffer directly. A window
  resize is not a session change: the column is viewport-relative and reflows
  through the renderer's own viewport update.
- **Views.** `etaf-md-preview--view-document` composes a centered reading
  column from `etaf-node` values: flexible spacers center a column whose width
  stretches to the window up to the session's measure. Inline content becomes
  styled `text` runs; links and footnote references carry `:on-press`
  boundaries that ETAF turns into Ebox keymaps. Media blocks use the ETAF
  `image` Host, which owns graphics and terminal adapters and the video poster
  frame.
- **Application.** `etaf-md-preview-app` is the only place that reads the
  session's refs; it also provides the palette tokens for its subtree. The
  shell adds what a Component cannot own: the buffer, the keymap, the initial
  measure and the commands.

## Rendering decisions

**Whole-document native scrolling.** The document is rendered as one tall
buffer instead of a fixed-height canvas with an internal scroll owner. A
window then scrolls the real buffer, so isearch, marks, `goto-char` and window
splitting behave normally, and heading navigation is a buffer search instead
of a scroll-offset protocol. The reading column is centered with flexible
spacers and stretches to the window up to the session's measure. The measure
is the only width the application owns, so a window resize is a framework
viewport update: the view is not rebuilt and the session does not change.

**Inline runs are separate `text` Hosts.** Inline style is expressed as one
`text` Host per run inside a normal `box` with `word` wrapping; Ebox composes
the children into one wrapped flow. Ebox's optional native layout module
represents a single fused child per Box, so it must not accept an inline Box
with several children: `ebox-native-reflow--compile-box` now declines that
form and the Elisp renderer produces the output. This keeps one authoritative
inline-composition algorithm and avoids duplicating it in the accelerator.

**Media through the ETAF image Host.** Images and video posters are prepared
by Ebox's image protocol (`ebox-image-prepare`/`ebox-image-create`) behind the
public ETAF `image` Host. The app computes the cell grid from the source's
pixel size (ffprobe for videos) and the frame metrics, caps the height, and
wraps the image in an interactive Box for opening. Missing files and remote
URLs never reach the Host; they become labeled placeholder cards.

**Session as the only state.** There is no second copy of the outline, theme
or document. Navigation recomputes heading positions from the live buffer on
demand; the state snapshot copies values for tests and scripts. This keeps the
model usable without a buffer and keeps rendering reproducible from the
document plus the environment.

## Performance envelope

`benchmarks/performance-scenarios.el` registers this application's two
accepted budgets with the workspace performance driver:

- `md-preview.mount.manual` parses and mounts the maintained manual from a
  fresh session into a fresh buffer: 100 ms.
- `md-preview.refit.width` cycles the reading measure through several widths
  on a mounted manual through the retained native frame: 10 ms.

Both scenarios run in batch and GUI modes. The recorded baseline on the shared
development machine (Emacs 31.1, aarch64, load average around 3-5) is:

| Mode | Mount p50 | Refit p50 | Mount allocation |
| --- | --- | --- | --- |
| batch | ~346 ms (15 samples) | ~21 ms (15 samples) | ~2.2M cons |
| GUI | ~500 ms (5 samples) | ~34 ms (5 samples) | ~6.7M cons |

A mount sample that lands on a collection carries 65-160 ms of garbage
collection, so allocation is on the latency path, not beside it. Graphical
frames also register one display template per image cell, which is why the
GUI mount allocates about three times as much as the batch mount. Advised
phase times for one batch mount: layout compilation ~48 ms, the native
session frame ~40 ms, tape materialization ~21 ms, source-cluster
compilation ~15 ms, fragment-owner projection ~12 ms, property-template
registration ~11 ms and layout-control JSON ~2 ms. Retention-index building
and fragment-owner projection are linear already.

The remaining gap is structural: every mount compiles a fresh layout package
and creates a fresh native session, while the refit path reuses both. Meeting
the mount budget therefore requires reusing a compiled package and its native
session across mounts of an unchanged document, not tuning individual render
steps. On the refit path the residual cost is transaction-scoped by design:
each candidate forks and releases a native session, and rollback retains a
full-range surface snapshot.

## Verification

- Public behaviour tests mount real runtimes in batch Emacs and read the
  visible buffer back (text, faces, keymaps, placeholders). They are
  inventoried in `tests/acceptance.json` and run by `make acceptance`.
- `scripts/md-preview-gui-probe.el` checks the frame-level invariants a batch
  run cannot see: no line wider than its window, an interactive link present,
  a nonempty outline.
- `benchmarks/render-large-document.el` prints parse, mount and refresh times
  for a deterministic generated document; it is a comparison baseline, not a
  gate.
- `benchmarks/performance-scenarios.el` registers the mount and refit
  workloads with the workspace driver (`--mode batch|gui`); the budgets and
  the diagnostic passes live there rather than in `make check`.
