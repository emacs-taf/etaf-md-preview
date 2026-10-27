;;; etaf-md-preview.el --- Modern Markdown preview in ETAF -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: ETAF contributors
;; Version: 0.1.8
;; Package-Requires: ((emacs "29.1") (etaf "0.3.0") (etaf-ui "0.2.0"))
;; Keywords: hypermedia, docs
;; URL: https://github.com/emacs-taf/etaf-md-preview

;;; Commentary:

;; Public API
;;
;; Load this package with (require 'etaf-md-preview) after adding this
;; repository and the ETAF, ETAF UI, Ebox and ECSS roots to `load-path'.  The
;; previewer renders a local Markdown document as a modern reading view: a
;; centered measure with a typographic heading scale, fenced code blocks with
;; language tags, blockquotes, nested lists, task list markers, pipe tables,
;; thematic breaks, footnotes, clickable links and inline images, plus local
;; image and video cards whose poster frame is prepared through ETAF's image
;; Host; activating a video card plays it inline through ETAF's video
;; Component.  Front matter is stripped into document metadata.  The whole
;; document publishes into one buffer with native Emacs scrolling, isearch
;; and window splitting, so no pane owns a private scrolling model.
;;
;; A session is the unit of state.  `etaf-md-preview-create' builds one from a
;; file or from text, `etaf-md-preview-open' displays it in its own buffer,
;; the navigation and display functions below drive it, and
;; `etaf-md-preview-state' reports it as plain data.  Every displayed value is
;; an ETAF reactive source, so a reload, a width change or a palette change
;; repaints the mounted preview without a refresh call.
;;
;; Only the interfaces listed below are supported for use by other packages.
;; All unlisted symbols and implementation features are internal, including
;; names without a double hyphen.  Do not require files below lisp/ from
;; consumers.  This Commentary is the authoritative export list;
;; implementations and precise docstrings remain beside their definitions, and
;; the manual contains workflows.
;;
;; Function: etaf-md-preview-doctor ()
;; Return capability facts for reading Markdown.  The shape matches
;; `etaf-install-doctor'; ffmpeg is optional and only affects video cards and
;; terminal image conversion.
;; Function: etaf-md-preview-open (&optional source)
;;;###autoload (autoload 'etaf-md-preview-open "etaf-md-preview" nil t)
;; Mount a markdown preview and return its buffer.
;; SOURCE is a file path, a session from `etaf-md-preview-create', or nil to
;; prompt for a file.  Interactively it defaults to the current buffer's
;; markdown file and then to a file prompt; a file that already has a preview
;; buffer is selected instead of rendered twice.  Opening a session mounts
;; that session, so callers can render a document they built themselves.  The
;; buffer is an ETAF application (`etaf-app-open') whose root keys add outline
;; navigation, reloading, theming and closing; the reading measure starts from
;; the showing window and later resizes reflow through the framework's viewport
;; update, so the document is never rebuilt for a window geometry change.
;; Closing the buffer releases the session's Runtime, so reopening the same
;; file renders a fresh preview.
;;
;; Function: etaf-md-preview-close ()
;;;###autoload (autoload 'etaf-md-preview-close "etaf-md-preview" nil t)
;; Unmount the current preview buffer and kill it.
;; Signals `user-error' when the current buffer is not a preview.
;;
;; Function: etaf-md-preview-view (&optional session)
;;;###autoload (autoload 'etaf-md-preview-view "etaf-md-preview" nil t)
;; Return the preview View for SESSION.
;; SESSION defaults to a new empty session; pass a session from
;; `etaf-md-preview-create' to mount a real document yourself with
;; `etaf-mount', for example inside a tab or a larger layout.
;;
;; Function: etaf-md-preview-create (&key file text title theme flavor)
;; Return a new headless preview session.
;; Exactly one of FILE (a markdown document read from disk) or TEXT (the
;; document itself) is required; a missing or unreadable FILE signals
;; `etaf-md-preview-error'.  TITLE overrides the document title and THEME
;; selects a palette from `etaf-md-preview-themes', defaulting to the frame's
;; background mode.  FLAVOR selects the markdown dialect: `gfm' (the default)
;; enables the GitHub extensions (tables, task lists, strikethrough, autolinks,
;; footnotes and front matter), `commonmark' parses the core specification
;; only, and a feature plist toggles individual extensions.  The session owns
;; no buffer, so it is a complete headless preview for scripts and tests.
;;
;; Function: etaf-md-preview-state (session)
;; Return SESSION's state as a detached plist snapshot.
;; Keys are `:file', `:title', `:dir', `:theme', `:flavor', `:background',
;; `:width', `:outline' (a list of (LEVEL TEXT ID)), `:error', `:blocks'
;; \(top-level block count) and `:footnotes' (definition labels in reference
;; order).  `:background' is `palette' when the preview paints its palette's
;; page background and nil when it keeps the Emacs theme's background.  The
;; snapshot copies every value, so mutating it never changes the session; use
;; it to render outside ETAF or to assert behavior in tests.
;;
;; Function: etaf-md-preview-refresh (session)
;; Re-read and re-parse SESSION's document and return it.
;; A session created from text re-parses the original text; a file session
;; re-reads the file.  A read failure signals `etaf-md-preview-error' and
;; leaves the published document untouched.
;;
;; Function: etaf-md-preview-set-theme (session theme)
;; Show SESSION with the palette THEME and return it.
;; THEME is `auto' or a key of `etaf-md-preview-themes' (`dark', `light' or
;; `sepia'); any other value signals `etaf-md-preview-error'.  `auto' returns
;; to the face-derived surfaces and paints no page background.
;;
;; Function: etaf-md-preview-cycle-theme (session)
;; Switch SESSION to the next palette and return its name.
;;
;; Function: etaf-md-preview-outline (session)
;; Return SESSION's outline as a list of (LEVEL TEXT ID), in document order.
;;
;; Function: etaf-md-preview-next-heading ()
;;;###autoload (autoload 'etaf-md-preview-next-heading "etaf-md-preview" nil t)
;; Move point to the next outline heading of the current preview.
;; Reports "No further heading" when the document has no later heading and
;; signals `user-error' when the current buffer is not a preview.
;;
;; Function: etaf-md-preview-previous-heading ()
;;;###autoload (autoload 'etaf-md-preview-previous-heading "etaf-md-preview" nil t)
;; Move point to the previous outline heading of the current preview.
;; Reports "No earlier heading" at the first heading and signals
;; `user-error' when the current buffer is not a preview.
;;
;; Function: etaf-md-preview-locate (session index)
;; Move point to SESSION's outline heading INDEX and recenter.
;; INDEX is zero-based into `etaf-md-preview-outline'.  Return the buffer
;; position, or nil when SESSION has no live buffer or the heading is not
;; visible.
;;
;; Function: etaf-md-preview-adjust-width (session delta)
;; Widen SESSION's maximum reading measure by DELTA character cells and return
;; the new maximum.  The result is clamped to `etaf-md-preview-width-min' and
;; `etaf-md-preview-width-max'; a window narrower than the maximum fills the
;; available columns instead.
;;
;; Variable: etaf-md-preview-theme (default `auto')
;; Initial palette of `etaf-md-preview-create'.  `auto' derives the document
;; colors from the active frame's own faces (default, shadow, outline-1 and the
;; semantic faces) and paints no page background, so panels, tables, code
;; blocks and rules match whatever Emacs theme is active; `dark', `light' and
;; `sepia' select that fixed palette explicitly, including its page background.
;; `etaf-md-preview-set-theme' and `etaf-md-preview-cycle-theme' always select
;; an explicit palette, so the page background follows the chosen theme.  An
;; open preview keeps a live theme cell that a panel's title-bar switch also
;; writes, so a host can pin a fixed page or return to the face-derived one.
;;
;; Variable: etaf-md-preview-flavor (default `gfm')
;; Default Markdown dialect of `etaf-md-preview-create' when it is not given a
;; `:flavor': `gfm', `commonmark', or a feature plist.
;;
;; Variable: etaf-md-preview-width-min (default 40)
;; Narrowest reading measure in character cells.
;;
;; Variable: etaf-md-preview-width-max (default 110)
;; Widest reading measure in character cells; a wider window keeps this measure
;; and centers it.
;;
;; Error: etaf-md-preview-error
;; Invalid markdown preview session or document input
;; Catch this condition with condition-case; failed calls do not return a value.

;;; Code:

(defgroup etaf-md-preview nil
  "Markdown preview appearance and reading behavior."
  :group 'text)

;;;###autoload
(eval-and-compile
  (add-to-list 'load-path
               (expand-file-name "lisp"
                                 (file-name-directory
                                  (or load-file-name
                                      (bound-and-true-p byte-compile-current-file)
                                      buffer-file-name)))))

(require 'etaf-ui)
(require 'etaf-md-preview-theme)
(require 'etaf-md-preview-model)
(require 'etaf-md-preview-views)
(require 'etaf-md-preview-app)

(defun etaf-md-preview-doctor ()
  "Return capability facts for reading Markdown in ETAF.
The shape matches `etaf-install-doctor': :required and :optional lists of
(LABEL AVAILABLE REMEDY).  Reading text needs no external program."
  (list :required nil
        :optional
        (list (list "ffmpeg" (and (executable-find "ffmpeg") t)
                    "Optional: install ffmpeg to play video cards and convert terminal images."))))


(provide 'etaf-md-preview)
;;; etaf-md-preview.el ends here
