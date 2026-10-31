;;; etaf-md-preview-model.el --- Session state for etaf-md-preview -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Session state and navigation of the markdown previewer.  A session is a
;; plist of reactive refs: it owns the parsed document, the palette name, the
;; reading width and the error text, and it can exist without any buffer, so
;; scripts and tests drive the same model the mounted application uses.
;;
;; All display values are read through `etaf-md-preview--read' during render,
;; so writing a ref repaints the mounted preview without a refresh call.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'etaf)
(require 'etaf-ui)
(require 'etaf-md-preview-theme)
(require 'etaf-md-preview-views)

(define-error 'etaf-md-preview-error "Markdown preview error")

(defcustom etaf-md-preview-width-min 40
  "Narrowest reading measure, in character cells.
`etaf-md-preview-adjust-width' clamps its result to this value."
  :type 'integer
  :group 'etaf-md-preview)

(defcustom etaf-md-preview-width-max 96
  "Widest reading measure, in character cells.
A window wider than this measure keeps the measure and centers the column.
A measure wider than this runs a long way past a comfortable line: at 110
cells a paragraph of Chinese holds more than fifty characters per line."

  :type 'integer
  :group 'etaf-md-preview)

(defcustom etaf-md-preview-flavor 'gfm
  "Default Markdown dialect of `etaf-md-preview-create'.
The session's `:flavor' argument overrides it.  `gfm' enables the GitHub
extensions and front matter, `commonmark' parses the core specification only,
and a feature plist toggles individual extensions."
  :type '(choice (const :tag "GitHub Flavored Markdown" gfm)
                 (const :tag "CommonMark only" commonmark)
                 (sexp :tag "Feature plist"))
  :group 'etaf-md-preview)

;;; ---------------------------------------------------------------------------
;;; Refs
;;; ---------------------------------------------------------------------------

(defun etaf-md-preview--ref (session key)
  "Return SESSION's reactive source named KEY."
  (plist-get session key))

(defun etaf-md-preview--read (session key)
  "Return SESSION's current value for KEY, tracked while rendering."
  (etaf-value (etaf-md-preview--ref session key)))

(defun etaf-md-preview--write (session key value)
  "Publish VALUE as SESSION's new value for KEY; return VALUE.
A write that would not change the reading is skipped, because the preview
re-renders on every write."
  (let ((ref (etaf-md-preview--ref session key)))
    (unless (equal (etaf-value ref) value)
      (etaf-set-value ref value))
    value))

;;; ---------------------------------------------------------------------------
;;; Source helpers
;;; ---------------------------------------------------------------------------

(defun etaf-md-preview--remote-url-p (url)
  "Return non-nil when URL points to a remote resource."
  (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*://" url))

(defun etaf-md-preview--read-source (file)
  "Return FILE's text, signaling `etaf-md-preview-error' on failure."
  (unless (file-readable-p file)
    (signal 'etaf-md-preview-error (list (format "Cannot read file: %s" file))))
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun etaf-md-preview--parse (text flavor)
  "Parse TEXT with FLAVOR, reporting failures as `etaf-md-preview-error'.
Parsing is shared with the other document viewers; this package keeps its
own error type at its boundary."
  (condition-case err
      (etaf-ui-markdown-parse text flavor)
    (etaf-ui-markdown-error
     (signal 'etaf-md-preview-error (cdr err)))))

;;; ---------------------------------------------------------------------------
;;; Session
;;; ---------------------------------------------------------------------------

(defun etaf-md-preview--block-source (block text)
  "Return BLOCK's own source slice in TEXT, or nil without a range."
  (let ((begin (plist-get block :begin))
        (end (plist-get block :end)))
    (and (integerp begin) (integerp end) (<= 0 begin end (length text))
         (substring text begin end))))

(defun etaf-md-preview--rendered-block (block)
  "Return BLOCK without the source range it carries.
A block's `:begin' and `:end' move when text above it changes, while the
block displays the same content; comparing what a block displays keeps its
published entry reusable across such an edit."
  (if (or (plist-member block :begin) (plist-member block :end))
      (let ((copy (copy-sequence block)))
        (cl-remf copy :end)
        (cl-remf copy :begin)
        copy)
    block))

(defun etaf-md-preview--block-entries (session document text published)
  "Return DOCUMENT's top-level blocks as keyed publishing entries.
An entry is `(:key KEY :block BLOCK :view CALL)'.  KEY names the block's type
and a digest of the block's own source, plus an occurrence number so two
identical blocks stay distinct.  A block's source, not its position, is its
identity: a block inserted or removed above it leaves every unchanged block's
key, and therefore its Component instance, alone.

PUBLISHED maps the previous publish's keys to their entries.  A block whose
source and parsed content both survive unchanged keeps its published entry,
CALL included, so its instance keeps its Scope, Hosts and video handles, and
the list costs one comparison for it instead of a new call."
  (let ((counts (make-hash-table :test #'equal))
        (entries nil))
    (dolist (block (plist-get document :children))
      (let* ((digest (concat (or (and (plist-get block :type)
                                      (symbol-name (plist-get block :type)))
                                 "block")
                             ":"
                             (secure-hash
                              'sha1
                              (or (etaf-md-preview--block-source block text) ""))))
             (occurrence (1+ (gethash digest counts 0)))
             (key (if (= 1 occurrence) digest (format "%s#%d" digest occurrence)))
             (old (and published (gethash key published))))
        (puthash digest occurrence counts)
        (push (if (and old (equal (etaf-md-preview--rendered-block
                                   (plist-get old :block))
                                  (etaf-md-preview--rendered-block block)))
                  old
                (list :key key :block block
                      :view (etaf-md-preview--block-view session key block)))
              entries)))
    (nreverse entries)))

(defun etaf-md-preview--published-table (entries)
  "Return ENTRIES as a table from each entry's key to the entry."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry entries) (puthash (plist-get entry :key) entry table))
    table))

(defun etaf-md-preview--keyed-document (session document contents published)
  "Return (KEYED . TABLE) for DOCUMENT parsed from CONTENTS.
KEYED is DOCUMENT with its top-level blocks replaced by publishing entries;
TABLE maps each entry's key to the entry, to be published with the document
and passed back as PUBLISHED by the next parse."
  (let ((entries (etaf-md-preview--block-entries
                  session document contents published)))
    (cons (plist-put (copy-sequence document) :children entries)
          (etaf-md-preview--published-table entries))))

(cl-defun etaf-md-preview-create (&key file text title theme flavor)
  "Create a headless preview session and return it.
FILE names a markdown document to read; TEXT supplies the document directly
for buffers and tests.  Exactly one source is required.  TITLE overrides the
document title; THEME selects a palette and defaults to the value of
`etaf-md-preview-theme' (`auto' derives the document colors from the active
frame's faces and paints no page background).  FLAVOR selects the markdown
dialect and defaults to `etaf-md-preview-flavor': `gfm' enables the GitHub
extensions and front matter, `commonmark' parses the core only, and a feature
plist may toggle individual extensions."
  (let* ((file (and file (expand-file-name file)))
         (contents (cond (text text)
                         (file (etaf-md-preview--read-source file))
                         (t (signal 'etaf-md-preview-error
                                    '("etaf-md-preview-create needs :file or :text")))))
         (flavor (or flavor etaf-md-preview-flavor))
         (requested (or theme etaf-md-preview-theme))
         (auto (eq requested 'auto))
         (palette (if auto (etaf-md-preview-auto-theme) requested))
         ;; Every field exists before the first parse, because a published
         ;; block call refers to this session and later writes must stay in
         ;; place rather than build a second plist.
         (session (list :file file
                        :text (unless file contents)
                        :flavor flavor
                        :features nil
                        :dir (if file (file-name-directory file)
                               default-directory)
                        :document (etaf-ref nil :name 'etaf-md-preview-document)
                        ;; Publishing entries of the last parse, keyed by block
                        ;; identity; an unchanged block keeps its entry.
                        :published nil
                        ;; The rendering environment shared by the blocks of
                        ;; one appearance.
                        :env-cache nil
                        :title (etaf-ref (or title
                                             (and file
                                                  (file-name-nondirectory file))
                                             "Markdown")
                                        :name 'etaf-md-preview-title)
                        :theme (etaf-ref palette :name 'etaf-md-preview-theme)
                        ;; `auto' derives its surface palette from the active
                        ;; frame's faces and paints no page background; a named
                        ;; palette paints its page.
                        :theme-source (etaf-ref (if auto 'frame 'palette)
                                                :name 'etaf-md-preview-theme-source)
                        :theme-mode (etaf-ref requested :name 'etaf-md-preview-theme-mode)
                        :width (etaf-ref 92 :name 'etaf-md-preview-width)
                        :error (etaf-ref nil :name 'etaf-md-preview-error-text)
                        :buffer nil
                        ;; Cards whose video plays inline; written by
                        ;; activation and read while rendering so a press
                        ;; repaints exactly the video cards.
                        :video-active (etaf-ref nil :name 'etaf-md-preview-video-active)
                        ;; One stable native-playback handle per card key so a
                        ;; repaint re-registers the same handle instead of
                        ;; invalidating the card's activation closure.
                        :video-handles (make-hash-table :test #'equal))))
    (etaf-md-preview--install session contents title)
    session))

(defun etaf-md-preview--video-card (session url occurrence)
  "Return (KEY . HANDLE) for URL's OCCURRENCE in SESSION.
A document can show the same source on several cards; the occurrence number
counts the cards in render order so each owns its own player.  KEY names the
card in SESSION's active-video set and HANDLE registers its player while the
card is playing inline."
  (let* ((table (plist-get session :video-handles))
         (key (list url occurrence))
         (handle (or (and (hash-table-p table) (gethash key table))
                     (let ((new (make-symbol "etaf-md-preview-video")))
                       (when (hash-table-p table)
                         (puthash key new table))
                       new))))
    (cons key handle)))

(defun etaf-md-preview-state (session)
  "Return SESSION's state as a detached plist snapshot.
Keys are `:file', `:title', `:dir', `:theme', `:background', `:width',
`:outline', `:error', `:blocks' (top-level block count) and `:footnotes'
\(labels).  `:background' is `palette' when the preview paints its palette's
page background, and nil when it leaves the Emacs theme's background."
  (let ((document (etaf-md-preview--read session :document)))
    (list :file (plist-get session :file)
          :title (etaf-md-preview--read session :title)
          :dir (plist-get session :dir)
          :flavor (plist-get session :flavor)
          :theme (etaf-md-preview--read session :theme)
          :background (when (eq (etaf-md-preview--read session :theme-source) 'palette)
                        'palette)
          :width (etaf-md-preview--read session :width)
          :outline (copy-tree (plist-get document :outline))
          :error (etaf-md-preview--read session :error)
          :blocks (length (plist-get document :children))
          :footnotes (mapcar (lambda (footnote) (plist-get footnote :label))
                             (plist-get document :footnotes)))))

(defun etaf-md-preview--install (session contents &optional title)
  "Parse CONTENTS and publish it as SESSION's document; return the document.
TITLE, when given, names the document instead of its own front matter or file.
The parser and the title fallback are shared by the first parse and every
refresh path: reading the file again, following a save, and following an edit
that was not saved.  Each top-level block keeps the publishing entry it had
while its source and parsed content survive, so one edited block repaints
itself and every other block keeps its instance."
  (let* ((keyed (etaf-md-preview--keyed-document
                 session
                 (etaf-md-preview--parse contents (plist-get session :flavor))
                 contents
                 (plist-get session :published)))
         (document (car keyed)))
    (etaf-batch
      (etaf-set-value (etaf-md-preview--ref session :document) document)
      (etaf-set-value (etaf-md-preview--ref session :title)
                      (or title
                          (plist-get document :title)
                          (and (plist-get session :file)
                               (file-name-nondirectory (plist-get session :file)))
                          "Markdown"))
      (etaf-set-value (etaf-md-preview--ref session :error) nil))
    (plist-put session :features (plist-get document :features))
    (plist-put session :published (cdr keyed))
    document))

(defun etaf-md-preview--publish (session contents)
  "Publish CONTENTS as SESSION's document and return SESSION."
  (etaf-md-preview--install session contents)
  session)

(defun etaf-md-preview-refresh (session)
  "Re-read and re-parse SESSION's document.
Return SESSION.  A read failure signals `etaf-md-preview-error' and leaves the
published document untouched."
  (let ((file (plist-get session :file)))
    (etaf-md-preview--publish
     session
     (if file (etaf-md-preview--read-source file)
       (or (plist-get session :text) "")))))

(defun etaf-md-preview-set-theme (session theme)
  "Show SESSION with palette THEME; return SESSION.
THEME is `auto', `dark', `light' or `sepia'; anything else signals
`etaf-md-preview-error'.  Selecting a palette also paints its page
background, while `auto' returns to the face-derived surfaces."
  (unless (or (eq theme 'auto) (assq theme etaf-md-preview-themes))
    (signal 'etaf-md-preview-error
            (list (format "Unknown markdown preview theme: %S" theme))))
  (etaf-batch
    (etaf-md-preview--write session :theme-mode theme)
    (etaf-md-preview--write session :theme
                             (if (eq theme 'auto)
                                 (etaf-md-preview-auto-theme)
                               theme))
    (etaf-md-preview--write session :theme-source
                             (if (eq theme 'auto) 'frame 'palette)))
  session)

(defun etaf-md-preview-cycle-theme (session)
  "Switch SESSION to the next palette and return its name."
  (let* ((themes (mapcar #'car etaf-md-preview-themes))
         (current (etaf-md-preview--read session :theme))
         (tail (cdr (memq current themes))))
    (etaf-md-preview-set-theme session (or (car tail) (car themes)))
    (etaf-md-preview--read session :theme)))

;;; ---------------------------------------------------------------------------
;;; Width and layout fitting
;;; ---------------------------------------------------------------------------

(defun etaf-md-preview--clamp-width (width)
  "Return WIDTH clamped to the supported reading measure."
  (max etaf-md-preview-width-min
       (min etaf-md-preview-width-max (round width))))

(defun etaf-md-preview-adjust-width (session delta)
  "Widen SESSION's maximum reading measure by DELTA cells and return it.
The window may still be narrower than the returned cap, in which case the
column fills the window."
  (let ((width (etaf-md-preview--clamp-width
                (+ (etaf-md-preview--read session :width) delta))))
    (etaf-md-preview--write session :width width)
    width))

(defun etaf-md-preview--fit-width (session)
  "Set SESSION's reading measure from the window that shows it.
The stored width is the maximum measure: the column is viewport-relative, so
a later resize narrows it without changing this value.  The window's own
measure excludes the gutter the column keeps on each side.  A headless
session keeps its current width.  Return the width in effect."
  (let* ((buffer (plist-get session :buffer))
         (window (and (buffer-live-p buffer) (get-buffer-window buffer t))))
    (if (window-live-p window)
        (let ((width (etaf-md-preview--clamp-width
                      (- (window-body-width window)
                         (* 2 etaf-md-preview--measure-gutter)))))
          (etaf-md-preview--write session :width width)
          width)
      (etaf-md-preview--read session :width))))

;;; ---------------------------------------------------------------------------
;;; Navigation
;;; ---------------------------------------------------------------------------

(defun etaf-md-preview-outline (session)
  "Return SESSION's outline as a list of (LEVEL TEXT ID)."
  (copy-tree (plist-get (etaf-md-preview--read session :document) :outline)))

(defun etaf-md-preview--heading-match-forward (text)
  "Search forward for heading TEXT at the start of a line.
Return the match start, or nil.  Leading blank characters (including the
display spaces the renderer uses for centering) may precede the match."
  (let ((found nil))
    (while (and (not found) (search-forward text nil t))
      (let ((line-start (line-beginning-position)))
        (when (string-blank-p
               (buffer-substring-no-properties line-start (match-beginning 0)))
          (setq found (match-beginning 0)))))
    found))

(defun etaf-md-preview--heading-positions (session)
  "Return SESSION's heading buffer positions as a list of (INDEX . POSITION).
The scan walks the outline in document order, so repeated heading texts map to
successive occurrences."
  (let ((buffer (plist-get session :buffer))
        (outline (etaf-md-preview-outline session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (cl-loop for entry in outline
                   for index from 0
                   for position = (etaf-md-preview--heading-match-forward (nth 1 entry))
                   when position collect (cons index position)))))))

(defun etaf-md-preview-locate (session index)
  "Move point to SESSION's outline heading INDEX and recenter.
Return the position, or nil when the heading is not visible."
  (let* ((buffer (plist-get session :buffer))
         (position (cdr (assq index (etaf-md-preview--heading-positions session)))))
    (when (and position (buffer-live-p buffer))
      (with-selected-window (or (get-buffer-window buffer t) (selected-window))
        (with-current-buffer buffer
          (goto-char position)
          (recenter 0)))
      position)))

(defun etaf-md-preview--adjacent-heading (session direction)
  "Move point to the next outline heading in DIRECTION.
DIRECTION is 1 for forward and -1 for backward.  Return the position moved to,
or nil when there is no heading left in that direction."
  (let* ((positions (etaf-md-preview--heading-positions session))
         (buffer (plist-get session :buffer))
         (point-now (if (buffer-live-p buffer)
                        (with-current-buffer buffer (point))
                      0))
         (target (if (> direction 0)
                     (cl-find-if (lambda (cell) (> (cdr cell) point-now)) positions)
                   (car (last (cl-remove-if-not
                               (lambda (cell) (< (cdr cell) point-now))
                               positions))))))
    (when target
      (etaf-md-preview-locate session (car target)))))

;;; ---------------------------------------------------------------------------
;;; Opening targets
;;; ---------------------------------------------------------------------------

(defun etaf-md-preview--open-url (session url)
  "Open URL referenced by SESSION's document.
In-document anchors jump to the matching heading; remote targets use the
browser; local paths open in Emacs."
  (cond
   ((string-prefix-p "#" url)
    (let ((anchor (substring url 1)))
      (cl-loop for entry in (etaf-md-preview-outline session)
               for index from 0
               when (equal (nth 2 entry) anchor)
               return (etaf-md-preview-locate session index)
               finally (message "No heading matches #%s" anchor))))
   ((etaf-md-preview--remote-url-p url)
    (browse-url url))
   (t
    (let ((path (expand-file-name url (or (plist-get session :dir)
                                          default-directory))))
      (if (file-exists-p path)
          (find-file-other-window path)
        (message "File not found: %s" path))))))

(defun etaf-md-preview--open-file (path)
  "Open PATH with the operating system's default application.
Emacs's `browse-url' routes local non-HTML files back into Emacs, which would
show a video as a binary buffer; media cards therefore ask the desktop to open
the file directly."
  (let ((opener (pcase system-type
                  ('darwin "open")
                  ('gnu/linux "xdg-open")
                  (_ nil))))
    (if (and opener (executable-find opener))
        (call-process opener nil 0 nil path)
      (browse-url-of-file path))))

(defun etaf-md-preview--jump-footnote (session label)
  "Move point to footnote LABEL in SESSION's buffer."
  (let ((buffer (plist-get session :buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (if (search-forward (format "[%s]" (or (plist-get
                                                (cl-find label
                                                         (plist-get
                                                          (etaf-md-preview--read
                                                           session :document)
                                                          :footnotes)
                                                         :key (lambda (footnote)
                                                                (plist-get footnote :label))
                                                         :test #'equal)
                                                :number)
                                               label))
                             nil t)
            (progn (beginning-of-line) (recenter 0))
          (message "Footnote %s not found" label))))))

;;; ---------------------------------------------------------------------------
;;; Rendering environment
;;; ---------------------------------------------------------------------------

(defun etaf-md-preview--page-background (session)
  "Return the page background SESSION paints, or nil for none.
The session paints its palette's background only after an explicit palette
selection; the face-derived `auto' appearance leaves the active Emacs theme's
background visible instead."
  (when (eq (etaf-md-preview--read session :theme-source) 'palette)
    (etaf-md-preview-color :bg (etaf-md-preview--read session :theme))))

(defun etaf-md-preview--display-frame (session)
  "Return the frame showing SESSION, or nil when it has no window.
A face query answers for a frame, so the palette is derived from the frame the
preview is read in rather than from whichever frame is selected."
  (let ((buffer (plist-get session :buffer)))
    (when (buffer-live-p buffer)
      (when-let* ((window (get-buffer-window buffer t)))
        (window-frame window)))))

(defun etaf-md-preview--session-palette (session)
  "Return SESSION's surface palette, or nil for the named palette colors.
Sessions created with the `auto' theme derive their surfaces from the faces of
the frame showing them, so panels, tables and code blocks match the user's own
theme.  A session that has not selected a palette explicitly uses the
face-derived palette; a named palette returns its own color alist so the shared
renderer never needs a theme name."
  (if (eq (etaf-md-preview--read session :theme-source) 'palette)
      (cdr (assq (etaf-md-preview--read session :theme) etaf-md-preview-themes))
    (etaf-md-preview--palette-from-faces
     (etaf-md-preview--display-frame session))))

(defun etaf-md-preview--frame-surface (frame)
  "Return FRAME's default foreground and background.
The face-derived palette follows these two faces, so a theme switch changes
them and the shared environment is rebuilt instead of keeping the previous
palette.  A session shown in no window derives its palette from the selected
frame, so FRAME nil reads that frame."
  (if frame
      (cons (face-attribute 'default :foreground frame)
            (face-attribute 'default :background frame))
    (cons (face-attribute 'default :foreground)
          (face-attribute 'default :background))))

(defun etaf-md-preview--env-inputs (session)
  "Return the values SESSION's rendering environment is built from.
Every value the environment prints is read here, once.  Reading them is what
makes the shared environment follow a palette, width or theme change, so a new
input belongs both in this list and in `etaf-md-preview--build-env'.  A
face-derived palette also reads the appearance, which is what makes a block
re-render after a theme switch instead of keeping the faces it first read."
  (let* ((source (etaf-md-preview--read session :theme-source))
         (face-derived (not (eq source 'palette)))
         (frame (etaf-md-preview--display-frame session)))
    (list :theme (etaf-md-preview--read session :theme)
          :source source
          :width (etaf-md-preview--read session :width)
          :background (etaf-md-preview--page-background session)
          :surface (and face-derived (etaf-md-preview--frame-surface frame))
          :appearance (and face-derived (etaf-appearance))
          :dir (plist-get session :dir)
          :features (plist-get session :features))))

(defun etaf-md-preview--build-env (session)
  "Build SESSION's rendering environment from its current inputs."
  (list :theme (etaf-md-preview--read session :theme)
        :palette (etaf-md-preview--session-palette session)
        :width (etaf-md-preview--read session :width)
        :background (etaf-md-preview--page-background session)
        :dir (plist-get session :dir)
        :features (plist-get session :features)
        :open-url (lambda (url) (etaf-md-preview--open-url session url))
        :open-link (lambda (url) (etaf-md-preview--open-url session url))
        :open-file #'etaf-md-preview--open-file
        :render-media #'etaf-md-preview--render-media
        :alert-labels etaf-md-preview--alert-labels
        :video-active-p
        (lambda (key)
          (member key (etaf-md-preview--read session :video-active)))
        :activate-video
        (lambda (key)
          (etaf-set-value
           (etaf-md-preview--ref session :video-active)
           (cons key (etaf-md-preview--read session :video-active))))
        :footnote-jump (lambda (label) (etaf-md-preview--jump-footnote session label))))

(defun etaf-md-preview--env (session)
  "Return SESSION's rendering environment, reused until one of its inputs changes.
A block Component reads this plist in its own render, so its colors and
measure follow the session; a publish that changes no input hands every
retained block the same environment object, which keeps its props unchanged."
  (let ((inputs (etaf-md-preview--env-inputs session)))
    (let ((cached (plist-get session :env-cache)))
      (if (equal (car cached) inputs)
          (cdr cached)
        (let ((env (etaf-md-preview--build-env session)))
          (plist-put session :env-cache (cons inputs env))
          env)))))

(defun etaf-md-preview--card-env (session env scope)
  "Return ENV with video card identities for one render pass of SCOPE.
SCOPE names the pass, so two cards of one source stay distinct while a card
keeps its key across repaints of the same block.  ENV is shared between
publishes, so the occurrence counter cannot live in it."
  (let ((occurrences (make-hash-table :test #'equal)))
    (plist-put (copy-sequence env) :video-card
               (lambda (url)
                 (etaf-md-preview--video-card
                  session url
                  (list scope (cl-incf (gethash url occurrences 0))))))))


(provide 'etaf-md-preview-model)
;;; etaf-md-preview-model.el ends here
