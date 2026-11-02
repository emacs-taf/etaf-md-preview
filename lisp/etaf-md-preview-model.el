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

(defun etaf-md-preview--session-text (session)
  "Return SESSION's current source text, reading its file buffer when it has one."
  (let ((source (plist-get session :source-buffer)))
    (if (buffer-live-p source)
        (with-current-buffer source
          (buffer-substring-no-properties (point-min) (point-max)))
      (or (plist-get session :text) ""))))

(defun etaf-md-preview--table-skeleton (block)
  "Return BLOCK's table as the pipe-and-dashes source that defines it.
A table with no cells left paints an empty grid, which gives the caret nothing
to stand on; the skeleton is what lets a reader fill it back in."
  (let* ((columns (max 1 (length (or (plist-get block :aligns)
                                     (plist-get block :header)))))
         (cells (number-sequence 1 columns))
         (bar (concat "|" (mapconcat (lambda (_) "     |") cells "")))
         (rule (concat "|" (mapconcat (lambda (_) "-----|") cells ""))))
    (concat bar "\n" rule "\n" bar)))

(defun etaf-md-preview--block-marker (block)
  "Return the source skeleton BLOCK paints when its render has no content.
A heading carries its hashes, a list item its bullet, a quote its angle, a
fence its backticks, a table its pipes and rule: these are the characters the
render folds away, and showing them once the content is gone is what lets a
reader see what is left and type it back."
  (pcase (plist-get block :type)
    ('heading (concat (make-string (max 1 (or (plist-get block :level) 1)) ?#) " "))
    ('list-item (if-let* ((start (plist-get block :start)))
                    (format "%s. " start)
                  "- "))
    ('blockquote "> ")
    ('thematic-break "---")
    ('code-block (let ((info (or (plist-get block :info)
                                 (plist-get block :lang) "")))
                   (if (string-empty-p info) "```" (concat "```" info))))
    ('table (etaf-md-preview--table-skeleton block))
    (_ nil)))

(defun etaf-md-preview--blank-p (value)
  "Return non-nil when VALUE holds no text."
  (string-empty-p (string-trim (or value ""))))

(defun etaf-md-preview--block-content (block text)
  "Return BLOCK's own text in TEXT without its marker and line break.
The rendered cells of a block stand for this text, so an empty result means the
block paints nothing and the page would show a gap where its source sits."
  (when-let* ((begin (plist-get block :begin))
              (end (plist-get block :end))
              ((integerp begin))
              ((integerp end))
              ((<= 0 begin end (length text))))
    (let* ((slice (substring text begin end))
           (marker (etaf-md-preview--block-marker block)))
      (when (and marker (string-prefix-p marker slice))
        (setq slice (substring slice (length marker))))
      (string-trim slice))))

(defun etaf-md-preview--inline-text (node)
  "Return the text NODE paints.
A parsed cell holds a list of inline nodes rather than one node, and an inline
node holds its own children, so this walks whatever shape it is given."
  (cond ((stringp node) node)
        ((null node) "")
        ((and (consp node) (keywordp (car node)))
         (or (plist-get node :text)
             (mapconcat #'etaf-md-preview--inline-text (plist-get node :children) "")))
        ((listp node)
         (mapconcat #'etaf-md-preview--inline-text node ""))
        (t "")))

(defun etaf-md-preview--table-blank-p (block)
  "Return non-nil when BLOCK's table holds no cell text.
Only the pipes, dashes and padding that define the grid are left, so the render
paints an empty grid and the caret has nowhere to stand inside it."
  (cl-every (lambda (row)
              (cl-every (lambda (cell)
                          (etaf-md-preview--blank-p (etaf-md-preview--inline-text cell)))
                        (if (listp row) row nil)))
            (remq nil (cons (plist-get block :header) (plist-get block :rows)))))

(defun etaf-md-preview--block-empty-p (block &optional text)
  "Return non-nil when BLOCK paints no content, so the page must show its marker.
Deleting a heading's text is the case a reader meets first, but the rule holds
for every block that carries a structure: a list item, a quote, a fence and a
table all keep a skeleton in the source once their text is gone, and that
skeleton is what lets the caret stand there and type it back.

The block answers this from its own fields.  A render can run before the
session publishes its text, and a block judged by that text would be painted as
if it had content and keep that paint."
  (and (etaf-md-preview--block-marker block)
       (pcase (plist-get block :type)
         ('table (etaf-md-preview--table-blank-p block))
         ((or 'heading 'list-item 'blockquote 'code-block)
          ;; A quote keeps its text in its children and a heading keeps it in
          ;; `:text'; both answer the same question through the same walk.
          (etaf-md-preview--blank-p (etaf-md-preview--inline-text block)))
         (_ (when text
              (etaf-md-preview--blank-p (etaf-md-preview--block-content block text)))))))

(defun etaf-md-preview--edit-slice (source)
  "Return (TEXT . TRAILING-NEWLINES) for SOURCE as shown in an edit session.
TEXT drops the block's final newline and counts it in TRAILING-NEWLINES, so
the write-back keeps the blank line that separates the block from the next
one, whatever the editor leaves at the end."
  (if (and (stringp source) (string-suffix-p "\n" source))
      (cons (substring source 0 -1) 1)
    (cons (or source "") 0)))

(defconst etaf-md-preview--edit-ref 'etaf-md-preview-edit-raw
  "Host reference of the single block shown as source while it is edited.
One block is edited at a time, so one stable reference serves every session and
the Runtime can report the host the session opens on.")

(defconst etaf-md-preview--block-ref-prefix "etaf-md-preview-block-"
  "Prefix of the symbol naming a block's box for point-to-block lookup.")

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

(defun etaf-md-preview--gap-blocks (blocks text)
  "Return a blank block for each stretch of blank TEXT holds between BLOCKS.
A blank line is a line the document paints nothing on: the page would leave a
gap where the source holds a line, so a reader who presses RET to start a
paragraph has no row to type on and no cell to stand the caret on.  Painting
one empty row per blank line is what keeps a source offset and the cell it is
painted on in step, which is what lets the caret stay where the reader put it.

A block's range ends after its own final newline, so every stretch the blocks
leave uncovered is blank -- between two blocks, before the first, after the
last.  A file that ends with a newline ends on an empty line, which is where
RET at the end of the document puts the caret, so that position gets a row
standing for the text past its last character.  An empty file gets that row."
  (let* ((size (length text))
         (blocks (or blocks nil))
         (cursor 0)
         (result nil))
    (dolist (block blocks)
      (let ((begin (or (plist-get block :begin) size))
            (end (or (plist-get block :end) cursor)))
        (when (etaf-md-preview--blank-range-p text cursor begin)
          (push (etaf-md-preview--blank-block begin cursor text) result))
        (setq cursor (max cursor end))))
    (cond ((< cursor size)
           (when (etaf-md-preview--blank-range-p text cursor size)
             (push (etaf-md-preview--blank-block size cursor text) result)))
          ((or (= size 0) (eq (aref text (1- size)) ?\n))
           (push (list :type 'blank :begin size :end (1+ size) :lines 1) result)))
    (nreverse result)))

(defun etaf-md-preview--blank-range-p (text begin end)
  "Return non-nil when TEXT holds only blank space between BEGIN and END.
What the blocks leave out is not necessarily blank: front matter is metadata
the page shows as its title and subtitle rather than as lines, and painting a
row for it would open the page with the rows the file spends on it."
  (and (> end begin) (string-blank-p (substring text begin end))))

(defun etaf-md-preview--blank-block (end begin text)
  "Return a blank block covering TEXT between BEGIN and END, one row per line.
An empty line is a newline, so the stretch holds as many lines as it holds
newlines; the page paints exactly that many empty rows."
  (list :type 'blank :begin begin :end end
        :lines (max 1 (cl-count ?\n text :start begin :end end))))

(defun etaf-md-preview--paintable-blocks (blocks text)
  "Return BLOCKS in source order with their blank stretches filled in as rows.
Both lists are ordered by source position already, so one merge keeps the page
in the order the file holds."
  (let ((blanks (etaf-md-preview--gap-blocks blocks text))
        (result nil))
    (while (or blocks blanks)
      (cond
       ((null blanks) (push (pop blocks) result))
       ((null blocks) (push (pop blanks) result))
       ((<= (or (plist-get (car blocks) :begin) 0)
            (or (plist-get (car blanks) :begin) 0))
        (push (pop blocks) result))
       (t (push (pop blanks) result))))
    (nreverse result)))

(defun etaf-md-preview--block-entries (session document text published)
  "Return DOCUMENT's top-level blocks as keyed publishing entries.
An entry is `(:key KEY :block BLOCK :editing FLAG :ref REF :view CALL)'.
KEY names the block's type and a digest of the block's own source, plus an
occurrence number so two identical blocks stay distinct.  A block's source,
not its position, is its identity: a block inserted or removed above it leaves
every unchanged block's key, and therefore its Component instance, alone.

The list also carries a blank block for every stretch of blank the file holds
between its blocks, because the page must paint a row a reader can type on
there; a blank row holds no source text, so its key is its place among them.

PUBLISHED maps the previous publish's keys to their entries.  A block whose
source, parsed content and editing state all survive keeps its published entry,
CALL included, so its instance keeps its Scope, Hosts and video handles, and
the list costs one comparison for it instead of a new call.  The block being
edited builds a fresh call: that call carries the source it shows and the
reference the edit session opens on."
  (let ((counts (make-hash-table :test #'equal))
        (editing-key (plist-get session :editing-key))
        (blank-index 0)
        (entries nil))
    (dolist (block (etaf-md-preview--paintable-blocks
                    (plist-get document :children) text))
      (let* ((blank (eq (plist-get block :type) 'blank))
             (source (or (etaf-md-preview--block-source block text) ""))
             (digest (unless blank
                       (concat (or (and (plist-get block :type)
                                        (symbol-name (plist-get block :type)))
                                   "block")
                               ":"
                               (secure-hash 'sha1 source))))
             ;; A blank row paints no source text, so the only thing that tells
             ;; one from another is where it sits among the blank rows.
             (digest (or digest (format "blank:%d" (cl-incf blank-index))))
             (occurrence (1+ (gethash digest counts 0)))
             (key (if (= 1 occurrence) digest (format "%s#%d" digest occurrence)))
             (editing (equal key editing-key))
             (ref (make-symbol etaf-md-preview--block-ref-prefix))
             (old (and published (gethash key published)))
             (same (and old
                        (equal (etaf-md-preview--rendered-block
                                (plist-get old :block))
                               (etaf-md-preview--rendered-block block))
                        (eq (and (plist-get old :editing) t) editing))))
        (puthash digest occurrence counts)
        (push (cond
               ;; Only the call is retained: the block itself is the freshly
               ;; parsed one, so its source offsets follow an edit above it.
               (same (list :key key :block block :editing nil
                           :ref (plist-get old :ref)
                           :view (plist-get old :view)))
               (editing (list :key key :block block :editing t
                              :ref ref
                              :view (etaf-md-preview--block-view
                                     session key block ref
                                     (list :source (car (etaf-md-preview--edit-slice
                                                         (or (plist-get
                                                              session
                                                              :editing-source)
                                                             source)))
                                           :ref etaf-md-preview--edit-ref))))
               (t (list :key key :block block :editing nil
                        :ref ref
                        :view (etaf-md-preview--block-view
                               session key block ref))))
              entries)))
    (nreverse entries)))

(defun etaf-md-preview--published-table (entries)
  "Return ENTRIES as a table from each entry's key to the entry."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry entries) (puthash (plist-get entry :key) entry table))
    table))

(defun etaf-md-preview--block-ref-table (entries)
  "Return ENTRIES as a table from each entry's box reference to its key."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (entry entries)
      (when-let* ((ref (plist-get entry :ref)))
        (puthash ref (plist-get entry :key) table)))
    table))

(defun etaf-md-preview--keyed-document (session document contents published)
  "Return (KEYED . TABLE) for DOCUMENT parsed from CONTENTS.
KEYED is DOCUMENT with its top-level blocks replaced by publishing entries;
TABLE maps each entry's key to the entry, to be published with the document
and passed back as PUBLISHED by the next parse."
  (let ((entries (etaf-md-preview--block-entries
                  session document contents published)))
    (plist-put session :block-refs (etaf-md-preview--block-ref-table entries))
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
                        ;; The block shown as source, its slice, and the live
                        ;; native edit session while one is open.
                        :editing-key nil
                        :editing-source nil
                        :edit nil
                        ;; Box reference to block key, so a command can find
                        ;; the block the point sits in.
                        :block-refs (make-hash-table :test #'eq)
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
`:outline', `:error', `:blocks' (top-level block count, blank rows aside)
and `:footnotes'
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
          ;; The published list also carries the blank rows the page paints
          ;; between blocks; a reader counting blocks means the document's own.
          :blocks (cl-count-if (lambda (entry)
                                 (not (eq (plist-get (plist-get entry :block)
                                                     :type)
                                          'blank)))
                               (plist-get document :children))
          :footnotes (mapcar (lambda (footnote) (plist-get footnote :label))
                             (plist-get document :footnotes)))))

(defconst etaf-md-preview--reparse-neighbours 1
  "Unchanged blocks parsed again on each side of an edit.
A block can change shape together with the block next to it: deleting the
blank line between two paragraphs makes them one paragraph, so the parse has
to see them together.")

(defconst etaf-md-preview--fence-regexp "^ *\\(```\\|~~~\\)"
  "Line start of a code fence in markdown source.")

(defun etaf-md-preview--shared-start (old new)
  "Return how many characters OLD and NEW share from the start."
  (let ((common (min (length old) (length new))))
    (let ((result (compare-strings old 0 common new 0 common)))
      (if (eq result t) common (1- (abs result))))))

(defun etaf-md-preview--shared-end (old new limit)
  "Return how many characters at the end of OLD and NEW are the same, up to LIMIT."
  (let ((low 0)
        (high limit))
    (while (< low high)
      (let ((middle (/ (+ low high 1) 2)))
        (if (eq t (compare-strings old (- (length old) middle) nil
                                   new (- (length new) middle) nil))
            (setq low middle)
          (setq high (1- middle)))))
    low))

(defun etaf-md-preview--changed-span (old new)
  "Return (OLD-START OLD-END NEW-END) naming what differs between OLD and NEW.
Both ends are found by comparing the strings, so the cost follows the size of
the edit rather than the size of the document."
  (let* ((start (etaf-md-preview--shared-start old new))
         (tail (etaf-md-preview--shared-end old new (- (min (length old) (length new)) start))))
    (list start (- (length old) tail) (- (length new) tail))))

(defun etaf-md-preview--fence-count (text from to)
  "Return how many code fence lines TEXT holds between FROM and TO."
  (let ((count 0)
        (start from))
    (while (let ((found (string-match etaf-md-preview--fence-regexp text start)))
             (when (and found (< found to))
               (setq count (1+ count)
                     start (match-end 0))
               t)))
    count))

(defun etaf-md-preview--splice-parse (session contents old-text old-document)
  "Return OLD-DOCUMENT updated for CONTENTS, or nil when it needs a whole parse.
The blocks before and after an edit are the same text at a shifted offset, so
only the blocks the edit could have touched are parsed again.  A whole parse
is needed when the edit reaches the front matter, and when it changes how many
code fences the region holds: a fence opens a block that can end outside the
region, and only the whole text tells where."
  (pcase-let* ((`(,old-start ,old-end ,_)
                (etaf-md-preview--changed-span old-text contents))
               (delta (- (length contents) (length old-text)))
               (children (plist-get old-document :children))
               (count (length children)))
    (when children
      (let* ((first (or (cl-position-if (lambda (block)
                                          (>= (or (plist-get block :end) 0) old-start))
                                        children)
                        count))
             (after (or (cl-position-if (lambda (block)
                                          (> (or (plist-get block :begin) 0) old-end))
                                        children)
                        count))
             (head (max 0 (- first etaf-md-preview--reparse-neighbours)))
             (tail (min count (+ after etaf-md-preview--reparse-neighbours)))
             (from (if (> head 0) (plist-get (nth (1- head) children) :end) 0))
             (to (if (< tail count) (plist-get (nth tail children) :begin)
                    (length old-text)))
             (to-new (+ to delta)))
        (when (and (<= from old-start)
                   (<= old-end to)
                   (> from 0)           ; front matter holds the title
                   (<= from to-new (length contents))
                   (cl-evenp (etaf-md-preview--fence-count old-text from to))
                   (cl-evenp (etaf-md-preview--fence-count contents from to-new)))
          (let* ((fragment (etaf-md-preview--parse (substring contents from to-new)
                                                   (plist-get session :flavor)))
                 (middle (mapcar (lambda (block)
                                   (let ((copy (copy-sequence block)))
                                     (setq copy (plist-put copy :begin (+ from (plist-get block :begin))))
                                     (plist-put copy :end (+ from (plist-get block :end)))))
                                 (plist-get fragment :children)))
                 (suffix (mapcar (lambda (block)
                                   (let ((copy (copy-sequence block)))
                                     (setq copy (plist-put copy :begin (+ delta (plist-get block :begin))))
                                     (plist-put copy :end (+ delta (plist-get block :end)))))
                                 (nthcdr tail children)))
                 (spliced (append (seq-take children head) middle suffix)))
            (when (etaf-md-preview--blocks-ordered-p spliced (length contents))
              (plist-put (copy-sequence old-document) :children spliced))))))))

(defun etaf-md-preview--blocks-ordered-p (blocks size)
  "Return non-nil when BLOCKS name increasing ranges inside SIZE characters."
  (let ((edge 0) (ok t))
    (dolist (block blocks)
      (let ((begin (plist-get block :begin))
            (end (plist-get block :end)))
        (unless (and (integerp begin) (integerp end)
                     (<= edge begin) (<= begin end) (<= end size))
          (setq ok nil))
        (setq edge end)))
    ok))

(defun etaf-md-preview--parse-changed (session contents)
  "Return the document CONTENTS describes, parsing only what changed.
A preview republishes after every keystroke and parsing is the largest part of
that cost, so the blocks the edit left alone keep the parse they already have."
  (let* ((flavor (plist-get session :flavor))
         (cached (plist-get session :parsed))
         (document (cond ((null cached) (etaf-md-preview--parse contents flavor))
                         ((equal (car cached) contents) (cdr cached))
                         (t (or (etaf-md-preview--splice-parse
                                 session contents (car cached) (cdr cached))
                                (etaf-md-preview--parse contents flavor))))))
    (plist-put session :parsed (cons contents document))
    document))

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
                 (etaf-md-preview--parse-changed session contents)
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
