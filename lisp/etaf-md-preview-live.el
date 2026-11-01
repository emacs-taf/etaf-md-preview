;;; etaf-md-preview-live.el --- Type into the source behind the painted page  -*- lexical-binding: t; -*-

;;; Commentary:

;; The painted page is a projection of the markdown source.  What a reader
;; changes on it is a change to that source: a keystroke lands on a painted
;; cell, the projection names the source offset it stands for, the source is
;; edited there, and the page repaints from the edited source.  The cell the
;; keystroke produced never survives the repaint, so the page always shows a
;; rendering and never the characters being typed.
;;
;; Text is accepted natively.  The page is writable while it accepts typing, so
;; `self-insert-command' and every input method work exactly as they do in any
;; buffer: quail composes in the page, the reader sees the candidates, and the
;; change hook picks up the finished text.  Motion keys run against the source
;; through the projection, so `C-e' stops at the end of a paragraph rather than
;; at the end of a padded screen line.

;;; Code:

(require 'etaf)
(require 'etaf-md-preview-model)
(require 'etaf-md-preview-app)

(defvar etaf-md-preview-live--session nil
  "Preview session whose painted page accepts typed input, or nil.")

(defun etaf-md-preview--block-marker-length (block text)
  "Return how many source characters of BLOCK's marker the paint leaves out.
A heading paints its text without the hashes that announced it, so the painted
cells start that many characters into the block.  A bullet and a quote paint a
replacement glyph in place of their marker, which occupies the same width, so
their marker stays part of the mapping."
  (when-let* ((begin (plist-get block :begin))
              (end (plist-get block :end))
              ((<= 0 begin end (length text))))
    (let ((slice (substring text begin end)))
      (if (eq (plist-get block :type) 'heading)
          (if (string-match "\\`#+ " slice) (match-end 0) 0)
        0))))

(defun etaf-md-preview--projection-entries (session)
  "Return SESSION's painted blocks as projection entries.
Each entry starts where the block's painted text starts, which is after the
markdown marker the render folds away."
  (let ((entries nil)
        (source (etaf-md-preview--live-source session))
        (text (or (plist-get session :text) "")))
    (when source
      (setq text (with-current-buffer source
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (maphash (lambda (_key entry)
               (let* ((block (plist-get entry :block))
                      (begin (plist-get block :begin))
                      (end (plist-get block :end))
                      (marker (if (etaf-md-preview--block-empty-p block text)
                                  0
                                (etaf-md-preview--block-marker-length block text))))
                 (when-let* ((ref (plist-get entry :ref))
                             ((integerp begin)) ((integerp end))
                             ((< begin end)))
                   ;; The paint draws no line break, so the block's trailing
                   ;; newline carries no cell; leaving it in the range would
                   ;; spread it across the text and shift every character.
                   (when (and (> end (1+ begin))
                              (eq (aref text (1- end)) ?\n))
                     (setq end (1- end)))
                   (push (cons ref (cons (+ begin (or marker 0)) end)) entries))))
             (or (plist-get session :published) (make-hash-table :test #'equal)))
    entries))

;;;###autoload
(defun etaf-md-preview-projection (session)
  "Return SESSION's painted page as source ranges, or nil when unmounted."
  (when-let* ((buffer (plist-get session :buffer))
              (runtime (and (buffer-live-p buffer)
                            (etaf-runtime-for-buffer buffer))))
    (etaf-projection runtime (etaf-md-preview--projection-entries session)
                     (etaf-md-preview--session-text session))))

(defun etaf-md-preview--live-source (session)
  "Return SESSION's editable source buffer, or nil."
  (let ((buffer (plist-get session :source-buffer)))
    (and (buffer-live-p buffer) buffer)))

(defun etaf-md-preview--live-length (session)
  "Return the length of SESSION's source, or 0."
  (if-let* ((source (etaf-md-preview--live-source session)))
      (with-current-buffer source (buffer-size))
    0))

(defun etaf-md-preview--live-offset (session projection)
  "Return the source offset the painted caret names in SESSION.
The page is repainted before this is asked, so the projection describes exactly
what is on screen and the caret needs no remembered position."
  (cond ((>= (point) (point-max)) (etaf-md-preview--live-length session))
        (projection (etaf-projection-source-offset projection (point)))))

(defun etaf-md-preview--live-repaint (session)
  "Republish SESSION's document from its source and draw it now.
An edit on the page must reach the screen before the next key is read, so the
repaint runs here instead of waiting for the change follower's timer: a
deferred repaint would leave the caret working against an older page."
  (let ((source (etaf-md-preview--live-source session))
        (buffer (plist-get session :buffer)))
    (when (and (buffer-live-p source) (buffer-live-p buffer))
      (etaf-md-preview--publish
       session
       (with-current-buffer source
         (buffer-substring-no-properties (point-min) (point-max))))
      (let ((runtime (etaf-runtime-for-buffer buffer)))
        (when runtime (etaf-runtime-flush runtime))))))

(defun etaf-md-preview--live-write-source (session start delete insert)
  "In SESSION's source replace DELETE characters at START with INSERT.
An edit made on the page is not a file edit the reader started, so the source
is left unmodified: asking to save after every keystroke would interrupt the
sentence.  The session saves once, when it ends."
  (when-let* ((source (etaf-md-preview--live-source session)))
    (let ((etaf-md-preview--writing-back t))
      (with-current-buffer source
        (save-excursion
          (goto-char (1+ start))
          (when (> delete 0) (delete-char delete))
          (when (> (length insert) 0) (insert insert)))
        (set-buffer-modified-p nil)))))

(defun etaf-md-preview--live-write (session start delete insert)
  "In SESSION's source replace DELETE characters at START with INSERT.
The write goes through the visiting buffer, then SESSION repaints at once, so
the page and the source never disagree while a hand is typing."
  (etaf-md-preview--live-write-source session start delete insert)
  (etaf-md-preview--live-repaint session))

(defun etaf-md-preview--live-type (text)
  "Insert TEXT at the painted caret, as if the user had typed it."
  (when-let* ((session etaf-md-preview-live--session)
              (projection (etaf-md-preview-projection session))
              (offset (etaf-md-preview--live-offset session projection)))
    (etaf-md-preview--live-write session offset 0 text)
    (etaf-md-preview--live-place-caret session (+ offset (length text)))))

(defun etaf-md-preview--live-delete (backward)
  "Delete one source character at the painted caret, BACKWARD or forward."
  (when-let* ((session etaf-md-preview-live--session)
              (projection (etaf-md-preview-projection session))
              (offset (etaf-md-preview--live-offset session projection))
              (start (if backward (1- offset) offset)))
    (when (and (>= start 0) (< start (etaf-md-preview--live-length session)))
      (etaf-md-preview--live-write session start 1 "")
      (etaf-md-preview--live-place-caret session start))))

;;;; Input channel

;; Every key the page receives runs in the source buffer.  The page is a
;; rendering, so nothing is typed into it: the key names a position there, the
;; projection turns that into a source offset, the key runs at that offset in
;; the markdown buffer, and the page repaints from the edited source.  That is
;; what makes an input method work: quail composes in the source buffer, where
;; `input-method-function' and the composition text live, and the page only
;; ever shows the finished document.

(defun etaf-md-preview--live-run-key (session key)
  "Run KEY at the page's caret in SESSION's source, then repaint the page.
KEY is already the character the reader produced.  A system input method
composes outside Emacs and hands over finished characters, so nothing here
needs to translate keys; driving quail instead would block while it waited for
a candidate the page cannot show."
  (let* ((projection (etaf-md-preview-projection session))
         (offset (and projection (etaf-md-preview--live-offset session projection)))
         (source (etaf-md-preview--live-source session)))
    (when (and offset source (buffer-live-p source))
      (with-current-buffer source
        (goto-char (1+ offset))
        (let* ((event (if (characterp key) key (aref key 0)))
               (last-command-event event)
               (binding (key-binding (vector event))))
          (cond
           ((and (commandp binding) (not (eq binding 'undefined)))
            (condition-case nil
                (call-interactively binding)
              (error (insert (string event)))))
           (t (insert (string event)))))
        (let ((target (1- (point))))
          ;; An edit made on the page is not a file edit the reader started, so
          ;; the source stays unmodified and the session saves once, when it
          ;; ends; prompting to save after every keystroke would interrupt the
          ;; sentence.
          (set-buffer-modified-p nil)
          (etaf-md-preview--live-repaint session)
          (etaf-md-preview--live-place-caret session target))))))

(defun etaf-md-preview--live-input (key)
  "Hand the typed KEY to the source buffer and keep the page a rendering.
This runs below the command loop, so an input method sees the key first; quail
composes in the source buffer and the page repaints once a character lands."
  (when etaf-md-preview-live--session
    (etaf-md-preview--live-run-key etaf-md-preview-live--session key))
  ;; Return nil: the page must never receive the character itself.
  nil)

(defun etaf-md-preview--live-move (command)
  "Run COMMAND against the source and put the caret where it lands.
A key on the painted page names a position in the document, not in the
rendering: the caret goes to the source offset COMMAND reached, and the
projection says which painted cell holds it.  That is why `C-e' stops at the
end of a paragraph instead of the end of the padded screen line."
  (let* ((session etaf-md-preview-live--session)
         (projection (etaf-md-preview-projection session))
         (offset (and projection (etaf-md-preview--live-offset session projection)))
         (source (etaf-md-preview--live-source session))
         (target (and offset source
                      (with-current-buffer source
                        (save-excursion
                          (goto-char (1+ offset))
                          (call-interactively command)
                          (1- (point)))))))
    (when target
      (etaf-md-preview--live-place-caret session target))))

(defun etaf-md-preview--live-move-command (command)
  "Return an interactive command that runs COMMAND against the source."
  (lambda () (interactive) (etaf-md-preview--live-move command)))

(defun etaf-md-preview--live-self-insert ()
  "Insert the character the reader produced into the source, not the page.
A system input method composes outside Emacs and hands over finished
characters, so they arrive as `self-insert-command' rather than through
`input-method-function'; this is where they reach the markdown buffer."
  (interactive)
  (when etaf-md-preview-live--session
    (etaf-md-preview--live-run-key etaf-md-preview-live--session
                                   last-command-event)))

(defun etaf-md-preview--live-delete-back-command ()
  "Delete the source character before the caret and repaint the page.
The page is read-only, so its own deletion commands cannot run here; deleting
is a change to the source like every other key."
  (interactive)
  (etaf-md-preview--live-delete t))

(defun etaf-md-preview--live-delete-forward-command ()
  "Delete the source character after the caret and repaint the page."
  (interactive)
  (etaf-md-preview--live-delete nil))

(defun etaf-md-preview--live-newline-command ()
  "Break the source line at the caret and repaint the page."
  (interactive)
  (etaf-md-preview--live-type "\n"))

(defconst etaf-md-preview--live-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<escape>") #'etaf-md-preview-exit-document)
    ;; Typed characters, including everything a system input method produces,
    ;; belong to the source buffer.
    (define-key map [remap self-insert-command] #'etaf-md-preview--live-self-insert)
    ;; The page cannot delete or break lines itself: it is read-only, so these
    ;; run against the source as well.
    (dolist (key '("DEL" "<backspace>"))
      (define-key map (kbd key) #'etaf-md-preview--live-delete-back-command))
    (define-key map (kbd "C-d") #'etaf-md-preview--live-delete-forward-command)
    (dolist (key '("RET" "<return>"))
      (define-key map (kbd key) #'etaf-md-preview--live-newline-command))
    ;; The canvas binds these to focus navigation because a painted page is
    ;; normally an interface, not text.  While the page accepts typing the
    ;; caret is text, and these keys name document positions, so they run
    ;; against the source and the projection puts the caret where they landed.
    (dolist (entry '(("<left>" . backward-char) ("C-b" . backward-char)
                     ("<right>" . forward-char) ("C-f" . forward-char)
                     ("<home>" . move-beginning-of-line) ("C-a" . move-beginning-of-line)
                     ("<end>" . move-end-of-line) ("C-e" . move-end-of-line)
                     ("M-f" . forward-word) ("M-b" . backward-word)
                     ("M-<" . beginning-of-buffer) ("M->" . end-of-buffer)))
      (define-key map (kbd (car entry))
                      (etaf-md-preview--live-move-command (cdr entry))))
    ;; Vertical motion follows the painted lines, which is what the reader
    ;; sees when a paragraph wraps; the page's own map keeps scrolling too.
    (dolist (event '(down-mouse-1 drag-mouse-1 mouse-1))
      (define-key map (vector event) #'mouse-set-point))
    map)
  "Keys whose meaning comes from the document rather than the page.
Typing, deletion and newlines are not bound: the page accepts text natively,
so an input method composes there and the change hooks carry the result to the
source.")

;;;; Session

(defun etaf-md-preview-exit-document ()
  "Return the painted page to navigation and drop the input channel."
  (interactive)
  (when etaf-md-preview-live--session
    (let ((buffer (plist-get etaf-md-preview-live--session :buffer))
          (source (etaf-md-preview--live-source etaf-md-preview-live--session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq-local overriding-local-map nil)
          (setq-local buffer-read-only t)
          (setq-local input-method-function nil)
          ;; Give the page back the cursor and the modality rule it had
          ;; before it accepted typing.
          (kill-local-variable 'cursor-type)
          (kill-local-variable 'etaf-cursor-follows-input-modality)))
      ;; The page wrote through the source while it was edited, so the file
      ;; gets the text once the session ends instead of prompting mid-sentence.
      (when (buffer-live-p source)
        (with-current-buffer source
          (when (buffer-modified-p) (save-buffer)))))
    (setq etaf-md-preview-live--session nil)))

;;;###autoload
;;;; Public commands

;;;###autoload
(defun etaf-md-preview-editing-p ()
  "Return non-nil when the current preview buffer accepts in-place typing."
  (and etaf-md-preview-live--session
       (equal (plist-get etaf-md-preview-live--session :buffer) (current-buffer))
       t))

;;;###autoload
(defun etaf-md-preview-insert (text)
  "Insert TEXT into the source at the painted caret and repaint.
The page keeps its rendered shape; this is the command form of typing, for
scripts and for keys that arrive outside the page's own input."
  (interactive "sInsert: ")
  (etaf-md-preview--live-type text))

;;;###autoload
(defun etaf-md-preview-delete-char (&optional backward)
  "Delete one source character at the painted caret and repaint.
Interactively it deletes backwards; with BACKWARD nil it deletes forwards."
  (interactive "P")
  (etaf-md-preview--live-delete (if backward t (not (called-interactively-p 'any)))))

(defun etaf-md-preview-edit-document ()
  "Type into the markdown source from the painted page.
The page stays read-only: keys arrive as events below the command loop, edit
the source and let the preview repaint the blocks that changed.  The caret
moves over the painted cells natively, so motion and the markdown-free view are
unchanged."
  (interactive)
  (let* ((session (etaf-md-preview--current-session))
         (buffer (plist-get session :buffer)))
    ;; Editing needs the file's buffer; opening it here keeps the preview
    ;; read-only for readers who never ask to edit.
    (unless (etaf-md-preview--live-source session)
      (when-let* ((file (plist-get session :file)))
        (plist-put session :source-buffer (find-file-noselect file))
        (etaf-md-preview--live-start session)))
    (unless (etaf-md-preview--live-source session)
      (user-error "This preview has no source buffer to edit"))
    (unless (and (buffer-live-p buffer) (etaf-runtime-for-buffer buffer))
      (user-error "This preview is not mounted"))
    (setq etaf-md-preview-live--session session)
    (with-current-buffer buffer
      (setq-local overriding-local-map etaf-md-preview--live-map)
      ;; A press on the page places a caret the user must see, so the cursor
      ;; keeps showing instead of following the pointer's hidden style.
      (setq-local etaf-cursor-follows-input-modality nil)
      (setq-local cursor-type 'bar)
      ;; The page never holds the reader's text: every key it receives runs in
      ;; the source buffer, so the page stays a rendering and an input method
      ;; composes where text really lives.
      (setq-local buffer-read-only t)
      (setq-local input-method-function #'etaf-md-preview--live-input))
    ;; A reader who is already somewhere on the page keeps that place; only a
    ;; page that has not been pointed at yet starts at the top of the source.
    (when (<= (point) (point-min))
      (when-let* ((projection (etaf-md-preview-projection session))
                  (position (etaf-projection-position
                             projection
                             (etaf-md-preview--live-start-offset
                              session (or (plist-get session :live-offset) 0)))))
        (with-current-buffer buffer
          (goto-char position)
          (dolist (window (get-buffer-window-list buffer nil t))
            (set-window-point window position)))))
    session))

(defun etaf-md-preview--live-start-offset (session offset)
  "Return where editing should begin when the page opens at OFFSET.
An empty block paints its marker so the reader can see what is left, and the
caret belongs after that marker: typing on it would put text in front of the
hashes and turn the heading into a paragraph."
  (let ((text (etaf-md-preview--session-text session))
        (found nil))
    (maphash (lambda (_key entry)
               (let ((block (plist-get entry :block)))
                 (when (and (not found)
                            (= offset (or (plist-get block :begin) -1))
                            (etaf-md-preview--block-empty-p block text))
                   (setq found (+ offset
                                  (length (etaf-md-preview--block-marker block)))))))
             (or (plist-get session :published) (make-hash-table :test #'equal)))
    (or found offset)))

(defun etaf-md-preview--live-place-caret (session offset)
  "Put the painted caret on the cell that paints source OFFSET.
A window shows its own point, so every window displaying the page moves too;
without that the caret a user sees never follows the edit."
  (when-let* ((buffer (plist-get session :buffer))
              ((buffer-live-p buffer))
              (projection (etaf-md-preview-projection session))
              (position (etaf-projection-position projection offset)))
    (with-current-buffer buffer
      (goto-char position)
      (dolist (window (get-buffer-window-list buffer nil t))
        (set-window-point window position)))))

(provide 'etaf-md-preview-live)

;;; etaf-md-preview-live.el ends here
