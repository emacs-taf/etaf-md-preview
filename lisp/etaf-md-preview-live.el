;;; etaf-md-preview-live.el --- Type into the source behind the painted page  -*- lexical-binding: t; -*-

;;; Commentary:

;; The painted page is a projection of the markdown source, so it stays
;; read-only: nothing here ever changes a painted cell.  Typing is handled as an
;; event that edits the source, and the page follows by repainting the blocks
;; that changed, the same way any other reactive update reaches the screen.
;;
;; The caret is Emacs' own: it moves over the painted cells natively, so arrow
;; keys, Home/End and the markup-free reading experience are untouched.  Only
;; the input channel is ours, and it sits at `input-method-function', below the
;; command loop, so an input method translates keys into text the same way it
;; does in any buffer while the page itself never becomes writable.

;;; Code:

(require 'etaf)
(require 'etaf-md-preview-model)
(require 'etaf-md-preview-app)

(defvar etaf-md-preview-live--session nil
  "Preview session whose painted page accepts typed input, or nil.")

(defvar-local etaf-md-preview-live--inner-input nil
  "Input method handler the page wrapped when it took the input channel.
An active input method translates keys itself; wrapping it keeps that
translation and only changes where the result goes.")

(defvar etaf-md-preview-live--anchor nil
  "Last caret as (PAINTED-POSITION . SOURCE-OFFSET), or nil.
The page repaints asynchronously, so between keystrokes the projection still
describes the previous paint; the anchor carries the source offset across that
gap and keeps consecutive keystrokes advancing instead of drifting back.")

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
                      (marker (etaf-md-preview--block-marker-length block text)))
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
    (etaf-projection runtime (etaf-md-preview--projection-entries session))))

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
  "Return the source offset the painted caret names in SESSION."
  (let ((anchor etaf-md-preview-live--anchor))
    (cond ((and anchor (= (car anchor) (point))) (cdr anchor))
          ((>= (point) (point-max)) (etaf-md-preview--live-length session))
          (projection (etaf-projection-source-offset projection (point))))))

(defun etaf-md-preview--live-write (session start delete insert)
  "In SESSION's source replace DELETE characters at START with INSERT.
The write goes through the visiting buffer, so the preview's change follower
publishes the document the same way it does for a hand edit."
  (when-let* ((source (etaf-md-preview--live-source session)))
    (with-current-buffer source
      (save-excursion
        (goto-char (1+ start))
        (when (> delete 0) (delete-char delete))
        (when (> (length insert) 0) (insert insert))))))

(defun etaf-md-preview--live-type (text)
  "Insert TEXT at the painted caret, as if the user had typed it."
  (when-let* ((session etaf-md-preview-live--session)
              (projection (etaf-md-preview-projection session))
              (offset (etaf-md-preview--live-offset session projection)))
    (etaf-md-preview--live-write session offset 0 text)
    ;; The repaint that follows publishes a new page, so the caret is placed
    ;; against that page rather than the one still on screen.
    (setq etaf-md-preview-live--anchor (cons (point) (+ offset (length text))))))

(defun etaf-md-preview--live-delete (backward)
  "Delete one source character at the painted caret, BACKWARD or forward."
  (when-let* ((session etaf-md-preview-live--session)
              (projection (etaf-md-preview-projection session))
              (offset (etaf-md-preview--live-offset session projection))
              (start (if backward (1- offset) offset)))
    (when (and (>= start 0) (< start (etaf-md-preview--live-length session)))
      (etaf-md-preview--live-write session start 1 "")
      (setq etaf-md-preview-live--anchor (cons (point) start)))))

;;;; Input channel

(defun etaf-md-preview--live-input (key)
  "Translate the typed KEY into a source edit and swallow it.
This runs below the command loop.  An input method, when one is active, has
already replaced the handler with its own translator, so the page keeps that
translator and only redirects the text it produces."
  (let* ((inner etaf-md-preview-live--inner-input)
         (translated (if inner
                         (condition-case nil (funcall inner key)
                           (error nil))
                       (list key)))
         (text (and translated (concat translated))))
    (cond ((null text) nil)
          ((string= text "\r") (etaf-md-preview--live-type "\n"))
          ((string= text "\d") (etaf-md-preview--live-delete t))
          ((string= text "\177") (etaf-md-preview--live-delete t))
          ;; Motion, function keys and anything else that is not text belong
          ;; to the command loop: hand the key back rather than swallow it.
          ((string-match-p "\\`[[:cntrl:]]\\'" text)
           (push key unread-command-events))
          (t (etaf-md-preview--live-type text)))
    ;; Return nil: the painted page must never receive the character itself.
    nil))

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
      (etaf-md-preview--live-place-caret session target)
      ;; The caret can only stand where the page paints a cell, so the anchor
      ;; records the offset of the painted cell, not the source offset the
      ;; command asked for: `C-a' names the line start, but a folded marker
      ;; leaves the caret on the line's first visible character.
      (setq etaf-md-preview-live--anchor
            (cons (point)
                  (or (and projection
                           (etaf-projection-source-offset projection (point)))
                      target))))))

(defun etaf-md-preview--live-move-command (command)
  "Return an interactive command that runs COMMAND against the source."
  (lambda () (interactive) (etaf-md-preview--live-move command)))

(defconst etaf-md-preview--live-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "DEL") #'etaf-md-preview--live-delete-back-command)
    (define-key map (kbd "<backspace>") #'etaf-md-preview--live-delete-back-command)
    (define-key map (kbd "C-d") #'etaf-md-preview--live-delete-forward-command)
    (dolist (key '("RET" "<return>"))
      (define-key map (kbd key) #'etaf-md-preview--live-newline-command))
    (define-key map (kbd "<escape>") #'etaf-md-preview-exit-document)
    ;; The canvas binds these to focus navigation because a painted page is
    ;; normally an interface, not text.  While the page accepts typing the
    ;; caret is text, so the same keys must move it as they do anywhere else.
    ;; Horizontal motion, word motion and document edges are document
    ;; positions: run them against the source and map the result back.
    (dolist (entry '(("<left>" . backward-char) ("C-b" . backward-char)
                     ("<right>" . forward-char) ("C-f" . forward-char)
                     ("<home>" . move-beginning-of-line) ("C-a" . move-beginning-of-line)
                     ("<end>" . move-end-of-line) ("C-e" . move-end-of-line)
                     ("M-f" . forward-word) ("M-b" . backward-word)
                     ("M-<" . beginning-of-buffer) ("M->" . end-of-buffer)))
      (define-key map (kbd (car entry))
                      (etaf-md-preview--live-move-command (cdr entry))))
    ;; Vertical motion follows the painted lines, which is what the reader
    ;; sees when a paragraph wraps.
    (dolist (entry '(("<up>" . previous-line) ("C-p" . previous-line)
                     ("<down>" . next-line) ("C-n" . next-line)
                     ("C-v" . scroll-up-command) ("M-v" . scroll-down-command)
                     ("<prior>" . scroll-down-command) ("<next>" . scroll-up-command)
                     ("<wheel-up>" . mwheel-scroll) ("<wheel-down>" . mwheel-scroll)))
      (define-key map (kbd (car entry)) (cdr entry)))
    ;; A press on the page moves the caret, as it does in any buffer.
    (dolist (event '(down-mouse-1 drag-mouse-1 mouse-1))
      (define-key map (vector event) #'mouse-set-point))
    map)
  "Keys that move the caret and edit text while the page accepts typing.
Typing keys are not bound: they arrive through the input channel, so an input
method composes as it does in any buffer.")

(defun etaf-md-preview--live-delete-back-command ()
  "Delete the source character before the painted caret."
  (interactive)
  (etaf-md-preview--live-delete t))

(defun etaf-md-preview--live-delete-forward-command ()
  "Delete the source character after the painted caret."
  (interactive)
  (etaf-md-preview--live-delete nil))

(defun etaf-md-preview--live-newline-command ()
  "Break the source line at the painted caret."
  (interactive)
  (etaf-md-preview--live-type "\n"))

;;;; Public commands

;;;###autoload
(defun etaf-md-preview-editing-p ()
  "Return non-nil when the current preview buffer accepts in-place typing."
  (and etaf-md-preview-live--session
       (equal (plist-get etaf-md-preview-live--session :buffer) (current-buffer))
       t))

;;;###autoload
(defun etaf-md-preview-insert (text)
  "Insert TEXT into the source at the painted caret.
The painted page keeps its rendered shape: the text reaches the source and the
preview repaints the blocks that changed."
  (interactive "sInsert: ")
  (etaf-md-preview--live-type text))

;;;###autoload
(defun etaf-md-preview-delete-char (&optional backward)
  "Delete one source character at the painted caret.
BACKWARD, the default when called interactively, deletes the character before
the caret; otherwise the one after it."
  (interactive "P")
  (etaf-md-preview--live-delete (if backward t (not (called-interactively-p 'any)))))

(defun etaf-md-preview--live-ime-advice (original key)
  "Send an input method's translation to the source instead of the page.
An active input method installs itself as the key translator; this keeps that
translator and only redirects the text it produces, which is what lets Chinese
reach the markdown source while the page stays a rendering."
  (let ((result (funcall original key)))
    (if (and result etaf-md-preview-live--session)
        (progn (etaf-md-preview--live-type (concat result)) nil)
      result)))

(advice-add 'quail-input-method :around #'etaf-md-preview--live-ime-advice)

(defun etaf-md-preview--live-rewrap ()
  "Take the input channel back after an input method installs itself."
  (when (and etaf-md-preview-live--session
             (equal (plist-get etaf-md-preview-live--session :buffer)
                    (current-buffer)))
    (setq-local input-method-function #'etaf-md-preview--live-input)))

(add-hook 'input-method-deactivate-hook #'etaf-md-preview--live-rewrap)

;;;; Session

(defun etaf-md-preview-exit-document ()
  "Return the painted page to navigation and drop the input channel."
  (interactive)
  (when etaf-md-preview-live--session
    (let ((buffer (plist-get etaf-md-preview-live--session :buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq-local overriding-local-map nil)
          (setq-local buffer-read-only t)
          (setq-local input-method-function etaf-md-preview-live--inner-input)
          (setq-local etaf-md-preview-live--inner-input nil))))
    (setq etaf-md-preview-live--session nil
          etaf-md-preview-live--anchor nil)))

;;;###autoload
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
    (setq etaf-md-preview-live--session session
          etaf-md-preview-live--anchor nil)
    (with-current-buffer buffer
      (setq-local overriding-local-map etaf-md-preview--live-map)
      (setq-local cursor-type 'bar)
      ;; The page is a rendering: typed text belongs to the source, never here.
      (setq-local buffer-read-only t)
      ;; A press on the page places a caret the user must see, so the cursor
      ;; keeps showing instead of following the pointer's hidden style.
      (setq-local etaf-cursor-follows-input-modality nil)
      (setq-local cursor-type 'bar)
      (setq-local etaf-md-preview-live--inner-input input-method-function)
      (setq-local input-method-function #'etaf-md-preview--live-input))
    (when-let* ((projection (etaf-md-preview-projection session)))
      (let ((position (etaf-projection-position
                       projection
                       (or (plist-get session :live-offset) 0))))
        (when position
          (with-current-buffer buffer
            (goto-char position)
            (dolist (window (get-buffer-window-list buffer nil t))
              (set-window-point window position))))))
    session))

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

(defun etaf-md-preview--live-settle (session)
  "Put the caret back on SESSION's freshly repainted page.
The repaint publishes from a timer, and moving a window inside that timer
re-enters the pending redisplay, so placement waits for the next idle moment."
  (when (and (eq etaf-md-preview-live--session session)
             etaf-md-preview-live--anchor)
    (let ((offset (cdr etaf-md-preview-live--anchor)))
      (run-with-idle-timer
       0 nil
       (lambda ()
         (when (eq etaf-md-preview-live--session session)
           (etaf-md-preview--live-place-caret session offset)))))))

(add-hook 'etaf-md-preview-after-repaint-functions #'etaf-md-preview--live-settle)

(provide 'etaf-md-preview-live)

;;; etaf-md-preview-live.el ends here
