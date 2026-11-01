;;; etaf-md-preview-live.el --- Edit the markdown a preview paints  -*- lexical-binding: t; -*-

;;; Commentary:

;; A preview is a rendering of a markdown file, so nothing is typed into it.
;; This module binds the painted page to the file's buffer through
;; `etaf-document': a key names a position on the page, the projection turns
;; that into a markdown offset, the key runs there, and the page repaints from
;; the edited file.
;;
;; What stays here is what only markdown knows: which range of the file each
;; painted block came from, and what a block looks like when its text is gone.
;; Everything about accepting keys, moving the caret and saving is shared with
;; every other page that renders a document.

;;; Code:

(require 'cl-lib)
(require 'etaf)
(require 'etaf-md-preview-app)
(require 'etaf-md-preview-model)
(require 'etaf-md-preview-views)

(defvar etaf-md-preview-live--session nil
  "Preview session whose page currently accepts typing.")

(defvar etaf-md-preview-live--document nil
  "Editing session of the page that currently accepts typing.
The preview session is a reactive value, so it cannot hold this: the editing
session belongs to the module.")

;;;; What markdown alone knows

(defun etaf-md-preview--block-marker-length (block text)
  "Return how many characters BLOCK's painted marker covers in TEXT.
An empty block paints its source skeleton, and the caret belongs after it:
typing in front of the hashes would turn the heading into a paragraph."
  (if (etaf-md-preview--block-empty-p block text)
      (length (etaf-md-preview--block-marker block))
    0))

(defun etaf-md-preview--projection-entries (session)
  "Return the source range each painted block of SESSION stands for.
A block's range is the text it renders: its marker is skipped, because the
render folds it away, and its trailing newline is dropped, because the line
break belongs between blocks rather than to either of them."
  (let* ((text (etaf-md-preview--session-text session))
         (by-key (make-hash-table :test #'equal))
         (entries nil))
    ;; The session records each entry's box reference against its key, so a
    ;; block found by key resolves to the Host the projection reads.
    (let ((refs (plist-get session :block-refs)))
      (when refs
        (maphash (lambda (ref key) (puthash key ref by-key)) refs)))
    (when text
      (maphash
       (lambda (key entry)
         (let* ((block (plist-get entry :block))
                (begin (plist-get block :begin))
                (end (plist-get block :end))
                (blank (eq (plist-get block :type) 'blank))
                (marker (etaf-md-preview--block-marker-length block text)))
           ;; A blank row paints one cell for a line whose only character is its
           ;; newline; an empty document paints that cell for the start of an
           ;; empty file, so this range is the one place it may name no text.
           (cond
            (blank
             (when-let* ((ref (gethash key by-key)))
               (push (cons ref (cons begin end)) entries)))
            ((and (integerp begin) (integerp end) (< begin end) (<= end (length text)))
             (let* ((start (+ begin marker))
                    (stop (if (and (> end start) (eq (aref text (1- end)) ?\n))
                             (1- end)
                           end)))
               (when (< start stop)
                 (when-let* ((ref (gethash key by-key)))
                   (push (cons ref (cons start stop)) entries))))))))
       (or (plist-get session :published) (make-hash-table :test #'equal))))
    (nreverse entries)))

(defun etaf-md-preview-projection (session)
  "Return SESSION's painted preview as ranges of its markdown source."
  (when-let* ((buffer (plist-get session :buffer))
              (runtime (and (buffer-live-p buffer)
                            (etaf-runtime-for-buffer buffer)))
              (text (etaf-md-preview--session-text session)))
    (etaf-projection runtime (etaf-md-preview--projection-entries session) text)))

;;;; Source access

(defun etaf-md-preview--live-source (session)
  "Return the buffer holding SESSION's markdown, opening its file when needed."
  (or (let ((buffer (plist-get session :source-buffer)))
        (and (buffer-live-p buffer) buffer))
      (when-let* ((file (plist-get session :file)))
        (when (file-exists-p file)
          (let ((buffer (find-file-noselect file)))
            (plist-put session :source-buffer buffer)
            buffer)))))

(defun etaf-md-preview--live-length (session)
  "Return the number of characters in SESSION's markdown."
  (length (or (etaf-md-preview--session-text session) "")))

;;;; Repainting the page

(defun etaf-md-preview--live-repaint (session)
  "Redraw SESSION's page from its markdown, now."
  (etaf-md-preview--publish session (etaf-md-preview--session-text session))
  (when-let* ((buffer (plist-get session :buffer))
              (runtime (and (buffer-live-p buffer)
                            (etaf-runtime-for-buffer buffer))))
    (etaf-runtime-flush runtime)))

(defun etaf-md-preview--live-refresh (session text)
  "Redraw SESSION from TEXT, the current markdown."
  (unless (plist-get session :source-buffer)
    (plist-put session :text text))
  (etaf-md-preview--live-repaint session))

;;;; Session

;;;###autoload
(defun etaf-md-preview-editing-p ()
  "Return non-nil when the current preview buffer accepts in-place typing."
  (and etaf-md-preview-live--session
       (equal (plist-get etaf-md-preview-live--session :buffer) (current-buffer))
       t))

;;;###autoload
(defun etaf-md-preview-insert (text)
  "Insert TEXT into the markdown at the painted caret and repaint.
The page keeps its rendered shape; this is the command form of typing, for
scripts and for keys that arrive outside the page's own input."
  (interactive "sInsert: ")
  (etaf-document-insert text))

;;;###autoload
(defun etaf-md-preview-delete-char (&optional backward)
  "Delete one markdown character at the painted caret and repaint.
Interactively it deletes backwards; with BACKWARD nil it deletes forwards."
  (interactive "P")
  (etaf-document-delete-char (if backward t (not (called-interactively-p 'any)))))

(defun etaf-md-preview-exit-document ()
  "Return the painted page to navigation and save the markdown."
  (interactive)
  (when etaf-md-preview-live--session
    (when etaf-md-preview-live--document
      (etaf-document-close etaf-md-preview-live--document)
      (setq etaf-md-preview-live--document nil))
    ;; Editing ended, so the page follows the file's buffer again: a change made
    ;; anywhere else still reaches the page.
    (etaf-md-preview--live-start etaf-md-preview-live--session)
    (setq etaf-md-preview-live--session nil)))

(defun etaf-md-preview-edit-document ()
  "Type into the markdown source from the painted page.
The page stays read-only: a key names a cell, the projection turns it into a
markdown offset, the key runs in the file's buffer, and the page repaints from
the edited markdown."
  (interactive)
  (let* ((session (etaf-md-preview--current-session))
         (buffer (plist-get session :buffer))
         (source (etaf-md-preview--live-source session)))
    (unless source
      (user-error "This preview has no source buffer to edit"))
    (unless (and (buffer-live-p buffer) (etaf-runtime-for-buffer buffer))
      (user-error "This preview is not mounted"))
    (setq etaf-md-preview-live--session session)
    ;; The page repaints itself after every key it forwards, so the follower
    ;; that repaints on a buffer change would repeat the same work for the same
    ;; text: one keystroke would cost two whole-page repaints.
    (etaf-md-preview--live-stop (plist-get session :file))
    (let ((document (etaf-document-open
                     buffer
                     :source source
                     :entries (lambda () (etaf-md-preview--projection-entries session))
                     :republish (lambda (text)
                                  (etaf-md-preview--live-refresh session text))
                     :keymap '(("<escape>" . etaf-md-preview-exit-document)))))
      (setq etaf-md-preview-live--document document)
      ;; A reader who is already somewhere on the page keeps that place; only a
      ;; page that has not been pointed at yet starts at the top of the file.
      (when (<= (with-current-buffer buffer (point)) (point-min))
        (etaf-document-goto
         document
         (or (plist-get session :live-offset) 0)))
      document)))

(provide 'etaf-md-preview-live)

;;; etaf-md-preview-live.el ends here
