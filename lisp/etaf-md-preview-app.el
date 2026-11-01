;;; etaf-md-preview-app.el --- Application shell of etaf-md-preview -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The application layer binds one preview session to the Views.  The
;; `etaf-md-preview-app' Component is the only place that reads the session's
;; reactive sources, so a palette, width or document change repaints the whole
;; preview in one place; it also provides the palette tokens to its subtree so
;; shared ETAF UI Components follow the same colors.
;;
;; `etaf-md-preview-open' opens the session as an ETAF application: the
;; application buffer, its root keys, the initial reading measure, the page
;; background and the commands for outline navigation, reload and theming.  A
;; window resize reflows through the framework's own viewport update, so the
;; application never rebuilds the view for a geometry change.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'filenotify)
(require 'etaf)
(require 'etaf-ui)
(require 'etaf-md-preview-theme)
(require 'etaf-md-preview-model)
(require 'etaf-md-preview-views)

(defun etaf-md-preview--buffer-name (file)
  "Return the preview buffer name for FILE."
  (format "*ETAF Markdown: %s*" (file-name-nondirectory file)))

(defconst etaf-md-preview--keys
  '(("n" etaf-md-preview-next-heading "下一个标题")
    ("p" etaf-md-preview-previous-heading "上一个标题")
    ("C-c j" etaf-md-preview-goto-heading "选择标题")
    ("e" etaf-md-preview-edit-block "编辑这一块")
    ("r" etaf-md-preview-reload "重新加载文档")
    ("t" etaf-md-preview--cycle-theme-command "切换主题")
    ("[" etaf-md-preview-narrower "缩窄阅读栏")
    ("]" etaf-md-preview-wider "加宽阅读栏")
    ("C-c h" etaf-help "查看快捷键")
    ("q" etaf-md-preview-close "关闭预览"))
  "Root keys of a preview opened by `etaf-md-preview-open'.
Native scrolling, isearch and link activation stay available; the keys only
add outline navigation, reloading, theming and closing.")

(defun etaf-md-preview--current-session ()
  "Return the preview session of the current buffer."
  (or (etaf-app-state)
      (user-error "Not an ETAF markdown preview buffer")))

;;; ---------------------------------------------------------------------------
;;; Navigation and display commands
;;; ---------------------------------------------------------------------------

(defun etaf-md-preview-next-heading ()
  "Move to the next outline heading of the current preview."
  (interactive)
  (let ((session (etaf-md-preview--current-session)))
    (unless (etaf-md-preview--adjacent-heading session 1)
      (message "No further heading"))))

(defun etaf-md-preview-previous-heading ()
  "Move to the previous outline heading of the current preview."
  (interactive)
  (let ((session (etaf-md-preview--current-session)))
    (unless (etaf-md-preview--adjacent-heading session -1)
      (message "No earlier heading"))))

(defun etaf-md-preview-goto-heading ()
  "Choose an outline heading of the current preview and move to it."
  (interactive)
  (let* ((session (etaf-md-preview--current-session))
         (outline (etaf-md-preview-outline session))
         (candidates
          (cl-loop for entry in outline
                   for index from 0
                   collect (cons (format "%s%s %s"
                                         (make-string (* 2 (1- (nth 0 entry))) ?\s)
                                         (make-string (1+ (nth 0 entry)) ?#)
                                         (nth 1 entry))
                                 index))))
    (unless candidates
      (user-error "This document has no headings"))
    (let ((choice (completing-read "Heading: " candidates nil t)))
      (etaf-md-preview-locate session (cdr (assoc choice candidates))))))

(defun etaf-md-preview-reload ()
  "Re-read and repaint the current preview."
  (interactive)
  (let ((session (etaf-md-preview--current-session)))
    (etaf-md-preview-refresh session)
    (message "Reloaded %s" (etaf-md-preview--read session :title))))

(defun etaf-md-preview--cycle-theme-command ()
  "Switch the current preview to the next palette."
  (interactive)
  (let ((session (etaf-md-preview--current-session)))
    (etaf-md-preview-cycle-theme session)
    (message "Theme: %s" (etaf-md-preview--read session :theme))))

(defun etaf-md-preview-wider ()
  "Widen the current preview's reading column."
  (interactive)
  (let* ((session (etaf-md-preview--current-session))
         (cap (etaf-md-preview-adjust-width session 4)))
    (message "Reading width: %d (max %d)"
             (etaf-md-preview--effective-width session) cap)))

(defun etaf-md-preview-narrower ()
  "Narrow the current preview's reading column."
  (interactive)
  (let* ((session (etaf-md-preview--current-session))
         (cap (etaf-md-preview-adjust-width session -4)))
    (message "Reading width: %d (max %d)"
             (etaf-md-preview--effective-width session) cap)))

(defun etaf-md-preview-close ()
  "Unmount the current preview, kill its buffer and free its window.
The current window goes away when the frame has another window to spare, so a
preview opened beside its source closes back onto the source instead of leaving
a second copy of it.  Signals `user-error' when the current buffer is not a
preview."
  (interactive)
  (etaf-md-preview--current-session)
  (let ((window (selected-window)))
    (etaf-app-close)
    (when (and (window-live-p window) (not (one-window-p t)))
      (ignore-errors (delete-window window)))))

;;; ---------------------------------------------------------------------------
;;; Window fitting
;;; ---------------------------------------------------------------------------

(defun etaf-md-preview--effective-width (session)
  "Return the reading width the window actually gives SESSION.
The session's `:width' is the maximum measure; a narrower window fills the
available columns instead, so this reports what is on screen outside the
column's side gutters."
  (let* ((buffer (plist-get session :buffer))
         (window (and (buffer-live-p buffer) (get-buffer-window buffer t)))
         (cap (etaf-md-preview--read session :width)))
    (if (window-live-p window)
        (min cap (max 24 (- (window-body-width window)
                            (* 2 etaf-md-preview--measure-gutter))))
      cap)))

;;; ---------------------------------------------------------------------------
;;; Following the source file
;;; ---------------------------------------------------------------------------

(defconst etaf-md-preview--follow-tick 0.016
  "Seconds between a source change and its repaint.
One frame at 60 Hz.  A change only schedules a repaint when none is pending,
so a burst of keystrokes coalesces into one repaint per frame instead of one
per keystroke, and typing never waits for the keys to stop.")

(defconst etaf-md-preview--follow-retry 0.2
  "Seconds between attempts to watch a file that is temporarily missing.")

(defvar etaf-md-preview--writing-back nil
  "Non-nil while the preview writes an edit back into its source buffer.
The change hook runs inside that write, which comes from the preview itself:
following it would republish the document and replace the block the live edit
session is editing.")

(defvar etaf-md-preview--follow-sessions (make-hash-table :test #'equal)
  "Displayed preview sessions by absolute source file name.")

(defvar etaf-md-preview--follow-watches (make-hash-table :test #'equal)
  "Change watches by absolute source file name.")

(defvar etaf-md-preview--follow-timers (make-hash-table :test #'equal)
  "Pending repaints by absolute source file name.")

(defvar etaf-md-preview--live-sources (make-hash-table :test #'equal)
  "Visiting buffers followed for unsaved edits by absolute source file name.")

(defun etaf-md-preview--live-changed (&rest _)
  "Repaint the preview of the file being edited on the next frame.
The hook runs on every buffer change; scheduling keeps one pending repaint
per file, so a keystroke joins the frame that is already coming."
  (when (and buffer-file-name (not etaf-md-preview--writing-back))
    (etaf-md-preview--follow-schedule (expand-file-name buffer-file-name))))

(defun etaf-md-preview--live-start (session)
  "Make SESSION follow unsaved edits of its visiting buffer, when it has one.
A file with no visiting buffer has nothing to follow until it is opened, and a
session built from text never follows a buffer."
  (when-let* ((path (plist-get session :file))
              (buffer (get-file-buffer (expand-file-name path))))
    (puthash (expand-file-name path) buffer etaf-md-preview--live-sources)
    (plist-put session :source-buffer buffer)
    (with-current-buffer buffer
      (add-hook 'after-change-functions #'etaf-md-preview--live-changed nil t))))

(defun etaf-md-preview--live-stop (file)
  "Stop following unsaved edits of FILE and drop its visiting buffer."
  (when-let* ((file (and file (expand-file-name file)))
              (buffer (gethash file etaf-md-preview--live-sources)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (remove-hook 'after-change-functions #'etaf-md-preview--live-changed t)))
    (remhash file etaf-md-preview--live-sources)))

(defconst etaf-md-preview--follow-attempts 3
  "Times to retry a watch while its file is missing.
An editor that saves atomically deletes the old file before it renames the
new one over the name, and the watch must be re-armed once the name exists
again; each retry waits `etaf-md-preview--follow-retry'.")

(defun etaf-md-preview--follow-saved ()
  "Repaint the preview of the file being saved, when one is open."
  (when (and buffer-file-name (not etaf-md-preview--writing-back))
    (etaf-md-preview--follow-schedule (expand-file-name buffer-file-name))))

(defun etaf-md-preview--follow-watch (file)
  "Arm, or re-arm, FILE's change watch and return its descriptor.
Return nil when the file cannot be watched right now, for example because it
is missing; the caller retries.  The watch follows the file's own node, so an
atomic replace leaves it watching a node nothing writes again, and every event
re-arms it."
  (when-let* ((old (gethash file etaf-md-preview--follow-watches)))
    (ignore-errors (file-notify-rm-watch old))
    (remhash file etaf-md-preview--follow-watches))
  (when-let* ((watch
               (condition-case nil
                   (file-notify-add-watch
                    file '(change)
                    (lambda (event)
                      (etaf-md-preview--follow-event file event)))
                 (file-error nil))))
    (puthash file watch etaf-md-preview--follow-watches)
    watch))

(defun etaf-md-preview--follow-event (file event)
  "Repaint FILE's preview for EVENT, and re-arm the watch only when it died.
A watch is re-armed for `stopped' and `deleted' alone: an editor that saves by
replacing the file takes the watched node away, so the name has to be watched
again.  Re-arming on every event instead re-adds a watch from inside its own
callback, and the backend reports the same write again, which never settles."
  (let ((action (nth 1 event)))
    (when (memq action '(changed created renamed))
      (etaf-md-preview--follow-schedule file))
    (when (memq action '(stopped deleted))
      (etaf-md-preview--follow-rearm file etaf-md-preview--follow-attempts))))

(defun etaf-md-preview--follow-rearm (file attempts)
  "Watch FILE again, retrying ATTEMPTS times while it is missing."
  (when (gethash file etaf-md-preview--follow-sessions)
    (unless (etaf-md-preview--follow-watch file)
      (when (> attempts 0)
        (run-with-timer etaf-md-preview--follow-retry nil
                        #'etaf-md-preview--follow-rearm file (1- attempts))))))

(defun etaf-md-preview--follow-register (session)
  "Make the displayed SESSION follow its source file.
A session created from text has no file, so it never follows anything."
  (when-let* ((path (plist-get session :file)))
    (let ((file (expand-file-name path)))
      (puthash file session etaf-md-preview--follow-sessions)
      (etaf-md-preview--follow-watch file)
      (etaf-md-preview--live-start session)
      (add-hook 'after-save-hook #'etaf-md-preview--follow-saved t))))

(defun etaf-md-preview--follow-unregister (file)
  "Stop following FILE: drop its watch, pending repaint and session."
  (when-let* ((file (and file (expand-file-name file))))
    (etaf-md-preview--live-stop file)
    (when-let* ((watch (gethash file etaf-md-preview--follow-watches)))
      (ignore-errors (file-notify-rm-watch watch))
      (remhash file etaf-md-preview--follow-watches))
    (when-let* ((timer (gethash file etaf-md-preview--follow-timers)))
      (cancel-timer timer)
      (remhash file etaf-md-preview--follow-timers))
    (remhash file etaf-md-preview--follow-sessions)
    (when (zerop (hash-table-count etaf-md-preview--follow-sessions))
      (remove-hook 'after-save-hook #'etaf-md-preview--follow-saved))))

(defun etaf-md-preview--follow-schedule (file)
  "Repaint FILE's preview on the next frame.
A repaint already scheduled for FILE is kept rather than replaced, so the
delay measures from the first change of the burst and the preview follows a
typing hand instead of going quiet until it stops."
  (when (and (gethash file etaf-md-preview--follow-sessions)
             (not (gethash file etaf-md-preview--follow-timers)))
    (puthash file
             (run-with-timer etaf-md-preview--follow-tick nil
                             #'etaf-md-preview--follow-repaint file)
             etaf-md-preview--follow-timers)))

(defun etaf-md-preview--follow-repaint (file)
  "Repaint FILE's preview after its source changed.
The repaint republishes the document and lets the framework draw it the way
any other reactive change does: forcing a flush or moving the reading window
from the timer that runs here re-enters the update while a redisplay is
pending, and that spins instead of settling."
  (remhash file etaf-md-preview--follow-timers)
  ;; An outside change wins over an open session: what the session holds is
  ;; already written to the source buffer, so ending it loses nothing.
  (when-let* ((editing (gethash file etaf-md-preview--follow-sessions)))
    (etaf-md-preview--edit-finish editing))
  (when-let* ((session (gethash file etaf-md-preview--follow-sessions))
              (buffer (plist-get session :buffer))
              ((buffer-live-p buffer)))
    (condition-case error
        (let ((source (plist-get session :source-buffer)))
          (if (buffer-live-p source)
              (etaf-md-preview--publish
               session
               (with-current-buffer source
                 (buffer-substring-no-properties (point-min) (point-max))))
            (etaf-md-preview-refresh session)))
      (etaf-md-preview-error
       (message "Preview kept its last render: %s"
                (error-message-string error))))))

;;; ---------------------------------------------------------------------------
;;; Editing a block in place
;;; ---------------------------------------------------------------------------

(defun etaf-md-preview--block-at-point (session)
  "Return the key of the block the point sits in, or nil.
Every block is published inside a box whose reference the session maps back to
the block's key, so the lookup walks out from the text under the point.  The
rendered line carries padding outside those boxes, so a point in that padding
falls back to the rest of its line."
  (let* ((runtime (etaf-runtime-for-buffer (current-buffer)))
         (refs (plist-get session :block-refs)))
    (when (and runtime refs)
      (let ((block (lambda (position)
                     (when-let* ((ref (etaf-host-ref-at
                                       runtime position
                                       (lambda (ref) (gethash ref refs)))))
                       (gethash ref refs)))))
        (or (funcall block (point))
            (save-excursion
              (goto-char (line-beginning-position))
              (catch 'found
                (while (< (point) (line-end-position))
                  (when-let* ((key (funcall block (point))))
                    (throw 'found key))
                  (forward-char 1))
                nil)))))))

(defun etaf-md-preview--edit-command ()
  "End the open edit session."
  (interactive)
  (when-let* ((session (ignore-errors (etaf-app-state))))
    (etaf-md-preview--edit-finish session)))

(defun etaf-md-preview--edit-keymap ()
  "Return the keymap the edit session consults before its own editing keys."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "ESC") #'etaf-md-preview--edit-command)
    map))

(defun etaf-md-preview--edit-changed (session value)
  "Write VALUE back over SESSION's editing block in its source buffer.
The write is marked as the preview's own, so the follow hook leaves it alone:
the session is already showing it."
  (let* ((edit (plist-get session :edit))
         (buffer (plist-get session :source-buffer))
         (begin (plist-get edit :begin))
         (end (plist-get edit :end))
         (text (substring-no-properties value)))
    (when (and edit (buffer-live-p buffer) (integerp begin))
      (with-current-buffer buffer
        (let ((etaf-md-preview--writing-back t))
          ;; One keystroke is one undo step: the delete and the insert that
          ;; replace the block's source belong to the same change group.
          (atomic-change-group
            (goto-char (min begin (point-max)))
            (delete-region begin (min end (point-max)))
            (goto-char (min begin (point-max)))
            (insert text))))
      (plist-put edit :end (+ begin (length text)))
      (plist-put session :editing-source text))))

(defun etaf-md-preview--edit-start (session key)
  "Show block KEY of SESSION as its source and edit it in place.
The block is published again with its own source in one text host, and a
native edit session opens on that host: the caret, motion, selection, undo,
kill and input methods are the buffer's own.  Every change is written back to
the source buffer as it happens, so the two views never disagree."
  (etaf-md-preview--edit-finish session)
  (let* ((buffer (plist-get session :source-buffer))
         (preview (plist-get session :buffer))
         (runtime (and (buffer-live-p preview) (etaf-runtime-for-buffer preview)))
         (entry (and key (gethash key (plist-get session :published))))
         (block (plist-get entry :block))
         (begin (plist-get block :begin))
         (end (plist-get block :end))
         (trailing (cdr (etaf-md-preview--edit-slice
                         (etaf-md-preview--block-source
                          block (with-current-buffer buffer
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))))))
    (unless (and runtime (buffer-live-p buffer) (integerp begin) (integerp end))
      (user-error "This block cannot be edited in place"))
    (with-current-buffer buffer (undo-boundary))
    (plist-put session :editing-key key)
    (etaf-md-preview--publish
     session (with-current-buffer buffer
               (buffer-substring-no-properties (point-min) (point-max))))
    (etaf-runtime-flush runtime)
    (let ((opened (etaf-input-begin
                   runtime etaf-md-preview--edit-ref
                   :caret 0
                   :keymap (etaf-md-preview--edit-keymap)
                   :on-change (lambda (value)
                                (etaf-md-preview--edit-changed session value))
                   :on-leave (lambda () (etaf-md-preview--edit-finish session)))))
      (plist-put session :edit (list :begin (1+ begin)
                                     :end (1+ (- end trailing))
                                     :session opened)))
    session))

(defun etaf-md-preview--edit-finish (session)
  "End SESSION's edit session, if any, and render the block again."
  (when (plist-get session :edit)
    (ignore-errors (etaf-input-end (plist-get (plist-get session :edit) :session)))
    (plist-put session :edit nil)
    (plist-put session :editing-key nil)
    (plist-put session :editing-source nil)
    (let ((buffer (plist-get session :source-buffer)))
      (when (buffer-live-p buffer)
        (etaf-md-preview--publish
         session (with-current-buffer buffer
                   (buffer-substring-no-properties (point-min) (point-max)))))))
  session)

(defun etaf-md-preview-edit-block ()
  "Edit the block at point as its markdown source.
The block shows the text it was parsed from, with the caret, motion,
selection, undo and input methods of an ordinary buffer, and every change goes
back into the source buffer at once.  `Esc', a click outside the block, or
ending the preview renders it again."
  (interactive)
  (let* ((session (etaf-md-preview--current-session))
         (key (etaf-md-preview--block-at-point session)))
    (unless key
      (user-error "No markdown block under the point"))
    (etaf-md-preview--edit-start session key)))

(etaf-define-component etaf-md-preview-app (&key session)
  "Render the markdown preview for SESSION and own its lifecycle.
SESSION must come from `etaf-md-preview-create'.  Mounting provides the
palette for its subtree; every displayed value is read from the session's
reactive sources, so a refresh, a width change or a theme change repaints the
whole preview without a remount."
  :bindings ((preview (or session
                          (signal 'etaf-md-preview-error
                                  '("etaf-md-preview-app needs the :session from `etaf-md-preview-create'"))))
             (document (etaf-md-preview--ref preview :document))
             (theme (etaf-md-preview--ref preview :theme))
             (mode (etaf-md-preview--ref preview :theme-mode))
             (tokens (etaf-computed
                      (lambda ()
                        (etaf-md-preview-theme-tokens
                         (etaf-value theme)
                         (etaf-md-preview--session-palette preview)))
                      :name 'etaf-md-preview-theme-tokens)))
  :setup
  (progn
    (etaf-theme-provide tokens)
    ;; One appearance state: `auto' resolves to the frame's faces, a named
    ;; mode paints its fixed page, and a panel's title-bar switch writes the
    ;; same cell as `t', so the document and the panel chrome follow it.
    (etaf-watch mode
                (lambda (mode _old)
                  (etaf-batch
                    (etaf-md-preview--write preview :theme
                                             (if (eq mode 'auto)
                                                 (etaf-md-preview-auto-theme)
                                               mode))
                    (etaf-md-preview--write preview :theme-source
                                             (if (eq mode 'auto) 'frame 'palette))))
                :immediate t :name 'etaf-md-preview-mode)
    (when-let* ((surface (etaf-surface-host)))
      (etaf-surface-declare surface :theme tokens :mode mode
                            :modes etaf-md-preview--surface-modes)))
  :render
  (etaf-md-preview--view-document
   preview
   (etaf-value document)
   (etaf-md-preview--env preview)))

(defun etaf-md-preview-view (&optional session)
  "Return the preview View for SESSION.
SESSION defaults to a new session over an empty document; pass a session from
`etaf-md-preview-create' to mount a real one."
  (etaf-view (etaf-md-preview-app
              :session (or session (etaf-md-preview-create :text "")))))

;;;###autoload
(defun etaf-md-preview-open (&optional source)
  "Open a markdown preview and return its buffer.
SOURCE is a file path, a session from `etaf-md-preview-create', or nil to
prompt for a file.  Interactively it defaults to the current buffer's
markdown file and then to a file prompt.  Opening a file that already has a
preview buffer selects it instead of rendering a second copy; opening a
session mounts that session, which lets scripts render a document they built
themselves."
  (interactive
   (list (read-file-name
          "Markdown file: " nil nil t
          (when (and buffer-file-name
                     (string-match-p "\\.\\(md\\|markdown\\|mdown\\|mkd\\)\\'" buffer-file-name))
            (file-relative-name buffer-file-name default-directory)))))
  (if (and (listp source) (etaf-ref-p (plist-get source :document)))
      (etaf-md-preview--display source (etaf-md-preview--buffer-name-for-session source))
    (let* ((file (expand-file-name source))
           (name (etaf-md-preview--buffer-name file))
           (existing (get-buffer name)))
      (if (and existing (etaf-app-state existing))
          (switch-to-buffer existing)
        (etaf-md-preview--display (etaf-md-preview-create :file file) name)))))

;;;###autoload
(defun etaf-md-preview (&optional source)
  "Show SOURCE's preview beside its source: source on the left, preview right.
SOURCE is a file path.  Interactively it is the current buffer's Markdown
file, and a buffer that visits no Markdown file prompts for one.  Any previous
window layout gives way to those two panes, the point stays in the source, and
`etaf-md-preview-close' gives the layout back to the source alone."
  (interactive
   (list (or (and buffer-file-name
                  (string-match-p "\\.\\(md\\|markdown\\|mdown\\|mkd\\)\\'"
                                  buffer-file-name)
                  (expand-file-name buffer-file-name))
             (expand-file-name
              (read-file-name "Markdown file: " nil nil t
                              (and buffer-file-name
                                   (file-relative-name buffer-file-name
                                                       default-directory)))))))
  (let* ((file (expand-file-name source))
         (buffer (or (get-file-buffer file) (find-file-noselect file))))
    (delete-other-windows)
    (switch-to-buffer buffer)
    (select-window (split-window-right))
    (etaf-md-preview-open file)
    (when-let* ((window (get-buffer-window buffer t)))
      (select-window window))))

(defun etaf-md-preview--buffer-name-for-session (session)
  "Return the preview buffer name for SESSION."
  (if-let* ((file (plist-get session :file)))
      (etaf-md-preview--buffer-name file)
    "*ETAF Markdown*"))

(defun etaf-md-preview--display (session name)
  "Open SESSION as the application buffer NAME and return that buffer.
The showing window sets the initial reading measure only; later resizes
reflow the viewport-relative column without changing it, so a width chosen
with `[' and `]' survives them.  Cells the page does not paint, such as the
sub-cell remainder at the window edge, take the page background, and a
session without one lets the Emacs theme show through."
  (let* ((measured nil)
         (buffer
          (etaf-app-open
           name
           (etaf-view (etaf-md-preview-app :session session :keys etaf-md-preview--keys))
           :state session
           :mode-line t
           :background (lambda () (etaf-md-preview--page-background session))
           :on-fit (lambda (_window)
                     (unless measured
                       (setq measured t)
                       (plist-put session :buffer (current-buffer))
                       (etaf-md-preview--fit-width session)))
           :on-close (lambda ()
                       (etaf-md-preview--edit-finish session)
                       (etaf-md-preview--follow-unregister (plist-get session :file))
                       (plist-put session :buffer nil)))))
    (plist-put session :buffer buffer)
    (etaf-md-preview--follow-register session)
    (add-hook 'kill-buffer-hook
              (lambda ()
                (etaf-md-preview--edit-finish session)
                (etaf-md-preview--follow-unregister (plist-get session :file)))
              t t)
    (with-current-buffer buffer
      (setq-local imenu-create-index-function
                  (lambda ()
                    (let ((outline (etaf-md-preview-outline session)))
                      (mapcar (lambda (cell)
                                (cons (nth 1 (nth (car cell) outline))
                                      (cdr cell)))
                              (etaf-md-preview--heading-positions session))))))
    (etaf-runtime-flush (etaf-runtime-for-buffer buffer))
    buffer))

(provide 'etaf-md-preview-app)
;;; etaf-md-preview-app.el ends here
