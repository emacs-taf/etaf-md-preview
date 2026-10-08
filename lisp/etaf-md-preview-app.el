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
;; `etaf-md-preview-open' adds what a Component cannot own: the buffer, the
;; buffer-local keymap, the initial reading measure, and the commands for
;; outline navigation, reload and theming.  A window resize reflows through the
;; framework's own viewport update, so the application never rebuilds the view
;; for a geometry change.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'etaf)
(require 'etaf-ui)
(require 'etaf-md-preview-theme)
(require 'etaf-md-preview-model)
(require 'etaf-md-preview-views)

(defvar-local etaf-md-preview--session nil
  "Preview session shown by the current buffer's runtime.")

(defun etaf-md-preview--buffer-name (file)
  "Return the preview buffer name for FILE."
  (format "*ETAF Markdown: %s*" (file-name-nondirectory file)))

(defvar etaf-md-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'etaf-md-preview-next-heading)
    (define-key map (kbd "p") #'etaf-md-preview-previous-heading)
    (define-key map (kbd "TAB") #'etaf-md-preview-goto-heading)
    (define-key map (kbd "r") #'etaf-md-preview-reload)
    (define-key map (kbd "t") #'etaf-md-preview--cycle-theme-command)
    (define-key map (kbd "[") #'etaf-md-preview-narrower)
    (define-key map (kbd "]") #'etaf-md-preview-wider)
    (define-key map (kbd "?") #'etaf-md-preview-help)
    (define-key map (kbd "q") #'etaf-md-preview-close)
    map)
  "Keymap of a buffer opened by `etaf-md-preview-open'.
Native scrolling, isearch and link activation stay available; the map only
adds outline navigation, reloading, theming and closing.")

(defun etaf-md-preview--current-session ()
  "Return the preview session of the current buffer."
  (or etaf-md-preview--session
      (user-error "Not an ETAF markdown preview buffer")))

(defun etaf-md-preview--remap-canvas-face (session)
  "Paint the current buffer's unpainted cells with SESSION's background.
Ebox paints whole character cells; remapping the default face keeps the
sub-cell remainder at the window edge in the page's background.  A session
without a page background resets the remap so the Emacs theme shows through."
  (if-let* ((background (etaf-md-preview--page-background session)))
      (face-remap-set-base 'default (list :background background))
    (face-remap-reset-base 'default)))

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
    (etaf-md-preview--remap-canvas-face session)
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

(defun etaf-md-preview-help ()
  "Show the previewer's key bindings."
  (interactive)
  (message
   (concat "n/p 上/下个标题 · TAB 选择标题 · r 重新加载 · t 切换主题"
           " · [ ] 调整栏宽 · q 关闭 · 链接/图片按 RET 打开 · 视频按 RET 播放/暂停")))

(defun etaf-md-preview-close ()
  "Unmount the current preview and kill its buffer."
  (interactive)
  (let ((session (etaf-md-preview--current-session))
        (buffer (current-buffer)))
    (when-let* ((runtime (etaf-runtime-for-buffer buffer)))
      (etaf-unmount runtime))
    (plist-put session :buffer nil)
    (kill-buffer buffer)))

;;; ---------------------------------------------------------------------------
;;; Window fitting
;;; ---------------------------------------------------------------------------

(defun etaf-md-preview--effective-width (session)
  "Return the reading width the window actually gives SESSION.
The session's `:width' is the maximum measure; a narrower window fills the
available columns instead, so this reports what is on screen."
  (let* ((buffer (plist-get session :buffer))
         (window (and (buffer-live-p buffer) (get-buffer-window buffer t)))
         (cap (etaf-md-preview--read session :width)))
    (if (window-live-p window)
        (min cap (max 24 (- (window-body-width window) 4)))
      cap)))

;;; ---------------------------------------------------------------------------
;;; Application Component and entry points
;;; ---------------------------------------------------------------------------

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
             (tokens (etaf-computed
                      (lambda ()
                        (etaf-md-preview-theme-tokens
                         (etaf-value theme)
                         (etaf-md-preview--session-palette preview)))
                      :name 'etaf-md-preview-theme-tokens)))
  :setup
  (etaf-theme-provide tokens)
  :render
  (etaf-md-preview--view-document
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
            buffer-file-name))))
  (if (and (listp source) (etaf-ref-p (plist-get source :document)))
      (etaf-md-preview--display source (etaf-md-preview--buffer-name-for-session source))
    (let* ((file (expand-file-name source))
           (name (etaf-md-preview--buffer-name file))
           (existing (get-buffer name)))
      (if (and existing (buffer-live-p existing)
               (buffer-local-value 'etaf-md-preview--session existing))
          (pop-to-buffer existing)
        (etaf-md-preview--display (etaf-md-preview-create :file file) name)))))

(defun etaf-md-preview--buffer-name-for-session (session)
  "Return the preview buffer name for SESSION."
  (if-let* ((file (plist-get session :file)))
      (etaf-md-preview--buffer-name file)
    "*ETAF Markdown*"))

(defun etaf-md-preview--display (session name)
  "Mount SESSION in the buffer NAME and return that buffer."
  (let ((buffer (get-buffer-create name)))
    (plist-put session :buffer buffer)
    (with-current-buffer buffer
      (setq-local etaf-md-preview--session session)
      (setq-local truncate-lines t)
      ;; The preview owns every column it paints; a sub-cell rounding
      ;; remainder must not surface as Emacs's truncation glyph.
      (setq-local buffer-display-table
                  (let ((table (copy-sequence (or standard-display-table
                                                  (make-display-table)))))
                    (set-display-table-slot table 'truncation ?\s)
                    table))
      (etaf-md-preview--remap-canvas-face session)
      (use-local-map etaf-md-preview-mode-map)
      (setq-local imenu-create-index-function
                  (lambda ()
                    (let ((outline (etaf-md-preview-outline session)))
                      (mapcar (lambda (cell)
                                (cons (nth 1 (nth (car cell) outline))
                                      (cdr cell)))
                              (etaf-md-preview--heading-positions session))))))
    (pop-to-buffer buffer)
    (etaf-md-preview--fit-width session)
    (etaf-mount buffer (etaf-md-preview-view session))
    (etaf-runtime-flush (etaf-runtime-for-buffer buffer))
    buffer))

(provide 'etaf-md-preview-app)
;;; etaf-md-preview-app.el ends here
