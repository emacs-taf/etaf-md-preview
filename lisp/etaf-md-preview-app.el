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
  "Unmount the current preview and kill its buffer."
  (interactive)
  (etaf-md-preview--current-session)
  (etaf-app-close))

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
      (if (and existing (etaf-app-state existing))
          (switch-to-buffer existing)
        (etaf-md-preview--display (etaf-md-preview-create :file file) name)))))

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
           :on-close (lambda () (plist-put session :buffer nil)))))
    (plist-put session :buffer buffer)
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
