;;; etaf-md-preview-theme.el --- Palettes for etaf-md-preview -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Internal palettes of the markdown previewer.  A palette is a plain alist of
;; semantic color keys; the views read every color through
;; `etaf-md-preview-color', so both palettes share one layout and a palette
;; change never leaks a half-updated set of colors.

;;; Code:

(require 'cl-lib)

(defconst etaf-md-preview-themes
  '((dark . ((:bg . "#0d1117")
             (:ink . "#e6edf3")
             (:muted . "#9198a1")
             (:faint . "#6e7681")
             (:accent . "#4493f8")
             (:accent-soft . "#152c4d")
             (:border . "#30363d")
             (:panel . "#161b22")
             (:panel2 . "#1c2430")
             (:quote-bar . "#3d444d")
             (:quote-bg . "#12161c")
             (:table-header . "#161b22")
             (:table-stripe . "#11151b")
             (:heading . "#e6edf3")
             (:heading-rule . "#30363d")
             (:ok . "#3fb950")
             (:ok-soft . "#12261e")
             (:warn . "#d29922")
             (:warn-soft . "#272115")
             (:danger . "#f85149")
             (:danger-soft . "#2a1518")
             (:important . "#a371f7")
             (:important-soft . "#221a38")))
    (light . ((:bg . "#ffffff")
              (:ink . "#1f2328")
              (:muted . "#59636e")
              (:faint . "#818b98")
              (:accent . "#0969da")
              (:accent-soft . "#ddf4ff")
              (:border . "#d1d9e0")
              (:panel . "#f6f8fa")
              (:panel2 . "#eef1f4")
              (:quote-bar . "#d1d9e0")
              (:quote-bg . "#f9fbfd")
              (:table-header . "#f0f3f6")
              (:table-stripe . "#fbfcfd")
              (:heading . "#1f2328")
              (:heading-rule . "#d1d9e0")
              (:ok . "#1a7f37")
              (:ok-soft . "#dafbe1")
              (:warn . "#9a6700")
              (:warn-soft . "#fff8c5")
              (:danger . "#d1242f")
              (:danger-soft . "#ffebe9")
              (:important . "#8250df")
              (:important-soft . "#fbefff")))
    (sepia . ((:bg . "#f5ecd9")
              (:ink . "#3b3024")
              (:muted . "#7a6a53")
              (:faint . "#9a8a72")
              (:accent . "#b3541e")
              (:accent-soft . "#efddc4")
              (:border . "#d8c9ab")
              (:panel . "#efe3cc")
              (:panel2 . "#e9dcc2")
              (:quote-bar . "#c9b694")
              (:quote-bg . "#f0e6d2")
              (:table-header . "#eadcc0")
              (:table-stripe . "#f1e8d6")
              (:heading . "#3b3024")
              (:heading-rule . "#d8c9ab")
              (:ok . "#3f6212")
              (:ok-soft . "#e2ecd0")
              (:warn . "#92400e")
              (:warn-soft . "#f7e6c4")
              (:danger . "#9f1239")
              (:danger-soft . "#f5d6cf")
              (:important . "#6d28d9")
              (:important-soft . "#e9dcf5"))))
  "Color palettes of the markdown previewer.")

(defun etaf-md-preview-auto-theme ()
  "Return the palette matching the current frame's background mode."
  (if (eq (frame-parameter nil 'background-mode) 'dark) 'dark 'light))

(defun etaf-md-preview-color (key theme)
  "Return THEME's color for semantic KEY.
Signal `etaf-md-preview-error' when either is unknown."
  (let ((palette (cdr (assq theme etaf-md-preview-themes))))
    (unless palette
      (signal 'etaf-md-preview-error
              (list (format "Unknown markdown preview theme: %S" theme))))
    (let ((color (cdr (assq key palette))))
      (unless color
        (signal 'etaf-md-preview-error
                (list (format "Unknown markdown preview color: %S" key))))
      color)))

(defun etaf-md-preview--mono-family ()
  "Return the family of the frame's fixed-pitch font, or nil.
A frame whose fixed-pitch face inherits the default family yields nil, and
code spans then use the frame's own font instead of guessing a family."
  (let ((family (face-attribute 'fixed-pitch :family nil t)))
    (and (stringp family) (not (string-empty-p family)) family)))

(defun etaf-md-preview--base-pixels ()
  "Return the frame's default font size in CSS reference pixels."
  (let ((height (face-attribute 'default :height nil t)))
    (if (numberp height)
        (max 8 (round (/ (* height 4.0) 30.0)))
      16.0)))

(defun etaf-md-preview-theme-tokens (theme)
  "Return the ETAF UI token plist for palette THEME.
The application provides this plist through `etaf-theme-provide', so shared
ETAF UI Components follow the previewer's palette."
  (let* ((dark (eq theme 'dark))
         (bg (etaf-md-preview-color :bg theme))
         (panel (etaf-md-preview-color :panel theme))
         (panel2 (etaf-md-preview-color :panel2 theme))
         (ink (etaf-md-preview-color :ink theme))
         (line (etaf-md-preview-color :border theme))
         (muted (etaf-md-preview-color :muted theme))
         (accent (etaf-md-preview-color :accent theme))
         (accent-soft (etaf-md-preview-color :accent-soft theme)))
    (list :ink ink :paper bg :panel panel :line line :muted muted
          :accent accent :accent-soft accent-soft
          :ui-fg ink :ui-bg bg :ui-border line :ui-muted-fg muted
          :ui-disabled-fg muted :ui-disabled-bg panel :ui-disabled-border line
          :ui-accent accent :ui-accent-fg "#ffffff" :ui-accent-soft-bg accent-soft
          :ui-button-primary-fg "#ffffff" :ui-button-primary-bg accent
          :ui-button-secondary-fg accent :ui-button-secondary-bg accent-soft
          :ui-button-ghost-fg muted
          :ui-success-fg (if dark "#3fb950" "#1a7f37")
          :ui-success-bg (if dark "#12261e" "#dafbe1")
          :ui-success-border (if dark "#2ea04366" "#aceebb")
          :ui-warning-fg (if dark "#d29922" "#9a6700")
          :ui-warning-bg (if dark "#272115" "#fff8c5")
          :ui-warning-border (if dark "#d2992266" "#d4a72c")
          :ui-danger-fg (if dark "#f85149" "#d1242f")
          :ui-danger-bg (if dark "#2a1518" "#ffebe9")
          :ui-danger-border (if dark "#f8514966" "#ff818266")
          :ui-info-fg accent :ui-info-bg accent-soft :ui-info-border line
          :ui-panel-fg ink :ui-panel-bg panel :ui-panel-border line
          :ui-divider-bg line
          :ui-progress-track panel2 :ui-progress-value accent
          :ui-table-border line :ui-table-selected-fg accent
          :ui-table-selected-bg accent-soft
          :ui-checkbox-enabled-fg ink :ui-checkbox-disabled-fg muted)))

(provide 'etaf-md-preview-theme)
;;; etaf-md-preview-theme.el ends here
