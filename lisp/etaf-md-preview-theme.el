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
(require 'etaf-ui)

(defconst etaf-md-preview-themes
  '((dark . ((:bg . "#191a1c")
             (:ink . "#e6edf3")
             (:muted . "#9198a1")
             (:faint . "#6e7681")
             (:accent . "#4493f8")
             (:accent-soft . "#152c4d")
             (:border . "#303338")
             (:panel . "#202225")
             (:panel2 . "#25272b")
             (:quote-bar . "#43464b")
             (:quote-bg . "#1c1e21")
             (:table-header . "#202225")
             (:table-stripe . "#1c1e21")
             (:heading . "#e6edf3")
             (:heading-rule . "#303338")
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

(defconst etaf-md-preview--surface-modes
  '((light . "亮") (auto . "随") (dark . "暗") (sepia . "纸"))
  "Appearance modes the preview offers its host, in display order.
`auto' derives the document colors from the active Emacs theme; the other
three paint a fixed page palette.")

(defcustom etaf-md-preview-theme 'auto
  "Initial palette of `etaf-md-preview-create' and the page it paints.
`auto' derives the document colors from the active frame's own faces
(`default', `shadow', `outline-1' and the semantic faces) and paints no page
background, so the preview matches whatever Emacs theme is active.  `dark',
`light' and `sepia' select a fixed palette explicitly, including its page
background; `etaf-md-preview-set-theme' and `etaf-md-preview-cycle-theme'
always select one of those fixed palettes."
  :type '(choice (const :tag "Follow the Emacs theme" auto)
                 (const :tag "Dark page" dark)
                 (const :tag "Light page" light)
                 (const :tag "Sepia page" sepia))
  :group 'etaf-md-preview)

(defun etaf-md-preview-auto-theme ()
  "Return the palette matching the host appearance."
  (etaf-appearance))

(defun etaf-md-preview--face-color (face attribute fallback)
  "Return FACE's ATTRIBUTE color, or FALLBACK when it is unspecified."
  (let ((value (ignore-errors (face-attribute face attribute nil t))))
    (if (and (stringp value) (not (equal value "unspecified")))
        value
      fallback)))

(defun etaf-md-preview--mix-color (base overlay weight)
  "Return BASE mixed toward OVERLAY by WEIGHT (0 to 1) as #rrggbb.
Unknown color names fall back to BASE so a theme with named colors still
resolves a usable surface."
  (let ((base-rgb (ignore-errors (color-values base)))
        (overlay-rgb (ignore-errors (color-values overlay))))
    (if (and base-rgb overlay-rgb)
        (let ((mixed (cl-loop for base-value in (cl-subseq base-rgb 0 3)
                              for overlay-value in (cl-subseq overlay-rgb 0 3)
                              collect (ash (round (+ (* base-value (- 1 weight))
                                                     (* overlay-value weight)))
                                           -8))))
          (apply #'format "#%02x%02x%02x" mixed))
      base)))

(defun etaf-md-preview--palette-from-faces ()
  "Return a palette derived from the active frame's own faces.
Every surface is a blend of the frame's background toward its foreground or
a semantic accent, so `auto' matches whatever Emacs theme is active instead
of a fixed page palette."
  (let* ((background (etaf-md-preview--face-color 'default :background "#ffffff"))
         (foreground (etaf-md-preview--face-color 'default :foreground "#1f2328"))
         (shadow (etaf-md-preview--face-color
                  'shadow :foreground
                  (etaf-md-preview--mix-color background foreground 0.55)))
         (link (or (etaf-md-preview--face-color 'link :foreground nil)
                   (etaf-md-preview--face-color 'button :foreground nil)
                   (etaf-md-preview--face-color
                    'font-lock-function-name-face :foreground nil)
                   foreground))
         (heading (etaf-md-preview--face-color 'outline-1 :foreground foreground))
         (ok (etaf-md-preview--face-color 'success :foreground "#1a7f37"))
         (warn (etaf-md-preview--face-color 'warning :foreground "#9a6700"))
         (danger (etaf-md-preview--face-color 'error :foreground "#d1242f"))
         (important (etaf-md-preview--face-color
                     'font-lock-keyword-face :foreground link)))
    (list (cons :bg background)
          (cons :ink foreground)
          (cons :muted shadow)
          (cons :faint (etaf-md-preview--mix-color shadow background 0.4))
          (cons :accent link)
          (cons :accent-soft (etaf-md-preview--mix-color background link 0.18))
          (cons :border (etaf-md-preview--mix-color background foreground 0.18))
          (cons :panel (etaf-md-preview--mix-color background foreground 0.06))
          (cons :panel2 (etaf-md-preview--mix-color background foreground 0.10))
          (cons :quote-bar (etaf-md-preview--mix-color background foreground 0.25))
          (cons :quote-bg (etaf-md-preview--mix-color background foreground 0.04))
          (cons :table-header (etaf-md-preview--mix-color background foreground 0.07))
          (cons :table-stripe (etaf-md-preview--mix-color background foreground 0.03))
          (cons :heading heading)
          (cons :heading-rule (etaf-md-preview--mix-color background foreground 0.18))
          (cons :ok ok)
          (cons :ok-soft (etaf-md-preview--mix-color background ok 0.14))
          (cons :warn warn)
          (cons :warn-soft (etaf-md-preview--mix-color background warn 0.14))
          (cons :danger danger)
          (cons :danger-soft (etaf-md-preview--mix-color background danger 0.14))
          (cons :important important)
          (cons :important-soft (etaf-md-preview--mix-color background important 0.14)))))

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

(defun etaf-md-preview--base-pixels ()
  "Return the frame's default font size in CSS reference pixels."
  (let ((height (face-attribute 'default :height nil t)))
    (if (numberp height)
        (max 8 (round (/ (* height 4.0) 30.0)))
      16.0)))

(defun etaf-md-preview-theme-tokens (theme &optional palette)
  "Return the ETAF UI token plist for palette THEME.
PALETTE, when given, supplies the colors instead of THEME's named palette
\(used by the face-derived `auto' appearance).  The application provides this
plist through `etaf-theme-provide', so shared ETAF UI Components follow the
previewer's colors."
  (let* ((dark (eq theme 'dark))
         (lookup (lambda (key)
                   (or (cdr (assq key palette))
                       (etaf-md-preview-color key theme))))
         (bg (funcall lookup :bg))
         (panel (funcall lookup :panel))
         (panel2 (funcall lookup :panel2))
         (ink (funcall lookup :ink))
         (line (funcall lookup :border))
         (muted (funcall lookup :muted))
         (accent (funcall lookup :accent))
         (accent-soft (funcall lookup :accent-soft)))
    (etaf-ui-theme-tokens
     (list :bg bg :fg ink :panel panel :track panel2 :border line :muted muted
           :accent accent :accent-fg "#ffffff" :accent-soft accent-soft
           :success (if dark "#3fb950" "#1a7f37")
           :success-bg (if dark "#12261e" "#dafbe1")
           :success-border (if dark "#2ea04366" "#aceebb")
           :warning (if dark "#d29922" "#9a6700")
           :warning-bg (if dark "#272115" "#fff8c5")
           :warning-border (if dark "#d2992266" "#d4a72c")
           :danger (if dark "#f85149" "#d1242f")
           :danger-bg (if dark "#2a1518" "#ffebe9")
           :danger-border (if dark "#f8514966" "#ff818266")
           :info accent :info-bg accent-soft :info-border line))))

(provide 'etaf-md-preview-theme)
;;; etaf-md-preview-theme.el ends here
