;;; md-preview-gui-probe.el --- Drive and inspect a live preview -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The preview's typography can only be judged in a real graphical frame:
;; heading scales, the reading measure, media grids and link hit areas depend
;; on the frame's fonts and pixel metrics.  This file is the maintained probe
;; a GUI check loads, so probes stop being throwaway files that bypass the
;; byte-compilation gate.
;;
;; Load it in a graphical Emacs with this repository and the provider roots on
;; `load-path', then:
;;
;;   (etaf-md-preview-gui-open "README.md" "150x44")  ; open at a frame size
;;   (etaf-md-preview-gui-status)                     ; one-line state for logs
;;   (etaf-md-preview-gui-keys "nt")                  ; real keys through the loop
;;   (etaf-md-preview-gui-check)                      ; invariants; nil when healthy
;;   (etaf-md-preview-gui-close)                      ; release the preview buffer
;;
;; Nothing here is part of the package's public API.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'etaf-md-preview)

(defvar etaf-md-preview-gui--session nil
  "Preview session the probe drives, or nil before the first open.")

(defun etaf-md-preview-gui-open (&optional file size)
  "Open FILE as a preview in the selected frame sized SIZE.
SIZE is a \"WIDTHxHEIGHT\" string and defaults to `MD_PREVIEW_GUI_SIZE' or
\"150x44\".  FILE defaults to the repository README.  Return the new session."
  (let* ((size (or size (getenv "MD_PREVIEW_GUI_SIZE") "150x44"))
         (parts (split-string size "x"))
         (frame (selected-frame))
         (file (or file (getenv "MD_PREVIEW_GUI_FILE")
                   (expand-file-name "README.md"
                                     (file-name-directory
                                      (directory-file-name
                                       (file-name-directory (or load-file-name
                                                                buffer-file-name))))))))
    (set-frame-parameter frame 'fullscreen nil)
    (set-frame-size frame (string-to-number (car parts))
                    (string-to-number (cadr parts)))
    (delete-other-windows)
    (setq etaf-md-preview-gui--session (etaf-md-preview-create :file file))
    (etaf-md-preview-open etaf-md-preview-gui--session)
    (delete-other-windows)
    etaf-md-preview-gui--session))

(defun etaf-md-preview-gui-close ()
  "Unmount and kill the probe's preview buffer."
  (when-let* ((session etaf-md-preview-gui--session)
              (buffer (plist-get session :buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (etaf-md-preview-close))))
  (setq etaf-md-preview-gui--session nil))

(defun etaf-md-preview-gui-status ()
  "Return a one-line summary of the preview's state for logging."
  (let ((state (and etaf-md-preview-gui--session
                    (etaf-md-preview-state etaf-md-preview-gui--session))))
    (format "frame=%dx%d theme=%s width=%s blocks=%s outline=%s footnotes=%s"
            (frame-width) (frame-height)
            (plist-get state :theme) (plist-get state :width)
            (plist-get state :blocks) (length (plist-get state :outline))
            (length (plist-get state :footnotes)))))

(defun etaf-md-preview-gui-keys (keys)
  "Send KEYS through the real command loop and return the new status.
KEYS is a `kbd' string, so \"nt\" moves to the next heading and cycles theme."
  (let ((buffer (and etaf-md-preview-gui--session
                     (plist-get etaf-md-preview-gui--session :buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (execute-kbd-macro (kbd keys)))))
  (etaf-md-preview-gui-status))

(defun etaf-md-preview-gui-check ()
  "Return preview layout problems, or nil when the frame looks healthy.
Checks the invariants a text frame can violate silently: painted lines that
are wider than the window, text drawn without the palette's background, a
missing heading outline, and links that lost their activation keymap."
  (let* ((session etaf-md-preview-gui--session)
         (buffer (and session (plist-get session :buffer)))
         (problems nil))
    (unless (buffer-live-p buffer)
      (push "no live preview buffer" problems))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((window (get-buffer-window buffer t))
               (columns (and (window-live-p window) (window-body-width window)))
               (position (point-min))
               (links 0))
          (unless (window-live-p window)
            (push "the preview buffer is not shown in a live window" problems))
          (when columns
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (let* ((line (buffer-substring (line-beginning-position)
                                               (line-end-position)))
                       (width (string-width (substring-no-properties line))))
                  (when (> width columns)
                    (push (format "line %d is %d columns in a %d-column window"
                                  (line-number-at-pos) width columns)
                          problems)))
                (forward-line 1))))
          (while (< position (point-max))
            (when (get-text-property position 'keymap)
              (cl-incf links))
            (setq position (next-property-change position nil (point-max))))
          (unless (> links 0)
            (push "the document rendered without any interactive link" problems))))
      (unless (plist-get (etaf-md-preview-state session) :outline)
        (push "the document has no outline" problems)))
    (nreverse problems)))

(defun etaf-md-preview-gui-table-boundaries (header)
  "Check every displayed table row below HEADER against its column boundaries.
Measure actual screen positions, not string widths: post-publication font
changes can invalidate layout measurements.  The entire table must be visible.
Return its row boundary lists or signal an error for missing or shifted edges."
  (let* ((buffer (plist-get etaf-md-preview-gui--session :buffer))
         (window (get-buffer-window buffer t))
         rows expected border-color)
    (unless (window-live-p window)
      (error "Preview has no displayed window"))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (search-forward header)
        (beginning-of-line)
        (let ((continue t))
          (while (and continue (not (eobp)))
            (let ((position (point)) (end (line-end-position)) edges)
              (while (< position end)
                (when (equal (get-text-property position 'display)
                             '(space :width (1)))
                  (let* ((face (get-text-property position 'face))
                         (faces (if (keywordp (car-safe face)) (list face) face))
                         (color (seq-some
                                 (lambda (entry)
                                   (and (listp entry) (plist-get entry :background)))
                                 faces)))
                    ;; A one-pixel filler is not a border.  Use the first
                    ;; outer edge's paint to distinguish them at narrow widths.
                    (unless border-color (setq border-color color))
                    (when (and color (equal color border-color))
                      (let ((screen (posn-at-point position window)))
                        (unless screen
                          (error "Table row %d is not visible" (line-number-at-pos)))
                        (push (car (posn-x-y screen)) edges)))))
                (setq position (1+ position)))
              (setq edges (nreverse edges))
              (if (null edges)
                  (setq continue nil)
                (unless expected (setq expected edges))
                (unless (equal expected edges)
                  (error "Table row %d boundaries %S differ from %S"
                         (line-number-at-pos) edges expected))
                (push edges rows)))
            (forward-line 1)))))
    (unless (> (length rows) 1)
      (error "No complete table found for %s" header))
    (nreverse rows)))

(provide 'md-preview-gui-probe)
;;; md-preview-gui-probe.el ends here
