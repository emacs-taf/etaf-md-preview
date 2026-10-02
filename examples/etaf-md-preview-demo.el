;;; etaf-md-preview-demo.el --- Open the bundled sample document -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Runnable usage example.  It opens `examples/sample.markdown' with the package's
;; public entry, so a checkout can render the sample without typing a path:
;;
;;   (load-file "examples/etaf-md-preview-demo.el")
;;   M-x etaf-md-preview-demo-open
;;
;; The sample demonstrates every supported markdown construction, including
;; the local screenshot in `examples/'.

;;; Code:

(require 'etaf-md-preview)

(defun etaf-md-preview-demo-open ()
  "Open the bundled sample document as a preview."
  (interactive)
  (etaf-md-preview-open
   (expand-file-name "sample.markdown"
                     (file-name-directory (or load-file-name buffer-file-name)))))

(provide 'etaf-md-preview-demo)
;;; etaf-md-preview-demo.el ends here
