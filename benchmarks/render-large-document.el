;;; render-large-document.el --- Batch render baseline -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Development benchmark for etaf-md-preview.  It generates a deterministic
;; document with SECTIONS sections and prints parse, mount and refresh times,
;; so a change can be compared against a recorded baseline.  Run it with
;; `make benchmark'; it is not part of the public API and is not a gate.

;;; Code:

(require 'cl-lib)
(require 'etaf)
(require 'etaf-md-preview)

(defconst etaf-md-preview-benchmark-sections
  (string-to-number (or (getenv "MD_PREVIEW_BENCH_SECTIONS") "120"))
  "Number of sections the benchmark document contains.")

(defun etaf-md-preview-benchmark--document ()
  "Return the benchmark markdown document."
  (mapconcat
   (lambda (index)
     (format (concat "## 第 %d 节 Section %d\n\n"
                     "This paragraph mentions **bold %d** and *italic* with "
                     "`code` and a [link](https://example.com/%d).\n\n"
                     "- first item\n- second item\n  - nested item\n\n"
                     "> A quoted line for section %d.\n\n"
                     "```elisp\n(defun section-%d () %d)\n```\n\n"
                     "| name | value |\n|:--|--:|\n| a | %d |\n| b | %d |\n\n")
             index index index index index index index index (1+ index))
   (number-sequence 1 etaf-md-preview-benchmark-sections)
   "\n"))

(defun etaf-md-preview-benchmark--milliseconds (function)
  "Call FUNCTION once and return its elapsed milliseconds."
  (let ((start (float-time)))
    (funcall function)
    (* 1000.0 (- (float-time) start))))

(let* ((document (etaf-md-preview-benchmark--document))
       (session nil)
       (buffer nil)
       (parse-ms (etaf-md-preview-benchmark--milliseconds
                  (lambda () (setq session (etaf-md-preview-create :text document)))))
       (mount-ms (etaf-md-preview-benchmark--milliseconds
                  (lambda ()
                    (setq buffer (etaf-mount "*etaf-md-preview-benchmark*"
                                             (etaf-md-preview-view session)
                                             '(:viewport-width 900 :viewport-height 50))))))
       (refresh-ms (etaf-md-preview-benchmark--milliseconds
                    (lambda () (etaf-md-preview-refresh session)))))
  (unwind-protect
      (with-current-buffer buffer
        (princ (format (concat "sections=%d chars=%d lines=%d "
                               "parse=%.1fms mount=%.1fms refresh=%.1fms\n")
                       etaf-md-preview-benchmark-sections (length document)
                       (line-number-at-pos (point-max))
                       parse-ms mount-ms refresh-ms)))
    (when (buffer-live-p buffer) (kill-buffer buffer))))

(provide 'render-large-document)
;;; render-large-document.el ends here
