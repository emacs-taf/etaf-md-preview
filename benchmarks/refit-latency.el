;;; refit-latency.el --- Batch resize-refit baseline -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Development benchmark for the preview's window-resize path.  It mounts a
;; generated document and times `etaf-runtime-refit' across a width sweep,
;; printing wall time, the selected projection and the per-phase breakdown
;; reported by Ebox.  Run it with `make refit-benchmark'; it is not part of
;; the public API and is not a gate.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'etaf)
(require 'etaf-md-preview)

(defconst etaf-md-preview-refit-benchmark-sections
  (string-to-number (or (getenv "MD_PREVIEW_BENCH_SECTIONS") "120"))
  "Number of sections the benchmark document contains.")

(defconst etaf-md-preview-refit-benchmark-reports
  (string-to-number (or (getenv "MD_PREVIEW_BENCH_REPORTS") "3"))
  "Number of full phase reports to print.")

(defun etaf-md-preview-refit-benchmark--document ()
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
             index index index index index index index index (1+ index)))
   (number-sequence 1 etaf-md-preview-refit-benchmark-sections)
   "\n"))

(defun etaf-md-preview-refit-benchmark--milliseconds (function)
  "Call FUNCTION once and return its elapsed milliseconds."
  (let ((start (float-time)))
    (funcall function)
    (* 1000.0 (- (float-time) start))))

(let* ((document (etaf-md-preview-refit-benchmark--document))
       (session (etaf-md-preview-create :text document))
       (buffer (etaf-mount "*etaf-md-preview-refit-benchmark*"
                           (etaf-md-preview-view session)
                           '(:viewport-width 900 :viewport-height 50)))
       (runtime (etaf-runtime-for-buffer buffer))
       (reports nil)
       (results nil))
  (unwind-protect
      (progn
        (with-current-buffer buffer
          (ebox-buffer-set-observer
           buffer
           (lambda (_buffer report)
             (when-let* ((phases (plist-get report :phase-duration-ms)))
               (push (list :kind (plist-get report :projection-kind)
                           :duration (plist-get report :duration-ms)
                           :phases phases)
                     reports)))))
        ;; Warm up: the first publication prepares caches for its width.
        (etaf-runtime-refit runtime 900 50)
        (dolist (width '(760 960 840 1040 860 940 800 1000))
          (push (list :width width
                      :ms (etaf-md-preview-refit-benchmark--milliseconds
                           (lambda () (etaf-runtime-refit runtime width 50))))
                results))
        (princ (format "sections=%d lines=%d refits=%d\n"
                       etaf-md-preview-refit-benchmark-sections
                       (with-current-buffer buffer
                         (line-number-at-pos (point-max)))
                       (length results)))
        (dolist (result (nreverse results))
          (princ (format "width=%4d %8.1fms\n"
                         (plist-get result :width)
                         (plist-get result :ms))))
        (dolist (report (seq-take (nreverse reports)
                                  etaf-md-preview-refit-benchmark-reports))
          (princ (format "report kind=%s duration=%.1fms phases=%s\n"
                         (plist-get report :kind)
                         (plist-get report :duration)
                         (plist-get report :phases)))))
    (when (buffer-live-p buffer) (kill-buffer buffer))))

(provide 'refit-latency)
;;; refit-latency.el ends here
