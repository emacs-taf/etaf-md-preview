;;; performance-scenarios.el --- Independent preview workloads -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Register the preview's window-resize and mount paths with the shared
;; performance driver.  The fixtures mount the maintained user manual and
;; refit it across widths; every operation uses public APIs.  Run it with:
;;
;;   python3 scripts/performance.py run --mode batch \
;;     --scenario-file etaf-md-preview/benchmarks/performance-scenarios.el \
;;     --scenario md-preview.refit.width --diagnose

;;; Code:

(require 'cl-lib)
(require 'etaf)
(require 'etaf-md-preview)
(declare-function taf-benchmark-register "performance-driver" (id &rest properties))

(defconst etaf-md-preview-benchmark--file
  (expand-file-name "../examples/sample.markdown"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The maintained manual the resize scenario refits.")

(defconst etaf-md-preview-benchmark--widths
  (vector 700 1100 800 1000 750 950)
  "Widths the resize scenario cycles through, in pixels.")

(taf-benchmark-register
 "md-preview.refit.width" :version 1 :modes '(batch gui) :budget-ms 10
 :impact 'local :isolation 'series
 :description "User manual width refit through the retained Ebox tree"
 :setup
 (lambda (_mode)
   (let* ((session (etaf-md-preview-create :file etaf-md-preview-benchmark--file))
          (sink nil)
          (refits 0)
          (buffer (etaf-mount "*TAF md-preview refit*"
                              (etaf-md-preview-view session)
                              (list :viewport-width 900 :viewport-height 50
                                    :observer (lambda (report)
                                                (when sink (funcall sink report))))))
          (runtime (etaf-runtime-for-buffer buffer)))
     (list
      :buffer buffer
      :invoke
      (lambda (sample)
        (when (etaf-runtime-refit
               runtime
               (aref etaf-md-preview-benchmark--widths
                     (% sample (length etaf-md-preview-benchmark--widths)))
               50)
          (cl-incf refits)))
      :verify
      (lambda ()
        (with-current-buffer buffer
          (cond
           ((not (etaf-runtime-for-buffer buffer))
            "The preview runtime is not mounted")
           ((not (string-match-p "ETAF Markdown Preview 用户手册" (buffer-string)))
            "The manual's title is missing after a refit")
           ((zerop refits) "No refit changed the layout")
           (t t))))
      :observe
      (lambda (observer)
        (setq sink observer)
        (lambda () (setq sink nil)))
      :cleanup
      (lambda ()
        (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(taf-benchmark-register
 "md-preview.mount.manual" :version 1 :modes '(batch gui) :budget-ms 100
 :impact 'local
 :description "Parse and mount the complete manual into a fresh buffer"
 :setup
 (lambda (mode)
   (let ((buffer (generate-new-buffer " *TAF md-preview mount*"))
         sink)
     (list
      :buffer buffer
      :invoke
      (lambda (_sample)
        (let ((session (etaf-md-preview-create :file etaf-md-preview-benchmark--file)))
          (etaf-mount
           buffer (etaf-md-preview-view session)
           (append
            (when (eq mode 'batch) '(:viewport-width 900 :viewport-height 50))
            (list :observer
                  (lambda (observed-report)
                    (when sink (funcall sink buffer observed-report))))))))
      :verify
      (lambda ()
        (with-current-buffer buffer
          (cond
           ((not (etaf-runtime-for-buffer buffer)) "The manual did not mount")
           ((not (string-match-p "ETAF Markdown Preview 用户手册"
                                (buffer-substring-no-properties (point-min) (point-max))))
            "The mounted manual has no title")
           (t t))))
      :observe (lambda (observer)
                 (setq sink observer)
                 (lambda () (setq sink nil)))
      :cleanup (lambda ()
                 (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(provide 'performance-scenarios)
;;; performance-scenarios.el ends here
