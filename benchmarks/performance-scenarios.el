;;; performance-scenarios.el --- Independent preview workloads -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Register the preview's window-resize path with the shared performance
;; driver.  The fixture mounts the maintained user manual and refits it across
;; widths; every operation uses the package's public API.  Run it with:
;;
;;   python3 scripts/performance.py run --mode batch \
;;     --scenario-file etaf-apps/etaf-md-preview/benchmarks/performance-scenarios.el \
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

(defun etaf-md-preview-benchmark--environment ()
  "Report the renderer capabilities this fixture exercises."
  (let ((native (ebox-native-reflow-runtime-report)))
    (list :native_available (if (plist-get native :available-p) t :false)
          :native_layout (if (plist-get native :layout-ready-p) t :false)
          :native_hash (plist-get native :loaded-module-hash)
          :native_path (plist-get native :loaded-module-path))))

(taf-benchmark-register
 "md-preview.refit.width" :version 1 :modes '(batch gui) :budget-ms 10
 :impact 'local :isolation 'series
 :description "User manual width refit through the retained native frame"
 :setup
 (lambda (_mode)
   (let* ((session (etaf-md-preview-create :file etaf-md-preview-benchmark--file))
          (buffer (etaf-mount "*TAF md-preview refit*"
                              (etaf-md-preview-view session)
                              '(:viewport-width 900 :viewport-height 50)))
          (runtime (etaf-runtime-for-buffer buffer))
          (sink nil)
          (report nil))
     (with-current-buffer buffer
       (ebox-buffer-set-observer
        buffer
        (lambda (observed-buffer observed-report)
          (when (eq (plist-get observed-report :provider) 'ebox)
            (setq report observed-report)
            (when sink (funcall sink observed-buffer observed-report))))))
     (list
      :buffer buffer
      :environment #'etaf-md-preview-benchmark--environment
      :invoke
      (lambda (sample)
        (etaf-runtime-refit
         runtime
         (aref etaf-md-preview-benchmark--widths
               (% sample (length etaf-md-preview-benchmark--widths)))
         50))
      :verify
      (lambda ()
        (with-current-buffer buffer
          (cond
           ((not (etaf-runtime-for-buffer buffer))
            "The preview runtime is not mounted")
           ((not (string-match-p "ETAF Markdown Preview 用户手册" (buffer-string)))
            "The manual's title is missing after a refit")
           ((and (ebox-native-reflow-layout-ready-p)
                 (not (eq (plist-get report :projection-kind) 'native-frame)))
            (format "The refit fell back to %S" (plist-get report :projection-kind)))
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
         sink report)
     (list
      :buffer buffer
      :environment #'etaf-md-preview-benchmark--environment
      :invoke
      (lambda (_sample)
        (let ((session (etaf-md-preview-create :file etaf-md-preview-benchmark--file)))
          (etaf-mount
           buffer (etaf-md-preview-view session)
           (append
            (when (eq mode 'batch) '(:viewport-width 900 :viewport-height 50))
            (list :observer
                  (lambda (observed-report)
                    (when (eq (plist-get observed-report :provider) 'ebox)
                      (setq report observed-report))
                    (when sink (funcall sink buffer observed-report))))))))
      :verify
      (lambda ()
        (with-current-buffer buffer
          (cond
           ((not (etaf-runtime-for-buffer buffer)) "The manual did not mount")
           ((not (string-match-p "ETAF Markdown Preview 用户手册"
                                (buffer-substring-no-properties (point-min) (point-max))))
            "The mounted manual has no title")
           ((and (ebox-native-reflow-layout-ready-p)
                 (not (eq (plist-get report :projection-kind) 'native-frame)))
            (format "Mount fell back to %S" (plist-get report :projection-kind)))
           (t t))))
      :observe (lambda (observer)
                 (setq sink observer)
                 (lambda () (setq sink nil)))
      :cleanup (lambda ()
                 (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(provide 'performance-scenarios)
;;; performance-scenarios.el ends here
