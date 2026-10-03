;;; etaf-md-preview-tests.el --- Acceptance tests for etaf-md-preview -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Public behaviour tests.  Every scenario drives the previewer through the
;; interfaces declared in `etaf-md-preview.el'.  Rendering scenarios mount a
;; real runtime in a batch Emacs and read the visible buffer back, so they
;; assert what a reader sees instead of internal parse structures.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'etaf)
(require 'etaf-md-preview)

(defconst etaf-md-preview-tests--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this test file, resolved while it is loaded.")

(defun etaf-md-preview-tests--fixture (name)
  "Return the absolute path of fixture NAME."
  (expand-file-name (concat "fixtures/" name) etaf-md-preview-tests--directory))

(defun etaf-md-preview-tests--mount (session)
  "Mount SESSION in a fresh buffer and return that buffer."
  (etaf-mount (generate-new-buffer " *md-preview-test*")
              (etaf-md-preview-view session)
              '(:viewport-width 900 :viewport-height 60)))

(defmacro etaf-md-preview-tests--with-mount (session &rest body)
  "Mount SESSION, run BODY in its buffer, and always kill the buffer."
  (declare (indent 1) (debug t))
  `(let ((buffer (etaf-md-preview-tests--mount ,session)))
     (unwind-protect
         (with-current-buffer buffer ,@body)
       (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun etaf-md-preview-tests--face-attribute (needle property)
  "Return PROPERTY of the face at the first occurrence of NEEDLE.
Ebox concatenates every contributing face, so the value may be a list of
plists or a single plist."
  (goto-char (point-min))
  (search-forward needle nil t)
  (let* ((face (or (get-text-property (match-beginning 0) 'face)
                   (plist-get (text-properties-at (match-beginning 0)) 'face)))
         (faces (cond ((null face) nil)
                      ((and (listp face) (keywordp (car face))) (list face))
                      ((listp face) face)
                      (t (list face)))))
    (cl-loop for entry in faces
             when (and (listp entry) (plist-get entry property))
             return (plist-get entry property))))

(defun etaf-md-preview-tests--keymap-at (needle)
  "Return non-nil when the first occurrence of NEEDLE is clickable."
  (goto-char (point-min))
  (search-forward needle nil t)
  (or (get-text-property (match-beginning 0) 'keymap)
      (plist-get (text-properties-at (match-beginning 0)) 'keymap)))

(defun etaf-md-preview-tests--media-grid-shape ()
  "Return the (COLUMNS . ROWS) of the media grid in the current buffer.
A converter prints one uniform block of `▀' characters; the monochrome
fallback prints the same shape as dots, so both are recognized."
  (let ((best (cons 0 0))
         (columns 0)
         (rows 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (string-trim
                      (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
               (cells (and (> (length line) 1)
                           (string-match-p "\\`[▀.]*\\'" line)
                           (length line))))
          (if (and cells (= cells columns))
              (cl-incf rows)
            (progn
              (when (> rows (cdr best)) (setq best (cons columns rows)))
              (setq columns (or cells 0)
                    rows (if cells 1 0)))))
        (forward-line 1))
      (when (> rows (cdr best)) (setq best (cons columns rows))))
    best))

(ert-deftest etaf-md-preview-tests-create-reports-document-state ()
  "Creating a session from text exposes title, outline and footnotes."
  (let* ((session (etaf-md-preview-create
                   :text (concat "---\ntitle: 我的笔记\n---\n\n"
                                 "# 第一章\n\n正文[^a]。\n\n## 小节\n\n"
                                 "另一个脚注[^b]。\n\n[^a]: 甲\n\n[^b]: 乙\n")))
         (state (etaf-md-preview-state session)))
    (should (equal (plist-get state :title) "我的笔记"))
    (should (equal (mapcar #'cadr (plist-get state :outline))
                   '("第一章" "小节")))
    (should (equal (plist-get state :footnotes) '("a" "b")))
    (should (eq (plist-get state :theme) 'dark))
    (should (> (plist-get state :blocks) 0))))

(ert-deftest etaf-md-preview-tests-render-inline-styles-and-links ()
  "A paragraph renders every run with its style and keeps links clickable."
  (let ((session (etaf-md-preview-create
                  :text (concat "plain **bold** *italic* `code` "
                                "[site](https://example.com).\n"))))
    (etaf-md-preview-tests--with-mount session
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "plain bold italic code site" text)))
      (should (eq (etaf-md-preview-tests--face-attribute "bold" :weight) 'bold))
      (should (eq (etaf-md-preview-tests--face-attribute "italic" :slant) 'italic))
      (should (etaf-md-preview-tests--keymap-at "site")))))

(ert-deftest etaf-md-preview-tests-heading-links-override-heading-color ()
  "Links in every heading keep link color, heading weight and activation."
  (let ((session (etaf-md-preview-create
                  :text (concat
                         (mapconcat
                          (lambda (level)
                            (format "%s Heading%d [Link%d](https://example.com/%d)"
                                    (make-string level ?#) level level level))
                          '(1 2 3 4 5 6) "\n\n")
                         "\n\n[Reference](https://example.com/)\n")
                  :theme 'dark))
        opened)
    (etaf-md-preview-tests--with-mount session
      (let ((accent (etaf-md-preview-tests--face-attribute "Reference" :foreground)))
        (dolist (level '(1 2 3 4 5 6))
          (let ((link (format "Link%d" level)))
            (should (string-match-p
                     (format "Heading%d Link%d" level level)
                     (buffer-substring-no-properties (point-min) (point-max))))
            (should (equal (etaf-md-preview-tests--face-attribute link :foreground)
                           accent))
            (should-not (equal (etaf-md-preview-tests--face-attribute
                               (format "Heading%d" level) :foreground) accent))
            (should (eq (etaf-md-preview-tests--face-attribute link :weight) 'bold))
            (should (etaf-md-preview-tests--keymap-at link))
            (goto-char (match-beginning 0))
            (cl-letf (((symbol-function 'browse-url)
                       (lambda (url &rest _) (push url opened))))
              (call-interactively (key-binding (kbd "RET"))))))))
    (should (equal (nreverse opened)
                   (mapcar (lambda (n) (format "https://example.com/%d" n))
                           '(1 2 3 4 5 6))))))

(ert-deftest etaf-md-preview-tests-video-caption-activates-player ()
  "The caption that advertises RET to play is itself an activation target."
  (skip-unless (memq system-type '(darwin gnu/linux)))
  (let* ((video (expand-file-name "../examples/demo.mp4"
                                  etaf-md-preview-tests--directory))
         (session (etaf-md-preview-create :text (format "![Movie](%s)" video)))
         (real (symbol-function 'call-process))
         opened)
    (etaf-md-preview-tests--with-mount session
      (should (etaf-md-preview-tests--keymap-at "按 RET 播放"))
      (goto-char (match-beginning 0))
      (cl-letf (((symbol-function 'call-process)
                 (lambda (program &rest args)
                   (if (member program '("open" "xdg-open"))
                       (progn (push (car (last args)) opened) 0)
                     (apply real program args)))))
        (call-interactively (key-binding (kbd "RET")))))
    (should (equal opened (list video)))))

(ert-deftest etaf-md-preview-tests-reading-measure-stays-centered-on-refit ()
  "A capped reading measure stays centered without stretching its content."
  (let* ((session (etaf-md-preview-create :text "CenterProbe"))
         (cap (* (frame-char-width)
                 (plist-get (etaf-md-preview-state session) :width))))
    (etaf-md-preview-tests--with-mount session
      (dolist (width (list (+ cap 100) (+ cap 300) (max 40 (- cap 20)) (+ cap 100)))
        (etaf-runtime-refit (etaf-runtime-for-buffer (current-buffer)) width 60)
        (goto-char (point-min))
        (search-forward "CenterProbe")
        (let ((left (string-pixel-width
                     (buffer-substring (line-beginning-position) (match-beginning 0)))))
          (should (<= (abs (- left (/ (max 0 (- width cap)) 2))) 1)))))))

(ert-deftest etaf-md-preview-tests-alerts-end-with-their-body ()
  "Callouts add no empty row after the final body line."
  (dolist (kind '("NOTE" "TIP" "IMPORTANT" "WARNING" "CAUTION"))
    (let ((session (etaf-md-preview-create
                    :text (format "> [!%s]\n> First  \n> Last\n\nTail" kind))))
      (etaf-md-preview-tests--with-mount session
        (goto-char (point-min))
        (search-forward "First")
        (forward-line 1)
        (should (search-forward "Last" (line-end-position) t))
        (forward-line 1)
        (should (string-blank-p
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))))
        (forward-line 1)
        (should (search-forward "Tail" (line-end-position) t))))))

(ert-deftest etaf-md-preview-tests-render-block-structure ()
  "Headings, lists, tasks, quotes, code and tables all reach the buffer."
  (let ((session (etaf-md-preview-create
                  :text (concat "# Title\n\n- one\n- two\n  - nested\n\n"
                                "1. first\n\n- [x] done\n- [ ] todo\n\n"
                                "> quoted **text**\n\n"
                                "```elisp\n(defun hi ())\n```\n\n"
                                "| a | b |\n|:--|--:|\n| 1 | 2 |\n\n---\n"))))
    (etaf-md-preview-tests--with-mount session
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "Title" text))
        (should (string-match-p "one" text))
        (should (string-match-p "nested" text))
        (should (string-match-p "first" text))
        (should (string-match-p "☑ done" text))
        (should (string-match-p "☐ todo" text))
        (should (string-match-p "quoted text" text))
        (should (string-match-p "(defun hi ())" text))
        (should (string-match-p "elisp" text))
        (should (string-match-p "a" text))
        (should (string-match-p "2" text))))))

(ert-deftest etaf-md-preview-tests-footnotes-numbered-and-defined ()
  "Footnote references render numbered and definitions appear at the end."
  (let ((session (etaf-md-preview-create
                  :text "First[^one] and second[^two].\n\n[^one]: 甲\n\n[^two]: 乙\n")))
    (etaf-md-preview-tests--with-mount session
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "First\\[1\\] and second\\[2\\]" text))
        (should (string-match-p "\\[1\\] 甲" text))
        (should (string-match-p "\\[2\\] 乙" text))))))

(ert-deftest etaf-md-preview-tests-media-cards-show-placeholders ()
  "Missing local media renders a labeled card instead of failing."
  (let ((session (etaf-md-preview-create
                  :text (concat "![截图](/nonexistent/picture.png)\n\n"
                                "![录屏](/nonexistent/clip.mp4)\n\n"
                                "![远程](https://example.com/badge.svg)\n"))))
    (etaf-md-preview-tests--with-mount session
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "截图" text))
        (should (string-match-p "录屏" text))
        (should (string-match-p "https://example.com/badge.svg" text))))))

(ert-deftest etaf-md-preview-tests-media-keeps-source-aspect-under-height-cap ()
  "A wide image narrows its grid with the height cap instead of squashing.
The fixture is 4:1; a card that only capped the rows would render it at
roughly 9:1 while the visible cell grid must stay close to the source."
  (let* ((image (etaf-md-preview-tests--fixture "wide-400x100.png"))
         (session (etaf-md-preview-create
                   :text (format "![wide](%s)\n" image))))
    (etaf-md-preview-tests--with-mount session
      (let* ((shape (etaf-md-preview-tests--media-grid-shape))
             (columns (car shape))
             (rows (cdr shape)))
        (should (> rows 1))
        (should (> columns 1))
        (should (< (abs (- (/ (* 1.0 columns) rows) 4.0)) 1.2))))))

(ert-deftest etaf-md-preview-tests-local-media-opens-with-the-desktop ()
  "RET on a local media card asks the desktop to open the file.
Emacs's browser plumbing would put a video in a buffer; the card must reach
the system opener instead."
  (skip-unless (memq system-type '(darwin gnu/linux)))
  (let* ((image (etaf-md-preview-tests--fixture "wide-400x100.png"))
         (session (etaf-md-preview-create
                   :text (format "![wide](%s)\n" image)))
         (opened nil)
         (real (symbol-function 'call-process)))
    (etaf-md-preview-tests--with-mount session
      (goto-char (point-min))
      (search-forward "▀" nil t)
      (goto-char (match-beginning 0))
      (let ((binding (key-binding (kbd "RET"))))
        (cl-letf (((symbol-function 'call-process)
                   (lambda (program &rest args)
                     (if (member program '("open" "xdg-open"))
                         (progn (push (car (last args)) opened) 0)
                       (apply real program args)))))
          (when (commandp binding)
            (call-interactively binding)))))
    (should (equal opened (list image)))))

(ert-deftest etaf-md-preview-tests-refresh-re-reads-a-file ()
  "Refresh re-reads the file and publishes the new outline."
  (let* ((file (make-temp-file "etaf-md-preview-" nil ".md"))
         (session nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert "# First\n"))
          (setq session (etaf-md-preview-create :file file))
          (should (equal (mapcar #'cadr (plist-get (etaf-md-preview-state session)
                                                   :outline))
                         '("First")))
          (with-temp-file file (insert "# Second\n\nbody\n"))
          (etaf-md-preview-refresh session)
          (should (equal (mapcar #'cadr (plist-get (etaf-md-preview-state session)
                                                   :outline))
                         '("Second"))))
      (delete-file file))))

(ert-deftest etaf-md-preview-tests-theme-changes-and-rejects-unknown ()
  "Themes cycle in order and unknown palettes are rejected."
  (let ((session (etaf-md-preview-create :text "# x\n" :theme 'dark)))
    (should (equal (etaf-md-preview-set-theme session 'light) session))
    (should (eq (plist-get (etaf-md-preview-state session) :theme) 'light))
    (should (eq (plist-get (etaf-md-preview-state session) :background) 'palette))
    (should (eq (etaf-md-preview-cycle-theme session) 'sepia))
    (should-error (etaf-md-preview-set-theme session 'neon)
                  :type 'etaf-md-preview-error)))

(ert-deftest etaf-md-preview-tests-auto-theme-keeps-the-emacs-background ()
  "The default theme paints no page background; an explicit one does."
  (let* ((auto (etaf-md-preview-create
                :text "plain text here\n" :theme 'auto))
         (explicit (etaf-md-preview-create
                    :text "plain text here\n" :theme 'dark))
         (auto-buffer (etaf-md-preview-tests--mount auto))
         (dark-buffer (etaf-md-preview-tests--mount explicit)))
    (unwind-protect
        (progn
          (should-not (plist-get (etaf-md-preview-state auto) :background))
          (should (eq (plist-get (etaf-md-preview-state explicit) :background)
                      'palette))
          (with-current-buffer auto-buffer
            (should-not (etaf-md-preview-tests--face-attribute
                         "plain" :background)))
          (with-current-buffer dark-buffer
            ;; The dark palette's page background, as shipped.
            (should (equal (etaf-md-preview-tests--face-attribute
                            "plain" :background)
                           "#0d1117"))))
      (kill-buffer auto-buffer)
      (kill-buffer dark-buffer))))

(ert-deftest etaf-md-preview-tests-auto-theme-derives-surfaces-from-faces ()
  "The auto theme blends panels from the active frame's own faces."
  (let* ((background (face-attribute 'default :background nil t))
         (foreground (face-attribute 'default :foreground nil t))
         (session (etaf-md-preview-create :text "plain `code` here\n"))
         buffer
         dark-surface
         light-surface)
    (unwind-protect
        (progn
          (set-face-attribute 'default nil
                              :background "#101820" :foreground "#e0e0e0")
          (setq buffer (etaf-md-preview-tests--mount session))
          (setq dark-surface
                (with-current-buffer buffer
                  (etaf-md-preview-tests--face-attribute "code" :background)))
          (kill-buffer buffer)
          (setq buffer nil)
          (set-face-attribute 'default nil
                              :background "#fdf6e3" :foreground "#2b2b2b")
          (setq buffer (etaf-md-preview-tests--mount session))
          (setq light-surface
                (with-current-buffer buffer
                  (etaf-md-preview-tests--face-attribute "code" :background))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (set-face-attribute 'default nil
                          :background background :foreground foreground))
    (should (stringp dark-surface))
    (should (stringp light-surface))
    ;; The surface follows the frame, not a fixed palette.
    (should-not (equal dark-surface light-surface))
    (should-not (equal dark-surface "#161b22"))
    (should-not (equal light-surface "#161b22"))))

(ert-deftest etaf-md-preview-tests-custom-defaults-select-theme-flavor-and-bounds ()
  "The defcustoms choose the initial palette, dialect and measure bounds."
  (let ((etaf-md-preview-theme 'sepia)
        (etaf-md-preview-flavor 'commonmark)
        (etaf-md-preview-width-min 30)
        (etaf-md-preview-width-max 50))
    (let* ((session (etaf-md-preview-create :text "~~strike~~\n"))
           (state (etaf-md-preview-state session)))
      (should (eq (plist-get state :theme) 'sepia))
      (should (eq (plist-get state :background) 'palette))
      (should (eq (plist-get state :flavor) 'commonmark))
      (etaf-md-preview-tests--with-mount session
        (should (string-match-p "~~strike~~"
                                (substring-no-properties (buffer-string)))))
      (should (= (etaf-md-preview-adjust-width session -100) 30))
      (should (= (etaf-md-preview-adjust-width session 100) 50)))))

(ert-deftest etaf-md-preview-tests-locate-follows-the-outline ()
  "Locate moves point to the heading band and next/previous walk the outline."
  (let* ((session (etaf-md-preview-create
                   :text "# Alpha\n\nbody\n\n## Beta\n\nmore\n\n### Gamma\n\nend\n"))
         (buffer (etaf-md-preview-open session)))
    (unwind-protect
        (with-current-buffer buffer
          (should (etaf-md-preview-locate session 1))
          (should (looking-at-p "Beta"))
          (etaf-md-preview-next-heading)
          (should (looking-at-p "Gamma"))
          (etaf-md-preview-previous-heading)
          (should (looking-at-p "Beta"))
          (should (etaf-md-preview-locate session 0))
          (should (looking-at-p "Alpha")))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (etaf-md-preview-close))))))

(ert-deftest etaf-md-preview-tests-close-releases-and-reopens ()
  "Closing kills the buffer and the same session mounts again."
  (let* ((session (etaf-md-preview-create :text "# Again\n"))
         (buffer (etaf-md-preview-open session)))
    (with-current-buffer buffer
      (etaf-md-preview-close))
    (should-not (buffer-live-p buffer))
    (let ((second (etaf-md-preview-open session)))
      (unwind-protect
          (with-current-buffer second
            (should (string-match-p "Again" (substring-no-properties (buffer-string)))))
        (when (buffer-live-p second)
          (with-current-buffer second (etaf-md-preview-close)))))))

(ert-deftest etaf-md-preview-tests-flavor-commonmark-keeps-extensions-literal ()
  "The CommonMark flavor leaves GFM-only syntax as literal text."
  (let ((session (etaf-md-preview-create
                  :flavor 'commonmark
                  :text (concat "~~strike~~ and https://example.com\n\n"
                                "| a | b |\n|:--|--:|\n| 1 | 2 |\n\n"
                                "- [ ] task\n\n> [!NOTE]\n> body\n\n"
                                "note[^f]\n"))))
    (etaf-md-preview-tests--with-mount session
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "~~strike~~" text))
        (should (string-match-p "| a | b |" text))
        (should (string-match-p "\\[ \\] task" text))
        (should (string-match-p "\\[!NOTE\\]" text))
        (should (string-match-p "\\[\\^f\\]" text))))))

(ert-deftest etaf-md-preview-tests-flavor-gfm-converts-extensions ()
  "The GFM flavor converts the extensions the previewer defaults to."
  (let ((session (etaf-md-preview-create
                  :flavor 'gfm
                  :text (concat "~~strike~~ see https://example.com\n\n"
                                "| a | b |\n|:--|--:|\n| 1 | 2 |\n\n"
                                "- [ ] task\n\n> [!TIP]\n> helper\n\n"
                                "note[^f]\n\n[^f]: def\n"))))
    (etaf-md-preview-tests--with-mount session
      (let ((text (substring-no-properties (buffer-string))))
        (should-not (string-match-p "~~" text))
        (should (string-match-p "strike" text))
        (should (string-match-p "☐ task" text))
        (should (string-match-p "提示" text))
        (should (string-match-p "\\[1\\]" text))
        (should (etaf-md-preview-tests--keymap-at "https://example.com"))))))

(ert-deftest etaf-md-preview-tests-flavor-rejects-unknown-name ()
  "An unknown dialect name is reported instead of guessed."
  (should-error (etaf-md-preview-create :text "# x\n" :flavor 'markdown-extras)
                :type 'etaf-md-preview-error))

(ert-deftest etaf-md-preview-tests-front-matter-is-metadata ()
  "Front matter is stripped from the body and feeds the title."
  (let ((session (etaf-md-preview-create
                  :text "---\ntitle: Meta Title\ntags: [a, b]\n---\n\n# Real Title\n")))
    (should (equal (plist-get (etaf-md-preview-state session) :title) "Meta Title"))
    (etaf-md-preview-tests--with-mount session
      (should-not (string-match-p "tags" (substring-no-properties (buffer-string)))))))

(provide 'etaf-md-preview-tests)
;;; etaf-md-preview-tests.el ends here
