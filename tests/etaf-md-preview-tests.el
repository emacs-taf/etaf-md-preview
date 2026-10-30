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
(require 'outline)
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
fallback prints the same shape as dots, so both are recognized.  The grid
sits inside the card border, so only the longest run of cells on a line
counts."
  (let ((best (cons 0 0))
         (columns 0)
         (rows 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (run (let ((start 0) (longest 0))
                      (while (string-match "[▀.]+" line start)
                        (setq longest (max longest (- (match-end 0) (match-beginning 0)))
                              start (match-end 0)))
                      longest))
               (cells (and (> run 1) run)))
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
  "RET on a video caption starts playback inside the preview.
With a decoder the press must not leave the card for an external player, and
the repaint swaps the poster for the native canvas."
  (skip-unless (executable-find "ffmpeg"))
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
        (call-interactively (key-binding (kbd "RET"))))
      (sit-for 0.2)
      (goto-char (point-min))
      (should (search-forward "按 RET 播放/暂停" nil t)))
    (should-not opened)))

(ert-deftest etaf-md-preview-tests-video-ret-toggles-playback-in-place ()
  "RET on the live player toggles playback without restarting it.
The canvas advances while playing, holds its last frame once paused, and
moves on from that frame when playback resumes."
  (skip-unless (executable-find "ffmpeg"))
  (let ((session (etaf-md-preview-create
                  :text (format "![clip](%s)\n"
                                (etaf-md-preview-tests--fixture "clip.mp4")))))
    (etaf-md-preview-tests--with-mount session
      (goto-char (point-min))
      (search-forward "按 RET 播放" nil t)
      (goto-char (match-beginning 0))
      (call-interactively (key-binding (kbd "RET")))
      (let ((playing (buffer-string)))
        (sit-for 0.5)
        (should-not (equal playing (buffer-string))))
      (goto-char (point-min))
      (search-forward "按 RET 播放/暂停" nil t)
      (goto-char (match-beginning 0))
      (call-interactively (key-binding (kbd "RET")))
      (sit-for 0.4)
      (let ((paused (buffer-string)))
        (sit-for 0.5)
        (should (equal paused (buffer-string)))
        (call-interactively (key-binding (kbd "RET")))
        (sit-for 0.6)
        (should-not (equal paused (buffer-string)))))))

(ert-deftest etaf-md-preview-tests-video-without-decoder-opens-externally ()
  "Without ffmpeg a local video card keeps the poster and opens externally.
The card renders once with whatever decoder the environment provides and
again with ffmpeg hidden; the second render must use its own poster probe
instead of the cached playback metadata."
  (skip-unless (memq system-type '(darwin gnu/linux)))
  (let* ((video (expand-file-name "../examples/demo.mp4"
                                  etaf-md-preview-tests--directory))
         (session (etaf-md-preview-create :text (format "![Movie](%s)" video)))
         (real-process (symbol-function 'call-process))
         (real-find (symbol-function 'executable-find))
         opened)
    (etaf-md-preview-tests--with-mount session)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program &optional remote)
                 (unless (equal program "ffmpeg")
                   (funcall real-find program remote)))))
      (etaf-md-preview-tests--with-mount session
        (should (etaf-md-preview-tests--keymap-at "按 RET 播放"))
        (goto-char (match-beginning 0))
        (cl-letf (((symbol-function 'call-process)
                   (lambda (program &rest args)
                     (if (member program '("open" "xdg-open"))
                         (progn (push (car (last args)) opened) 0)
                       (apply real-process program args)))))
          (call-interactively (key-binding (kbd "RET"))))))
    (should (equal opened (list video)))))

(defconst etaf-md-preview-tests--documented-gutter 2
  "Cells the manual promises on each side of the reading measure.")

(defun etaf-md-preview-tests--background-at (position)
  "Return the painted background at POSITION, or nil."
  (let ((face (get-text-property position 'face)))
    (cl-some (lambda (entry)
               (and (listp entry) (plist-get entry :background)))
             (if (keywordp (car-safe face)) (list face) face))))

(defun etaf-md-preview-tests--page-margin-pixels ()
  "Return (LEADING . TRAILING) unpainted blank pixels of the current line.
A cell of the page margin is a space with no painted background; the table's
border and cell padding are spaces too, but they paint, so the walk stops at
them.  A displayed space counts its displayed width, not one character."
  (let* ((bol (line-beginning-position))
         (eol (line-end-position))
         (char (frame-char-width))
         (leading 0)
         (trailing 0)
         (painted-p (lambda (at)
                      (and (eq (char-after at) ?\s)
                           (not (null (get-text-property at 'display)))
                           (etaf-md-preview-tests--background-at at))))
         (width-p (lambda (at)
                    (let ((display (get-text-property at 'display)))
                      (if (and (consp display) (plist-get (cdr display) :width))
                          (let ((width (plist-get (cdr display) :width)))
                            (if (numberp width) width (car width)))
                        char))))
         (position bol))
    ;; The page margin is one displayed space per side; a table line adds
    ;; painted cells after it, so take the unpainted run at each end.
    (while (and (< position eol) (eq (char-after position) ?\s))
      (if (funcall painted-p position)
          (setq position eol)
        (setq leading (+ leading (funcall width-p position))
              position (1+ position))))
    (setq position (1- eol))
    (while (and (>= position bol) (eq (char-after position) ?\s))
      (if (funcall painted-p position)
          (setq position bol)
        (setq trailing (+ trailing (funcall width-p position))
              position (1- position))))
    (cons leading trailing)))

(ert-deftest etaf-md-preview-tests-narrow-window-keeps-the-measure-gutter ()
  "A narrow window keeps page margin on both sides of the reading measure.
Neither the text nor a table may reach the reading edge: the renderer
extends the last cell's paint into the window's sub-cell remainder, so a
table border on that edge would draw a bar across the gutter."
  (let ((session (etaf-md-preview-create
                  :text (concat "## 非目标\n\n"
                                "| 不做 | 原因 |\n| --- | --- |\n"
                                "| 手机端 | ETAF 是 Emacs 内的界面 |\n")))
        (margin (* etaf-md-preview-tests--documented-gutter (frame-char-width)))
        (char (frame-char-width)))
    (etaf-md-preview-tests--with-mount session
      (dolist (cells (list 46 30))
        (etaf-runtime-refit (etaf-runtime-for-buffer (current-buffer))
                            (* char cells) 40)
        (dolist (needle '("不做" "手机端"))
          (goto-char (point-min))
          (search-forward needle)
          (let ((blank (etaf-md-preview-tests--page-margin-pixels)))
            (should (>= (car blank) margin))
            (should (>= (cdr blank) margin))
            (should-not (etaf-md-preview-tests--background-at
                         (1- (line-end-position))))))))))

(defun etaf-md-preview-tests--foreground-at (needle)
  "Return the foreground color reported at the first NEEDLE, or nil."
  (goto-char (point-min))
  (search-forward needle)
  (let ((face (get-text-property (match-beginning 0) 'face)))
    (cl-some (lambda (entry)
               (and (listp entry) (plist-get entry :foreground)))
             (if (keywordp (car-safe face)) (list face) face))))

(defmacro etaf-md-preview-tests--with-faces (faces &rest body)
  "Run BODY with FACES set on the default and outline-1 faces.
FACES is a plist of `:foreground', `:background' and `:outline'; every face
attribute it changes is restored afterwards, so a scenario that paints a theme
leaves the frame as it found it."
  (declare (indent 1))
  `(let ((etaf-md-preview-tests--saved
          (list (face-attribute 'default :foreground nil t)
                (face-attribute 'default :background nil t)
                (face-attribute 'outline-1 :foreground nil t))))
     (unwind-protect
         (progn
           (set-face-attribute 'default nil
                               :foreground (or (plist-get ,faces :foreground) "#000000")
                               :background (or (plist-get ,faces :background) "#ffffff"))
           (set-face-attribute 'outline-1 nil
                               :foreground (or (plist-get ,faces :outline) 'unspecified))
           ,@body)
       (cl-destructuring-bind (foreground background outline) etaf-md-preview-tests--saved
         (when (stringp foreground) (set-face-attribute 'default nil :foreground foreground))
         (when (stringp background) (set-face-attribute 'default nil :background background))
         (set-face-attribute 'outline-1 nil :foreground (or outline 'unspecified))))))

(ert-deftest etaf-md-preview-tests-heading-color-separates-from-flat-themed-ink ()
  "A theme that paints its outline color as its body color gets a heading hue.
Several dark themes do that, and honoring them literally renders every heading
flat; the palette then derives the heading from the accent instead."
  (etaf-md-preview-tests--with-faces
      '(:foreground "#ffffff" :background "#263238" :outline "#eceff1")
    (etaf-md-preview-tests--with-mount (etaf-md-preview-create :text "# Heading\n")
      (let ((heading (etaf-md-preview-tests--foreground-at "Heading")))
        (should heading)
        (should-not (equal heading "#eceff1"))
        (should-not (equal heading "#ffffff"))))))

(ert-deftest etaf-md-preview-tests-heading-keeps-a-distinct-theme-color ()
  "A theme whose heading color differs from its body text keeps that color.
The derivation only replaces a heading color that would read as plain text."
  (etaf-md-preview-tests--with-faces
      '(:foreground "#383a42" :background "#fafafa" :outline "#0184bc")
    (etaf-md-preview-tests--with-mount (etaf-md-preview-create :text "# Heading\n")
      (should (equal "#0184bc"
                     (etaf-md-preview-tests--foreground-at "Heading"))))))

(ert-deftest etaf-md-preview-tests-auto-palette-follows-a-theme-switch ()
  "The face-derived palette follows an Emacs theme switch.
Ebox resolves colors into the buffer while a View renders, so the preview has
to render again after the switch Emacs reports through
`enable-theme-functions'; a palette derived once at mount left light text on a
light page."
  (let ((session (etaf-md-preview-create :text "ThemeProbe"))
        (original (face-attribute 'default :foreground nil t)))
    (unwind-protect
        (progn
          (set-face-attribute 'default nil :foreground "#101010")
          (etaf-md-preview-tests--with-mount session
            (should (equal "#101010"
                           (etaf-md-preview-tests--face-attribute "ThemeProbe" :foreground)))
            (set-face-attribute 'default nil :foreground "#202020")
            (run-hooks 'enable-theme-functions)
            (sleep-for 0.05)
            (should (equal "#202020"
                           (etaf-md-preview-tests--face-attribute "ThemeProbe" :foreground)))))
      (set-face-attribute 'default nil :foreground original))))

(ert-deftest etaf-md-preview-tests-reading-measure-stays-centered-on-refit ()
  "A capped reading measure stays centered without stretching its content.
The measure gutter sits on both sides, so a column that fills the window
keeps its text one gutter in rather than on the edge."
  (let* ((session (etaf-md-preview-create :text "CenterProbe"))
         (char (frame-char-width))
         (gutter (* char etaf-md-preview-tests--documented-gutter))
         (cap (* char (plist-get (etaf-md-preview-state session) :width))))
    (etaf-md-preview-tests--with-mount session
      (dolist (width (list (+ cap 100) (+ cap 300) (max 40 (- cap 20)) (+ cap 100)))
        (etaf-runtime-refit (etaf-runtime-for-buffer (current-buffer)) width 60)
        (goto-char (point-min))
        (search-forward "CenterProbe")
        (let ((left (string-pixel-width
                     (buffer-substring (line-beginning-position) (match-beginning 0)))))
          (should (<= (abs (- left (max gutter (/ (max 0 (- width cap)) 2)))) 1)))))))

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

(ert-deftest etaf-md-preview-tests-video-card-plays-with-a-native-decoder ()
  "A local video card offers inline playback when ffmpeg can decode it.
The card starts as a poster and names the action that mounts the player."
  (skip-unless (executable-find "ffmpeg"))
  (let ((session (etaf-md-preview-create
                  :text (format "![clip](%s)\n"
                                (etaf-md-preview-tests--fixture "clip.mp4")))))
    (etaf-md-preview-tests--with-mount session
      (let ((text (substring-no-properties (buffer-string))))
        (should (string-match-p "▶ clip" text))
        (should (string-match-p "按 RET 播放" text))))))

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

(ert-deftest etaf-md-preview-tests-open-side-by-side-keeps-the-source-window ()
  "The side-by-side command opens the preview next to the source.
The source buffer keeps the selected window, so an edit and its save need no
window switching, and closing the preview gives the window back."
  (let* ((file (make-temp-file "etaf-md-preview-beside-" nil ".md"))
         (source nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert "# Beside\n\nbody text\n"))
          (setq source (find-file-noselect file))
          (switch-to-buffer source)
          (delete-other-windows)
          (etaf-md-preview-open-side-by-side file)
          (let ((preview-window
                 (cl-find-if (lambda (window)
                               (string-prefix-p "*ETAF Markdown:"
                                                (buffer-name (window-buffer window))))
                             (window-list))))
            (should (= 2 (length (window-list))))
            (should (eq (window-buffer (selected-window)) source))
            (should preview-window)
            (select-window preview-window)
            (etaf-md-preview-close)
            (should (= 1 (length (window-list))))))
      (dolist (buffer (buffer-list))
        (when (and (string-prefix-p "*ETAF Markdown:" (buffer-name buffer))
                   (buffer-live-p buffer))
          (kill-buffer buffer)))
      (when (buffer-live-p source) (kill-buffer source))
      (delete-file file))))

(ert-deftest etaf-md-preview-tests-saving-the-source-repaints-the-preview ()
  "Saving a source file repaints its open preview without a manual reload.
The reader keeps the section it was reading instead of jumping to the top."
  (let* ((file (make-temp-file "etaf-md-preview-follow-" nil ".md"))
         (source nil)
         (preview nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "# Document\n\n")
            (dotimes (index 30)
              (insert (format "## S%d\n\nbody %d\n\n" index index))))
          (setq source (find-file-noselect file))
          (setq preview (etaf-md-preview-open file))
          (should (with-current-buffer preview
                    (string-match-p "body 3" (buffer-string))))
          (with-current-buffer preview
            (goto-char (point-min))
            (forward-line 30))
          (let ((line (with-current-buffer preview (line-number-at-pos))))
            (with-current-buffer source
              (goto-char (point-max))
              (insert "\n## Added\n\nnew body here\n")
              (save-buffer))
            (sit-for 0.5)
            (with-current-buffer preview
              (should (string-match-p "new body here" (buffer-string)))
              (should (> (line-number-at-pos) (- line 3))))))
      (when (buffer-live-p preview) (kill-buffer preview))
      (when (buffer-live-p source) (kill-buffer source))
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

(defun etaf-md-preview-tests--row-painting (buffer color)
  "Return the first position in BUFFER whose face paints COLOR as background."
  (with-current-buffer buffer
    (let ((pos (point-min))
          found)
      (while (and (< pos (point-max)) (not found))
        (when (equal color
                     (plist-get (get-text-property pos 'face) :background))
          (setq found pos))
        (setq pos (1+ pos)))
      found)))

(defun etaf-md-preview-tests--painted-background (buffer position)
  "Return the background BUFFER paints at POSITION."
  (with-current-buffer buffer
    (plist-get (get-text-property position 'face) :background)))

(etaf-define-component etaf-md-preview-tests--framing-host (&key surface session)
  "Provide SURFACE to the preview SESSION and render it below."
  :setup (etaf-surface-provide surface)
  :render (etaf-md-preview-view session))

(ert-deftest etaf-md-preview-tests-declares-its-surface-to-a-host ()
  "A host frames the preview from the surface the preview declares.
The panel's title-bar switch writes the session's theme cell, so the document
repaints with the chosen page instead of only the frame chrome."
  (let* ((session (etaf-md-preview-create :text "# Title\n\nBody text.\n"))
         (surface (etaf-surface-create))
         (buffer (generate-new-buffer " *etaf-md-preview framing host*")))
    (unwind-protect
        (progn
          (etaf-mount buffer
                      (etaf-view (etaf-md-preview-tests--framing-host
                                  :surface surface :session session))
                      (list :viewport-width 900 :viewport-height 60))
          (etaf-runtime-flush (etaf-runtime-for-buffer buffer))
          (should (equal '((light . "亮") (auto . "随") (dark . "暗") (sepia . "纸"))
                         (etaf-surface-modes surface)))
          (should (eq 'auto (etaf-value (etaf-surface-mode surface))))
          (etaf-set-value (etaf-surface-mode surface) 'light)
          (etaf-runtime-flush (etaf-runtime-for-buffer buffer))
          (should (eq 'light (plist-get (etaf-md-preview-state session) :theme)))
          (let* ((light (plist-get (etaf-value (etaf-surface-theme surface)) :ui-bg))
                 (pos (etaf-md-preview-tests--row-painting buffer light)))
            (should (equal light "#ffffff"))
            (should pos)
            (etaf-set-value (etaf-surface-mode surface) 'dark)
            (etaf-runtime-flush (etaf-runtime-for-buffer buffer))
            (let ((dark (plist-get (etaf-value (etaf-surface-theme surface)) :ui-bg)))
              (should (equal dark "#191a1c"))
              (should (equal dark
                             (etaf-md-preview-tests--painted-background
                              buffer pos))))))
      (when-let* ((runtime (etaf-runtime-for-buffer buffer)))
        (etaf-unmount runtime))
      (kill-buffer buffer))))

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
                           "#191a1c"))))
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

(ert-deftest etaf-md-preview-tests-open-prompt-defaults-to-the-relative-file ()
  "The file prompt defaults to the current buffer's file, as a relative name.
`read-file-name' resolves a relative default against its own directory; an
absolute one made the prompt show that directory twice."
  (let ((dir (make-temp-file "etaf-md-preview-prompt" t))
        (default nil))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "note.md" dir) (insert "# Note\n"))
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "note.md" dir)
                  default-directory (file-name-as-directory dir))
            (cl-letf (((symbol-function 'read-file-name)
                       (lambda (_prompt &optional _directory _predicate _mustmatch
                                preset &rest _)
                         (setq default preset)
                         buffer-file-name)))
              (call-interactively #'etaf-md-preview-open)))
          (should default)
          (should-not (file-name-absolute-p default))
          (should (equal "note.md" default)))
      (dolist (buffer (buffer-list))
        (when (string-prefix-p "*ETAF Markdown" (buffer-name buffer))
          (with-current-buffer buffer (ignore-errors (etaf-md-preview-close)))))
      (delete-directory dir t))))

(ert-deftest etaf-md-preview-tests-help-stays-in-preview ()
  "Help is searchable in place and preserves the preview's reading state."
  (save-window-excursion
    (let* ((session (etaf-md-preview-create :text "# Help acceptance\n\nKeep reading here.\n"))
           (buffer (etaf-md-preview-open session))
           (runtime (etaf-runtime-for-buffer buffer)))
      (unwind-protect
          (with-current-buffer buffer
            (let ((windows (window-list))
                  (before (etaf-md-preview-state session)))
              (dolist (key '("?" "C-c h"))
                (execute-kbd-macro (kbd key))
                (let* ((data (etaf-help-data runtime))
                       (entries (apply #'append (mapcar (lambda (group) (plist-get group :entries))
                                                        (plist-get data :groups))))
                       (reload (cl-find "r" entries :key (lambda (entry) (plist-get entry :key))
                                        :test #'equal))
                       (same-runtime (eq runtime (etaf-runtime-for-buffer buffer))))
                  (should same-runtime)
                  (should (equal windows (window-list)))
                  (should (eq (window-buffer) buffer))
                  (should (equal (plist-get reload :label) "重新加载文档")))
                (execute-kbd-macro (kbd "/"))
                (execute-kbd-macro "?")
                (execute-kbd-macro (kbd "<escape>"))
                (let ((editing (etaf-text-input-active-p runtime)))
                  (should-not editing))
                (should (string-match-p "快捷键 · 只读"
                                        (buffer-substring-no-properties (point-min) (point-max))))
                (execute-kbd-macro (kbd "<escape>"))
                (let ((state (etaf-md-preview-state session))
                      (text (buffer-substring-no-properties (point-min) (point-max))))
                  (should (equal state before))
                  (should (string-match-p "Help acceptance" text))))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (etaf-md-preview-close)))))))

;; The heading keys are root Host keys, so they win over the renderer's own
;; `n', `p' and `q' (line scrolling and `quit-window').
(ert-deftest etaf-md-preview-tests-keys-win-over-renderer-scrolling ()
  "`n' and `p' walk headings and `q' closes through the command loop."
  (save-window-excursion
    (let* ((session (etaf-md-preview-create
                     :text "# Alpha\n\nbody\n\n## Beta\n\nmore\n"))
           (buffer (etaf-md-preview-open session)))
      (unwind-protect
          (with-current-buffer buffer
            (should (etaf-md-preview-locate session 0))
            (execute-kbd-macro (kbd "n"))
            (should (looking-at-p "Beta"))
            (execute-kbd-macro (kbd "p"))
            (should (looking-at-p "Alpha"))
            (execute-kbd-macro (kbd "q"))
            (should-not (buffer-live-p buffer))
            (should-not (plist-get session :buffer)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (etaf-md-preview-close)))))))

(ert-deftest etaf-md-preview-tests-palette-paints-the-canvas ()
  "Choosing a palette paints unpainted cells with its page background."
  (let* ((session (etaf-md-preview-create :text "# Canvas\n"))
         (buffer (etaf-md-preview-open session))
         colors)
    (unwind-protect
        (with-current-buffer buffer
          (dolist (theme '(dark light sepia))
            (etaf-md-preview-set-theme session theme)
            (etaf-runtime-flush (etaf-runtime-for-buffer buffer))
            (let ((painted (delete-dups
                            (mapcar (lambda (spec) (plist-get spec :background))
                                    (alist-get 'default face-remapping-alist)))))
              ;; The base remap and the root's own canvas agree on one color.
              (should (= (length painted) 1))
              (push (car painted) colors)))
          (should (cl-every #'stringp colors))
          (should (= (length (delete-dups colors)) 3)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (etaf-md-preview-close))))))

(provide 'etaf-md-preview-tests)
;;; etaf-md-preview-tests.el ends here
