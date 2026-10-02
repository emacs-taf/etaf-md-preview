;;; etaf-md-preview-views.el --- Views of the markdown previewer -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Presentation layer of etaf-md-preview.  The functions here take a parsed
;; document plus a rendering environment and return ETAF views; they never
;; read session state directly, so one code path renders headless tests and a
;; mounted buffer alike.
;;
;; The environment is a plist with `:theme', `:width' (the reading column in
;; character cells), `:dir' (base directory for relative media), `:open-url'
;; and `:open-file' callbacks.  Views are built with `etaf-node', ETAF's
;; ordinary-Elisp construction entry, because the markdown tree is data.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'etaf)
(require 'etaf-ui)
(require 'etaf-md-preview-theme)

(defconst etaf-md-preview--video-extensions
  '("mp4" "mov" "m4v" "webm" "avi" "mkv" "ogv" "mpg" "mpeg" "wmv" "flv" "3gp")
  "File extensions rendered as a video card.")

(defconst etaf-md-preview--admonitions
  '("note" "tip" "important" "warning" "caution")
  "GitHub-style alert names recognized in blockquotes.")

;;; ---------------------------------------------------------------------------
;;; Environment helpers
;;; ---------------------------------------------------------------------------

(defun etaf-md-preview--env-color (env key)
  "Return ENV's color for semantic KEY.
ENV's face-derived `:palette', when present, wins over the named theme; an
unknown key in either source signals `etaf-md-preview-error'."
  (if-let* ((palette (plist-get env :palette)))
      (or (cdr (assq key palette))
          (signal 'etaf-md-preview-error
                  (list (format "Unknown markdown preview color: %S" key))))
    (etaf-md-preview-color key (plist-get env :theme))))

(defun etaf-md-preview--heading-pixels (level)
  "Return the heading font size for LEVEL."
  (let ((base (etaf-md-preview--base-pixels)))
    (round (pcase level
             (1 (* base 1.55))
             (2 (* base 1.32))
             (3 (* base 1.14))
             (_ base)))))

(defun etaf-md-preview--divider (&optional label)
  "Return a horizontal divider View, optionally labeled."
  (etaf-node 'etaf-divider (list :orientation 'horizontal :label label) nil))

(defun etaf-md-preview--spacer (&optional lines)
  "Return an empty spacer Box LINES lines tall."
  (etaf-node 'box (list :height (list 'lh (or lines 1))) nil))

(defconst etaf-md-preview--meta-labels
  '(("author" . "作者") ("date" . "日期") ("version" . "版本")
    ("license" . "许可") ("status" . "状态"))
  "Front matter keys shown in the document header, with their labels.")

(defun etaf-md-preview--meta-tags (meta)
  "Return the tag list of front matter META.
Both `[a, b]' and `a, b' spellings are accepted."
  (let ((raw (cdr (assoc "tags" meta))))
    (when (and raw (not (string-empty-p raw)))
      (let ((raw (replace-regexp-in-string "[][]" "" raw)))
        (delq nil
              (mapcar (lambda (item)
                        (let ((item (string-trim item)))
                          (unless (string-empty-p item) item)))
                      (split-string raw ",")))))))

(defun etaf-md-preview--meta-node (document env)
  "Return the document header for DOCUMENT's front matter, or nil.
The header carries the title, tag chips and the labeled metadata line, so a
document with front matter opens on a page-like header instead of raw text."
  (let* ((meta (plist-get document :meta))
         (title (cdr (assoc "title" meta)))
         (tags (etaf-md-preview--meta-tags meta))
         (labels (mapconcat
                  (lambda (key)
                    (let ((value (cdr (assoc key meta))))
                      (when (and value (not (string-empty-p value)))
                        (format "%s %s" (cdr (assoc key etaf-md-preview--meta-labels)) value))))
                  '("author" "date" "version" "license" "status")
                  "  ·  "))
         (base (etaf-md-preview--base-pixels)))
    (when title
      (etaf-node 'column
                 (list :width 'stretch :item-gap '(lh 1))
                 (append
                  (list
                   (etaf-node 'text
                              (list :font-weight 'bold
                                    :font-size (round (* base 1.9))
                                    :color (etaf-md-preview--env-color env :heading))
                              (list title)))
                  (when tags
                    (list
                     (etaf-node 'flex
                                (list :width 'stretch :gap '((lh 0) (ch 1)))
                                (mapcar (lambda (tag)
                                          (etaf-node 'etaf-tag
                                                     (list :text tag :variant 'neutral)
                                                     nil))
                                        tags))))
                  (when (and labels (not (string-empty-p labels)))
                    (list (etaf-node 'text
                                     (list :color (etaf-md-preview--env-color env :muted))
                                     (list labels)))))))))

;;; ---------------------------------------------------------------------------
;;; Inline rendering
;;; ---------------------------------------------------------------------------

(defun etaf-md-preview--run-props (run env)
  "Return the ETAF style props of inline RUN in ENV."
  (let ((props nil))
    (when (plist-get run :bold)
      (setq props (plist-put props :font-weight 'bold)))
    (when (plist-get run :italic)
      (setq props (plist-put props :font-style 'italic)))
    (when (plist-get run :strike)
      (setq props (plist-put props :text-decoration-line '(line-through))))
    (when (plist-get run :code)
      (setq props (plist-put props :background-color
                             (etaf-md-preview--env-color env :panel)))
      (let ((mono (etaf-md-preview--mono-family)))
        (when mono (setq props (plist-put props :font-family mono)))))
    (when (plist-get run :link)
      (setq props (plist-put props :color (etaf-md-preview--env-color env :accent)))
      (setq props (plist-put props :text-decoration-line '(underline))))
    (when (plist-get run :muted)
      (setq props (plist-put props :color (etaf-md-preview--env-color env :muted))))
    props))

(defun etaf-md-preview--flatten-inlines (nodes inherited env)
  "Return styled run entries for inline NODES in ENV.
INHERITED is the style attribute plist of the enclosing inline element.
Entries are (:text STRING :props PLIST) or (:view VIEW)."
  (let (result)
    (dolist (node nodes)
      (pcase (plist-get node :type)
        ('text
         (push (list :text (or (plist-get node :text) "") :props inherited) result))
        ('break
         (push (list :text (if (plist-get node :hard) "\n" " ") :props inherited)
               result))
        ('code
         (push (list :text (or (plist-get node :text) "")
                     :props (plist-put (copy-sequence inherited) :code t))
               result))
        ((or 'emph 'strong 'strike)
         (let ((child (copy-sequence inherited)))
           (when (eq (plist-get node :type) 'emph)
             (setq child (plist-put child :italic t)))
           (when (eq (plist-get node :type) 'strong)
             (setq child (plist-put child :bold t)))
           (when (eq (plist-get node :type) 'strike)
             (setq child (plist-put child :strike t)))
           (setq result
                 (append (nreverse
                          (etaf-md-preview--flatten-inlines
                           (plist-get node :children) child env))
                         result))))
        ((or 'link 'autolink)
         (let ((child (plist-put (copy-sequence inherited) :link (plist-get node :url))))
           (setq result
                 (append (nreverse
                          (etaf-md-preview--flatten-inlines
                           (plist-get node :children) child env))
                         result))))
        ('footnote-ref
         (push (list :view (etaf-md-preview--footnote-view node env)) result))
        ('image
         (push (list :view (etaf-md-preview--inline-image-view node env)) result))
        ('html
         (push (list :text (or (plist-get node :text) "")
                     :props (plist-put (copy-sequence inherited) :muted t))
               result))
        (_ nil)))
    (nreverse result)))

(defun etaf-md-preview--inline-runs (inlines env base)
  "Flatten INLINES into styled runs for ENV.
BASE is the plist of style attributes inherited from the enclosing block.
Adjacent text runs with equal styles are merged into one entry."
  (let ((runs nil))
    (dolist (run (etaf-md-preview--flatten-inlines inlines base env))
      (let* ((previous (car runs))
             (text (plist-get run :text))
             (props (plist-get run :props))
             (merge (and previous
                         (not (plist-get previous :view))
                         (not (plist-get run :view))
                         (plist-get previous :text)
                         (equal (plist-get previous :props) props)
                         (not (string-suffix-p "\n" (plist-get previous :text)))
                         (not (string-prefix-p "\n" text)))))
        (if merge
            (setcar runs (plist-put previous :text
                                    (concat (plist-get previous :text) text)))
          (push run runs))))
    (nreverse runs)))

(defun etaf-md-preview--runs-view (inlines env base &optional extra)
  "Return ETAF children for INLINES in ENV.
BASE is a run-attribute plist (`:bold', `:italic', `:strike', `:code',
`:link', `:muted'); EXTRA is an Ebox style plist applied to every text run."
  (let ((nodes nil))
    (dolist (run (etaf-md-preview--inline-runs inlines env base))
      (if-let* ((view (plist-get run :view)))
          (push view nodes)
        (let* ((attributes (plist-get run :props))
               (text (plist-get run :text))
               (props (append extra (etaf-md-preview--run-props attributes env)))
               (url (plist-get attributes :link))
               (props (if url
                          (append props
                                  (list :role 'link :tab-index 0 :help-echo url
                                        :on-press (lambda ()
                                                    (funcall (plist-get env :open-url)
                                                             url))))
                        props)))
          (push (etaf-node 'text props (list text)) nodes))))
    (nreverse nodes)))

(defun etaf-md-preview--footnote-view (node env)
  "Return the superscript reference View of footnote NODE in ENV."
  (let ((number (or (plist-get node :number)
                    (plist-get node :label)))
        (label (plist-get node :label)))
    (etaf-node 'text
               (list :color (etaf-md-preview--env-color env :accent)
                     :font-size (round (* (etaf-md-preview--base-pixels) 0.8))
                     :role 'link :tab-index 0
                     :help-echo (format "脚注 %s" label)
                     :on-press (lambda () (funcall (plist-get env :jump) label)))
               (list (format "[%s]" number)))))

(defun etaf-md-preview--inline-image-view (node env)
  "Return the inline chip View of image NODE in ENV."
  (let ((alt (or (plist-get node :alt) "图片"))
        (url (plist-get node :url)))
    (etaf-node 'text
               (list :color (etaf-md-preview--env-color env :accent)
                     :background-color (etaf-md-preview--env-color env :accent-soft)
                     :role 'link :tab-index 0
                     :help-echo (or url alt)
                     :on-press (lambda () (funcall (plist-get env :open-url) url)))
               (list (format "🖼 %s" alt)))))

;;; ---------------------------------------------------------------------------
;;; Media blocks
;;; ---------------------------------------------------------------------------

(defun etaf-md-preview--media-kind (path)
  "Return `video' or `image' for PATH by extension."
  (if (member (downcase (or (file-name-extension path) ""))
              etaf-md-preview--video-extensions)
      'video
      'image))

(defun etaf-md-preview--remote-url-p (url)
  "Return non-nil when URL points to a remote resource."
  (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*://" url))

(defun etaf-md-preview--absolute-media (path env)
  "Return PATH resolved against ENV's base directory."
  (let ((path (if (string-match-p "\\`file://" path)
                  (substring path 7)
                path)))
    (expand-file-name path (or (plist-get env :dir) default-directory))))

(defun etaf-md-preview--image-pixels (path)
  "Return PATH's pixel size as (WIDTH . HEIGHT), or nil.
Emacs caches images by file name, so drop PATH's entry first; a preview that
reloads an updated file must measure the bytes on disk, not the previous
render."
  (ignore-errors
    (clear-image-cache path)
    (when-let* ((image (create-image path nil nil)))
      (let ((size (image-size image t)))
        (cons (car size) (cdr size))))))

(defun etaf-md-preview--video-pixels (path)
  "Return PATH's video pixel size as (WIDTH . HEIGHT), or nil."
  (when-let* ((ffprobe (executable-find "ffprobe")))
    (ignore-errors
      (with-temp-buffer
        (when (zerop (call-process ffprobe nil t nil
                                   "-v" "error" "-select_streams" "v:0"
                                   "-show_entries" "stream=width,height"
                                   "-of" "csv=p=0:s=x" path))
          (let ((line (string-trim (buffer-string))))
            (when (string-match "\\`\\([0-9]+\\)x\\([0-9]+\\)" line)
              (cons (string-to-number (match-string 1 line))
                    (string-to-number (match-string 2 line))))))))))

(defun etaf-md-preview--media-grid (path kind max-columns)
  "Return the (COLUMNS . ROWS) grid for media PATH of KIND.
MAX-COLUMNS caps the width; the height follows the source aspect ratio and is
capped to a third of the display so one figure cannot own the whole window.
When the height cap applies, the width shrinks with it so the figure keeps the
source aspect ratio instead of being squashed into a shorter box."
  (let* ((pixels (pcase kind
                   ('video (etaf-md-preview--video-pixels path))
                   (_ (etaf-md-preview--image-pixels path))))
         (cell-width (max 1 (frame-char-width)))
         (cell-height (max 1 (frame-char-height)))
         (max-rows (max 6 (round (* (window-body-height) 0.45)))))
    (if pixels
        (let* ((pixel-width (car pixels))
               (pixel-height (cdr pixels))
               (columns (min max-columns
                             (max 8 (round (/ (* 1.0 pixel-width)
                                              cell-width)))))
               (rows (round (/ (* columns cell-width pixel-height)
                               (* pixel-width cell-height))))
               (rows (min (max 1 rows) max-rows))
               (columns (if (> (* columns cell-width pixel-height)
                               (* pixel-width rows cell-height))
                            (max 8 (round (/ (* 1.0 rows cell-height
                                              pixel-width)
                                             (* pixel-height cell-width))))
                          columns)))
          (cons columns rows))
      (cons (min max-columns 48) 14))))

(defun etaf-md-preview--media-card (url alt env)
  "Return a media card View for URL with ALT in ENV.
Local images and videos render through the ETAF image Host, which owns the
graphics/terminal adapters and the video poster frame; remote sources render
as a bordered placeholder that opens in the browser."
  (let* ((remote (etaf-md-preview--remote-url-p url))
         (path (unless remote (etaf-md-preview--absolute-media url env)))
         (kind (and path (etaf-md-preview--media-kind path)))
         (exists (and path (file-exists-p path)))
         (open (if (and path exists)
                   ;; Local media opens with the operating system's default
                   ;; application, so a video plays instead of appearing as a
                   ;; binary buffer; remote and missing targets keep the URL
                   ;; callback.
                   (lambda () (funcall (plist-get env :open-file) path))
                 (lambda () (funcall (plist-get env :open-url) url))))
         (columns (plist-get env :width))
         (caption (or alt (and path (file-name-nondirectory path)) url)))
    (cond
     ((and exists (eq kind 'image))
      (let* ((grid (etaf-md-preview--media-grid path kind (- columns 2)))
             (figure
              (etaf-node 'image
                         (list :src path
                               :columns (car grid)
                               :rows (cdr grid)
                               :adapter 'auto
                               :help-echo (format "%s（按 RET 打开）" caption))
                         nil)))
        (etaf-node 'column
                   (list :width 'stretch :cross-align 'center :item-gap '(lh 0))
                   (list
                    (etaf-node 'box
                               (list :border (list '(px 1) 'solid
                                                   (etaf-md-preview--env-color env :border))
                                     :on-press open :role 'button :tab-index 0
                                     :pointer 'hand
                                     :help-echo (format "%s（按 RET 打开）" caption))
                               (list figure))
                    (etaf-md-preview--caption-view
                     (format "%s%s" (if (string-empty-p caption) "" "▲ ") caption)
                     env)))))
     ((and exists (eq kind 'video))
      (let* ((grid (etaf-md-preview--media-grid path kind (- columns 2)))
             (poster
              (etaf-node 'image
                         (list :src path
                               :columns (car grid)
                               :rows (cdr grid)
                               :adapter 'auto)
                         nil)))
        (etaf-node 'column
                   (list :width 'stretch :cross-align 'center :item-gap '(lh 0))
                   (list
                    (etaf-node 'box
                               (list :border (list '(px 1) 'solid
                                                   (etaf-md-preview--env-color env :border))
                                     :on-press open :role 'button :tab-index 0
                                     :pointer 'hand
                                     :help-echo (format "%s（按 RET 播放）" caption))
                               (list poster))
                    (etaf-md-preview--caption-view
                     (format "▶ %s　·　按 RET 播放" caption)
                     env)))))
     (t
      (etaf-node 'column
                 (list :width 'stretch :cross-align 'center :item-gap '(lh 0))
                 (list
                  (etaf-node 'box
                             (list :border (list '(px 1) 'solid
                                                 (etaf-md-preview--env-color env :border))
                                   :background-color (etaf-md-preview--env-color env :panel)
                                   :padding '((lh 1) (ch 2))
                                   :on-press open :role 'button :tab-index 0
                                   :pointer 'hand
                                   :help-echo (format "%s（按 RET 打开）" url))
                             (list
                              (etaf-node 'text
                                         (list :color (etaf-md-preview--env-color env :muted))
                                         (list (format "🖼 %s" caption)))
                              (etaf-node 'text
                                         (list :color (etaf-md-preview--env-color env :faint)
                                               :font-size (round (* (etaf-md-preview--base-pixels) 0.85)))
                                         (list (format "  %s" url)))))
                  (etaf-md-preview--caption-view caption env)))))))

(defun etaf-md-preview--caption-view (text env)
  "Return a caption View for TEXT in ENV."
  (etaf-node 'text
             (list :color (etaf-md-preview--env-color env :muted)
                   :font-size (round (* (etaf-md-preview--base-pixels) 0.85)))
             (list text)))

;;; ---------------------------------------------------------------------------
;;; Block rendering
;;; ---------------------------------------------------------------------------

(defun etaf-md-preview--paragraph-node (block env)
  "Return the paragraph View of BLOCK in ENV."
  (if (and (= 1 (length (plist-get block :children)))
           (eq (plist-get (car (plist-get block :children)) :type) 'image))
      (let ((image (car (plist-get block :children))))
        (etaf-md-preview--media-card (plist-get image :url) (plist-get image :alt) env))
    (etaf-node 'box
               (list :width 'stretch :wrap-mode 'word)
               (etaf-md-preview--runs-view (plist-get block :children) env nil))))

(defun etaf-md-preview--heading-node (block env)
  "Return the heading View of BLOCK in ENV."
  (let* ((level (plist-get block :level))
         (color (if (>= level 5)
                    (etaf-md-preview--env-color env :muted)
                  (etaf-md-preview--env-color env :heading)))
         (children
          (etaf-md-preview--runs-view
           (plist-get block :children) env (list :bold t)
           (list :font-size (etaf-md-preview--heading-pixels level)
                 :color color))))
    (etaf-node 'column
               (list :id (plist-get block :id)
                     :width 'stretch
                     ;; Headings carry no margin or rule of their own: the block
                     ;; flow separates every sibling with exactly one blank
                     ;; line, including heading-to-heading.
                     :margin '((lh 0) (ch 0))
                     :item-gap '(lh 0))
               children)))

(defun etaf-md-preview--code-block-node (block env)
  "Return the code block View of BLOCK in ENV."
  (let* ((mono (etaf-md-preview--mono-family))
         (lang (plist-get block :lang))
         (body (etaf-node 'box
                          (list :width 'stretch :wrap-mode 'char
                                :font-family mono
                                :color (etaf-md-preview--env-color env :ink))
                          (list (etaf-node 'text nil
                                           (list (plist-get block :text))))))
         (header (when lang
                   (etaf-node 'flex
                              (list :width 'stretch :justify-content 'flex-end)
                              (list (etaf-node 'etaf-tag
                                               (list :text lang :variant 'neutral)
                                               nil))))))
    (etaf-node 'box
               (list :width 'stretch
                     :background-color (etaf-md-preview--env-color env :panel)
                     :border (list '(px 1) 'solid (etaf-md-preview--env-color env :border))
                     :padding-top (if header '(lh 0) '(lh 1))
                     :padding-bottom '(lh 1)
                     :padding-left '(ch 2)
                     :padding-right '(ch 2))
               (append (when header (list header)) (list body)))))

(defun etaf-md-preview--blockquote-node (block env)
  "Return the blockquote View of BLOCK in ENV.
A GitHub alert (`> [!NOTE]' and friends) becomes a tinted callout; any other
quote keeps the muted surface and the accent bar."
  (let* ((children (plist-get block :children))
         (alert (and (plist-get (plist-get env :features) :alerts)
                     (etaf-md-preview--alert-kind children))))
    (if alert
        (let* ((spec (etaf-md-preview--alert-spec alert))
               (color (etaf-md-preview--env-color env (nth 1 spec)))
               (soft (etaf-md-preview--env-color env (nth 2 spec))))
          (etaf-node 'box
                     (list :width 'stretch
                           :background-color soft
                           :border-left (list '(px 3) 'solid color)
                           :padding-top '(lh 0)
                           :padding-bottom '(lh 1)
                           :padding-left '(ch 2)
                           :padding-right '(ch 2))
                     (append
                      (list (etaf-node 'text
                                       (list :color color :font-weight 'bold)
                                       (list (format "%s %s" (nth 3 spec) (nth 0 spec)))))
                      (etaf-md-preview--view-blocks
                       (etaf-md-preview--alert-blocks children) env t))))
      (etaf-node 'box
                 (list :width 'stretch
                       :background-color (etaf-md-preview--env-color env :quote-bg)
                       :border-left (list '(px 3) 'solid
                                          (etaf-md-preview--env-color env :quote-bar))
                       :padding-top '(lh 0)
                       :padding-bottom '(lh 1)
                       :padding-left '(ch 2)
                       :padding-right '(ch 2))
                 (etaf-md-preview--view-blocks children env t)))))

(defun etaf-md-preview--alert-kind (blocks)
  "Return the GitHub alert kind when BLOCKS open with an alert marker."
  (let* ((first (car blocks))
         (children (and first (eq (plist-get first :type) 'paragraph)
                        (plist-get first :children)))
         (text (and children (eq (plist-get (car children) :type) 'text)
                    (plist-get (car children) :text))))
    (when (and text (string-match "\\`\\[!\\([A-Za-z]+\\)\\][ \t]*\\(?:\n\\|\\'\\)" text))
      (let ((kind (downcase (match-string 1 text))))
        (and (etaf-md-preview--alert-spec kind) kind)))))
(defun etaf-md-preview--alert-spec (kind)
  "Return (LABEL COLOR-KEY SOFT-KEY ICON) for alert KIND, or nil."
  (pcase (downcase kind)
    ("note" '("笔记" :accent :accent-soft "ℹ"))
    ("tip" '("提示" :ok :ok-soft "✔"))
    ("important" '("重要" :important :important-soft "★"))
    ("warning" '("警告" :warn :warn-soft "⚠"))
    ("caution" '("小心" :danger :danger-soft "⛔"))
    (_ nil)))

(defun etaf-md-preview--alert-blocks (blocks)
  "Return BLOCKS with the leading alert marker removed.
The document tree is shared across renders, so the first paragraph is rebuilt
instead of mutated."
  (let* ((first (car blocks))
         (children (plist-get first :children))
         (text (plist-get (car children) :text))
         (rest (replace-regexp-in-string
                "\\`\\[![A-Za-z]+\\][ \t]*\\n?" "" text)))
    (cons (let ((paragraph (copy-sequence first)))
            (plist-put paragraph :children
                       (if (string-empty-p rest)
                           (cdr children)
                         (cons (plist-put (copy-sequence (car children)) :text rest)
                               (cdr children)))))
          (cdr blocks))))

(defun etaf-md-preview--table-node (block env)
  "Return the table View of BLOCK in ENV.
The table draws its own grid: an outer border, a row separator, a column
separator, a tinted header row and zebra striping, so every column and
alignment stays visible at a glance."
  (let* ((aligns (plist-get block :aligns))
         (columns (length aligns))
         (rows (append (list (plist-get block :header))
                       (plist-get block :rows)))
         (last-row (1- (length rows)))
         (border (list '(px 1) 'solid (etaf-md-preview--env-color env :border)))
         (grids nil))
    (cl-loop for row in rows
             for row-index from 0
             do (let ((cells nil))
                  (cl-loop for inlines in row
                           for align in aligns
                           for column-index from 0
                           do (push
                               (etaf-node 'box
                                          (list :padding '((lh 0) (ch 1))
                                                :width 'stretch
                                                :overflow 'hidden
                                                :wrap-mode 'char
                                                :text-align (or align 'left)
                                                :border-bottom (and (/= row-index last-row)
                                                                    border)
                                                :border-right (and (/= column-index
                                                                       (1- columns))
                                                                   border)
                                                :background-color
                                                (cond
                                                 ((zerop row-index)
                                                  (etaf-md-preview--env-color env :table-header))
                                                 ((zerop (% row-index 2))
                                                  (etaf-md-preview--env-color env :table-stripe))
                                                 (t nil)))
                                          (etaf-md-preview--runs-view
                                           inlines env
                                           (when (zerop row-index) (list :bold t))))
                               cells))
                  ;; One grid per row keeps every row's columns aligned by the
                  ;; same fractional tracks while staying within the layouts
                  ;; the native renderer can accelerate.
                  (push (etaf-node 'grid
                                   (list :width 'stretch
                                         :grid-template-columns (make-list columns '(fr 1)))
                                   (nreverse cells))
                        grids)))
    (etaf-node 'box
               (list :width 'stretch :border border)
               (nreverse grids))))

(defun etaf-md-preview--thematic-break-node ()
  "Return the thematic break View."
  (etaf-md-preview--divider))

(defconst etaf-md-preview--html-media-regexp
  "<\\(video\\|img\\)\\b[^>]*>"
  "Opening tags whose source is rendered as a media card.")

(defun etaf-md-preview--html-attribute (tag name)
  "Return NAME's attribute value from TAG, or nil."
  (when (string-match (format "%s[ \t]*=[ \t]*[\"']\\([^\"']*\\)[\"']" name) tag)
    (match-string 1 tag)))

(defun etaf-md-preview--html-block-node (block env)
  "Return the View of raw HTML BLOCK in ENV.
Media elements become real media cards; other tags stay visible as code, so
the preview never silently drops content it cannot render."
  (let ((text (plist-get block :text)))
    (if (string-match etaf-md-preview--html-media-regexp text)
        (let* ((tag (match-string 0 text))
               (src (etaf-md-preview--html-attribute tag "src"))
               (alt (or (etaf-md-preview--html-attribute tag "alt")
                        (etaf-md-preview--html-attribute tag "title"))))
          (if src
              (etaf-md-preview--media-card src alt env)
            (etaf-md-preview--html-code-node text env)))
      (etaf-md-preview--html-code-node text env))))

(defun etaf-md-preview--html-code-node (text env)
  "Return a muted monospace block View for raw HTML TEXT in ENV."
  (etaf-node 'box
             (list :width 'stretch :wrap-mode 'char
                   :background-color (etaf-md-preview--env-color env :panel)
                   :padding-top '(lh 1)
                   :padding-bottom '(lh 1)
                   :padding-left '(ch 2)
                   :padding-right '(ch 2))
             (list (etaf-node 'text
                              (list :color (etaf-md-preview--env-color env :muted))
                              (list text)))))

(defun etaf-md-preview--list-node (block env)
  "Return the list View of BLOCK in ENV."
  (let ((tight (plist-get block :tight))
        (ordered (plist-get block :ordered))
        (start (or (plist-get block :start) 1))
        (index 0)
        (items nil))
    (dolist (item (plist-get block :items))
      (cl-incf index)
      (let* ((checked (plist-get item :checked))
             (task (plist-get item :task))
             (marker
              (cond
               (task (etaf-node 'text
                                (list :color (if checked
                                                 (etaf-md-preview--env-color env :accent)
                                               (etaf-md-preview--env-color env :muted)))
                                (list (if checked "☑" "☐"))))
               (ordered (etaf-node 'text
                                   (list :color (etaf-md-preview--env-color env :muted))
                                   (list (format "%d." (+ start (1- index))))))
               (t (etaf-node 'text
                             (list :color (etaf-md-preview--env-color env :muted))
                             (list "•")))))
             (content (etaf-md-preview--view-blocks (plist-get item :children) env tight)))
        (push (etaf-node 'flex
                         (list :width 'stretch)
                         (list
                          (etaf-node 'box
                                     (list :width '(ch 3) :flex-shrink 0)
                                     (list marker))
                          (etaf-node 'column
                                     (list :width 'stretch :min-width '(ch 0))
                                     content)))
              items)))
    (etaf-node 'column
               (list :width 'stretch
                     :item-gap (list 'lh (if tight 0 1)))
               (nreverse items))))

(defun etaf-md-preview--footnote-def-node (block env)
  "Return the footnote definition View of BLOCK in ENV."
  (etaf-node 'box
             (list :width 'stretch :wrap-mode 'word
                   :id (format "footnote-%s" (plist-get block :label)))
             (append
              (list (etaf-node 'text
                               (list :color (etaf-md-preview--env-color env :accent)
                                     :font-weight 'bold)
                               (list (format "[%s] " (plist-get block :number)))))
              (etaf-md-preview--runs-view
               (cl-loop for child in (plist-get block :children)
                        append (if (eq (plist-get child :type) 'paragraph)
                                   (plist-get child :children)
                                 nil))
               env nil))))

(defun etaf-md-preview--view-block (block env)
  "Return the list of Views for one BLOCK in ENV."
  (pcase (plist-get block :type)
    ('heading (list (etaf-md-preview--heading-node block env)))
    ('paragraph (list (etaf-md-preview--paragraph-node block env)))
    ('code-block (list (etaf-md-preview--code-block-node block env)))
    ('blockquote (list (etaf-md-preview--blockquote-node block env)))
    ('list (list (etaf-md-preview--list-node block env)))
    ('table (list (etaf-md-preview--table-node block env)))
    ('thematic-break (list (etaf-md-preview--thematic-break-node)))
    ('html-block (list (etaf-md-preview--html-block-node block env)))
    ('footnote-def (list (etaf-md-preview--footnote-def-node block env)))
    (_ nil)))

(defun etaf-md-preview--view-blocks (blocks env &optional tight)
  "Return Views for BLOCKS in ENV.
TIGHT drops the blank line between consecutive blocks."
  (let ((nodes nil)
        (first t))
    (dolist (block blocks)
      (when-let* ((views (etaf-md-preview--view-block block env)))
        (unless (or first tight)
          (push (etaf-md-preview--spacer) nodes))
        (setq first nil)
        (dolist (view views)
          (push view nodes))))
    (nreverse nodes)))

;;; ---------------------------------------------------------------------------
;;; Document
;;; ---------------------------------------------------------------------------

(defun etaf-md-preview--view-document (document env)
  "Return the mounted reading View of DOCUMENT in ENV.
The reading column is centered with flexible spacers and capped by ENV's
`:width', so wide frames keep a readable measure and narrow frames fill.
The cap only bounds the measure: the column itself is viewport-relative, so a
window resize reflows through the renderer's own viewport update instead of
rebuilding the document.  ENV's `:background', when set, paints the page;
without it the buffer keeps the Emacs theme's own background."
  (let* ((width (max 24 (or (plist-get env :width) 92)))
         (background (plist-get env :background))
         (hero (etaf-md-preview--meta-node document env))
         (body (append
                (when hero (list hero (etaf-md-preview--spacer)))
                (etaf-md-preview--view-blocks (plist-get document :children) env)
                (when-let* ((footnotes (plist-get document :footnotes)))
                  (append
                   (list (etaf-md-preview--spacer)
                         (etaf-md-preview--divider "脚注")
                         (etaf-md-preview--spacer))
                   (etaf-md-preview--view-blocks footnotes env))))))
    (etaf-node 'flex
               (append (list :flex-direction 'column :width '(vw 100))
                       (when background
                         (list :background-color background)))
               (list
                (etaf-node 'flex
                           (list :width 'stretch :align-items 'stretch)
                           (list
                            (etaf-node 'box
                                       (list :flex-grow 1 :flex-basis '(ch 0)
                                             :min-width '(ch 0))
                                       nil)
                            (etaf-node 'column
                                       (list :width 'stretch
                                             :max-width (list 'ch width)
                                             :flex-shrink 1
                                             :padding (list (list 'lh 1) (list 'ch 0))
                                             :item-gap '(lh 0)
                                             :color (etaf-md-preview--env-color env :ink))
                                       body)
                            (etaf-node 'box
                                       (list :flex-grow 1 :flex-basis '(ch 0)
                                             :min-width '(ch 0))
                                       nil)))))))

(provide 'etaf-md-preview-views)
;;; etaf-md-preview-views.el ends here
