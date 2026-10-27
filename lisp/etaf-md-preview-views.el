;;; etaf-md-preview-views.el --- Views of the markdown previewer -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Presentation layer of etaf-md-preview.  The document body comes from the
;; shared `etaf-ui-markdown' renderer; this module keeps what is specific to
;; the previewer: the face-derived or named palette, the front-matter header,
;; the reading column, and the media layer that turns images, videos and HTML
;; media elements into poster cards backed by ETAF's image and video Hosts.
;;
;; The functions take a parsed document plus a rendering environment and
;; return ETAF views; they never read session state directly, so one code path
;; renders headless tests and a mounted buffer alike.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'image)
(declare-function image-size "image.c" (spec &optional pixels frame))
(declare-function clear-image-cache "image.c" (&optional filter))
(require 'etaf)
(require 'etaf-ui)
(require 'etaf-md-preview-theme)

(defconst etaf-md-preview--alert-labels
  '((note . "笔记") (tip . "提示") (important . "重要")
    (warning . "警告") (caution . "小心"))
  "Chinese labels of the GitHub alert kinds, for ENV's `:alert-labels'.")

(defconst etaf-md-preview--video-extensions
  '("mp4" "mov" "m4v" "webm" "avi" "mkv" "ogv" "mpg" "mpeg" "wmv" "flv" "3gp")
  "File extensions rendered as a video card.")

(defun etaf-md-preview--env-color (env key)
  "Return ENV's color for semantic KEY.
ENV's face-derived `:palette', when present, wins over the named theme; an
unknown key in either source signals `etaf-md-preview-error'."
  (if-let* ((palette (plist-get env :palette)))
      (or (cdr (assq key palette))
          (signal 'etaf-md-preview-error
                  (list (format "Unknown markdown preview color: %S" key))))
    (etaf-md-preview-color key (plist-get env :theme))))

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

(defun etaf-md-preview--spacer (&optional lines)
  "Return an empty spacer Box LINES lines tall."
  (etaf-node 'box (list :height (list 'lh (or lines 1))) nil))

(defun etaf-md-preview--divider (&optional label)
  "Return a horizontal divider View, optionally labeled."
  (etaf-node 'etaf-divider (list :orientation 'horizontal :label label) nil))

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

(defconst etaf-md-preview--media-pixels-cache-limit 256
  "Maximum file pixel probes retained per process.")

(defvar etaf-md-preview--media-pixels-cache (make-hash-table :test #'equal)
  "Media probes keyed by file identity (path, size, modification time and prober).
Repeated previews of an unchanged file reuse the probe result; a changed
size or timestamp re-probes the bytes on disk.  One file can be measured by
several probes (an image's pixel size, a video's pixel size and a video's
playback metadata), so the prober is part of the key and a result never
answers a different probe.")

(defun etaf-md-preview--media-pixels-cached (path prober)
  "Return PATH's result from PROBER, reusing unchanged file probes.
PATH without readable attributes is probed directly."
  (let* ((attributes (file-attributes path))
         (key (and attributes
                   (list (expand-file-name path)
                         (file-attribute-size attributes)
                         (file-attribute-modification-time attributes)
                         prober)))
         (missing (make-symbol "missing-media-pixels")))
    (if (null key)
        (funcall prober path)
      (let ((cached (gethash key etaf-md-preview--media-pixels-cache missing)))
        (if (not (eq cached missing))
            cached
          (when (> (hash-table-count etaf-md-preview--media-pixels-cache)
                   etaf-md-preview--media-pixels-cache-limit)
            (clrhash etaf-md-preview--media-pixels-cache))
          (puthash key (funcall prober path)
                   etaf-md-preview--media-pixels-cache))))))

(defun etaf-md-preview--probe-image-pixels (path)
  "Return PATH's image pixel size as (WIDTH . HEIGHT), or nil.
Drop PATH's entry first so an updated file is measured from its bytes rather
than the previous render."
  (ignore-errors
    (clear-image-cache path)
    (when-let* ((image (create-image path nil nil)))
      (let ((size (image-size image t)))
        (cons (car size) (cdr size))))))

(defun etaf-md-preview--image-pixels (path)
  "Return PATH's pixel size as (WIDTH . HEIGHT), or nil.
Pixel measurement needs a graphic frame; a terminal frame renders the media
placeholder without probing.  Unchanged files reuse their cached probe."
  (when (display-graphic-p)
    (etaf-md-preview--media-pixels-cached
     path #'etaf-md-preview--probe-image-pixels)))

(defun etaf-md-preview--probe-video-pixels (path)
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

(defun etaf-md-preview--video-pixels (path)
  "Return PATH's video pixel size as (WIDTH . HEIGHT), or nil.
Unchanged files reuse their cached probe."
  (etaf-md-preview--media-pixels-cached
   path #'etaf-md-preview--probe-video-pixels))

(defun etaf-md-preview--probe-video-source (path)
  "Return PATH's native playback metadata, or nil when unsupported."
  (ignore-errors (etaf-video-source-info path)))

(defun etaf-md-preview--native-video-source (path)
  "Return PATH's playback metadata when the native player can run it.
Needs ffmpeg on `exec-path' and an ffprobe-readable source; unchanged files
reuse their cached probe."
  (when (executable-find "ffmpeg")
    (etaf-md-preview--media-pixels-cached
     path #'etaf-md-preview--probe-video-source)))

(defun etaf-md-preview--media-grid-for-pixels (pixels max-columns)
  "Return the (COLUMNS . ROWS) grid for PIXELS capped by MAX-COLUMNS.
The height follows the source aspect ratio and is capped to a third of the
display so one figure cannot own the whole window.  When the height cap
applies, the width shrinks with it so the figure keeps the source aspect
ratio instead of being squashed into a shorter box."
  (let ((cell-width (max 1 (frame-char-width)))
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

(defun etaf-md-preview--media-grid (path kind max-columns)
  "Return the (COLUMNS . ROWS) grid for media PATH of KIND.
MAX-COLUMNS caps the width; the height follows the source aspect ratio and is
capped to a third of the display so one figure cannot own the whole window.
When the height cap applies, the width shrinks with it so the figure keeps the
source aspect ratio instead of being squashed into a shorter box."
  (etaf-md-preview--media-grid-for-pixels
   (pcase kind
     ('video (etaf-md-preview--video-pixels path))
     (_ (etaf-md-preview--image-pixels path)))
   max-columns))

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
      (let* ((native (etaf-md-preview--native-video-source path))
             (grid (if native
                       (etaf-md-preview--media-grid-for-pixels
                        (and (numberp (plist-get native :width))
                             (numberp (plist-get native :height))
                             (cons (plist-get native :width)
                                   (plist-get native :height)))
                        (- columns 2))
                     (etaf-md-preview--media-grid path kind (- columns 2))))
             (card (funcall (plist-get env :video-card) path))
             (poster
              (etaf-node 'image
                         (list :src path
                               :columns (car grid)
                               :rows (cdr grid)
                               :adapter 'auto)
                         nil)))
        (cond
         ((null native)
          ;; No decoder: the poster card opens the system player as before.
          (etaf-node 'column
                     (list :width 'stretch :cross-align 'center :item-gap '(lh 0)
                           :on-press open :role 'button :tab-index 0
                           :pointer 'hand
                           :help-echo (format "%s（按 RET 播放）" caption))
                     (list
                      (etaf-node 'box
                                 (list :border (list '(px 1) 'solid
                                                     (etaf-md-preview--env-color env :border)))
                                 (list poster))
                      (etaf-md-preview--caption-view
                       (format "▶ %s　·　按 RET 播放" caption)
                       env))))
         ((funcall (plist-get env :video-active-p) (car card))
          ;; Playing inline: RET toggles this card's playback.
          (let ((handle (cdr card)))
            (etaf-node 'column
                       (list :width 'stretch :cross-align 'center :item-gap '(lh 0)
                             :on-press (lambda ()
                                         (etaf-video-toggle-playback handle))
                             :role 'button :tab-index 0 :pointer 'hand
                             :help-echo (format "%s（按 RET 播放/暂停）" caption))
                       (list
                        (etaf-node 'box
                                   (list :border (list '(px 1) 'solid
                                                       (etaf-md-preview--env-color env :border)))
                                   (list
                                    (etaf-node 'etaf-video
                                               (list :src path
                                                     :columns (car grid)
                                                     :rows (cdr grid)
                                                     :mode 'image
                                                     :autoplay t
                                                     :handle handle)
                                               nil)))
                        (etaf-md-preview--caption-view
                         (format "▶ %s　·　按 RET 播放/暂停" caption)
                         env)))))
         (t
          ;; A decoder is available, but decoding starts only when the
          ;; reader asks for it: the card shows the poster until RET mounts
          ;; the native player in place.
          (let ((key (car card)))
            (etaf-node 'column
                       (list :width 'stretch :cross-align 'center :item-gap '(lh 0)
                             :on-press (lambda ()
                                         (funcall (plist-get env :activate-video) key))
                             :role 'button :tab-index 0 :pointer 'hand
                             :help-echo (format "%s（按 RET 内嵌播放）" caption))
                       (list
                        (etaf-node 'box
                                   (list :border (list '(px 1) 'solid
                                                       (etaf-md-preview--env-color env :border)))
                                   (list poster))
                        (etaf-md-preview--caption-view
                         (format "▶ %s　·　按 RET 播放" caption)
                         env))))))))
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

;;; ---------------------------------------------------------------------------
;;; Media rendering
;;; ---------------------------------------------------------------------------

(defun etaf-md-preview--render-media (kind node env)
  "Return the media View for NODE of KIND in ENV.
Inline images render as a compact chip; block images, videos and HTML media
elements render as the full poster card with caption and activation."
  (pcase kind
    ('inline-image (etaf-md-preview--inline-image-view node env))
    (_ (etaf-md-preview--media-card (plist-get node :url)
                                    (plist-get node :alt) env))))

;;; ---------------------------------------------------------------------------
;;; Document
;;; ---------------------------------------------------------------------------

(defconst etaf-md-preview--measure-gutter 2
  "Character cells kept blank on each side of the reading measure.
A window narrower than the measure fills the column, so centering alone
leaves no margin there and text would start on the window edge.  The gutter
is padding, so it survives that case, and it also keeps a table's right
border off the window's last column, where the renderer would extend its
paint into the sub-cell remainder.")

(defun etaf-md-preview--view-document (document env)
  "Return the mounted reading View of DOCUMENT in ENV.
The document body renders through the shared `etaf-ui-markdown-view-blocks';
the reading column uses cross-axis centering and is capped by ENV's `:width',
so wide frames keep a readable measure and narrow frames fill down to the
measure gutter on both sides.  ENV's `:background', when set, paints the
page; without it the buffer keeps the Emacs theme's own background."
  (let* ((width (max 24 (or (plist-get env :width) 92)))
         (gutter etaf-md-preview--measure-gutter)
         (background (plist-get env :background))
         (hero (etaf-md-preview--meta-node document env))
         (body (append
                (when hero (list hero (etaf-md-preview--spacer)))
                (etaf-ui-markdown-view-blocks (plist-get document :children) env)
                (when-let* ((footnotes (plist-get document :footnotes)))
                  (append
                   (list (etaf-md-preview--spacer)
                         (etaf-md-preview--divider "脚注")
                         (etaf-md-preview--spacer))
                   (etaf-ui-markdown-view-blocks footnotes env))))))
    (etaf-node 'column
               (append (list :width '(vw 100) :cross-align 'center
                             :aria-label "Markdown 预览"
                             :help "链接和图片可直接激活；视频在文档内播放，缺少解码器时使用系统播放器。")
                       (when background
                         (list :background-color background)))
               (list
                (etaf-node 'column
                           (list :width 'stretch
                                 :max-width (list 'ch (+ width (* 2 gutter)))
                                 :overflow 'hidden
                                 :padding (list (list 'lh 1) (list 'ch gutter))
                                 :item-gap '(lh 0)
                                 :color (etaf-md-preview--env-color env :ink))
                           body)))))

(provide 'etaf-md-preview-views)
;;; etaf-md-preview-views.el ends here
