;;; dokuwiki-mode.el --- Major mode for DokuWiki document

;; Copyright (C)  2013-2017 Tsunenobu Kai

;; Author: Tsunenobu Kai <kai2nenobu@gmail.com>
;; URL: https://github.com/kai2nenobu/emacs-dokuwiki-mode
;; Version: 0.1.1
;; Keywords: hypermedia text DokuWiki

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defgroup dokuwiki-mode nil
  "Major mode for DokuWiki document."
  :group 'text
  :group 'dokuwiki
  :tag "DokuWiki"
  :link '(url-link "https://www.dokuwiki.org/dokuwiki"))

(defvar dokuwiki-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'outline-next-visible-heading)
    (define-key map (kbd "C-c C-p") 'outline-previous-visible-heading)
    (define-key map (kbd "C-c C-f") 'outline-forward-same-level)
    (define-key map (kbd "C-c C-b") 'outline-backward-same-level)
    (define-key map (kbd "C-c C-u") 'outline-up-heading)
    (define-key map (kbd "C-c C-@") 'outline-mark-subtree)
    map)
  "Keymap for the `dokuwiki-mode'.")

(defvar dokuwiki-smiley-list
  '("8-)" "8-O" ":-(" ":-)" "=) " ":-/" ":-\\" ":-?" ":-D" ":-P" ":-O"
    ":-X" ":-|" ";-)" "^_^" ":?:" ":!:" "LOL" "FIXME" "DELETEME")
  "Smiley list in DokuWiki.")

(defvar dokuwiki-outline-regexp " ?\\(=\\{2,6\\}\\)"
  "Regexp which indicates headline in DokuWiki.
See also `outline-regexp'.")

;;;; Faces
(defface dokuwiki-box '((t (:box t)))
  "Face enabled box property")

(defface dokuwiki-code '((t (:inherit shadow :foreground "forest green")))
  "DokuWiki face for code."
  :group 'dokuwiki)

(defface dokuwiki-list '((t (:inherit font-lock-type-face)))
  "DokuWiki face for list."
  :group 'dokuwiki)

(defface dokuwiki-verbatim '((t (:inherit shadow)))
  "DokuWiki face for text as is."
  :group 'dokuwiki)

(defface dokuwiki-footnote '((t (:inherit font-lock-builtin-face)))
  "DokuWiki face for footnote."
  :group 'dokuwiki)

(defface dokuwiki-headline-1 '((t (:inherit outline-1)))
  "DokuWiki face for level 1 headline."
  :group 'dokuwiki)

(defface dokuwiki-headline-2 '((t (:inherit outline-2)))
  "DokuWiki face for level 2 headline."
  :group 'dokuwiki)

(defface dokuwiki-headline-3 '((t (:inherit outline-3)))
  "DokuWiki face for level 3 headline."
  :group 'dokuwiki)

(defface dokuwiki-headline-4 '((t (:inherit outline-4)))
  "DokuWiki face for level 4 headline."
  :group 'dokuwiki)

(defface dokuwiki-headline-5 '((t (:inherit outline-5)))
  "DokuWiki face for level 5 headline."
  :group 'dokuwiki)

(defface dokuwiki-link '((t (:inherit link)))
  "DokuWiki face for link."
  :group 'dokuwiki)

(defface dokuwiki-image '((t (:inherit font-lock-variable-name-face)))
  "DokuWiki face for image."
  :group 'dokuwiki)

(defface dokuwiki-table '((t (:inherit font-lock-function-name-face)))
  "DokuWiki face for table."
  :group 'dokuwiki)

(defface dokuwiki-smiley '((t (:inherit font-lock-constant-face)))
  "DokuWiki face for smiley."
  :group 'dokuwiki)

(defface light-green-highlight '((t (:background "light green")))
  "Face for light green highlight")
  
(defface light-gray-highlight '((t (:background "light gray")))
  "Face for light gray highlight")

(defvar dokuwiki-hide-markup nil
  "Determines whether markup in the buffer will be hidden.")

(defun dokuwiki-toggle-markup-hiding (&optional arg)
  "Toggle the display or hiding of dokuwiki markup."
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq dokuwiki-hide-markup
        (if (eq arg 'toggle)
            (not dokuwiki-hide-markup)
          (> (prefix-numeric-value arg) 0)))
  (if dokuwiki-hide-markup
      (progn (add-to-invisibility-spec 'dokuwiki-markup)
             (message "dokuwiki-mode markup hiding enabled"))
    (progn (remove-from-invisibility-spec 'dokuwiki-markup)
           (message "dokuwiki-mode markup hiding disabled")))
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max) '(invisible nil face nil)))
  (font-lock-flush)
  (font-lock-ensure))


(make-variable-buffer-local 'dokuwiki-hide-markup)

(defun dokuwiki-font-lock-bold (beg end)
  "Hide bold markup and apply bold face between BEG and END."
  (when dokuwiki-hide-markup
    (remove-text-properties beg end '(invisible nil face nil))
    (set-text-properties beg (+ beg 2) '(invisible dokuwiki-markup))
    (set-text-properties (- end 2) end '(invisible dokuwiki-markup)))
  (set-text-properties (+ beg 2) (- end 2) '(face bold)))


(defun dokuwiki-font-lock-italic (beg end)
  "Hide italic markup and apply italic face between BEG and END."
  (when dokuwiki-hide-markup
    (remove-text-properties beg end '(invisible nil face nil))
    (set-text-properties beg (+ beg 2) '(invisible dokuwiki-markup))
    (set-text-properties (- end 2) end '(invisible dokuwiki-markup)))
  (set-text-properties (+ beg 2) (- end 2) '(face light-gray-highlight)))

(defun dokuwiki-font-lock-underline (beg end)
  "Hide underline markup and apply underline face between BEG and END."
  (when dokuwiki-hide-markup
    (remove-text-properties beg end '(invisible nil face nil))
    (set-text-properties beg (+ beg 2) '(invisible dokuwiki-markup))
    (set-text-properties (- end 2) end '(invisible dokuwiki-markup)))
  (set-text-properties (+ beg 2) (- end 2) '(face light-green-highlight)))


(defun dokuwiki-font-lock-monospace (beg end)
  "Hide monospace markup and apply monospace face between BEG and END."
  (when dokuwiki-hide-markup
    (remove-text-properties beg end '(invisible nil face nil))
    (set-text-properties beg (+ beg 2) '(invisible dokuwiki-markup))
    (set-text-properties (- end 2) end '(invisible dokuwiki-markup)))
  (set-text-properties (+ beg 2) (- end 2) '(face dokuwiki-code)))


(defun dokuwiki-font-lock-headline-1 (beg end)
  "Hide headline markup and apply headline face between BEG and END."
  (when dokuwiki-hide-markup
    (remove-text-properties beg end '(invisible nil face nil))
    (set-text-properties beg (+ beg 7) '(invisible dokuwiki-markup))
    (set-text-properties (- end 7) end '(invisible dokuwiki-markup)))
  (set-text-properties (+ beg 7) (- end 7) '(face dokuwiki-headline-1)))

(defun dokuwiki-font-lock-headline-2 (beg end)
  "Hide headline markup and apply headline face between BEG and END."
  (when dokuwiki-hide-markup
    (remove-text-properties beg end '(invisible nil face nil))
    (set-text-properties beg (+ beg 6) '(invisible dokuwiki-markup))
    (set-text-properties (- end 6) end '(invisible dokuwiki-markup)))
  (set-text-properties (+ beg 6) (- end 6) '(face dokuwiki-headline-2)))

(defun dokuwiki-font-lock-headline-3 (beg end)
  "Hide headline markup and apply headline face between BEG and END."
  (when dokuwiki-hide-markup
    (remove-text-properties beg end '(invisible nil face nil))
    (set-text-properties beg (+ beg 5) '(invisible dokuwiki-markup))
    (set-text-properties (- end 5) end '(invisible dokuwiki-markup)))
  (set-text-properties (+ beg 5) (- end 5) '(face dokuwiki-headline-3)))

(defun dokuwiki-font-lock-headline-4 (beg end)
  "Hide headline markup and apply headline face between BEG and END."
  (when dokuwiki-hide-markup
    (remove-text-properties beg end '(invisible nil face nil))
    (set-text-properties beg (+ beg 4) '(invisible dokuwiki-markup))
    (set-text-properties (- end 4) end '(invisible dokuwiki-markup)))
  (set-text-properties (+ beg 4) (- end 4) '(face dokuwiki-headline-4)))

(defun dokuwiki-font-lock-headline-5 (beg end)
  "Hide headline markup and apply headline face between BEG and END."
  (when dokuwiki-hide-markup
    (remove-text-properties beg end '(invisible nil face nil))
    (set-text-properties beg (+ beg 3) '(invisible dokuwiki-markup))
    (set-text-properties (- end 3) end '(invisible dokuwiki-markup)))
  (set-text-properties (+ beg 3) (- end 3) '(face dokuwiki-headline-5)))

(defun dokuwiki-font-lock-link (beg end)
  "Hide link target and apply link face between BEG and END."
  (when dokuwiki-hide-markup
    (remove-text-properties beg end '(invisible nil face nil))
    (set-text-properties beg (+ beg 2) '(invisible dokuwiki-markup))
    (set-text-properties (- end 2) end '(invisible dokuwiki-markup)))
  (let ((link-text-start (+ beg 2))
        (link-text-end (- end 2)))
    (when (match-string 3)  ; if there is link text
      (set-text-properties link-text-start link-text-end '(face dokuwiki-link))
      (set-text-properties (+ beg 2) (+ beg 3 (length (match-string 1))) '(invisible dokuwiki-markup)))))

; (defun dokuwiki-font-lock-anchor (beg end)
;   "Hide anchor markup and apply anchor face between BEG and END."
;   (let ((original-text (buffer-substring beg end)))
;     (when dokuwiki-hide-markup
;       (remove-text-properties beg end '(invisible nil face nil))
;       (set-text-properties beg (+ beg 2) '(invisible dokuwiki-markup))
;       (set-text-properties (- end 2) end '(invisible dokuwiki-markup))
;       (set-text-properties (+ beg 2) (- end 2) '(display (propertize "O" 'face '(:foreground "red")))))
;     (unless dokuwiki-hide-markup
;       (set-text-properties beg end '(display original-text)))))

(defun dokuwiki-font-lock-anchor (beg end)
  "Hide anchor markup and apply anchor face between BEG and END."
  (let ((original-text (buffer-substring beg end)))
    (when dokuwiki-hide-markup
      (remove-text-properties beg end '(invisible nil face nil))
      (set-text-properties beg (+ beg 2) '(invisible dokuwiki-markup))
      (set-text-properties (- end 2) end '(invisible dokuwiki-markup))
      (let ((o-pos (+ beg 2)))
        (set-text-properties o-pos (- end 2) '(display (propertize "O" 'face '(:foreground "red")) face '(:foreground "red")))))
    (unless dokuwiki-hide-markup
      (set-text-properties beg end '(display original-text face '(:foreground "blue"))))))


(defun dokuwiki-font-lock-strikethrough (beg end)
  "Hide strikethrough markup and apply strikethrough face between BEG and END."
  (when dokuwiki-hide-markup
    (remove-text-properties beg end '(invisible nil face nil))
    (set-text-properties beg (+ beg 2) '(invisible dokuwiki-markup))
    (set-text-properties (- end 2) end '(invisible dokuwiki-markup)))
  (set-text-properties (+ beg 2) (- end 2) '(face (:strike-through t))))

; :TODO: 所有的标记识别要绕开''' ''' 多行代码块
(defvar dokuwiki-font-lock-keywords
  `(
   ;; bold
   ("\\(\\*\\*\\)\\([^*]+\\)\\(\\*\\*\\)" (0 (dokuwiki-font-lock-bold (match-beginning 0) (match-end 0))))
   ;; italic
   ("\\(//\\)\\([^/]+\\)\\(//\\)" (0 (dokuwiki-font-lock-italic (match-beginning 0) (match-end 0))))
   ;; underline
   ("\\(__\\)\\([^_]+\\)\\(__\\)" (0 (dokuwiki-font-lock-underline (match-beginning 0) (match-end 0))))
   ;; monospace
   ("\\(''\\)\\([^']+\\)\\(''\\)" (0 (dokuwiki-font-lock-monospace (match-beginning 0) (match-end 0))))
   ;; verbatim
   ("%%.+?%%" (0 'dokuwiki-code t))
   ;; footnote
   ("((.+?))" (0 'dokuwiki-footnote))
   ;; headline
   ;; 标题中不运行出现等号
   ("\\( ?======\\)\\([^=]+\\)\\(======[ \t]*$\\)" (0 (dokuwiki-font-lock-headline-1 (match-beginning 0) (match-end 0))))
   ("\\( ?=====\\)\\([^=]+\\)\\(=====[ \t]*$\\)" (0 (dokuwiki-font-lock-headline-2 (match-beginning 0) (match-end 0))))
   ("\\( ?====\\)\\([^=]+\\)\\(====[ \t]*$\\)" (0 (dokuwiki-font-lock-headline-3 (match-beginning 0) (match-end 0))))
   ("\\( ?===\\)\\([^=]+\\)\\(===[ \t]*$\\)" (0 (dokuwiki-font-lock-headline-4 (match-beginning 0) (match-end 0))))
   ("\\( ?==\\)\\([^=]+\\)\\(==[ \t]*$\\)" (0 (dokuwiki-font-lock-headline-5 (match-beginning 0) (match-end 0))))
   ;; link
   ;; ("\\[\\[[^|]+?\\(?:\\(|\\)\\(.*?\\)\\)?\\]\\]"
   ;;  (0 'dokuwiki-link) (1 'dokuwiki-code t t)
   ;;  (2 'font-lock-string-face t t) (2 'underline append t)) 
   ("\\[\\[\\([^]|]+?\\)\\(?:\\(|\\)\\([^]]*?\\)\\)?\\]\\]"
     (0 (dokuwiki-font-lock-link (match-beginning 0) (match-end 0))))
    
   ;; anchor
   ("\\({{id: \\)\\([^}]+\\)\\(}}\\)" (0 (dokuwiki-font-lock-anchor (match-beginning 0) (match-end 0))))
   
   ;; strikethrough
   ("\\(~~\\)\\([^~]+\\)\\(~~\\)" (0 (dokuwiki-font-lock-strikethrough (match-beginning 0) (match-end 0))))

(defun dokuwiki-font-lock-strikethrough (beg end)
  "Hide strikethrough markup and apply strikethrough face between BEG and END."
  (when dokuwiki-hide-markup
    (remove-text-properties beg end '(invisible nil face nil))
    (set-text-properties beg (+ beg 2) '(invisible dokuwiki-markup))
    (set-text-properties (- end 2) end '(invisible dokuwiki-markup)))
  (set-text-properties (+ beg 2) (- end 2) '(face (:strike-through t))))

    
   ("https?://\\(\\([-_.!~*'()a-zA-Z0-9;?:@&=+$,%#]+\\)/?\\)+" (0 'dokuwiki-link))
   ;; image
   ("{{[^|]+?\\(|\\(.*?\\)\\)?}}"
    (0 'dokuwiki-image t)
    (1 'dokuwiki-code t t) (2 'font-lock-string-face t t))
   ;; table
   ("^[ \t]*[|^].*$" (0 'dokuwiki-table))
   ;; linebreak
   ("\\\\\\\\\\s-+" (0 'dokuwiki-code t))
   ;; list
   ("^\\(?: \\{2\\}\\|[\t]\\)[ \t]*" "\\([-*]\\).*$" nil nil (1 'dokuwiki-list))
   ;; code block
   ("^\\(?: \\{2\\}\\|[\t]\\)[ \t]*" dokuwiki-code-block-search
     nil nil (0 'dokuwiki-code t))
   ;; smiley
   ,@(mapcar #'(lambda (smiley)
                 (list (concat "\\W\\(" (regexp-quote smiley) "\\)\\W")
                       1 'dokuwiki-smiley))
             dokuwiki-smiley-list)
   ))

(defun dokuwiki-code-block-search (limit)
  (if (not (looking-at "[-*]"))
      (re-search-forward ".*$" limit t)))

(defun dokuwiki-outline-level ()
  "Compute a header's nesting level in `dokuwiki-mode'.
See also `outline-level'."
  (when (looking-at outline-regexp)
    (let ((const 7)
          (headline (match-string 1)))
      (- const (length headline)))))

;;;; Work with `outline-magic'
(eval-after-load "outline-magic"
  '(progn
     (define-key dokuwiki-mode-map (kbd "TAB") 'outline-cycle)
     (define-key dokuwiki-mode-map (kbd "<S-tab>")
       '(lambda () (interactive) (outline-cycle '(4))))
     (define-key dokuwiki-mode-map (kbd "<M-S-right>") 'outline-demote)
     (define-key dokuwiki-mode-map (kbd "<M-S-left>") 'outline-promote)
     (define-key dokuwiki-mode-map (kbd "<M-up>") 'outline-move-subtree-up)
     (define-key dokuwiki-mode-map (kbd "<M-down>") 'outline-move-subtree-down)
     (add-hook 'dokuwiki-mode-hook 'dokuwiki-outline-magic-hook)
     ;; Enable outline-magic features in `dokuwiki-mode' buffers
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (eq major-mode 'dokuwiki-mode) (dokuwiki-outline-magic-hook))))
     ))

(defun dokuwiki-outline-magic-hook ()
  "Hook to configure `outline-magic'."
  (set (make-local-variable 'outline-promotion-headings)
       '(("======" . 1) ("=====" . 2) ("====" . 3) ("===" . 4) ("==" . 5)))
  (set (make-local-variable 'outline-cycle-emulate-tab) t))

;;;###autoload
(define-derived-mode dokuwiki-mode text-mode "DokuWiki"
  "Major mode for DokuWiki document."
  (set (make-local-variable 'font-lock-defaults)
       '(dokuwiki-font-lock-keywords
         nil nil ((?_ . "w")) nil))
  (set (make-local-variable 'outline-regexp) dokuwiki-outline-regexp)
  (set (make-local-variable 'outline-level) 'dokuwiki-outline-level)
  (outline-minor-mode 1)
  )

(provide 'dokuwiki-mode)






(defun dokuwiki-imenu-create-nested-index ()
  "Create and return a nested imenu index alist for the current buffer."
  (let* ((root (list nil))
         (min-level 9999)
         headers)
    (save-excursion
      ;; Headings
      (goto-char (point-min))
      (while (re-search-forward "^\\(==+\\) \\(.*?\\) \\1$" (point-max) t)
        (let* ((hashes (match-string-no-properties 1))
               (heading (match-string-no-properties 2))
               (level (- 6 (length hashes))))
          (setq min-level (min min-level level))
          (push (list :heading heading
                      :point (match-beginning 0)
                      :level (- level (1- min-level))) headers)))
      (cl-loop with cur-level = 0
               with cur-alist = nil
               with empty-heading = "-"
               with self-heading = "."
               for header in (reverse headers)
               for level = (plist-get header :level)
               do
               (let ((alist (list (cons (plist-get header :heading) (plist-get header :point)))))
                 (cond
                  ((= cur-level level)  ; new sibling
                   (setcdr cur-alist alist)
                   (setq cur-alist alist))
                  ((< cur-level level)  ; first child
                   (dotimes (_ (- level cur-level 1))
                     (setq alist (list (cons empty-heading alist))))
                   (if cur-alist
                       (let* ((parent (car cur-alist))
                              (self-pos (cdr parent)))
                         (setcdr parent (cons (cons self-heading self-pos) alist)))
                     (setcdr root alist)) ; primogenitor
                   (setq cur-alist alist)
                   (setq cur-level level))
                  (t                    ; new sibling of an ancestor
                   (let ((sibling-alist (last (cdr root))))
                     (dotimes (_ (1- level))
                       (setq sibling-alist (last (cdar sibling-alist))))
                     (setcdr sibling-alist alist)
                     (setq cur-alist alist))
                   (setq cur-level level)))))
      (setq root (copy-tree root))
      (cdr root))))


(defun dokuwiki-display-inline-images ()
  "Add inline image overlays to image links in the buffer."
  (interactive)
  (unless (display-images-p)
    (error "Cannot show images"))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      ;; Update the regex to match DokuWiki's image format
      (let ((dokuwiki-regex-image "{{\\.\\(.*?\\)\\(\\.png\\|\\.jpg\\|\\.jpeg\\|\\.gif\\)\\(\\?width=\\([0-9]+\\)\\)?\\(\\?height=\\([0-9]+\\)\\)?}}"))
        (while (re-search-forward dokuwiki-regex-image nil t)
          (let* ((start (match-beginning 0))
                 (end (match-end 0))
                 ;; Update the file path according to DokuWiki's file structure
                 (file (concat (file-name-sans-extension (buffer-file-name)) "/"
                               (match-string-no-properties 1) (match-string-no-properties 2)))
                 (width (and (match-string-no-properties 4)
                             (string-to-number (match-string-no-properties 4))))
                 (height (and (match-string-no-properties 6)
                              (string-to-number (match-string-no-properties 6)))))
            (when (file-exists-p file)
              (let* ((image (create-image file nil nil :width width :height height)))
                (when image
                  (let ((ov (make-overlay start end)))
                    (overlay-put ov 'display image)
                    (overlay-put ov 'face 'default)
                    ;; Add a mouse click event to the overlay
                    (overlay-put ov 'mouse-face 'highlight)
                    (overlay-put ov 'help-echo "mouse-1: Open this image in a new buffer")
                    (overlay-put ov 'keymap (let ((map (make-sparse-keymap)))
                                              (define-key map [mouse-1]
                                                `(lambda ()
                                                   (interactive)
                                                   (find-file ,file)))
                                              map))))))))))))



(defun dokuwiki-remove-inline-images ()
  "Remove all overlays in the buffer."
  (interactive)
  (remove-overlays))

(defun dokuwiki-refresh-inline-images ()
  "Refresh inline image overlays in the buffer."
  (interactive)
  (dokuwiki-remove-inline-images)
  (dokuwiki-display-inline-images))

(defun dokuwiki-setup ()
  "Setup imenu for dokuwiki mode."
  (setq imenu-sort-function nil)  ; do not sort the imenu entries
  (setq imenu-create-index-function 'dokuwiki-imenu-create-nested-index))  ; use the custom index function

(add-hook 'dokuwiki-mode-hook 'dokuwiki-setup)  ; add the setup function to the dokuwiki mode hook

;;; dokuwiki-mode.el ends here
