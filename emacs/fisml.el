;; Copyright © 2017 by D. F. Hall <authorfunc@hardboiledbabylon.com>

;; Permission to use, copy, modify, and/or distribute this software for
;; any purpose with or without fee is hereby granted, provided that the
;; above copyright notice and this permission notice appear in all
;; copies.

;; THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
;; OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.

(require 'dfh-pair)

(defgroup fisml nil
  "Faces for highlighting Free Indirect Semantic Markup display table entries."
  :group 'fisml)      

(defface fisml-tag-face
  '((t (:inverse-video t)))
  "Face for `fisml-tag-face`."
  :group 'fisml)

(defface fisml-comment-face
  '((t (:inverse-video t)))
  "Face for `fisml-comment-face`."
  :group 'fisml)

(defface fisml-comment-attr-face
  '((t (:inverse-video t)))
  "Face for `fisml-comment-attr-face`."
  :group 'fisml)

(defface fisml-footnote-face
  '((t (:inverse-video t)))
  "Face for `fisml-footnote-face`."
  :group 'fisml)

(defface fisml-footnote-id-face
  '((t (:inverse-video t)))
  "Face for `fisml-footnote-id-face`."
  :group 'fisml)

(defface fisml-footnote-mark-face
  '((t (:inverse-video t)))
  "Face for `fisml-footnote-mark-face`."
  :group 'fisml)

(defface fisml-empty-tag-face
  '((t (:inverse-video t)))
  "Face for `fisml-empty-tag-face`."
  :group 'fisml)

(defface fisml-quote-tag-face
  '((t (:inverse-video t)))
  "Face for `fisml-quote-tag-face`."
  :group 'fisml)

(defface fisml-quote-itag-face
  '((t (:inverse-video t)))
  "Face for `fisml-quote-itag-face`."
  :group 'fisml)

(defface fisml-punct-tag-face
  '((t (:inverse-video t)))
  "Face for `fisml-punct-tag-face`."
  :group 'fisml)

(defface fisml-punct-itag-face
  '((t (:inverse-video t)))
  "Face for `fisml-punct-itag-face`."
  :group 'fisml)

(defface fisml-placeholder-tag-face
  '((t (:inverse-video t)))
  "Face for `fisml-placeholder-tag-face`."
  :group 'fisml)

(defface fisml-placeholder-itag-face
  '((t (:inverse-video t)))
  "Face for `fisml-placeholder-itag-face`."
  :group 'fisml)

(defvar fisml-tag-face 'fisml-tag-face)
(defvar fisml-comment-face 'fisml-comment-face)
(defvar fisml-comment-attr-face 'fisml-comment-attr-face)
(defvar fisml-footnote-face 'fisml-footnote-face)
(defvar fisml-footnote-id-face 'fisml-footnote-id-face)
(defvar fisml-footnote-mark-face 'fisml-footnote-mark-face)
(defvar fisml-empty-tag-face 'fisml-empty-tag-face)
(defvar fisml-quote-tag-face 'fisml-quote-tag-face)
(defvar fisml-quote-itag-face 'fisml-quote-itag-face)
(defvar fisml-punct-tag-face 'fisml-punct-tag-face)
(defvar fisml-punct-itag-face 'fisml-punct-itag-face)
(defvar fisml-placeholder-tag-face 'fisml-placeholder-tag-face)
(defvar fisml-placeholder-itag-face 'fisml-placeholder-itag-face)

(defvar fisml-fontlock
  '(("^\\({\\)\\(FOOTNOTE\\)\\(||\\)\\(.+?\\)\\(||\\)\\(.+?\\)\\(}\\)"
     (1 fisml-tag-face) (2 fisml-footnote-face)
     (3 fisml-tag-face) (4 fisml-footnote-id-face)
     (5 fisml-tag-face) (6 fisml-footnote-mark-face)
     (7 fisml-tag-face))
    ("^\\({!!}{@}{\\)\\([^{]+?\\)\\(}.*$\\)"
     (1 fisml-comment-face)
     (2 fisml-comment-attr-face)
     (3 fisml-comment-face))
    ("^{!!}.*$" . fisml-comment-face)
    ("\\({__\\)\\([^]}{[:space:]]+?\\)\\(__}\\)"
     (1 fisml-placeholder-tag-face) (2 fisml-placeholder-itag-face)
     (3 fisml-placeholder-tag-face))
    ("\\({\\)\\(\\!\\?\\)\\(}\\)"
     (1 fisml-punct-tag-face) (2 fisml-punct-itag-face)
     (3 fisml-punct-tag-face))
    ("\\({\\)\\(`\\{2\\}\\)\\(}\\)"
     (1 fisml-quote-tag-face) (2 fisml-quote-itag-face)
     (3 fisml-quote-tag-face))
    ("\\({\\)\\('\\{2\\}\\)\\(}\\)"
     (1 fisml-quote-tag-face) (2 fisml-quote-itag-face)
     (3 fisml-quote-tag-face))
    ("\\({\\)\\([%^]\\{0,1\\}-\\{1,3\\}[$%]\\{0,2\\}\\)\\(}\\)"
     (1 fisml-punct-tag-face) (2 fisml-punct-itag-face)
     (3 fisml-punct-tag-face))
    ("\\({\\)\\(\"*---\"*\\)\\(}\\)"
     (1 fisml-punct-tag-face) (2 fisml-punct-itag-face)
     (3 fisml-punct-tag-face))    
    ("\\({\\)\\([%^]\\{0,1\\}\\.\\.\\.[.?,!$%]\\{0,2\\}\\)\\(}\\)"
     (1 fisml-punct-tag-face) (2 fisml-punct-itag-face)
     (3 fisml-punct-tag-face))
    ("{}" . fisml-empty-tag-face)
    ("{[^{]+?}" .  fisml-tag-face)))

(defun fisml-insert-bookmark ()
  (interactive)
  (insert "{!!}{BOOKMARK}"))
	
(defun fisml-insert-paired-tag (arg)
    (let ((pair-left (concat "{" arg ">}"))
	  (pair-right (concat "{<" arg "}")))
      (dfh-insert-pair pair-left pair-right)))

(defun fisml-insert-paired-tag-semi-custom (arg)
  (let (( custom (upcase (read-string "custom:"))))
    (fisml-insert-paired-tag (concat arg custom))))

(defun fisml-insert-paired-tag-custom (cap)
  (let* ((got (read-string "tag:"))
	 (tag (if cap (upcase got) (downcase got))))
    (fisml-insert-paired-tag tag)))

(defun fisml-insert-nonpaired-tag-custom (cap)
  (interactive)
  (if cap
      (insert (concat "{" (upcase (read-string "tag:")) "}"))
    (insert (concat "{" (downcase (read-string "tag:")) "}"))))

(defun fisml-insert-paired-tag-vertical (arg)
  (let ((pair-left (concat "{" arg ">}"))
	(pair-right (concat "{<" arg "}"))
        (psave))
    (newline 2)
    (insert pair-left)
    (newline 2)
    (setq psave (point))
    (newline 2)
    (insert pair-right)
    (goto-char psave)))

(defun fisml-insert-header ()
  (interactive)
  (insert "{!!}FISML{!!}"))

(defun fisml-insert-bookmark-plain ()
  (interactive)
  (insert "{!!}{BOOKMARK}"))

(defun fisml-insert-bookmark-interactive ()
  (interactive)
  (insert (concat "{!!}{BOOKMAK}{" (read-string "bookmark:") "}")))

;; chapters, sections, comments, bookmarks, etc
;; should always occupy their own line, so relocate
;; the point to ensure we don't re-find the one on the
;; current line twice.
(defun fisml--find ( dir targ )
  (save-match-data
    (if dir
	(progn
	  (let ((point-save (point)))
	    (end-of-line)
	    (if (search-forward-regexp targ  nil t 1)
		(recenter)
	      (goto-char point-save)
	      (message "fisml: nothing next"))))
      (let ((point-save (point)))
	(beginning-of-line)
	(if (search-backward-regexp targ nil t 1)
	    (recenter)
	  (goto-char point-save)
	  (message "fisml: nothing prior"))))))

(defun fisml-next-comment ()
  (interactive)
  (fisml--find t "{!!}"))

(defun fisml-prev-comment ()
  (interactive)
  (fisml--find nil "{!!}"))

(defun fisml-next-chapter ()
  (interactive)
  (fisml--find t "{CHAPTER}"))

(defun fisml-prev-chapter ()
  (interactive)
  (fisml--find nil "{CHAPTER}"))

(defun fisml-next-section ()
  (interactive)
  (fisml--find t "{SECTION}"))

(defun fisml-prev-section ()
  (interactive)
  (fisml--find nil "{SECTION}"))

(defun fisml-next-bookmark ()
  (interactive)
  (fisml--find t "{!!}{BOOKMARK}"))

(defun fisml-prev-bookmark ()
  (interactive)
  (fisml--find nil "{!!}{BOOKMARK}"))

(defun fisml-delete-backward-char ()
  (interactive)
  (save-match-data
    (cond ( (use-region-p)
	    (delete-region (region-beginning) (region-end)))
	  ( (and (= ?} (preceding-char))
		 (/= ?} (char-before (- (point) 1))))
            (let ((endp (point)))
	      (if (search-backward "{" (line-beginning-position) t 1)
		  (delete-region (point) endp)
		(delete-char -1 nil))))
	  (t
	   (delete-char -1 nil)))))

(defun fisml-backward-delete-tag ()
  (save-match-data
    (let ((end (point)))
      (if (< (skip-chars-backward "^{" (line-beginning-position)) 0)
	  (if (not (bolp))
	      ;; { must be previous char
	      (delete-region (- (point) 1) end)
	    ;;if at bolp, there was no opening brace on this line
	    (goto-char end)
	    (delete-char -1 nil))
	;; didn't skip at all, { directly before point
	(delete-char -1 nil)))))

;; doesn't get {}
(defun fisml-forward-delete-tag ()
  (save-match-data
    (let ((beg (point)))
      ;; skip starting {
      (forward-char)
      (if (> (skip-chars-forward "^{}" (line-end-position)) 0)
	  (if (not (eolp))
	      (cond ( (= ?{ (following-char))
			(goto-char beg)
			(delete-char 1 nil))
		    ( (= ?} (following-char))
		      (let ((end (+ (point) 1)))
			(goto-char beg)
			(delete-region beg end)))
		    ( t nil)))
	(goto-char beg)
	(delete-char 1 nil)))))

(defun fisml-delete-tag (dir)
  (save-excursion
    (let ( (beg nil) (end nil))
      (if (= dir -1)
	  (progn
	    (setq end (point))
	    (if (= 0 (skip-chars-backward "^{" (line-beginning-position)))
		(setq beg (point))))
	(setq beg (point))
	(if (= 0 (skip-chars-forward "^}" (line-end-position)))
	    (setq end (point))))
      (if (and beg end)
	  (delete-region beg end)
	(delete-char dir nil)))))

(defun fisml--possible-tag-before-p ()
  (if (not (bobp))
      (if (= ?} (preceding-char))
	  t
	nil)
    nil))

(defun fisml--possible-tag-after-p ()
  (if (not (eobp))
      (if (= ?{ (following-char))
	  t
	nil)
    nil))    


(defun fisml-delete-forward-char ()
  (interactive)
  (save-match-data
    (cond ( (use-region-p)
	    (delete-region (region-beginning) (region-end)))
	  ( (and (= ?{ (following-char))
		 (/= ?{ (char-after (+ (point) 1))))
            (let ((endp (point)))
	      (if (search-forward "}" (line-end-position) t 1)
		  (delete-region (point) endp)
		(delete-char 1 nil))))
	  (t
	   (delete-char 1 nil)))))

(defun whitespace-p (x) (= ?\s (char-syntax x)))

(defun fisml-delete-backward-word ()
  (interactive)
  (save-match-data
    (cond ( (use-region-p)
	    (delete-region (region-beginning) (region-end)))
	  ( (bolp)
	    (delete-char -1))
	  ( (whitespace-p (preceding-char))
	    (let ((endp (point)))
	      (skip-chars-backward "[:space:]")
	      (skip-chars-backward "^}[:space:]")
	      (delete-region (point) endp)))
          ;; checking for }} might be unnecessary now
          ;; since footnotes have been reworked
	  ( (and (= ?} (preceding-char))
		 (/= ?} (char-before (- (point) 1))))
	    (let ((endp (point)))
	      (if (search-backward "{" (line-beginning-position) t 1)
		  (delete-region (point) endp)
		(delete-char -1))))
	  ( (= ?} (preceding-char))
	    (delete-char -1))
	  ( t
	    (let ((endp (point)))
	      (if (skip-chars-backward "^{}[:space:]")
		  (delete-region (point) endp)
		(delete-char -1)))))))

(defun fisml-norm-backward-delete-word ()
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (let (( endp (point) ))
      (backward-word)
      (delete-region (point) endp))))

(defun fisml-insert-footnote-anchor ()
  (interactive)
  (insert "{FN@||.||.}"))

(defun fisml-insert-footnote-skel ()
  (interactive)
  (insert "{FOOTNOTE||.||.}"))

(defun fisml-insert-first-para ()
  (interactive)
  (insert "{1ST_PARA#}"))

(defun fisml-insert-section ()
  (interactive)
  (insert "{SECTION"))

(defun fisml-insert-chapter ()
  (interactive)
  (insert "{CHAPTER}"))

;; run fisml_lint.pl on buffer
(defun fisml-lint-buffer ()
  (interactive)
  (let ((temp-buffer-max-height (/ (window-body-height) 2))
        (bufname "**FISML LINT**"))
    (with-temp-buffer-window bufname nil nil
                             (temp-buffer-resize-mode)
                             (call-process-region (point-min)
                                                  (point-max)
                                                  "fisml_lint.pl"
                                                  nil
                                                  bufname))) )

(defun fisml-redash ()
  (interactive)
  (if (use-region-p)
      (progn
        (save-match-data
          (save-excursion
            (let ((BEG (region-beginning))
                  (END (region-end)))
              (deactivate-mark)
              (goto-char BEG)
              (while (search-forward-regexp " \\|-"  END t nil)
	        (replace-match " {-} "))))))
    (message "No region active")))

;; ignore individual tags
;; but allow checking of contents inside the body tag of footnotes
(defun flyspell-ignore-fisml-tag ()
  "Function used for `flyspell-generic-check-word-predicate' to ignore fisml tags"
  (let ((f (get-text-property (- (point) 1)  'face)))
    (not (memq f '(fisml-tag-face fisml-footnote-face
				  fisml-footnote-id-face
                                  fisml-comment-attr-face
				  fisml-footnote-mark-face)))))

(setq flyspell-generic-check-word-predicate 'flyspell-ignore-fisml-tag)
		
(defvar fisml-mode-map
  (let ((map (make-sparse-keymap)))
    ;;tab
    (define-key map (kbd "TAB")
      (lambda () (interactive) (message "no tabs")))
;;      (lambda () (interactive) (insert (decode-char 'ucs #x0009))))
    ;;italics
    (define-key map (kbd "C-c 7 i")
      (lambda () (interactive) (fisml-insert-paired-tag "i") ))
    ;;bold
    (define-key map (kbd "C-c 7 b")
      (lambda () (interactive) (fisml-insert-paired-tag "b") ))
    ;;align left
    (define-key map (kbd "C-c 7 l")
      (lambda () (interactive) (fisml-insert-paired-tag "ALIGN_L") ))
    ;;align right
    (define-key map (kbd "C-c 7 r")
      (lambda () (interactive) (fisml-insert-paired-tag "ALIGN_R") ))
    ;;align center
    (define-key map (kbd "C-c 7 c")
      (lambda () (interactive) (fisml-insert-paired-tag "CENTER") ))
    ;;normal text
    (define-key map (kbd "C-c 7 n")
      (lambda () (interactive) (fisml-insert-paired-tag "n") ))
    
    ;;groupings
    (define-key map (kbd "C-c 7 g")
      (lambda () (interactive) (fisml-insert-paired-tag-semi-custom "GROUP") ))
    
    ;;block quote
    (define-key map (kbd "C-c 7 f B")
      (lambda () (interactive) (fisml-insert-paired-tag "BLOCK") ))
    ;;superscript
    (define-key map (kbd "C-c 7 s")
      (lambda () (interactive) (fisml-insert-paired-tag "SUP") ))
    ;;small caps
    ;; it would be nice if SC also upcased region
    (define-key map (kbd "C-c 7 S")
      (lambda () (interactive) (fisml-insert-paired-tag "sc") ))
    ;;verbatim
    (define-key map (kbd "C-c 7 v")
      (lambda () (interactive) (fisml-insert-paired-tag "VERBATIM") ))

    ;;headings
    (define-key map (kbd "C-c 7 h")
      (lambda () (interactive) (fisml-insert-paired-tag-semi-custom "H") ))

    ;;vertical space
    (define-key map (kbd "C-c 7 V")
      (lambda () (interactive) (insert "{VSPACE}")))
    
    ;;strike through
    (define-key map (kbd "C-c 7 -")
      (lambda () (interactive) (fisml-insert-paired-tag "s") ))
    ;;underline
    (define-key map (kbd "C-c 7 u")
      (lambda () (interactive) (fisml-insert-paired-tag "u") ))
    
    ;;blank character
    (define-key map (kbd "C-c 7 B")
      (lambda () (interactive)(insert "{BLANK}")))
    
    ;;metadata
    (define-key map (kbd "C-c 7 H") 'fisml-insert-header)
    (define-key map (kbd "C-c 7 C")
      (lambda () (interactive)(insert "{!!}")))
    
    ;;dividers
    (define-key map (kbd "C-c 7 d s") 'fisml-insert-section)
    (define-key map (kbd "C-c 7 d c") 'fisml-insert-chapter)
    (define-key map (kbd "C-c 7 d n")
      (lambda () (interactive)(insert "{NEWPAGE}")))    

    ;;hard tab
    (define-key map (kbd "C-c 7 T")
      (lambda () (interactive)(insert "{HSPACE}")))
    ;;hard newline
    (define-key map (kbd "C-c 7 N")
      (lambda () (interactive)(insert "{NEWLINE}")))
    ;;keep together
    (define-key map (kbd "C-c 7 k")
      (lambda () (interactive)(insert "{//}")))

    ;;custom pair
    (define-key map (kbd "C-c 7 P") (lambda () (interactive)
				      (fisml-insert-paired-tag-custom t)))
    (define-key map (kbd "C-c 7 p") (lambda () (interactive)
				      (fisml-insert-paired-tag-custom nil)))

    ;;single tag
    (define-key map (kbd "C-c 7 T") (lambda () (interactive)
				      (fisml-insert-nonpaired-tag-custom t)))
    (define-key map (kbd "C-c 7 t") (lambda () (interactive)
				      (fisml-insert-nonpaired-tag-custom nil)))


    ;;backspace
    (define-key map (kbd "DEL") 'fisml-delete-backward-char)
    ;;delete
    (define-key map (kbd "C-d") 'fisml-delete-forward-char)

    (define-key map (kbd "M-DEL") 'fisml-delete-backward-word)

    (define-key map (kbd "M-C-<backspace>") 'fisml-norm-backward-delete-word)

    (define-key map (kbd "C-<backspace>")
      (lambda () (interactive) (delete-char -1 nil)))

    (define-key map (kbd "C-c 7 {")
      (lambda () (interactive) (insert "{BRACE_L}")))
    (define-key map (kbd "C-c 7 }")
      (lambda () (interactive) (insert "{BRACE_R}")))
    (define-key map (kbd "C-c 7 [")
      (lambda () (interactive)
	(dfh-insert-pair "{BRACE_L}" "{BRACE_R}")))

    ;;punctuation specials
    (define-key map (kbd "C-c 7 .")
      (lambda () (interactive) (insert "{...}")))
    (define-key map (kbd "C-c 7 -")
      (lambda () (interactive) (insert "{---}")))

    map)
  "Keymap for `fisml-mode'.")
     
(setq fisml-syntax-table
      (let (( table (make-syntax-table text-mode-syntax-table)))
	table ))

;;###autoload
(define-derived-mode fisml-mode text-mode "FISML"
  "A major mode for editing Free Indirect Semantic Markup Language files."
  :syntax-table fisml-syntax-table
  (setq font-lock-defaults '(fisml-fontlock)))

(provide 'fisml)

(add-to-list 'magic-mode-alist `("{!!}FISML{!!}" . fisml-mode))
