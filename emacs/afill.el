;; Copyright © 2019 by D. F. Hall <authorfunction@hardboiledbabylon.com>

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

(require 'dfh-regex)

;; sometimes you still have to use M-q to clean up
;; even with this

;; also trashes undo to pieces

;; Someone once wrote a mode called 'Maniac Mode'[1] which fulfilled a
;; somewhat similar function, and it may prove that such a name might
;; be the appropriate appellation for a mode such as this, in general
;; terms.
;;
;; [1]https://www.emacswiki.org/emacs/ManiacMode

(require 'dfh-str)

(defun afill-post-fill-fixup-hook ()
  nil)

(defvar afill--disable-wrap-toggle nil)
(defvar afill-comment-prefix nil)
(defvar afill-disable-flyspell nil)

(defun in-paragraph-p ()
  (let ( (bc (preceding-char)) (ac (following-char)) )
    (if (and bc ac)
	(if (and (= bc ?\n) (= ac ?\n))
	    nil
	  t)
      t)))

(defun para--end ()
  (save-match-data
    (if (search-forward "\n\n" nil t 1)
	(skip-chars-backward "\n")
      (goto-char (point-max)))))

(defun para--beg ()
  (save-match-data
    (if (search-backward "\n\n" nil t 1)
	(skip-chars-forward "\n")
      (goto-char (point-min)))))

(defun paragraph-end ()
  (interactive)
  (if (in-paragraph-p)
      (para--end)))

(defun paragraph-beginning ()
  (interactive)
  (if (in-paragraph-p)
      (para--beg)))

;; if not in a paragraph, it won't matter
;; nothing will get formatted
;; so silently pass through
(defun paragraph-end-position ()
  (save-excursion
    (if (in-paragraph-p)
	(progn
	  (para--end)
	  (point))
      nil)))

(defun paragraph-beginning-position ()
  (save-excursion
    (if (in-paragraph-p)
	(progn
	  (para--beg)
	  (point))
      nil)))

(defun paragraph--next ()
  (save-match-data
    (if (not (in-paragraph-p))
	(skip-chars-forward "\n")
      (if (not (search-forward "\n\n" nil t 1))
	  nil
	t))))

(defun paragraph--prev ()
  (save-match-data
    (if (not (in-paragraph-p))
	(progn
	  (skip-chars-backward "\n")
	  (paragraph-beginning)
	  t)
      (if (search-backward "\n\n" nil t 1)
	  (progn
	    (paragraph-beginning)
	    t)
	nil))))

(defun paragraph-next ()
  (interactive)
  (if (not (paragraph--next))
      (message "last paragraph")))

(defun paragraph-prev ()
  (interactive)
  (if (not (paragraph--prev))
      (message "first paragraph")))

(defun line-endp ()
  (if (= (point) (line-end-position))
      t
    nil))

(defun line-begp ()
  (if (= (point) (line-beginning-position))
      t
    nil))

(defun char-before-before (p)
  (char-before (- (point) 1)))

(defun char-after-after (p)
  (char-after (+ (point) 1)))

;; but if at lbeg & one newline after bobp
;; this should be para

;; character before before is newline?
(defun cbbnp ()
  (let ((c1 (char-before (point)))
	(c2 (char-before-before (point))))
    
    (if (and c1 c2)
	(if (and (= c1 ?\n) (= c2 ?\n))
	    t
	  nil)
      nil)))

;; character after after is newline?
;; should \n\n at eob count the same as single \n ?
(defun caanp ()
  (let ((c1 (char-after (point)))
	(c2 (char-after-after (point))))

    (if (and c1 c2)
	(if (and (= c1 ?\n) (= c2 ?\n))
	    t
	  nil)
      nil)))

(defun blank-linep ()
  (if (= (line-beginning-position) (line-end-position))
      t
    nil))

(defun nonblank-linep ()
  (if (< (line-beginning-position) (line-end-position))
      t
    nil))

(defun eopp ()
  (cond ( (and (eobp) (bobp)) t)
	( (and (eobp) (blank-linep)) nil)
	( (eobp) t)
	( (caanp) t)
	(t nil)))

(defun bopp ()
  (cond	( (and (eobp) (bobp)) t)
	( (bobp) t)
	( (cbbnp) t)
	(t nil)))
      
(setq sentence-end-double-space nil)
;; first check what we're inserting before break
;; otherwise things get swallowed
;; fills column, removes space, then insert space, so loops forever
;; if wrap not disabled till end of line
(defconst space-c (decode-char 'ucs #x0020))
(defconst sentence-c (decode-char 'ucs #x2592))
(defvar afill-sentence-mark "%%")

;; if line starts with comment, treat as if disable-toggle
(defun afill-space-or-fill-sent ()
  (interactive)

  ;; this will also not insert a space-c at start of new empty
  ;; para that's been created after <carriage> at end of para
  (when (in-paragraph-p)

    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))

    (let ( (lend (line-end-position))
	   (lbeg (line-beginning-position))
	   (col (current-column)) )

      (unless (and afill-comment-prefix
                   (string=
                    (dfh-string-after lbeg (length afill-comment-prefix))
                    afill-comment-prefix))
        (if (> col fill-column)
	    (unless (= (preceding-char) space-c)
	      (afill--fill-region lbeg lend))))

      (cond ( (bopp)
	      (message "why need space or sentence at bop?") )
	    ( (= (point) lbeg)
	      (insert afill-sentence-mark) )
	    ( (= (preceding-char) space-c)
	      (insert afill-sentence-mark) )
	    ( (string= (dfh-preceding-string (length afill-sentence-mark))
		       afill-sentence-mark)
	      (message "sentence already started") )
	    ( t
	      (insert " "))))) )

;;fill-region with to-eop at start of line
;;will skip back to previous paragraph and do nothing
;;to current one
(defun afill-toggle-disable-wrap ()
  (interactive)
  (if afill--disable-wrap-toggle
      (progn
	(setq afill--disable-wrap-toggle nil)
	(message "Automatic hardwrap re-enabled"))
    (setq afill--disable-wrap-toggle t)
    (message "Automatic hardwrap temporarily disable until next enter")))

(defun afill--newline ()
  (newline 2))

;; better name recycle?
(defun afill--flush-newlines ()
  (let ((beg (point))
	(save (point)))
    (skip-chars-forward "\n")
    (delete-region beg (point))	
    (unless (eobp)
      (afill--newline)
      (goto-char save))))

;;eopp skip forward \n & delete; unless at eobp -> add 2 \n then 2 more

;; on enter, check if long-line-footnote mode has been enable and unset it
(defun afill-carriage-return ()
  (interactive)
  (cond ( (not (in-paragraph-p))
          (if (string= "\n\n" (dfh-preceding-string 2))
              (message "already in new empty paragraph")
	    (message "really should open new paragraph from end of paragraph")))
	( (and (bobp) (blank-linep))
	  (message "why open paragraph when nothing is here?"))
	( (bobp)
	  (afill-fill-paragraph)
	  (afill--newline)
	  (goto-char (point-min)))
        ( (bopp)
          (afill--newline)
          (backward-char 2))
	( (and (eopp) afill--disable-wrap-toggle)
	  (afill-toggle-disable-wrap)
	  (afill--newline))
	( (eopp)
	  (afill-fill-paragraph)
	  (afill--flush-newlines)
	  (afill--newline))
	( (bolp)
	  (newline))

	;; if in para, prev & next para guaranteed on split
	( (and (not (eolp)) (= space-c (preceding-char)))
	  (afill--newline)
	  (paragraph--prev)
	  (afill-fill-paragraph)
	  (paragraph--next)
	  (afill-fill-paragraph) )
	( (eolp)
	  ;; preceding char guaranteed by bolp check earlier
	  (if (= space-c (preceding-char))
	      (delete-char -1))
	  (afill--newline)
	  (delete-char 1 nil) ;; remove forward newline
	  (afill-fill-paragraph) )
	(t (afill--newline)
	   (afill-fill-paragraph))))

(defun afill--full-paraQ ()
  (interactive)
  (let ((beg (paragraph-beginning-position)) (end (paragraph-end-position)))
    (save-excursion
      (afill--unfill-region beg end)
      (goto-char beg)
      (while (and (<= (point) end) (skip-chars-forward "^ " end))
        (let ((col (current-column)))
          (when (>= col fill-column)
            (when (> col fill-column)
              (skip-chars-backward "^ " beg)
              (skip-chars-backward " " beg))
            (newline)
            (let ((start (point)))
              (skip-chars-forward " ")
              (delete-region start (point)))))
        (skip-chars-forward " " end)))))    

(defun afill--fill-region (beg end)
  ;; removes spaces at beginning of any paragraph
  ;; sub-line to avoid fill-region interpretting that
  ;; as a fill-indent
  (dfh-replace-regexp (concat "^ +") "" beg end)
  (fill-region beg end nil nil nil))

;; shouldn't need (eopp) here
(defun afill-fill-to-para-end ()
  (interactive)
  (save-excursion
    (if (and (not (eobp)) (in-paragraph-p))
	(afill--fill-region (point) (paragraph-end-position)))))

(defun afill-fill-bol-to-para-end ()
  (interactive)
  (save-excursion
    (if (in-paragraph-p)
	(afill--fill-region
	 (line-beginning-position)
	 (paragraph-end-position)))))

;; check for in paragraph should be
;; done by function that calls this
;; func so don't duplicate here
(defun paragraph-comment-block-p ()
  (let ((pbeg (paragraph-beginning-position)))
    (if (and afill-comment-prefix
             (string= afill-comment-prefix
                      (dfh-string-after pbeg (length afill-comment-prefix))))
        t
      nil)))
             

;;unless para = 1 line & < line_max
;; fill
;; afill-fill-all
(defun afill-fill-all ()
  (interactive)
  ;; check flyspell otherwise things get _really_
  ;; slow after this
  ;; and make sure hl-line-mode comes out clean
  ;; otherwise the highlight might be left momentarily
  ;; crossing multiple lines
  ;; afill-fill-paragraph shouldn't have to worry about
  ;; hl-line-mode due to the narrower scope in which it
  ;; works ;; unlike with flyspell-mode
  (let ((fly (and (boundp 'flyspell-mode) flyspell-mode afill-disable-flyspell))
        (highl (and (boundp 'hl-line-mode) hl-line-mode)))
    (when fly
      (message "temporarily disabling flypsell")
      (flyspell-mode 0))
    (when highl
      (hl-line-mode 0))
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward "\n")
      (delete-region (point-min) (point))
      (when (nonblank-linep)
        (afill-fill-paragraph)
        (while (paragraph--next)
          (afill-fill-paragraph))))
    (when fly
      (message "flyspell re-enabled")
      (flyspell-mode 1))
    (when highl
      (hl-line-mode 1))))

;; be careful with this
;; it doesn't always act exactly like pure fill-region
;; when starting selection from anywhere not at beginning
;; of line or paragraph

(defun afill--clear-para-beginning (beg)
  (let ((CB t)
        (CS t))
    (while (or CB CS)
      (setq CB (= (char-after beg) space-c))
      (setq CS (string= (dfh-string-after beg 2) afill-sentence-mark))
      (when CB
        (delete-region beg (+ beg 1)))
      (when CS
        (delete-region beg (+ beg 2))))))

;; fill all paras forward
;; fix any hanging indents so
;; M-q doesn't mistakenly pick them up
(defun afill-fill-paragraph ()
  (interactive)
  (save-excursion
    (when (in-paragraph-p)
      (unless (paragraph-comment-block-p)
	(let ((pbeg (paragraph-beginning-position))
	      (pend (paragraph-end-position)) )
	  (when (> pend pbeg)
            ;; 80x24/2
            ;; regions longer than this disable flyspell for
            ;; performance
            (let ((fly (and (boundp 'flyspell-mode)
                            flyspell-mode
                            afill-disable-flyspell
                            (> (- pend pbeg) 960))))
              (when fly
                (flyspell-mode 0))
              (afill--clear-para-beginning pbeg)
              ;; must update end-position because
              ;; that could have changed
	      (afill--fill-region pbeg
		                  (paragraph-end-position))
              (when fly
                (flyspell-mode 1))))))
          (goto-char (paragraph-end-position))
          (afill--flush-newlines) )))

(defun afill--unfill-region (beg end)
  (save-excursion
    (unless (<= end beg)
      (goto-char beg)
      (while (< (point) end)
        (skip-chars-forward "^\n" end)
        (unless (= (point) end)
          (delete-char 1)
          (insert " "))))))

(defun afill--unfill-region-true (beg end)
  (save-excursion
    (let ((rend (- end 1)))
      (unless (<= rend beg)
        (goto-char beg)
        (while (< (point) rend)
          (skip-chars-forward "^\n" rend)
          (unless (= (point) rend)
            (delete-char 1)
            (insert " ")
            (skip-chars-forward "\n" rend)))))))

(defun afill--unfill-para (beg end)
  (save-excursion
    (let ((rend (- end 1)))
      (unless (<= rend beg)
        (goto-char beg)
        (while (< (point) rend)
          (skip-chars-forward "^\n" rend)
          (unless (= (point) rend)
            (delete-char 1)
            (insert " ")))))))

(defun afill-unfill-to-para-end ()
  (interactive)
  (if (and (not (eobp)) (in-paragraph-p))
      (afill--unfill-region (point) (paragraph-end-position))))

(defun afill-unfill-paragraph ()
  (interactive)
  (if (in-paragraph-p)
      (afill--unfill-region (paragraph-beginning-position)
			    (paragraph-end-position))))

(defun afill-clean-trailing-whitespace ()
  (interactive)
  (save-excursion
    (save-match-data
      (dfh-replace-regexp " +$" "" (point-min) (point-max)))))

(defun afill-buffer-has-trailing-whitspace-p ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (if (re-search-forward " +$" nil t 1)
	  (message "buffer contains trailing whitespace")))))

(defun afill-next-trailing-whitespace ()
  (interactive)
  (save-match-data
    (re-search-forward " +$" nil t 1)))

(defun afill-buffer-has-extra-newlines-p ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (if (re-search-forward "\n\n\n+" nil t 1)
	  (message "buffer contains extra newlines")))))

(defun afill--kill-para ()    
  (let ( (pbeg (paragraph-beginning-position))
	 (pend (paragraph-end-position)) )

    (goto-char pbeg)
    (kill-region pbeg pend)))

(defun afill-kill-paragraph-replace ()
  (interactive)
  (when (in-paragraph-p)
    (afill--kill-para)))

(defun afill-kill-paragraph ()
  (interactive)
  
  (when (in-paragraph-p)
    
    (afill--kill-para)

    (if (= (point) (point-max))
	(afill-backward-delete-newline)
      (afill-forward-delete-newline))))

(defun afill-next-extra-newline ()
  (interactive)
  (save-match-data
    (if (re-search-forward "\n\n\n+" nil t 1)
	(goto-char (+ (match-beginning 0) 2)))))

;; will always have to skip at least one newline
(defun afill-backward-delete-newline ()
  (save-match-data
    (let ((end (point)))
      (skip-chars-backward "\n")
      (delete-region (point) end))))

;; will always have to skip at least one newline
;; goto beg to make undo work correctly
(defun afill-forward-delete-newline ()
  (save-match-data
    (let ((beg (point)))
      (skip-chars-forward "\n")
      (let ((end (point)))
	(goto-char beg)
	(delete-region beg end)))))

;; C-c K -> kill para but leave room to replace  
(defvar afill-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'afill-space-or-fill-sent)
    (define-key map (kbd "<RET>") 'afill-carriage-return)
    (define-key map (kbd "C-c ,") 'afill-toggle-disable-wrap)
    (define-key map (kbd "C-c .") 'afill-fill-paragraph)
    (define-key map (kbd "C-c !") 'afill-unfill-paragraph)
    (define-key map (kbd "<M-RET>") 'newline)
;;  (define-key map (kbd "s-]") 'paragraph-next)
;;  (define-key map (kbd "s-[") 'paragraph-prev)
    (define-key map (kbd "C-c [") 'paragraph-beginning)
    (define-key map (kbd "C-c ]") 'paragraph-end)
    ;; (define-key map (kbd "C-c }") 'paragraph-next)
    ;; (define-key map (kbd "C-c {") 'paragraph-prev)
    ;; (define-key map (kbd "C-c k") 'afill-kill-paragraph)
    map)
  "Keymap for `afill-mode'.")

(define-minor-mode afill
  "A mode for autfilling paragraphs."
  :lighter " afill"
  :keymap afill-keymap)

(provide 'afill)
