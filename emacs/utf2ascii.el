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

;;; utf2ascii.el --- minor mode for GNU Emacs

;; This should be set before loading this
;; via require if it's needed.
;; Can be nil, "mono", or "color".
(defvar u2a-real-term nil)

(defgroup utf2ascii nil
  "Minor mode for replacing display table utf8 characters with alternative ascii representations."
  :group 'editing)

;;;###autoload
(defcustom utf2ascii-mode nil
  "Toggle utf2ascii-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `utf2ascii-mode'."
  :set 'custom-set-minor-mode
  :initialize 'custom-initialize-default
  :version "1.1"
  :type 'boolean
  :group 'utf2ascii
  :require 'utf2ascii)

(defface u2a-face
  '((t (:foreground "dark red")))
  "Face for `u2a-face`."
  :group 'utf2ascii)

(defface u2a-face-b
  '((t (:foreground "blue")))
  "Face for `u2a-face B`."
  :group 'utf2ascii)

;; true terminal (or VGA)
;; monochrome
(defface u2a-face-mono
  '((t (:foreground "black" :background "gray")))
  "Face for `u2a-face-mono`."
  :group 'utf2ascii)

;; true terminal (or VGA)
;; color
(defface u2a-face-color
  '((t (:foreground "bright red")))
  "Face for `u2a-face-color`."
  :group 'utf2ascii)

(if u2a-real-term
    (cond ( (string= "mono" u2a-real-term)
	    (setq u2a-face-term 'u2a-face-mono) )
	  ( (string= "color" u2a-real-term)
	    (setq u2a-face-term 'u2a-face-color))
	  ( t
	    (setq u2a-face-term nil)) )
  (setq u2a-face-term nil))

(setq u2a_emdash (decode-char 'ucs #x2014))
(setq u2a_endash (decode-char 'ucs #x2013))
(setq u2a_ellipsis (decode-char 'ucs #x2026))
(setq u2a_half_shade (decode-char 'ucs #x2592))

(setq u2a_left_single_quote (decode-char 'ucs #x2018))
(setq u2a_right_single_quote (decode-char 'ucs #x2019))
(setq u2a_left_double_quote (decode-char 'ucs #x201C))
(setq u2a_right_double_quote (decode-char 'ucs #x201D))
(setq u2a_broken_pipe (decode-char 'ucs #x00A6))

;; utf8 capable terminal or graphical emacs
;; where we just want to fix-up monospace
;; characters
(setq u2a-char-list-full
      (list
       (list u2a_emdash "---" 'u2a-face)
       (list u2a_endash "--" 'u2a-face-b)
       (list u2a_ellipsis "..." 'u2a-face) ))

;; faces are variables here, so don't quote
(setq u2a-char-list-term
      (list
       (list u2a_emdash "---" u2a-face-term)
       (list u2a_endash "--" u2a-face-term)
       (list u2a_ellipsis "..." u2a-face-term)
       (list u2a_half_shade "%" u2a-face-term)
       (list u2a_left_single_quote "`" u2a-face-term)
       (list u2a_right_single_quote "'" u2a-face-term) 
       (list u2a_left_double_quote "``" u2a-face-term)
       (list u2a_right_double_quote "''" u2a-face-term)
       (list u2a_broken_pipe "|" u2a-face-term) ))

(if u2a-real-term
    (setq u2a-char-list u2a-char-list-term)
  (setq u2a-char-list u2a-char-list-full))

;;vconcat makes the string multiple objects
;;so they are processed in sequence
(defun utf2ascii-display-table ()
  (interactive)
  (when (not buffer-display-table)
    (setq buffer-display-table (make-display-table)))
  (dolist (list-x u2a-char-list)
    (aset buffer-display-table (car list-x) 
	  (vconcat
	   (mapcar (lambda (x)
		     (make-glyph-code x (caddr list-x)))
		   (vconcat (cadr list-x)))))))

;;;###autoload
(define-minor-mode utf2ascii-mode
  "With no argument, this command toggles Smart Quotes mode.
With a prefix argument ARG, turn Smart Quotes minor mode on if ARG
is positive, otherwise turn it off."
  :lighter (:eval " u>a")
  :global t
  :init-value t
  :keymap   '()
  (utf2ascii-display-table))

(define-globalized-minor-mode global-utf2ascii-mode
  utf2ascii-mode (lambda () (utf2ascii-mode 1)))

;;;###autoload
(defun turn-on-utf2ascii ()
  "Unconditionally turn on utf2ascii mode."
  (utf2ascii-mode 1))

;;;###autoload
(defun turn-off-utf2ascii ()
  "Unconditionally turn off utf2ascii mode."
  (utf2ascii-mode -1))

(provide 'utf2ascii)

;;; utf2ascii.el ends here
