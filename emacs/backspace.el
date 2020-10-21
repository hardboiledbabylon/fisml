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

;; combine fisml & afill
;; delete by fontlock property?
(require 'dfh-str)

(defun delete-backward--thing (thing)
  (let ((beg (point)))
    (skip-chars-backward thing)
    (delete-region beg (point))))

(defun delete-forward--thing (thing)
  (let ((beg (point)) (end nil))
    (skip-chars-forward thing)
    (setq end (point)) 
    (goto-char beg) ;; preserve point in case of undo
    (delete-region beg end)))

(defun delete-backward--to-and (char)
  (let ((beg nil) (end (point)))
    (skip-chars-backward (concat "^" (string char)))
    (if (= (preceding-char) char)
        (setq beg (- (point) 1))
      (setq beg (point)))
    (goto-char end) ;; preserve point in case of undo
    (delete-region beg end)))

(defun delete-forward--to-and (char)
  (let ((beg (point)) (end nil))
    (skip-chars-forward (concat "^" (string char)))
    (if (= (following-char) char)
        (setq end (+ (point) 1))
      (setq end (point)))
    (goto-char beg) ;; preserve point in case of undo
    (delete-region beg end)))

(defun delete-backward--str-or-char (str)
  (if (string= str (dfh-preceding-string (length str)))
      (delete-char (* -1 (length str)))
    (delete-char -1)))

(defun delete-forward--str-or-char (str)
  (if (string= str (dfh-following-string (length str)))
      (delete-char (* 1 (length str)))
    (delete-char 1)))
   
(defun bs-delete-backward-char ()
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (unless (bobp)
      (let ((pc (preceding-char)))
        (cond ( (= ?\n pc) (delete-backward--thing "\n"))
              ( (= ?} pc) (delete-backward--str-or-char "{}"))
              ( (= ?. pc) (delete-backward--thing "."))
              ( (= ?- pc) (delete-backward--thing "-"))
              ( (= ?` pc) (delete-backward--thing "`"))
              ( (= ?' pc) (delete-backward--thing "'"))
	      (t (delete-char -1)))) )))

(defun bs-delete-forward-char ()
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (unless (eobp)
      (let ((fc (following-char)))
        (cond (  (= ?\n fc) (delete-forward--thing "\n"))
              ( (= ?{ fc) (delete-forward--str-or-char "{}"))
              ( (= ?. fc) (delete-forward--thing "."))
              ( (= ?- fc) (delete-forward--thing "-"))
              ( (= ?` fc) (delete-forward--thing "`"))
              ( (= ?' fc) (delete-forward--thing "'"))
	    (t (delete-char 1)))) )))

;; acount for start quotes and paren
(setq bs--skipable "^ {}-")

(defun bs--backward-del-word ()
  (let ((end (point)))
    (skip-chars-backward bs--skipable (line-beginning-position))
    (delete-region (point) end)))

(defun bs--forward-del-word ()
  (let ((beg (point)) (end nil))
        (skip-chars-forward bs--skipable (line-end-position))
        (setq end (point))
        (goto-char beg)  ;; preserve point in case of undo
        (delete-region beg end)))

(defun bs-delete-whitespace-before ()
  (let ((end (point)))
    (skip-chars-backward " " (line-beginning-position))
    (delete-region (point) end)))

(defun bs-delete-whitespace-after ()
  (let ((beg (point)) (end nil))
    (skip-chars-forward " " (line-end-position))
    (setq end (point))
    (goto-char beg)  ;; preserve point in case of undo
    (delete-region beg end)))

(defconst space-c (decode-char 'ucs #x0020))
(defun bs-delete-backward-word ()
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (unless (bobp)
      (let ((pc (preceding-char)))
	(cond ( (= space-c pc)
                (bs-delete-whitespace-before)
		(bs-delete-backward-word))
              ( (= ?} pc) (delete-backward--to-and ?{))
              ( (= ?\n pc) (delete-backward--thing "\n"))
	      ( (= ?- pc) (bs-delete-backward-char)(bs-delete-backward-word))
	      ( t (bs--backward-del-word)))) )))

(defun bs-delete-forward-word ()
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (unless (eobp)
      (let ((pc (following-char)))
	(cond ( (= space-c pc)
                (bs-delete-whitespace-after)
		(bs-delete-forward-word))
              ( (= ?{ pc) (delete-forward--to-and ?}))
              ( (= ?\n pc) (delete-forward--thing "\n"))
	      ( (= ?- pc) (bs-delete-forward-char))
	      ( t (bs--forward-del-word)))) )))

(defvar backspace-keymap
  (let ((map (make-sparse-keymap)))
    ;;backspace
    (define-key map (kbd "DEL") 'bs-delete-backward-char)
    ;;delete
    (define-key map (kbd "C-d") 'bs-delete-forward-char)
    ;; delete forward word
    (define-key map (kbd "M-d") 'bs-delete-forward-word)
    ;; delete backward word
    (define-key map (kbd "M-<DEL>") 'bs-delete-backward-word)
    ;; special overrides to avoid reformatting behaviour
    ;; when necessary
    (define-key map (kbd "C-c <DEL>")
      (lambda()(interactive)(delete-char -1)))
    (define-key map (kbd "C-c C-d")
      (lambda()(interactive)(delete-char 1)))
    map)
  "Keymap for `backspace-mode'.")

(define-minor-mode backspace
  "A mode for controlling what backspace deletes."
  :lighter " BS"
  :keymap backspace-keymap)

(provide 'backspace)
