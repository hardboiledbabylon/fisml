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

;; non-interactive version of functions similair
;; to regexp-replace & replace-string

(defun dfh-replace-regexp (SRC REP BEG END)
  (save-match-data
    (save-excursion
      (goto-char BEG)
      (while (re-search-forward SRC END t nil)
	(replace-match REP)))))

(defun dfh-replace-string (SRC REP BEG END)
    (save-match-data
    (save-excursion
      (goto-char BEG)
      (while (search-forward SRC END t nil)
	(replace-match REP)))))

(provide 'dfh-regex)