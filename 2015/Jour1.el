#!/bin/bash
":"; exec emacs --no-site-file --script "$0" -- "$@" # -*-emacs-lisp-*-

(defun jour1 (file)
  (let ((input-s (with-temp-buffer
                   (insert-file-contents-literally file)
                   (buffer-substring (point-min) (point-max))))
	(flr 0))
    (let ((i 0)
	  (l (length input-s)))
      (while (and (>= flr 0) (< i l))
	(setq flr (if (equal (aref input-s i) ?\()
		      (1+ flr)
		    (1- flr)))
	(setq i (1+ i)))
      (princ (format "%d - %d\n" (- (count ?\( input-s) (count ?\) input-s)) i)))))

(jour1 "input1")
"280 - 1797"

(when (>= (length argv) 2)
  (message (jour1 (cadr argv))))

