#!/bin/bash
":"; exec emacs --no-site-file --script "$0" -- "$@" # -*-emacs-lisp-*-

(require 'cl-lib)

(defun jour1 (file)
  (let* ((input-s (with-temp-buffer
                    (insert-file-contents-literally file)
                    (buffer-substring (point-min) (point-max))))
         (flr 0)
         (i 0)
         (l (length input-s)))
    (while (and (>= flr 0) (< i l))
      (setq flr (if (equal (aref input-s i) ?\()
                    (1+ flr)
                  (1- flr)))
      (setq i (1+ i)))
    (princ (format "%d - %d\n" (- (cl-count ?\( input-s) (cl-count ?\) input-s)) i))))

(if (>= (length argv) 2)
    (jour1 (cadr argv))
  (jour1 "input"))

