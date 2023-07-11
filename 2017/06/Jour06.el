#!/bin/bash
":"; exec emacs --no-site-file --script "$0" -- "$@" # -*-emacs-lisp-*-

(require 'seq)

(defun distribute (lst m)
  (let* ((len (length lst))
         (val (nth m lst))
         (n (/ val len))
         (h (% val len))
         (l (nthcdr m lst))
         )
    ;; (message (format "m %d, val %d, l %S" m val l))
    (setcar l 0)
    (dotimes (i len lst)
      (setq l (if (cdr l) (cdr l) lst))
      (if (< i h)
          (setcar l (+ (car l) n 1))
        (setcar l (+ (car l) n))))))

(defun jour6 (lst)
  (let ((ref (cons (copy-tree lst) nil))
        rpos)
    ;; (message (format "lst %S" lst))
    (while (not rpos)
      (let ((m (seq-max lst)))
        (distribute lst (seq-position lst m))
        ;; (message (format "lst %S" lst))
        (setq rpos (seq-position ref lst))
        (push (copy-tree lst) ref)
        ;; (message (format "ref %S" ref))
        ))
    `(,(+ 1 rpos) ,(- (length ref) 1))))

;; (byte-compile 'distribute)
;; (byte-compile 'jour6)

;; (setq input '(0 2 7 0))
;; (setq input6 '(0 5 10 0 11 14 13 4 11 8 8 7 1 4 12 11))

;; (jour6 input)
;; (4 5)

;; (jour6 input6)
;; (1695 7864)

(if (< (length argv) 2)
    (message "Usage: Jour06.el <input file>")
  (let* ((input-s (with-temp-buffer
                   (insert-file-contents-literally (cadr argv))
                   (buffer-substring (point-min) (point-max))))
         (input (mapcar #'string-to-number (split-string input-s "\t" t "\n")))
         (rep (jour6 input)))
    (message "Part 1: %d" (cadr rep))
    (message "Part 2: %d" (car rep))
  ))
