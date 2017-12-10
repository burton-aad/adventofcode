#!/bin/bash
":"; exec emacs --no-site-file --script "$0" -- "$@" # -*-emacs-lisp-*-

(defun loop10 (eltnum input round)
  (let* ((l (number-sequence 0 (1- eltnum)))
         (d (last l))
         (skip 0)
         (m 0))
    (setcdr d l)
    (dotimes (_ round (nthcdr (- eltnum (% m eltnum)) l))
      (dolist (i input)
        (if (> i eltnum)
            (message (format "Invalid length %d" i)))
        (cond
         ((= i eltnum)
            (setcdr d nil)
            (nreverse l)
            (setcdr l d))
         ((= i 0)
            (setq l d))
         (t
            (let* ((s (nthcdr i d))
                   (n (cdr s)))
              (setcdr s nil)
              (setcdr d (nreverse l))
              (setcdr l n))))
        (setq d (nthcdr skip l))
        (setq l (cdr d))
        (setq m (+ m i skip))
        ;; (message (format "in %d -> l %S (skip %d, m %d)" i l skip m))
        (setq skip (1+ skip))))))

(defun jour10 (n file)
  (let ((input-s (with-temp-buffer
                   (insert-file-contents-literally file)
                   (buffer-substring (point-min) (point-max)))))
    ;; partie 1
    (let* ((input (mapcar #'string-to-number (split-string input-s "," t "\n")))
           (l (loop10 n input 1)))
      (message (format "rep 1 -> %d" (* (car l) (cadr l)))))

    ;; partie 2
    (let* ((suffix '(17 31 73 47 23))
           (input (append (mapcar #'string-to-char (split-string input-s "" t "\n*")) suffix))
           (l (loop10 n input 64))
           dense)
      (mapconcat #'identity
                 (mapcar (lambda (x) (format "%02x" x))
                         (dolist (i (number-sequence 0 (1- n) 16) (nreverse dense))
                           (push (apply 'logxor (seq-subseq l i (+ 16 i))) dense)))
                 ""))))


;; (jour10 256 "input10")
;; "1c46642b6f2bc21db2a2149d0aeeae5d"


(if (< (length argv) 2)
    (message "Usage: Jour10.el <input file>")
  (message (jour10 256 (cadr argv))))

