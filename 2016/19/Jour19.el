; run with emacs -Q --script

(require 'seq)
(setq input19 3017957)

(defun parse1 (l len)
  (let ((x l))
    (dotimes (_ (/ (1+ len) 2) x)
      (let ((e (car x))
	    (next (cadr x)))
	(setcdr e (+ (cdr e) (cdr next)))
	(setcdr x (cddr x))
	(setq x (cdr x))))))

(defun jour19 (input)
  (let ((len input)
	(l (seq-mapn #'cons (number-sequence 1 input) (make-list input 1))))
    (setcdr (nthcdr (1- input) l) l)
    (while (> len 1)
      (setq l (parse1 l len))
      (setq len (/ len 2)))
    (car l)))

;; (jour19 5)
(message "Part 1: %d" (car (jour19 input19)))
;; (1841611 . 3017957)


(defun jour19_2 (input)
  (let* ((len input)
	(l (seq-mapn #'cons (number-sequence 1 input) (make-list input 1)))
	(s (nthcdr (1- (/ len 2)) l)))
    (setcdr (last l) l)
    (while (> len 1)
      (let ((e (car l))
	    (next (cadr s)))
	(setcdr e (+ (cdr e) (cdr next)))
	(setcdr s (cddr s))
	(setq l (cdr l))
	(setq s (nthcdr (% len 2) s))
	(setq len (1- len))))
    (car l)))

;; (jour19_2 5)
(message "Part 2: %d" (car (jour19_2 input19)))
;; (1423634 . 3017957)