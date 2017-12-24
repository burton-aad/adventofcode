

;; circular list
(defun make-clist (elemts)
  (let ((r (copy-tree elemts)))
    (setcdr (last r) r)))

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

(defun sig (len lst)
  (let ((l (seq-subseq lst 0 len)))
    (concat (mapc 'number-to-string l))))

(defun u/max (len lst)
  (let ((m lst)
	(l (cdr lst)))
    (dotimes (x len m)
      (when (> (car l) (car m))
	(setq m l))
      (setq l (cdr l)))))

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


(setq input '(0 2 7 0))
(setq input6 '(0 5 10 0 11 14 13 4 11 8 8 7 1 4 12 11))

(jour6 input)
(4 5)

(jour6 input6)
(1695 7864)


(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f sec" (float-time (time-since time)))))

(byte-compile 'distribute)
(byte-compile 'jour6)
(measure-time
 (message (format "%S" (jour6 input6))))
"4.729974 sec"

(setq l (make-list 1000000 1))
(measure-time
 (seq-position l 2))

(measure-time
 (position 2 l))

(setq v (make-vector 1000000 1))
(measure-time
 (seq-position v 2))

(measure-time
 (position 2 v))
