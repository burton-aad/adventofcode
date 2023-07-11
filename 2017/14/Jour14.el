
(load (concat (file-name-directory (or load-file-name buffer-file-name)) "../10/Jour10.el") nil t)
(setq input14 "ljoxqyyw")

(defun int-to-binary-string (i &optional size)
  "convert an integer into it's binary representation in string format"
  (let ((res ""))
    (while (not (= i 0))
      (setq res (concat (number-to-string (logand i 1)) res))
      (setq i (lsh i -1)))
    (if (and size (< (length res) size))
        (concat (make-string (- size (length res)) ?0) res)
      (if (string= res "") "0" res))))

(defun count-chars (str chr)
  (1- (length (split-string str chr))))

(defun jour14 (input)
  (let (l)
    (dotimes (i 128 (apply '+ l))
      (push (apply '+
                   (mapcar
                    (lambda (x) (count-chars (int-to-binary-string x 8) "1"))
                    (knot-hash (format "%s-%d" input i))))
            l))))

;; (princ (format "flqrgnkx -> %d\n" (jour14 "flqrgnkx")))
(princ (format "Part 1: %d\n" (jour14 input14)))


(defun jour14_2 (input)
  (let* ((n 128)
         (l (make-vector n nil))
         (cnt 0))
    (dotimes (i n l)
      (aset l i (mapconcat #'identity
                           (mapcar
                            (lambda (x) (int-to-binary-string x 8))
                            (knot-hash (format "%s-%d" input i)))
                           "")))
    (dotimes (i n cnt)
      (dotimes (j n)
        (when (equal (aref (aref l i) j) ?1)
          (setq cnt (1+ cnt))
          (let ((f `(,(cons i j))))
            (while f
              (let ((i (caar f))
                    (j (cdr (pop f))))
                (when (equal (aref (aref l i) j) ?1)
                  (aset (aref l i) j ?0)
                  (if (> i 0) (push (cons (1- i) j) f))
                  (if (> j 0) (push (cons i (1- j)) f))
                  (if (< i (1- n)) (push (cons (1+ i) j) f))
                  (if (< j (1- n)) (push (cons i (1+ j)) f)))
    ))))))))

;; (princ (format "flqrgnkx -> %d\n" (jour14_2 "flqrgnkx")))
(princ (format "Part 2: %d\n" (jour14_2 input14)))
