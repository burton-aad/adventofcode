
(defun filtre-doublon (inlist)
  (let ((ref (car (last inlist)))
        (l nil))
    (dolist (v inlist l)
      (when (= ref v)
        (push v l))
      (setq ref v))))

(defun filtre-demitour (inlist)
  (let* ((s (length inlist))
         (ref (nthcdr (/ s 2) inlist))
         (l nil))
    (dolist (v inlist l)
      (when (= v (car ref))
        (push v l))
      (setq ref (cdr ref))
      (when (null ref)
        (setq ref inlist)))))

(defun jour1 (func input)
  (let ((l (mapcar 'string-to-number (split-string input "" t "\n*"))))
    (apply '+ (funcall func l))))


(princ
 (format "Part 1: %d\n"
         (jour1 'filtre-doublon
                (with-temp-buffer
                  (insert-file-contents-literally "input")
                  (buffer-substring (point-min) (point-max))))))
;; 1102

(princ
 (format "Part 2: %d\n"
         (jour1 'filtre-demitour
                (with-temp-buffer
                  (insert-file-contents-literally "input")
                  (buffer-substring (point-min) (point-max))))))
;; 1076
