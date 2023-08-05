#!emacs --script

(require 'seq)

(defun makeCircleList (start)
  (let ((circle (cons start ())))
    (nconc circle circle)))

(defun addToCircleList (lst elem)
  (let ((tail (cdr lst))
        (e (list elem)))
    (setcdr e tail)
    (setcdr lst e)))

(defun smax (seq)
  (seq-reduce #'max seq (elt seq 0)))

(defun jour09 (last num_players)
  (let ((i 0)
        (circle (makeCircleList 0))
        (players (make-vector num_players 0))
        (MRule 23))
    (while (< i last)
      (setq i (1+ i))
      (if (= (% i MRule) 0)
          (let ((p (% (1- i) num_players))
                (circ-elem-num (- i (* 2 (1- (/ i MRule))))))
            (setq circle (nthcdr (- circ-elem-num 1 7 1) circle)) ;; 1 decalage + 7 anti horaire + 1 pour avoir le precedent
            (aset players p (+ (elt players p) i (cadr circle)))
            (setcdr circle (cddr circle)))
        (addToCircleList circle i))
      (setq circle (cddr circle)))
    ;; (print circle)
    players))

;; (print (jour09 25 9))
;; (print (jour09 1618 10))
;; (print (jour09 7999 13))
;; (print (jour09 1104 17))
;; (print (jour09 6111 21))

;; input : 458 players; last marble is worth 72019 points
(let* ((last 72019)
       (players 458)
       (tmp (jour09 last players))
       (p1 (smax tmp))
       (p2 (seq-position tmp p1)))
  (princ (format "part 1: %S\n" p1)))
;; trop long...
;; (princ (format "part 2: %S\n" (elt (jour09 (* 100 last) players) p2))))
