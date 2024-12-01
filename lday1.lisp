;; AoC 2020 Day 1

(defpackage :lday1
  (:use :common-lisp)
  (:export
   #:range
   #:combinations-from
   #:read-datas
   #:choose-part
   #:show-solution
   #:bsearch
   #:part1
   #:part2
   #:main))

(in-package :lday1)

(defun range (start end)
  "Returns the list of integer from start to end, inclusive"
  (loop for i from start to end
        collect i))

;; ;; functional version but replaced with mapcan
;; (defun concat-map (f ls)
;;   (apply #'append (mapcar f ls)))

;; ;; iterative version
;; (defun concat-map (f ls)
;;   (loop for x in ls
;;         append (funcall f x)))

(defun combinations-from (k vec)
  "Returns the list of combinations of k elements from vector vec"
  (let ((n (1- (length vec))))
    (labels ((build (start depth comb)
               (if (= k depth)
                   (mapcar
                    (lambda(i) (cons (elt vec i) comb))
                    (range start n))
                   ;; (concat-map
                   (mapcan
                    (lambda(i)
                      (build (1+ i) (1+ depth) (cons (elt vec i) comb)))
                    (range start n)))))
      (build 0 1 NIL))))

(defun read-datas ()
  "Returns a sorted vector filled with numbers from day1.txt"
  (let ((lines (with-open-file (stream "day1.txt")
                 (loop for line = (read-line stream nil)
                       while line collect (parse-integer line)))))
    (sort (coerce lines 'vector) #'< )))

;; return T if x is in arr
;; arr is a sorted vector
;; recursive version
;; (defun bsearch (x arr)
;;   (labels ((do-bsearch (inf sup)
;;              (if (>= inf sup)
;;                  (= (aref arr inf) x)
;;                  (let ((mid (floor (+ inf sup) 2)))
;;                    (if (< (aref arr mid) x)
;;                        (do-bsearch (1+ mid) sup)
;;                        (do-bsearch inf mid))))))
;;     (let ((sup0 (1- (length arr))))
;;       (if (>= (aref arr sup0) x)
;;         (do-bsearch 0 sup0)))))

;; iterative version
(defun bsearch (x arr)
  "Returns T if x in the sorted vector arr"
  (let ((sup0 (1- (length arr))))
    (if (>= (aref arr sup0) x)
        (loop
          for test = T   then (< (aref arr mid) x)
          for inf = 0    then (if test (1+ mid) inf)
          for sup = sup0 then (if test sup mid)
          for mid = (floor (+ inf sup) 2)
          when (>= inf sup)
            return (= (aref arr inf) x)))))

(defun part1 (combs)
  "Returns the pair which sum is 2020"
  (loop for comb in combs
        when (= 2020 (apply #'+ comb))
          return comb))

(defun part2 (numbers combs)
  "Returns the triplet which sum is 2020"
  (loop for comb in combs
        for n = (- 2020 (apply #'+ comb))
        when (bsearch n numbers)
          return (cons n comb)))


(defun show-solution (part sol)
  "Display a combination and its product"
  (format T "~A: numbers: ~A.  Product: ~d~%" part sol (apply #'* sol)))

(defun main ()
  "Main entry of the programm"
  (let* ((datas (read-datas))
         (combs (combinations-from 2 datas)))
    (progn
      (show-solution "Part1" (part1 combs))
      (show-solution "Part2" (part2 datas combs)))))
