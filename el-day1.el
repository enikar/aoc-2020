;; -*- lexical-binding: t -*-
;; Solution of The first day of advent of code 2020 in emacs lisp.
(require 'cl-lib)

(defun aoc-d1-range (start end)
  "Returns the list of integer from start to end, inclusive"
  (cl-loop for i from start to end
           collect i))

(defun aoc-d1-concat-map (f ls)
  (apply #'append (mapcar f ls)))

;; (defun concat-map (f ls)
;;   (cl-loop for x in ls
;;            append (funcall f x)))

(defun aoc-d1-combinations-from (k vec)
  "Returns the list of combinations of k elements from vector vec"
  (let ((n (1- (length vec))))
    (cl-labels ((build (start depth comb)
                  (if (= k depth)
                      (mapcar
                       (lambda (i) (cons (aref vec i) comb))
                       (aoc-d1-range start n))
                    (aoc-d1-concat-map
                     (lambda(i)
                       (build (1+ i) (1+ depth) (cons (aref vec i) comb)))
                     (aoc-d1-range start n)))))
      (build 0 1 nil))))

(defun aoc-d1-bsearch (x arr)
  "Returns t if x in the sorted vector arr"
  (let ((sup0 (1- (length arr))))
    (if (>= (aref arr sup0) x)
        (cl-loop
         for test = t then (< (aref arr mid) x)
         for inf = 0 then (if test (1+ mid) inf)
         for sup = sup0 then (if test sup mid)
         for mid = (/ (+ sup inf) 2)
         when (>= inf sup)
           return (= (aref arr inf) x)))))

(defun aoc-d1-part1 (combs)
  "Returns the pair which sum is 2020"
  (cl-loop for comb in combs
           when (= 2020 (apply #'+ comb))
             return comb))

(defun aoc-d1-part2 (numbers combs)
  "Returns the triplet which sum is 2020"
  (cl-loop for comb in combs
           for n = (- 2020 (apply #'+ comb))
           when (aoc-d1-bsearch n numbers)
             return (cons n comb)))

(defun aoc-d1-slurp (fname)
  "Returns the whole content of the file `fname' as a string."
  (with-temp-buffer
    (insert-file-contents fname)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(defun aoc-d1-read-datas ()
  "Returns a sorted vector filled with numbers from input.txt"
  (let ((datas (split-string (aoc-d1-slurp "day1.txt") "\n" t)))
    (sort (vconcat (mapcar #'string-to-number datas))
          #'<)))

(defun aoc-d1-show-solution (part sol)
  "Display a combination and its product"
  (message "%s: numbers: %S.  Product: %d" part sol (apply #'* sol)))

(defun aoc-d1-main ()
  "Main entry of the program"
  (let* ((datas (aoc-d1-read-datas))
         (combs (aoc-d1-combinations-from 2 datas)))
    (progn
      (aoc-d1-show-solution "Part1" (aoc-d1-part1 combs))
      (aoc-d1-show-solution "Part2" (aoc-d1-part2 datas combs)))))
