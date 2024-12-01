#! /usr/bin/guile \
-e main -s
!#
;; Usage: guile -e main day1.scm

;; for get-line
(use-modules (ice-9 textual-ports))

;; for format
(use-modules (ice-9 format))

;; for reduce (almost a foldl1) and concatenate
;;(use-modules (srfi srfi-1))
(require-extension (srfi 1))
;; vector library : vector-binary-search
(require-extension (srfi 43))
;; compare procedures : integer-compare
(require-extension (srfi 67))
;; (enum 1 3) => '(1 2 3)
;; iota is in guile's core but also defines in srfi-1
;; iota count [start [step]]
;; (iota 2) => '(0 1)
;; (iota 3 1) => '(1 2 3)
(define (enum from to)
  (iota (1+ (- to from)) from))

;; Use concatenate instead of (apply append…)
(define (flat-map f ls)
  (concatenate (map f ls)))

;; Compute the list of all combinations of k elements from the vector
;; arr. The result is a list of list.
(define (combinations k arr)
  (let ((n (1- (vector-length arr))))
    (let loop ((start 0)
               (depth 1)
               (comb '()))
      (let* ((cons-comb (lambda(i) (cons (vector-ref arr i) comb)))
             (recurse (lambda(i) (loop (1+ i) (1+ depth) (cons-comb i)))))
        (if (= depth k)
            (map cons-comb (enum start n))
            (flat-map recurse (enum start n)))))))

;; Use apply with * et + since they can take more than 2
;; arguments. Hence, there is no need of fold
(define (product ls)
  (apply * ls))

(define (sum ls)
 (apply + ls))

;; Build a sorted vector of integers from the file "input.txt"
;; sort! is defined in guile
(define (read-datas)
  (letrec ((read-lines
            (lambda(port)
              (if (eof-object? (peek-char port))
                  '()
                  (let ((line (get-line port)))
                    (cons (string->number line)
                          (read-lines port)))))))
    (let ((lines (call-with-input-file "day1.txt" read-lines)))
      (sort! (list->vector lines) <))))

;; find is in srfi-1
(define (part1 combs)
  (find (lambda(x) (= 2020 (sum x)))
        combs))

;; vector-binary-search is in srfi-43
;; integer-compare is in srfi-67
;; datas must be a sorted vector
;; combs is the result of (combinations 2 datas)
(define (part2 datas combs)
  (let loop ((ls combs)) ;; tail recursive loop
    (if (not (null? ls))
        (let* ((p (car ls))
               (n (- 2020 (sum p))))
          (if (vector-binary-search datas n integer-compare)
              (cons n p)
              (loop (cdr ls)))))))


(define (display-solution part sol)
    (format #t "~a: numbers: ~a.  Product: ~d~%" part sol (product sol)))

(define (main arguments)
  (let* ((datas (read-datas))
         (combs (combinations 2 datas)))
    (begin (display-solution "Part1" (part1 combs))
           (display-solution "Part2" (part2 datas combs)))))


;; utility function for the repl
(define (display-list ls)
  (for-each (lambda(x) (format #t "~a~%" x))
            ls))
