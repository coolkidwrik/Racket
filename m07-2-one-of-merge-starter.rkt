;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname m07-2-one-of-merge-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment lectures/m07-2-one-of-merge)

(@cwl wriksen) ;replace ??? with your cwl


(@problem 1)
#|
Design a function that consumes two lists of numbers. Each list
is already sorted in increasing order. The function should produce
the merged list sorted in increasing order.
Show the cross product of type comments table, the simplification,
and the correspondence between table cells and cond cases.

For example:

  (merge (list 2 3 5) (list 1 4 6)) --> (list 1 2 3 4 5 6) 
  
As a reminder, here is a data definition for a list of numbers To 
save space later we are calling it LON instead of ListOfNumber
|#

;; Data Definitions:

(@htdd LON)
;; LON is one of:
;;  - empty
;;  - (cons Number LON)
;; interp. a list of numbers
(define LON1 empty)
(define LONA (list 2 3 5))
(define LONB (list 1 4 6))

(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Function:
(@htdf merge)
(@signature LON LON -> LON)
;; produces a combination of both lists in increasing order
;; CONSTRAINT: lst1 and lst2 are already sorted
#|

    lst1            empty     (cons Number LON) 
lst2


empty               empty     lst1


(cons Number LON)   lst2      (if (< (first lst1) (first lst2))
                                (cons (first lst1)
                                (merge (rest lst1) lst2))
                                (cons (first lst2)
                                (merge lst1 (rest lst2))))]))

|#
(check-expect (merge empty empty) empty)
(check-expect (merge empty (list 1 2 3)) (list 1 2 3))
(check-expect (merge (list 1 2 3) empty) (list 1 2 3))
(check-expect (merge LONA LONB) (list 1 2 3 4 5 6))
(check-expect (merge LONB LONB) (list 1 1 4 4 6 6))

;(define (merge lst1 lst2) lst1)

(@template 2-one-of)

(define (merge lst1 lst2)
  (cond [(empty? lst1) lst2]
        [(empty? lst2) lst1]
        [else
         (if (< (first lst1) (first lst2))
          (cons (first lst1)
              (merge (rest lst1) lst2))
          (cons (first lst2)
              (merge lst1 (rest lst2))))]))





