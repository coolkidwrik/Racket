;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname m07-2-one-of-prefixes-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment lectures/m07-2-one-of-prefixes)

(@cwl wriksen) ;replace ??? with your cwl

(@problem 1)
#|
Design a function that consumes two lists of strings and produces
true if the first list is a prefix of the second, meaning that 
the first list matches, 1 for 1, the beginning of the second list.

For example:

  (prefix=? (list "a" "b") (list "a" "b" "c")) --> true
  (prefix=? (list "a" "b") (list "b" "c"))     --> false
  (prefix=? (list "a" "b") (list "a" "b"))     --> true
  (prefix=? (list "a" "b") (list "a"))         --> false

  
As a reminder, here is a data definition for a list of strings. To 
save space later we are calling it LOS instead of ListOfString.
|#

;; Data Definitions:

(@htdd LOS)
;; LOS is one of:
;;  - empty
;;  - (cons String LOS)
;; interp. a list of strings
(define LOS1 empty)
(define LOS2 (cons "a" (cons "b" empty)))

(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))]))

;; Function:
(@htdf prefix=?)
(@signature LOS LOS -> Boolean)
;; produce true if lst1 is prefix of lst2
(check-expect (prefix=? empty empty) true)
(check-expect (prefix=? empty (cons "s" empty)) true)
(check-expect (prefix=? (cons "s" empty) empty) false)
(check-expect (prefix=? (list "a" "b") (list "a" "b" "c")) true)
(check-expect (prefix=? (list "a" "b") (list "b" "c")) false)

;(define (prefix=? lst1 lst2) false)

(@template 2-one-of)

(define (prefix=? lst1 lst2)
  (cond [(empty? lst1) true]
        [(empty? lst2) false]
        [else
         (and (string=? (first lst1) (first lst2))
              (prefix=? (rest lst1) (rest lst2)))]))












