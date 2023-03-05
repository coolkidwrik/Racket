;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2020W2-MT1-P1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@assignment mt1-p1)

(@cwl wriksen)   ;fill in your CWL here (same CWL you put for 110 problem sets)

(@problem 1) ;DO NOT DELETE this line

;;
;; First read through the following PARTIAL data definition.  You do NOT need
;; to, and you MUST NOT edit or complete the data definition.
;;

(@htdd QualComp)
;; QualComp is one of:
;;  - "less-than"
;;  - "equal"
;;  - "greater-than"
;; interp. a qualitative difference between two quantities
;; <examples are redundant for enumerations>



;;
;; THE WORK YOU NEED TO DO begins here.
;;
;; Design a function that consumes two numbers and produces a QualComp
;; representing the relationship between the first number and the second.
;; Your function must be called compare.
;;
;; For example (compare 3.5 9) should produce "less-than".
;;
;; You must have a complete design including @htdf, @signature, and @template
;; annotations, as well as a purpose, tests, a commented out stub and a
;; function definition.
;;
;; If your file has errors that prevent it from running (red errors) then
;; you will lose no fewer than 50% marks.  Failing tests will also lose marks
;; but not as many.
;;

(@htdf compare)
(@signature Number Number -> QualComp)
;; produces a comparison between the first and second number
(check-expect (compare 3.5 9) "less-than")
(check-expect (compare 5   5) "equal")
(check-expect (compare 9 3.5) "more-than")

;(define (compare n1 n2) "less-than")

(@template Number)

(define (compare n1 n2)
  (cond [(< n1 n2) "less-than"]
        [(= n1 n2) "equal"]
        [(> n1 n2) "more-than"]))




