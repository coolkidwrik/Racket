;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname m06-regions-all-with-color-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require spd/tags)

(@assignment lectures/m06-regions-all-with-color)

(@cwl wriksen) ;replace ??? with your cwl

;;
;; Region and ListOfRegion data definitions provided. 
;;
(@problem 1)
(@htdd Region ListOfRegion)
(define-struct single (label weight color))
(define-struct group (color subs))
;; Region is one of:
;;  - (make-single String Natural Color)
;;  - (make-group Color ListOfRegion)
;; interp.
;;  an arbitrary-arity tree of regions
;;  single regions have label, weight and color
;;  groups have a color and a list of sub-regions
;;
;;  weight is a unitless number indicating how much weight
;;  the given single region contributes to whole tree

;; ListOfRegion is one of:
;;  - empty
;;  - (cons Region ListOfRegion)
;; interp. a list of regions

;; All the Ss and Gs are Regions
(define S1 (make-single "one" 20 "red"))
(define S2 (make-single "two" 40 "blue"))
(define S3 (make-single "three" 60 "orange"))
(define S4 (make-single "four" 30 "black"))
(define S5 (make-single "five" 50 "purple"))
(define S6 (make-single "six" 80 "yellow"))

(define G1 (make-group "red"  (list S1 S2 S3)))
(define G2 (make-group "blue" (list G1 S4)))
(define G3 (make-group "orange" (list S5 S6)))
(define G4 (make-group "black" (list G2 G3)))

(define LORE empty)
(define LOR123 (list S1 S2 S3))

(define (fn-for-region r)
  (cond [(single? r)
         (... (single-label r)
              (single-weight r)
              (single-color r))]
        [(group? r)
         (... (group-color r)
              (fn-for-lor (group-subs r)))]))

(define (fn-for-lor lor)
  (cond [(empty? lor) (...)]
        [else
         (... (fn-for-region (first lor))
              (fn-for-lor (rest lor)))]))






;; Design a function that consumes a region and a string
;; and produces a list of all contained regions with the given color.
;; Include the root if it has that color.   Be sure to
;; follow the structure above, where the @htdf tag names both
;; functions, the signatures are grouped, single purpose, tests
;; are grouped, and each function has its own template tag.

(@htdf all-with-color—-region  all-with-color--lor)
(@signature Region String -> ListOfRegion)
(@signature ListOfRegion String -> ListOfRegion)
;; produce a list of all contained regions with that color(possibly including r)
(check-expect (all-with-color—-region S1 "blue") empty)
(check-expect (all-with-color--lor empty "blue") empty)
(check-expect (all-with-color—-region S1 "red") (list S1))
(check-expect (all-with-color--lor G1 "blue") (list S1))
(check-expect (all-with-color--lor G1 "red") (list G1 S1))
(check-expect (all-with-color--lor G4 "blue") (list G2 S2))


;(define (all-with-color—-region r c) empty)
;(define (all-with-color--lor lor c) empty)

(@template Region)

(define (all-with-color—-region r c)
  (cond [(single? r)
         (append (single-color r) empty)]
        [(group? r)
         (all-with-color--lor lor c (group-subs r))]))



(@template ListOfRegion)

(define (all-with-color--lor lor c)
  (cond [(empty? lor) empty]
        [else
         (if (group-color(first lor))
             (append (all-with-color—-region r c (first lor))
              (all-with-color--lor lor c (rest lor))))]))











