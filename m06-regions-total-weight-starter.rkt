;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname m06-regions-total-weight-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require spd/tags)

(@assignment lectures/m06-regions-total-weight)

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






;; Design a function that produces the total weight of a region 


(@htdf total-weight--region total-weight--lor) ;pair of MR functions
(@signature Region -> Integer)
(@signature ListOfRegion -> Integer)
;; produce total weight of region / list of region
(check-expect (total-weight--lor empty) 0)
(check-expect (total-weight--region S1) 20)
(check-expect (total-weight--lor (list S1 S2 S3)) (+ 20 40 60))
(check-expect (total-weight--region G1) (+ 20 40 60))
(check-expect (total-weight--region G4) 280)

;(define (total-weight--region r) 0)
;(define (total-weight--lor lor) 0)

;; ** ADD TEMPLATE TAGS, COPY TEMPLATES COMPLETE FUNCTION DESIGN **



(@template Region)

(define (total-weight--region r)
  (cond [(single? r)
         (single-weight r)]
        [(group? r)
         (total-weight--lor (group-subs r))]))

(@template ListOfRegion)

(define (total-weight--lor lor)
  (cond [(empty? lor) 0]
        [else
         (+   (total-weight--region (first lor))
              (total-weight--lor (rest lor)))]))