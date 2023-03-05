;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname mt1-p4-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
(require 2htdp/image)

(@assignment exams/2021w1-mt1/mt1-p4)

(@cwl ???) ;fill in your CWL here (same as for problem sets)

(@problem 1) ;DO NOT EDIT OR DELETE THIS LINE! (This is actually problem 4)
;;
;; This is an HtDF problem. In your solution you must include a complete
;; HtDF function design including all design elements, a commented out
;; stub and all relevant @tags.
;;
;; - Submissions which do not pass a Check Syntax test will incur a
;;   significant penalty.
;; - Submissions that cannot even start running their tests will also
;;   incur a significant penalty.
;;

;;
;; CONSTANTS:   PLEASE USE THESE CONSTANTS IN YOUR CHECK-EXPECTS
;;
(define I20x30 (rectangle 20 30 "solid" "red"))
(define I15x10 (rectangle 15 10 "solid" "blue"))
(define I40x10 (rectangle 40 10 "solid" "magenta"))
(define I20x90 (rectangle 20 90 "solid" "pink"))
(define I10x50 (rectangle 10 50 "solid" "orange"))
(define I80x60 (rectangle 80 60 "solid" "brown"))

;;
;; DATA DEFINITIONS:
;;

(@htdd ListOfImage)
;; ListOfImage is one of:
;;  - empty
;;  - (cons Image ListOfImage)
;; interp. a list of images
(define LOI1 empty)
(define LOI2 (cons I20x30 (cons I10x50 empty)))

(@dd-template-rules one-of             ;2 cases
                    atomic-distinct    ;empty
                    compound           ;(cons Image ListOfImage)
                    self-ref)          ;(rest loi) is ListOfImage

(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))

;;
;; Please design a function that consumes a list of images as its first argument
;; and a number as its second argument.  The function should produce the sum of
;; the areas of just those images with area greater than the number.
;;
;; For example:
;;   (sum-areas-greater-than (cons I15x10 (cons I20x30 empty)) 500)
;;
;;  would produce 600.
;;
;; To help you we are providing an additional function, which you can use as if
;; it was a primitive:
;;
;;   Image -> Natural
;;   image-area   
;;   produces the product of the image width * image height
;;
;; The definition for this function is at the end of the file.  We did not do
;; a full htdf because this an exam.
;;


(@htdf sum-areas-greater-than)









;;********************************
;; Supplied primitive below here.
;;

(define (image-area img)
  (* (image-width img) (image-height img)))
