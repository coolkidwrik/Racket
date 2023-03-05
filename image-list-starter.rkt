;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname image-list-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require spd/tags)

(@assignment bank/self-ref-p6)
(@cwl wriksen)

(define R1 (rectangle 50 50 "solid" "red"))
(define R2 (rectangle 50 100 "solid" "red"))

;; =================
;; Data definitions:

(@problem 1)
;; Design a data definition to represent a list of images. Call it ListOfImage.
(@htdd ListOfImage)
;;ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;;interp. list of images or empty list

(@dd-template-rules one-of
                    atomic-distinct ;empty
                    compound        ;ListOfImage
                    self-ref)       ;natural recurssion

(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))


;; =================
;; Functions:

(@problem 2)
;; Design a function that consumes a list of images and produces a number 
;; that is the sum of the areas of each image. For area, just use the image's 
;; width times its height.

(@htdf sum)
(@signature ListOfImage -> number)
;;produces a sum of all shape areas in the list
(check-expect (sum empty) 0)
(check-expect (sum (cons R1 empty)) (* (image-width R1) (image-height R1)))
(check-expect (sum (cons R1 (cons R2 empty)))
              (+ (* (image-width R1) (image-height R1))
              (* (image-width R2) (image-height R2)))) 

;(define (sum loi) 0)

(@template ListOfImage)

(define (sum loi)
  (cond [(empty? loi) 0]
        [else
         (+ (* (image-width (first loi)) (image-height (first loi)))
              (sum (rest loi)))]))

