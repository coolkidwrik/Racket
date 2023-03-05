;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m05-arrange-images-v2-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(@assignment lectures/m05-arrange-images)
;; arrange-images-starter.rkt (problem statement)
;; arrange-images-v1.rkt      (includes ListOfImage)
;; arrange-images-v2.rkt      (+ arrange-images and 2 wish-list entries)

(@cwl wriksen) ;replace ??? with your cwl





#|

PROBLEM:

In this problem imagine you have a bunch of pictures that you would like to 
store as data and present in different ways. We'll do a simple version of that 
here, and set the stage for a more elaborate version later.

(A) Design a data definition to represent an arbitrary number of images.

(B) Design a function called arrange-images that consumes an arbitrary number
    of images and lays them out left-to-right in increasing order of size.

|#
(@problem 1)
;; Constants:

(define BLANK (square 0 "solid" "white"))

;; for testing:
(define I1 (rectangle 10 20 "solid" "blue"))
(define I2 (rectangle 20 30 "solid" "red"))
(define I3 (rectangle 30 40 "solid" "green"))


;; Data definitions:

(@htdd ListOfImage)
;; ListOfImage is one of:
;;  - empty
;;  - (cons Image ListOfImage)
;; interp. An arbitrary number of images
(define LOI1 empty)
(define LOI2 (cons I1
                   (cons I2
                         empty)))

(@dd-template-rules   one-of             ; 2 cases
                      atomic-distinct    ; empty
                      compound           ; (cons Image ListOfImage)
                      self-ref)          ; (rest ListOfImage) is ListOfImage

(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))

;; Functions:
(@htdf arrange-images)
(@signature ListOfImage -> Image)
;; lay out images left to right in increasing order of size
(check-expect (arrange-images empty) BLANK)
(check-expect (arrange-images (cons I1 (cons I2 empty)))
              (beside I1 I2 BLANK))
(check-expect (arrange-images (cons I2 (cons I1 empty)))
              (beside I1 I2 BLANK))

;(define (arrange-images loi) BLANK) ;stub

;;have to sort list before rendering

(@template fn-composition)

(define (arrange-images loi)
  (layout-images (sort-images loi)))

(@htdf layout-images)
(@signature ListOfImage -> Images)
;; glue images together using beside(preserve the oreder of list)
;; !!!

(define (layout-images loi) BLANK)



(@htdf sort-images)
(@signature ListOfImage -> ListOfImages)
;; sort the list (preserve the oreder of list)
(check-expect (sort-images empty) empty)
(check-expect (sort-images (cons I1 empty)) (cons I1 empty))
(check-expect (sort-images (cons I1 (cons I2 empty))) (cons I1 (cons I2 empty)))
(check-expect (sort-images (cons I2 (cons I1 empty))) (cons I1 (cons I2 empty)))


;(define (sort-images loi) loi)

(@template ListOfImage)

(define (sort-images loi)
  (cond [(empty? loi) empty]
        [else
         ;;(first loi) goes in an arbitrary depth 
         ;;so operate on arbitrary sized data
         ;;go in an arbitrary depth into an arbitrary size
         (insert (first loi)
              (sort-images (rest loi)))]))

(@htdf insert)
(@signature Image ListOfImage -> ListOfImage)
;;insert the image into proper place in the list(preserve order)
;;loi is already sorted in increasing order of size
(check-expect (insert I1 empty) (cons I1 empty))
(check-expect (insert I1 (cons I2 empty)) (cons I1 (cons I2 empty)))
(check-expect (insert I2 (cons I1 empty)) (cons I1 (cons I2 empty)))
(check-expect (insert I3 (cons I1 (cons I2 empty)))
              (cons I1 (cons I2 (cons I3 empty))))

;(define (insert img loi) loi)


(@template ListOfImage); add in the img parameter

(define (insert img loi)
  (cond [(empty? loi) (cons img empty)]
        [else
         ;; this fucntion is about inserting
         ;;comparing sizes is a different kind of knowledge
         ;;domain shift rule
         (if <is img larger than (first loi)>
             (cons (first loi)
                   (insert img (rest loi)))
              (cons img loi))]))

(@htdf larger?)
(@signature Image Image -> Boolean)
;;!!!






;(define (arrange-images loi)
;  (cond [(empty? loi) BLANK]
;        [else
;         (beside (first loi)
;              (fn-for-loi (rest loi)))]))



