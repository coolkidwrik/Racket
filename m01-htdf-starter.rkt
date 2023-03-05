;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m01-htdf-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require spd/tags)

(@assignment lectures/m01-htdf)

(@cwl wriksen)

(@problem 1)
;;
;; Design a function, called topple, that takes an image and rotates it 
;; by 90 degrees.
;;
(@htdf topple)
(@signature Image -> Image)
;; produce image rotated by 90 degrees

(check-expect (topple (rectangle 10 20 "solid" "blue"))
              (rectangle 20 10 "solid" "blue"))
(check-expect (topple (triangle 20 "solid" "red"))
              (rotate 90 (triangle 20 "solid" "red")))

;(define (topple img) empty-image)

(@template Image)


(define (topple img)
   (rotate 90 img))

(@problem 2)
;;
;; Design a function that consumes the name of something and produces a
;; "checkbox line" image that allows someone to check off that item.  For 
;; example (checkbox-line "apples") would produce an image with a small
;; check box next to the word apples.
;;
(@htdf checkbox-line)
(@signature String -> Image)
;;produce image of box next to text
(check-expect (checkbox-line "hello")
              (beside (square 30 "outline" "black")
                      (text "hello" 30 "black")))
(check-expect (checkbox-line "oranges")
              (beside (square 30 "outline" "black")
                      (text "oranges" 30 "black")))

;(define (checkbox-line str) empty-image)

(@template String)

(define (checkbox-line str)
  (beside (square 30 "outline" "black")
          (text str 30 "black")))


(@problem 3)
;;
;; Design a function, that consumes an image and determines whether it is tall.
;;
(@htdf tall?)
(@signature Image -> Boolean)
;; produce true if image is tall
(check-expect (tall? (rectangle 10 20 "solid" "black")) true)
(check-expect (tall? (rectangle 20 10 "solid" "black")) false)
(check-expect (tall? (rectangle 10 10 "solid" "black")) false)


;(define (tall? img) false)
(@template Image)

(define (tall? img)
  (> (image-height img) (image-width img)))



(@problem 4)
;;
;; Design a function, called image>, that takes two images and determines 
;; whether the first is larger than the second.
;;
(define IMG1 (rectangle 10 20 "solid" "black"))
(define IMG2 (rectangle 10 10 "solid" "black"))

(@htdf image>)
(@signature Image Image -> Boolean)
;produces true if first is taller
(check-expect (image> IMG1 IMG2) true)
(check-expect (image> IMG2 IMG1) false)
(check-expect (image> IMG1 IMG1) false)

;(define (image> img1 img2) false)
(@template Image)
(define (image> img1 img2)
  (> (* (image-height img1) (image-width img1))
     (* (image-height img2) (image-width img2))))





