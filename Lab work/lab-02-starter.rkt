;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-02-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(@assignment labs/lab-02)

;; If you are:
;;   - A 110 or 107 student replace the first set of '???'s with your cwl.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   - A UBC Extended Learning student, replace the first set of ??? with
;;     the your email address as confirmed in the email you received from
;;     extended learning.  The handin password is also in that email.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   
(@cwl wriksen)

;; HtDF Lab, Problem 1

;; PROBLEM:
;;
;; Design a function called square? that consumes an image and determines 
;; whether the image's height is the same as the image's width.
(@problem 1)

(define IMG1 (rectangle 10 20 "solid" "blue"))
(define IMG2 (rectangle 20 10 "solid" "blue"))
(define IMG3 (rectangle 20 20 "solid" "blue"))

(@htdf square?) ;!!!UNCOMMENT this line when you start on this function
(@signature Image -> Boolean)
;;produces true if the image height is equal to image width
(check-expect (square? IMG1) false)
(check-expect (square? IMG2) false)
(check-expect (square? IMG3) true)

;(define (square? img) false)

(@template Image)

(define (square? img)
  (= (image-width img) (image-height img)))






;; HtDF Lab, Problem 2

;; PROBLEM:
;; 
;; Design a function named underline that consumes an image 
;; and produces that image underlined by a black line of the same width. 
;; 
;; 
;; For example, 
;; 
;;   (underline (circle 20 "solid" "green"))
;; 
;; should produce
;;
;;   (above (circle 20 "solid" "green")
;;          (rectangle 40 2 "solid" "black"))
(@problem 2)
(@htdf underline) ;!!!UNCOMMENT this line when you start on this function
(@signature Image -> Image)
;;produces image with black line underneath
(check-expect (underline IMG1)
              (above IMG1
                     (rectangle 10 2 "solid" "black")))
(check-expect (underline IMG2)
              (above IMG2
                     (rectangle (image-width IMG2) 2 "solid" "black")))

;(define (underline img) empty-image)

(@template Image)

(define (underline img)
  (above img
         (rectangle (image-width img) 2 "solid" "black")))






;; HtDF Lab, Problem 3

;; PROBLEM:
;; 
;; A (much too) simple scheme for pluralizing words in English is to add an 
;; s at the end unless the word already ends in s.
;; 
;; Design a function that consumes a string, and adds s to the end unless 
;; the string already ends in s.
(@problem 3)
(@htdf pluralize) ;!!!UNCOMMENT this line when you start on this function
(@signature String -> String)
;;produces given string with "s" at the end, unless ends in "s"
(check-expect (pluralize "apple") "apples")
(check-expect (pluralize "apples") "apples")

;(define (pluralize str) "")

(@template String)

(define (pluralize str)
  (if (string=? str "")
      ""
      (if (string=?
           (substring str (- (string-length str) 1) (string-length str)) "s")
          str
          (string-append str "s"))))






;; HtDF Lab, Problem 4

;; PROBLEM:
;; 
;; Design a function called nth-char-equal? that consumes two strings 
;; and a natural and produces true if the strings both have length greater 
;; than n and have the same character at position n.
;; 
;; 
;; Note, the signature for such a function is:
;; 
;; (@signature String String Natural -> Boolean)
;; 
;; 
;; The tag and template for such a function are:
;; 
;; (@template String)
;; 
;; (define (nth-char-equal? s1 s2 n)
;;   (... s1 s2 n))
(@problem 4)
(@htdf nth-char-equal?) ;!!!UNCOMMENT this line when you start on this function
(@signature String String Natural -> Boolean)
;;produces true if both strings are longer than n and have the same char at n
(check-expect (nth-char-equal? "apple" "apricot" 2) true)
(check-expect (nth-char-equal? "" "" 2) false)
(check-expect (nth-char-equal? "apple" "" 2) false)
(check-expect (nth-char-equal? "apple" "bananas" 2) false)
(check-expect (nth-char-equal? "apple" "apricot" 6) false)
(check-expect (nth-char-equal? "apple" "pinneapple" 5) true)

;(define (nth-char-equal? str1 str2 n) false)

(@template String)

(define (nth-char-equal? str1 str2 n)
  (and (>= (string-length str1) n) (>= (string-length str2) n) 
      (string=? (substring str1 (- n 1) n) (substring str2 (- n 1) n))))








