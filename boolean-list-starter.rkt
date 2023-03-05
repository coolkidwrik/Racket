;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname boolean-list-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@assignment bank/self-ref-p3)
(@cwl wriksen)

;; =================
;; Data definitions:

(@problem 1)
;; Design a data definition to represent a list of booleans.
;; Call it ListOfBoolean.
(@htdd ListOfBoolean)
;;ListOfBoolean is one of:
;; - empty
;; - (cons Boolean ListOfBoolean)
;;interp. list of boolean values, or an empty list
(@dd-template-rules one-of
                    atomic-distinct ;empty
                    compound         ;ListOfBoolean
                    self-ref) ;natural recurssion

(define (fn-for-lob lob)
  (cond[(empty? lob) true] 
       [else
        (... (first lob)
            (fn-for-lob (rest lob)))]))


;; =================
;; Functions:

(@problem 2)
;; Design a function that consumes a list of boolean values and produces true 
;; if every value in the list is true. If the list is empty, your function 
;; should also produce true. Call it all-true?

(@htdf all-true?)
(@signature ListOfBoolean -> Boolean)
;;produces true if all elements in list are true, or empty
(check-expect (all-true? empty) true)
(check-expect (all-true? (cons true empty)) true)
(check-expect (all-true? (cons false empty)) false)
(check-expect (all-true? (cons true (cons false empty))) false)

;(define (all-true? lob) false)

(@template ListOfBoolean)

(define (all-true? lob)
  (cond[(empty? lob) true] 
       [else
        (and (first lob)
            (all-true? (rest lob)))]))