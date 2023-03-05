;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m04-lon-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment lectures/m04-lon)

(@cwl wriksen) ;replace ??? with your cwl



(@htdd ListOfNumber)
;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers
(define LON1 empty)
(define LON2 (cons 1 (cons 2 (cons 3 empty))))

(@dd-template-rules one-of             ;2 cases
                    atomic-distinct    ;empty
                    compound           ;(cons Number ListOfNumber)
                    self-ref)          ;(rest lon) is ListOfNumber

(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))


#|
PROBLEMs:

Design a function that computes the sum of a list of numbers.

Design a function that counts the number of elements in a
list of numbers.

Design a function that produces a new list, where each element  
is 2 times the corresponding element in the original list.
|#

(@problem 1)
(@htdf sum)
(@signature ListOfNumber -> Number)
;;produces the sum of all numbers in the list
(check-expect (sum empty) 0)
(check-expect (sum (cons 1 empty)) 1)
(check-expect (sum (cons 1 (cons 2 empty))) 3)
(check-expect (sum (cons 1 (cons 2 (cons 3 empty)))) 6)

;(define (sum lon) 0)

(@template ListOfNumber)

(define (sum lon)
  (cond [(empty? lon) 0]
        [else
         (+ (first lon) 
              (sum (rest lon)))]))


(@problem 2)
(@htdf count)
(@signature ListOfNumber -> Number)
;;produces how many elements are in the list
(check-expect (count empty) 0)
(check-expect (count (cons 1 empty)) 1)
(check-expect (count (cons 1 (cons 2 empty))) 2)
(check-expect (count (cons 1 (cons 2 (cons 3 empty)))) 3)

;(define (count lon) 0)

(@template ListOfNumber)

(define (count lon)
  (cond [(empty? lon) 0]
        [else
         (+ 1
              (count (rest lon)))]))

(@problem 3)
(@htdf product)
(@signature ListOfNumber -> Number)
;;produces product of list elemenst
(check-expect (product empty) 0)
(check-expect (product (cons 1 empty)) 1)
(check-expect (product (cons 1 (cons 2 empty))) 2)
(check-expect (product (cons 4 (cons 2 (cons 3 empty)))) (* 4 2 3))

;(define (product lon) 0)

(@template ListOfNumber)

(define (product lon)
  (cond [(empty? lon) 0]
        [else
         (* (first lon)
              (if (= (product (rest lon)) 0)
                  1
                  (product (rest lon))))]))



(@problem 4)
;(@htdf doubles)


