;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname m08-abstract-functions-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(@assignment lectures/m08-abstract-functions)
(@cwl wriksen) ;replace ??? with your cwl



#|

One of the most important ideas in software is abstraction. What abstraction
means is that we:
  - recognize that some functionality seems to be repeated/common
  - package that common behaviour in a reusable way
  - so that future (and current) uses of that common behaviour
    can be easier to develop, easier to understand, more reliable etc.

So abstraction is a verb:
  - to recognize the common functionality and package it

and a noun:
  - the packaged up common behaviour.


We have already seen one form of abstraction - templates.

(@template Region ListOfRegion encapsulated)
(define (fn-for-region r)
  (local [(define (fn-for-region r)
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
                        (fn-for-lor (rest lor)))]))]

    (fn-for-region r)))

When we see that, we know that it captures the common behaviour
of traversing a Region tree and that we can reuse that by copying
it and editing.

We can also combine the template based abstractions:


(@template encapsulated Region ListOfRegion try-catch)

(define (fn-for-region r)
  (local [(define (fn-for-region r)
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
                   (local [(define try (fn-for-region (first lor)))]
                     (if (not (false? try))
                         try
                         (fn-for-lor (rest lor))))]))]

    (fn-for-region r)))


That's very powerful.  As we have said before, expert thinking is 
partly characterized by being able to think about programs in terms
of abstractions like these that are larger than individual program
constructs.


There's another way to go about it, which works in fewer cases, but
provides much greater abstraction when it does work.  The idea is to
desing a function that captures the reusable behaviour, with special
arguments corresponding to each of the ... in the template.

Again, abstractions packaged as functions are very powerful when they
work for a problem; but they don't work for all problems. So after 
module 8 we will use both kinds of abstraction as appropriate.


|#



(@htdd ListOfNumber)
;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers


(@problem 1)
#|
Develop a function definition for an abstract function to simplify the 
following two functions. Complete your work by re-defining the original
functions to use the new abstract function. You do not need to do the
signature, purpose or tests for the new function.
|#

(@htdf all-greater?)
(@signature ListOfNumber Number -> Boolean)
;; produce true if every number in lon is greater than x.
(check-expect (all-greater? empty 0) true)
(check-expect (all-greater? (list 2 -3 -4) -6) true)
(check-expect (all-greater? (list -2 -3 -4) -3) false)

(@template ListOfNumber)

(define (all-greater? lon x)
  (local [(define (>x? n) (> n x))]
    (andmap2 >x? lon)))


(@htdf all-positive?)
(@signature ListOfNumber -> Boolean)
;; produce true if every number in lon is positive?
(check-expect (all-positive? empty) true)
(check-expect (all-positive? (list 2 3 -4)) false)
(check-expect (all-positive? (list 2 3  4)) true)

(@template ListOfNumber)

(define (all-positive? lon)
  (andmap2 positive? lon))


(define (andmap2 p lon)
  (cond [(empty? lon) true]
        [else
         (and (p (first lon))
              (andmap2 p (rest lon)))]))

#|
(define (all-positive? lon)
  (cond [(empty? lon) true]
        [else
         (and (positive? (first lon))
              (all-positive? (rest lon)))]))




(local [define (>x? n) (> n x)])
|#


(@problem 2)
#|
Complete the design of the filter2 abstract function with signature,
purpose and tests.
|#

(@htdf positive-only)
(@signature ListOfNumber -> ListOfNumber)
;; produce list with only postive? elements of lon
(check-expect (positive-only empty) empty)
(check-expect (positive-only (list 1 -2 3 -4)) (list 1 3))

;(define (positive-only lon) empty)   ;stub

(@template use-abstract-fn)

(define (positive-only lon)
  (filter2 positive? lon))


(@htdf negative-only)
(@signature ListOfNumber -> ListOfNumber)
;; produce list with only negative? elements of lon
(check-expect (negative-only empty) empty)
(check-expect (negative-only (list 1 -2 3 -4)) (list -2 -4))

;(define (negative-only lon) empty)   ;stub

(define (negative-only lon)
  (filter2 negative? lon))


; type parameters X Y Z  T U V
; (listof Egg) (listof Number) (listof x)
; (X -> Boolean)
; derive signature using type inference


(@htdf filter2)
;(@signature __ __ -> ___)
;(@signature ( -> ) __ -> ___)
;(@signature (__ -> __) ___ -> ___)
;(@signature (X -> __) (listof X) -> ___)
(@signature (X -> Boolean) (listof X) -> (listof X))
;; produce list of only those elements of lon for which p gives true
(check-expect (filter2 positive? empty) empty)
(check-expect (filter2 positive? (list 1 -2 3 -4)) (list 1 3))
(check-expect (filter2 negative? (list 1 -2 3 -4)) (list -2 -4))
(check-expect (filter2 empty?
               (list (list 1 2) empty (list 3 4) empty)) (list empty empty))


(@template (listof X))
(define (filter2 p lox)
  (cond [(empty? lox) empty]
        [else 
         (if (p (first lox))
             (cons (first lox) 
                   (filter2 p (rest lox)))
             (filter2 p (rest lox)))])) 



(@problem 3)
;; 
;; Write the definition of all-greater-than using filter2. 
;;

(@htdf all-greater-than)
(@signature Number (listof Number) -> (listof Number))
;; produce list of all elements of lon > than n
(check-expect (all-greater-than 3 empty) empty)
(check-expect (all-greater-than 3 (list 1 4 2 5)) (list 4 5))

(define (all-greater-than n lon) empty)


(@problem 4)
;;
;; Complete the design of the map2 abstract function with signature.
;;

(@htdf squares)
(@signature (listof Number) -> (listof Number))
;; produce list of sqr of every number in lon
(check-expect (squares empty) empty)
(check-expect (squares (list 3 4)) (list 9 16))

(define (squares lon) (map2 sqr lon))

(@htdf square-roots)
(@signature (listof Number) ->(listof Number))
;; produce list of sqrt of every number in lon
(check-expect (square-roots empty) empty)
(check-expect (square-roots (list 9 16)) (list 3 4))

(define (square-roots lon) (map2 sqrt lon))



(@htdf map2)
;(@signature __ __ -> ___)
;(@signature ( -> ) __ -> ___)
;(@signature (__ -> __) ___ -> ___)
(@signature (X -> Y) (listof X) -> (listof Y))
;; given fn and (list n0 n1 ...) produce (list (fn n0) (fn n1) ...)
(check-expect (map2 sqr empty) empty)
(check-expect (map2 sqr (list 2 4)) (list 4 16))
(check-expect (map2 sqrt (list 16 9)) (list 4 3))
(check-expect (map2 abs (list 2 -3 4)) (list 2 3 4))

(define (map2 fn lon)
  (cond [(empty? lon) empty]
        [else
         (cons (fn (first lon))
               (map2 fn (rest lon)))]))

(@problem 5)
;;
;; Design an abstract function called foldr2 based on the (listof X) template.
;; Work backwards through the HtDF recipe starting from the fn definition.
;;



(@htdf foldr2)
(@signature (X Y -> Y) Y (listof X) -> Y)

;; fold function for (
(check-expect (foldr2 + 0 empty) 0)
(check-expect (foldr2 + 0 (list 2 3 4)) 9)
(check-expect (foldr2 string-append "" (list "foo" "bar")) "foobar")


(@template (listof X))


(define (fn-for-lox c1 b1lox)
  (cond [(empty? lox) b1]
        [else
         (c1 (first lox)
              (fn-for-lox (rest lox)))]))







