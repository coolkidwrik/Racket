;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname f-p3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(@assignment f-p3)

(@cwl ???)   ;fill in your CWL here (same CWL you put for problem sets)

(@problem 1) ;This is actually problem 3, DO NOT EDIT OR DELETE THIS LINE!

;; PROBLEM
;;
;;
;; Complete the design of the following function by writing the template tag
;; and the function definition.  Your answer must use built-in abstract
;; functions.  Any answer that includes any part of the recursive Natural
;; template or the (listof X) template will receive 0 marks.  You are free
;; to add additional check-expects if those examples are helpful for you.
;;

(@htdf stacked-e-squares)
(@signature (listof Color) Natural -> Image)
;; produce stack of solid squares of size n for just colors that end with "e"

(check-expect (stacked-e-squares empty 10) empty-image)
(check-expect (stacked-e-squares (list "blue") 20) (square 20 "solid" "blue"))
(check-expect (stacked-e-squares (list "blue" "red") 20)
              (square 20 "solid" "blue"))
(check-expect (stacked-e-squares (list "blue" "red" "purple" "green") 20)
              (above (square 20 "solid" "blue")
                     (square 20 "solid" "purple")))
(check-expect (stacked-e-squares (list "red" "green" "grey") 20)
              empty-image)
              

;(define (stacked-e-squares loc n) empty-image) ;stub

(@template fn-composition use-abstract-fn)

(define (stacked-e-squares loc n)
  (foldr above empty-image
           (map (λ (s) (square n "solid" s))
                (filter
                 (λ (c) (equal? (substring c (sub1 (string-length c))) "e"))
                 loc))))
#;
(define (stacked-e-squares loc n)
  (local [(define (make-a-square c)
            (square n "solid" c))

          (define (only-e s)
            (equal? "e" (substring s (sub1 (string-length s)))))] 
    (foldr above empty-image
           (map make-a-square (filter only-e loc)))))







(@template fn-composition use-abstract-fn)
#;
(define (stacked-e-squares loc n)
  (foldr above
         empty-image
         (map (λ (c) (square n "solid" c))
              (filter (λ (s)
                   (equal?
                    (substring s (sub1 (string-length s))) "e")) loc))))
;; correct
;; alternate solution-->
#;
(define (stacked-e-squares loc n)
  (local [(define (fn c)
            (string=? "e" (substring c (sub1 (string-length c)))))
          (define (fn2 c)
            (square n "solid" c))]
    (foldr above empty-image (map fn2 (filter fn loc)))))

 






