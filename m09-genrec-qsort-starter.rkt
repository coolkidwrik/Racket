;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname m09-genrec-qsort-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(@assignment lectures/m09-genrec-qsort)

(@cwl wriksen) ;replace ??? with your cwl

(@problem 1)
;;
;; We are now going to turn back to sorting

;;
;; Here's the idea:
;;  If we have to sort a list, we will first make the problem
;;  smaller by breaking it into two lists, sort them, and then
;;  put them back together. 
;;
;;  We will break it in two lists by taking the first element -- 
;;  which we call the PIVOT, and then filtering out two lists, one
;;  w/ elements less than the pivot and one w/ elements greater
;;  than the pivot. Once those lists are sorted, we can append
;;  the < list, a list consisting of just the pivot and the >
;;  list back back together to get the result.
;;
;;                   (list 6 8 1 9 3 7 2) 
;;                   /         |        \
;;                  /          |         \
;;        (list 1 3 2)         6        (list 8 9 7)
;;         /  |  \                         /    |    \
;;        /   |   \                       /     |     \
;;     empty  1 (list 3 2)          (list 7)    8    (list 9)
;;                /  |  \             / | \           /  |  \
;;               /   |   \           /  |  \         /   |   \
;;          (list 2) 3  empty     empty 7 empty    empty 9  empty
;;           /     \
;;        empty   empty  
;; This way of sorting is called QUICKSORT. It is a generative
;; recursion.

(@htdf qsort)
(@signature (listof Number) ->  (listof Number))
;; produce list of numbers sorted in ASCENDING order
;; CONSTRAINT: lon contains no duplicates
(check-expect (qsort empty) empty)
(check-expect (qsort (list 1)) (list 1))
(check-expect (qsort (list 1 2)) (list 1 2))
(check-expect (qsort (list 2 1)) (list 1 2))
(check-expect (qsort (list 6 8 1 9 3 7 2) ) (list 1 2 3 6 7 8 9))

;(define (qsort lon) lon)




#|
Three part termination argument.

Base case: lon is empty

Reduction step: elements of lon < p, elements of lon > p

Argument that repeated application of reduction step will eventually 
reach the base case:

new data is shorter than lon by at least 1 element, so
it will reach empty

|#

(@template genrec use-abstract-fn)

(define (qsort lon)
  (cond [(empty? lon) empty]
        [else
         (local [(define p (first lon))
                 (define (<p? x) (< x p))
                 (define (>p? x) (> x p))] 
           (append (qsort (filter <p? lon))
                   (list p)
                   (qsort (filter >p? lon))))]))







