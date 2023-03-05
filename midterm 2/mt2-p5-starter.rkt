;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mt2-p5-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))

(require spd/tags)
(require 2htdp/image)

(@assignment exams/2021w1-mt2/mt2-p5)

(@cwl wriksen)   ;fill in your CWL here (same CWL you put for 110 problem sets)

(@problem 1) ;this is actually problem 5 - DO NOT EDIT OR DELETE THIS LINE!


#|

In this problem you will be working with a simplified version of the data
definition the autograder uses to represent the grading of a submission.
We are providing you with the data definition, you must design two functions
that operate on that data.

|#

(@htdd Score ListOfScore)

(define-struct header   (txt subs))
(define-struct terminal (mark txt))
;; Score is one of:
;;  - (make-header String (listof Score))
;;  - (make-terminal Natural String)
;; interp.
;;
;;  An abitrary-arity tree of Score.  There are header scores and
;;  terminal scores.
;;
;;  A header provides a kind of section divider in a grading report;
;;  it has no actual mark of its own. It just has the text of the
;;  header message, and a list of Score underneath the header.
;;
;;  A terminal score has a mark in points and the text describing the
;;  that mark.
;;
(define S1 (make-terminal .05 "tests"))
(define P1 (make-header "Problem 1"
                        (list (make-terminal  .10 "signature")
                              (make-terminal  .05 "tests")
                              (make-terminal  .10 "template tag")
                              (make-terminal 0    "template not intact"))))
(define P3A (make-header "Domain Analysis"
                         (list (make-terminal  .10 "constants")
                               (make-terminal  .10 "changing")
                               (make-terminal  .10 "options"))))
(define P3B (make-header "Changing information"
                         (list (make-terminal  .05 "define-struct")
                               (make-terminal  .10 "type comment")
                               (make-terminal  .10 "interpretation")
                               (make-terminal 0    "examples"))))

(define TOP
  (make-header "Overall"
               (list P1
                     (make-header "Problem 2"
                                  (list (make-terminal .05 "rules")
                                        (make-terminal .10 "template")))
                     (make-header "Problem 3"
                                  (list (make-header "Part A" (list P3A))
                                        (make-header "Part B" (list P3B)))))))



;(@template Score ListOfScore encapsulated)
(define (fn-for-score s)
  (local [(define (fn-for-score s)
            (cond [(header? s)
                   (... (header-txt s)
                        (fn-for-los (header-subs s)))]
                  [(terminal? s)
                   (... (terminal-mark s)
                        (terminal-txt s))]))
          (define (fn-for-los los)
            (cond [(empty? los) (...)]
                  [else
                   (... (fn-for-score (first los))
                        (fn-for-los (rest los)))]))]
    (fn-for-score s)))


(define (fold-score c1 c2 c3 b1 s)
  (local [(define (fn-for-score s)
            (cond [(header? s)
                   (c1 (header-txt s)
                       (fn-for-los (header-subs s)))]
                  [(terminal? s)
                   (c2 (terminal-mark s)
                       (terminal-txt s))]))
          (define (fn-for-los los)
            (cond [(empty? los) b1]
                  [else
                   (c3 (fn-for-score (first los))
                       (fn-for-los (rest los)))]))]
    (fn-for-score s)))


#|
Complete the design of the two functions below.  NOTE THAT:
  - you must include all htdf recipe elements except stubs
  - for one function your @template must be use-abstract-fn
  - for the other function your must template using the encapsulated
    templates above
  - you MUST use the most appropriate template strategy for each
    function
  - if both functions use the same template approach the second
    function will receive a mark of 0
|#

;; PROBLEM 5A
;;
;; A function that consumes a Score and produces the total of all
;; the marks in the score tree.  For example:
;;     (total-marks TOP) should produce .95

(@htdf total-score)
(@signature Score -> Number)
;; produces the total of all marks in the tree
(check-expect (total-score TOP) 0.95)

;(define (total-score s) 0)

(@template use-abstract-fn)


(define (total-score s)
  (local [(define (c1 h nrh) nrh)
          (define (c2 tm tt) tm)]
    (fold-score c1 c2 + 0 s)))




#;
(define (fold-score c1 c2 + b s)
  (local [(define (c1 h nrh) nrh)
          (define (c2 tm tt) tm)]
    (local [(define (fn-for-score s)
              (cond [(header? s)
                     (c1 (header-txt s)
                         (fn-for-los (header-subs s)))]
                    [(terminal? s)
                     (c2 (terminal-mark s)
                         (terminal-txt s))]))
            (define (fn-for-los los)
              (cond [(empty? los) b1]
                    [else
                     (c3 (fn-for-score (first los))
                         (fn-for-los (rest los)))]))]
      (fn-for-score s))))


#;
(define (total-score s)
  (local [(define (fn-for-score s)
            (cond [(header? s)
                   (fn-for-los (header-subs s))]

                  [(terminal? s)
                   (terminal-mark s)]))
          (define (fn-for-los los)
            (cond [(empty? los) 0]
                  [else
                   (+   (fn-for-score (first los))
                        (fn-for-los (rest los)))]))]
    (fn-for-score s)))

;; PROBLEM 5B
;;
;; A function that consumes a String and a Score, and searches the
;; score tree for a header score with the given text.
;;

(@htdf find-section)
(@signature String Score -> Boolean)
;;produce true if found, else false
(check-expect (find-section "Overall" TOP) true)

;(define (find-section st sc) false)


(@template Score ListOfScore encapsulated)

(define (find-section st sc)
  (local [(define (fn-for-score st sc)
            (cond [(header? sc)
                   (if (string=? (header-txt sc) st)
                       true
                       (fn-for-los st (header-subs sc)))]))
          
          (define (fn-for-los st los)
            (cond [(empty? los) false]
                  [else
                   (or  (fn-for-score (first los))
                        (fn-for-los st (rest los)))]))]
    (fn-for-score st sc)))