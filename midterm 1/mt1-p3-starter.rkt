;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname mt1-p3-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
(require 2htdp/image)

(@assignment exams/2021w1-mt1/mt1-p3)

(@cwl wriksen) ;fill in your CWL here (same as for problem sets)

(@problem 1) ;DO NOT EDIT OR DELETE THIS LINE! (This is actually problem 3)
;;
;; Below is a partial data definition.  It includes just the @htdd tag, and
;; the type comment.  You do not need the interpretation or the data
;; examples for this problem.
;;
;; You must complete the @dd-template-rules tag and the actual template
;; based on the type comment provided.
;;
;; Note that:
;;  - this problem will be autograded
;;  - the order of the dd-template rules and the template must be the same
;;    as in the type comment
;;  - you must not comment out the @dd-template-rules tag or the
;;    actual template function
;;  - any submission for which Check Syntax fails will receive 0 marks
;;
(@htdd Farfle)
(define-struct one (a b))
;; Farfle is one of:
;;  - false
;;  - "A"
;;  - "B"
;;  - (make-one String Farfle)

(@dd-template-rules one-of          ; 4 cases
                    atomic-distinct ; false
                    atomic-distinct ; "A"
                    atomic-distinct ; "B"
                    compound)       ; (make-one String Farfle)

(define (fn-for-farfle f)
  (cond [(false? f)       (...)]
        [(string=? f "A") (...)]
        [(string=? f "B") (...)]
        [else
         (... (one-a f)
              (one-b f))]))

