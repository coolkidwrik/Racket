;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mt2-p1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment exams/2021w1-mt2/mt2-p1)

(@cwl wriksen)   ;fill in your CWL here (same CWL you put for 110 problem sets)

#|

For this problem you will be working with the type comments and arrows
shown in the handout or:

  https://cs110.students.cs.ubc.ca/exams/2021w1-mt2/mt2-p1-figure.pdf


ERRORS AND FAILING TESTS

A file that does not run without errors will receive zero marks.  At this point
in the course there is no reason for you to hand in a file that does not run.
Run your work often while building the templates and filling in the arrows
constant and you should have no problem being sure your file runs. Also be sure
to run EVERY TIME BEFORE YOU SUBMIT.

We have special checks (tests) at the end of the file.	Any tests that fail
indicate that you have not understood or followed the instructions on how to
form your answer.  Files that have failing checks may earn as few as zero marks.
IF YOU GET A FAILING CHECK, take the time to CAREFULLY READ THE MESSAGE.  We
have designed these tests so that the name of the failing tests indicates what 
is incorrect.

If you get more than one error or failing test we recommend fixing the first
one and then re-running before trying to fix the later ones.

|#

(@problem 1) ;THIS IS REALLY PROBLEM 1A.  DO NOT EDIT OR DELETE THIS LINE!
;;
;; Complete the templates. You MUST use these function and parameter
;; names.  You must not delete or comment out any of these function
;; definitions.	 You must not change the order of these function
;; definitions.
;;

(define (fn-for-foo f)
  (... (foo-x f)
       (fn-for-baz (foo-lst f))))

(define (fn-for-baz b)
  (... (fn-for-zorker (baz-z b))    
       (fn-for-lof (baz-lst b))))     

(define (fn-for-zorker z)
  (... (zorker-i z)
       (zorker-j z)))

(define (fn-for-lof lof)
  (cond [(empty? lof) (...)]
        [else
         (... (first lof)
              (fn-for-lof (rest lof)))]))



(@problem 2) ;THIS IS REALLY PROBLEM 1B.  DO NOT EDIT OR DELETE THIS LINE!
;;
;; In the arrows constant below you must replace every "???" string with
;; one of the strings "R", "SR", or "MR" to indicate the kind of the
;; corresponding arrow in the pdf file specified above.
;;


(define arrows
  (list (list "A" "MR")   ;replace "???" with one of "R" "SR" "MR"
	(list "B" "R")   ;replace "???" with one of "R" "SR" "MR"
	(list "C" "MR")   ;replace "???" with one of "R" "SR" "MR"
	(list "D" "MR")   ;replace "???" with one of "R" "SR" "MR"
	(list "E" "SR"))) ;replace "???" with one of "R" "SR" "MR"





;;============================================================================
;;============================================================================
;;============================================================================
;; You should ignore this code.	 Do not read or edit below here.
;;
;; This code is here so that running the file will verify that your answer is
;; well-formed.

(define REF-LABELS  (list "R"  "SR" "MR" ))
(define CALL-LABELS (list "NH" "NR" "NMR"))

(define (list-has-length-5 l)
  (= (length l) 5))

(define (all-sublists-have-length-2 l)
  (andmap (λ (m) (= m 2)) (map length l)))

(define (all-sublist-first-elements-are-A-to-E l)
  (equal? (map first l) '("A" "B" "C" "D" "E")))

;(define (all-sublist-first-elements-are-1-to-6 l)
;  (equal? (map first l) (build-list 6 add1)))

(define (all-sublist-second-elements-are-R-SR-MR l)
  (andmap (lambda (sl) (member? (second sl) REF-LABELS)) l))

;(define (all-sublist-second-elements-are-NH-NR-NMR l)
;  (andmap (lambda (sl) (member? (second sl) CALL-LABELS)) l))

;(define (all-sublist-second-elements-are-A-to-F l)
;  (andmap (lambda (sl)
;	     (member? (second sl) '("A" "B" "C" "D" "E" "F")))
;	   l))

(check-satisfied arrows list-has-length-5)
(check-satisfied arrows all-sublists-have-length-2)
(check-satisfied arrows all-sublist-first-elements-are-A-to-E)
(check-satisfied arrows all-sublist-second-elements-are-R-SR-MR)



