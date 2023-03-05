;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m02-grade-standing-work) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment lectures/m02-grade-standing)

(@cwl wriksen)

(@problem 1)

(@htdd GradeStanding)
;;GradeStanding is one of:
;; - Natural
;; - "H"
;; - "P"
;; - "F"
;; - "T"
;; interp. if number percent grade, or standing
;; CONSTRAINT: if natural must be in [0, 100]
(define GS1 100)
(define GS2 49)
(define GS3 "P")

(@dd-template-rules one-of
                    atomic-non-distinct ;Natural
                    atomic-distinct
                    atomic-distinct
                    atomic-distinct
                    atomic-distinct)   

(define (fn-for-grade-standing gs)
  (cond [(number? gs) (... gs)]
        [(string=? "H") (...)]
        [(string=? "P") (...)]
        [(string=? "T") (...)]
        [else (...)]))

(@htdf excellent?)
(@signature GradeStanding -> Boolean)
;;produces true if gs >= 90
(check-expect (excellent? 89) #f)
(check-expect (excellent? 90) #t)
(check-expect (excellent? 91) #t)
(check-expect (excellent? "H") #f)
(check-expect (excellent? "P") #f)
(check-expect (excellent? "T") #f)
(check-expect (excellent? "F") #f)


;(define (excellent? gs) false)

(@template GradeStanding)

(define (excellent? gs)
  (cond [(number? gs) (>= gs 90)]
        [(string=? "H" gs) false]
        [(string=? "P" gs) false]
        [(string=? "T" gs) false]
        [else false]))




#|
INFORMATION   Data
  95 85       95 85   integers [0, 100]

F             "F"
H             "H"

|#