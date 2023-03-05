;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname pset-06-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)

(@assignment psets/pset-06); Do not edit or remove this tag

;; If you are:
;;   - A 110 or 107 student replace the first set of '???'s with your cwl.
;;     For problem sets, If you have a partner, please replace the second
;;     set of '???'s with their cwl.  Remember this, it is what you will
;;     do with these @cwl annotations for the whole course.
;;   - A UBC Extended Learning student, replace the first set of ??? with
;;     your email address as confirmed in the email you received from
;;     extended learning.  The handin password is also in that email.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   
(@cwl wriksen)


(@problem 1)
;; Below is the start of a data definition called Course that represents limited
;; information about UBC courses.  Below there are only two example data.  
;; Please complete this definition by adding constants C110, C213, C313 and C317
;; which are representations of the descendent tree for 110, 213, 313 and 317.  
;; You can find the information you need at
;;  https://cs110.students.cs.ubc.ca/psets/pset-06-image.png
;;
;; NOTE 1: Use the information in the image above, rather than any other source.
;;         We are significantly simplying the information.
;;
;; NOTE 2: Do this very carefully, the autograder wants to see correct results
;;         from the functions you design to operate on this data.
;;
;; NOTE 3: The tree you will make for C110 will be a bit odd because 210 has 110
;;         as a pre-req, and both 213 and 221 have 210 as a pre-req, and313 has
;;         213 AND 221 as a pre-req, and 317 has 213 AND 221 as a pre-req. As a
;;         result, 313 and 317 will both show up twice in your descendent tree
;          for C110. This is okay for this problem set.
;; NOTE 4: Expect this step of the problem set to take you some time.


(@htdd Course ListOfCourse)
(define-struct course (number credits dependents))
;; Course is (make-course Natural Natural ListOfCourse)
;; interp. a course with a course number,
;;         the number of credits the course is worth, and a
;;         list of courses that list this course as a direct pre-requisite

;; ListOfCourse is one of:
;; - empty
;; - (cons Course ListOfCourse)
;; interp. a list of courses

#|
(define C110 (make-course 110 4 LOC0))
(define C213 (make-course 213 4 (list (make-course 121 4 LOC0)
                                      (make-course 210 4 (list C110)))))
(define C313 (make-course 313 3 (list C213
                                      (make-course 221 4 (list
                                                          (make-course 210 4
                                                        (list C110)))))))
(define C317 (make-course 317 3 (list C213
                                      (make-course 221 4
                                                   (list (make-course 210 4
                                                      (list C110)))))))
|#
(define LOC0 empty)
(define C100 (make-course 100 3 LOC0))
(define C313 (make-course 313 3 LOC0))
(define C317 (make-course 317 3 LOC0))
(define C213 (make-course 213 4
                          (list
                           C313
                           C317))) 
 
(define C110
  (make-course 110 4
               (list (make-course 189 1 LOC0)
                     (make-course 203 3 LOC0)
                     (make-course 210 4
                                  (list
                                   C213
                                   (make-course 221 4
                                                (list
                                                 (make-course 304 3 LOC0)
                                                 C313
                                                 (make-course 314 3 LOC0)
                                                 C317
                                                 (make-course 320 3 LOC0)
                                                 (make-course 322 3 LOC0)))
                                   (make-course 310 4
                                                (list (make-course 319 4 LOC0)))
                                   (make-course 311 3 LOC0)
                                   (make-course 312 3 LOC0)))
                     (make-course 302 3 LOC0)
                     (make-course 303 3 LOC0))))



(define (fn-for-course c)
  (... (course-number c)
       (course-credits c)
       (fn-for-loc (course-dependents c))))

(define (fn-for-loc loc)
  (cond [(empty? loc) (...)]
        [else
         (... (fn-for-course (first loc))
              (fn-for-loc (rest loc)))]))

(@problem 2)
;;
;; Design a function that produces the list of all the course numbers in the
;; course's tree including the given course's number.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;
(@htdf number--c number--loc)
(@signature Course -> ListOfNatural)
(@signature ListOfCourse -> ListOfNatural)
;; produces list of all course numbers within course tree
(check-expect (number--c C100) (list 100))
(check-expect (number--c C213) (list 213 313 317))
(check-expect (number--c C110) (list 110 189 203 210 213 313 317 221 304 313
                                     314 317 320 322 310 319 311 312 302 303))

(check-expect (number--loc LOC0) empty)
(check-expect (number--loc (list C100)) (list 100))
(check-expect (number--loc (list C213
                                 C313
                                 C317))
              (list 213 313 317 313 317))

;(define (number--c c) empty)
;(define (number--loc loc) empty)

(@template Course)
(@template ListOfCourse)

(define (number--c c)
  (cons (course-number c)
        (number--loc (course-dependents c))))

(define (number--loc loc)
  (cond [(empty? loc) empty]
        [else
         (append (number--c (first loc))
                 (number--loc (rest loc)))]))


(@problem 3)
;;
;; Design a function that takes two arguments: a Course and a Natural, in that
;; order. It produces the list of courses in the tree that are worth that
;; many credits or more.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;
(@htdf credit--c credit--loc)
(@signature Course Natural -> ListOfCourse)
;; produce list of courses with a tree that exceed/equal credits given
(check-expect (credit--c C100 3) (list C100))
(check-expect (credit--c C100 2) (list C100))
(check-expect (credit--c C100 4) empty)
(check-expect (credit--c C110 4)
              (list
               C110
               (make-course 210 4
                            (list
                             C213
                             (make-course 221 4
                                          (list
                                           (make-course 304 3 LOC0)
                                           C313
                                           (make-course 314 3 LOC0)
                                           C317
                                           (make-course 320 3 LOC0)
                                           (make-course 322 3 LOC0)))
                             (make-course 310 4
                                          (list (make-course 319 4 LOC0)))
                             (make-course 311 3 LOC0)
                             (make-course 312 3 LOC0)))
               C213
               (make-course 221 4
                            (list
                             (make-course 304 3 LOC0)
                             C313
                             (make-course 314 3 LOC0)
                             C317
                             (make-course 320 3 LOC0)
                             (make-course 322 3 LOC0)))
               (make-course 310 4
                            (list (make-course 319 4 LOC0))) 
               (make-course 319 4 LOC0)))
(check-expect (credit--c C213 3) (list C213 C313 C317))
(check-expect (credit--c C213 4) (list C213))


(check-expect (credit--loc empty 2) empty) 
(check-expect (credit--loc (list C100) 3) (list C100))
(check-expect (credit--loc (list C100) 2) (list C100))
(check-expect (credit--loc (list C317 C213) 3) (list C317 C213 C313 C317)) 
(check-expect (credit--loc
               (list C317 C110) 4)
              (list
                              C110
                              (make-course 210 4
                               (list
                                C213
                                (make-course 221 4
                                 (list
                                  (make-course 304 3 LOC0)
                                  C313
                                  (make-course 314 3 LOC0)
                                  C317
                                  (make-course 320 3 LOC0)
                                  (make-course 322 3 LOC0)))
                                  (make-course 310 4
                                   (list (make-course 319 4 LOC0)))
                                  (make-course 311 3 LOC0)
                                  (make-course 312 3 LOC0)))
                                  C213
                                  (make-course 221 4
                                   (list
                                    (make-course 304 3 LOC0)
                                    C313
                                    (make-course 314 3 LOC0)
                                    C317
                                    (make-course 320 3 LOC0)
                                    (make-course 322 3 LOC0)))
                                     (make-course 310 4
                                        (list (make-course 319 4 LOC0)))
                                           (make-course 319 4 LOC0)))

;(define (credit--c c n) empty)
;(define (credit--loc loc n) empty)

(@template Course)
(@template ListOfCourse)

(define (credit--c c n)
  (if (>= (course-credits c) n)
      (cons c
            (credit--loc (course-dependents c) n)) 
      (credit--loc (course-dependents c) n)))

(define (credit--loc loc n)
  (cond [(empty? loc) empty]
        [else
         (append (credit--c (first loc) n)
                 (credit--loc (rest loc) n))]))

(@problem 4)
;;
;; Design a function that produces the largest course number in the tree.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;
(@htdf coursen--c coursen--loc)
(@signature Course -> Natural)
;; produces the largest course number in the tree
(check-expect (coursen--c C100) 100)
(check-expect (coursen--c C213) 317)
(check-expect (coursen--c C110) 322)

(check-expect (coursen--loc LOC0) 0)
(check-expect (coursen--loc (list C100 C110)) 322)

;(define (coursen--c c) 0)
;(define (coursen--loc loc) 0)

(@template Course)
(@template ListOfCourse)

(define (coursen--c c)
  (if (> (course-number c)
         (coursen--loc (course-dependents c)))
      (course-number c)
      (coursen--loc (course-dependents c))))

(define (coursen--loc loc)
  (cond [(empty? loc) 0]
        [else
         (if (> (coursen--c (first loc))
                (coursen--loc (rest loc))) 
             (coursen--c (first loc))
             (coursen--loc (rest loc)))]))


(@problem 5)
;;
;; Design a function that takes two arguments: a Course and a Natural, in that
;; order. It produces the course in the tree with that course number. If it
;; can't find a course in the given tree with that course number, it signals
;; failure by producing false.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;
(@htdf course?--c course?--loc)
(@signature Course Natural -> Course or false)
;; produce list of courses with a tree that exceed/equal credits given
(check-expect (course?--c C100 100) C100)
(check-expect (course?--c C100 96) false)
(check-expect (course?--c C110 100) false)
(check-expect (course?--c C110 322) (make-course 322 3 LOC0))
(check-expect (course?--c C110 317) C317)
(check-expect (course?--c C110 310) (make-course 310 4
                                      (list (make-course 319 4 LOC0))))

(check-expect (course?--loc empty 100) false)
(check-expect (course?--loc (list C100) 100) C100)
(check-expect (course?--loc (list C100) 99) false)
(check-expect (course?--loc (list C100 C110) 322) (make-course 322 3 LOC0))
(check-expect (course?--loc (list C110 C213) 317) C317)


;(define (course?--c c n) C100)
;(define (course?--loc loc n) C100)

(@template Course)
(@template ListOfCourse try-catch)

(define (course?--c c n)
  (if (= (course-number c)
         n)  
      c
      (course?--loc (course-dependents c) n))) 

(define (course?--loc loc n)
  (cond [(empty? loc) false]
        [else
         (if (not (false? (course?--c (first loc) n)))
             (course?--c (first loc) n)
             (course?--loc (rest loc) n))]))




  







         