;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pset-10-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)

(@assignment psets/pset-10); Do not edit or remove this tag

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
(define A1 (list 1 2 3 4 5 6 7 8 9 10))
(define A2 (list 1 2 3 4 6 5 7 8 9 10))
(define A3 (reverse (list 1 2 3 4 5 6 7 8 9 10))) 

(define B1 (list A1 A2 A3))
(list (list 1 2 3 4 5 6 7 8 9 10)
      (list 1 2 3 4 6 5 7 8 9 10)
      (list 1 2 3 4 5 6 7 8 9 10))

(foldr (λ (a b) (if (> a b) a b))
       (first (list 1 2 3 4 5 6 7 8 9 10))
       (list 1 2 3 4 5 6 7 8 9 10))



(@problem 1)
;;
;; Complete the design of the following function.
;;
;; Hint: sometimes for functions with accumulators it is useful for
;;       the trampoline to deal with the special case of the empty
;;       list or empty tree using an if expression.
;;

(@htdf max-num-repeats)
(@signature (listof String) -> Natural)
;; produce maximum number of times  same string appears consecutively in los0
(check-expect (max-num-repeats empty) 0)
(check-expect (max-num-repeats (list "cat")) 1)
(check-expect (max-num-repeats (list "cat" "bird" "dog")) 1)
(check-expect (max-num-repeats (list "cat" "cat" "bird" "dog")) 2)
(check-expect (max-num-repeats (list "cat" "cat" "bird" "dog" "dog" "dog"))
              3)
(check-expect (max-num-repeats (list "cat" "cat" "cat"
                                     "bird"
                                     "boy" "boy" "boy"
                                     "toy" "toy" "toy" "toy" "toy"
                                     "trick"
                                     "zebra" "zebra" "zebra" "zebra"))
              5)
(check-expect (max-num-repeats (list "dog" "cat" "bird"
                                     "dog" "dog" "dog"
                                     "moose" "dog"))
              3)

;(define (max-num-repeats los) 0)

(@template (listof String) accumulator)
(define (max-num-repeats los0)
  ;; Invarient: prev; String, previous element, or empty
  ;; Invarient: rsf; Natural, number of repeats
  ;; Invarient: max; Natural, max rsf
  ;; ((list "bird" "cat" "cat" "dog") "bird" 0 0)
  ;; ((list        "cat" "cat" "dog") "bird" 1 1)
  ;; ((list              "cat" "dog") "cat"  1 1)
  ;; ((list                    "dog") "cat"  2 2)
  ;; ((list                         ) "dog"  1 2) 
  
  (local [(define (repeats los prev rsf max)
            (cond [(empty? los) max]
                  [else
                   (if  (string=? prev (first los))
                        (repeats (rest los)
                                 (first los)
                                 (add1 rsf)
                                 (if (<= max (add1 rsf))
                                     (add1 rsf)
                                     max))
                        (repeats (rest los) (first los) 1 max))]))]
    (repeats los0 (if (empty? los0)
                      empty
                      (first los0)) 0 0)))   




#;
(define (max-num-repeats los0)
  ;; Invarient: acc1; String, previous element
  ;; Invarient: acc2; Natural, number of repeats
  ;; Invarient: acc3; Natural, largest number
  ;; ((list "bird" "cat" "cat" "dog") "bird")
  ;; ((list        "cat" "cat" "dog") "bird")
  ;; ((list              "cat" "dog") "cat")
  ;; ((list                    "dog") "cat")
  ;; ((list                         ) "")
  (local [(define (repeats los acc1 acc2 acc3)
            (cond [(empty? los) acc3]
                  [else
                   (if  (not (string=? acc1 (first los)))
                        (repeats (rest los) (first los) 0 acc2)
                        (if (> acc2 acc3)
                            (repeats (rest los)
                                     (first los) (+ 1 acc2) acc2)
                            (repeats (rest los)
                                     (first los) (+ 1 acc2) acc3)))]))]
    (repeats los0 (first los0) 0 0))) 



(@problem 2)
;;
;; Complete the design of the following function.
;;
;; Your solution MUST BE TAIL RECURSIVE.
;;

(@htdf list-range)
(@signature (listof Integer) -> Natural)
;; produce the difference between the max and min integer in the list
;; CONSTRAINT: loi0 has at least one element
(check-expect (list-range (list 100)) 0)
(check-expect (list-range (list 2 -5 -10 50 80)) 90)
(check-expect (list-range (list 5000 -5 -100 50 0)) 5100)
(check-expect (list-range (list 3 8 1 2 9 4 2 3 -5)) 14)
(check-expect (list-range (list -5000 3 2 2 4 5000 4 2 3)) 10000)
(check-expect (list-range (list 400 500 500 400)) 100)

;(define (list-range loi) 0)
(@template (listof Integer) accumulator)

(define (list-range loi0)
  ;;INVARIENT: max; larger than all integers before
  ;;INVARIENT: min; smaller than all integers before
  (local [(define (range loi max min)
            (cond [(empty? loi) (- max min)]
                  [else (range (rest loi)
                               (if (<= max (first loi))
                                   (first loi)
                                   max)
                               (if (>= min (first loi))
                                   (first loi)
                                   min))]))]
    (range loi0 (first loi0) (first loi0)))) 

 
#;
(define (list-range loi0)
  ;;INVARIENT: max; larger than all integers before
  ;;INVARIENT: min; smaller than all integers before
  (local [(define (range loi max min)
            (cond [(empty? loi) (- max min)]
                  [else
                   (if  (<= max (first loi))
                        (range (rest loi) (first loi) min)
                        (if (>= min (first loi))
                            (range (rest loi) max (first loi))
                            (range (rest loi) max min)))]))]
    (range loi0 (first loi0) (first loi0)))) 



(@problem 3)
;;
;; Complete the design of the following function.
;;
;; Your solution MUST BE TAIL RECURSIVE.
;;
;; Hint: sometimes for functions with accumulators it is useful for
;;       the trampoline to deal with the special case of the empty
;;       list or empty tree using an if expression.
;;
(@htdf in-alphabetical-order?)
(@signature (listof String) -> Boolean)
;; produce true if list is sorted in order according to string-ci<=?
(check-expect (in-alphabetical-order? empty) true)
(check-expect (in-alphabetical-order? (list "a")) true)
(check-expect (in-alphabetical-order? (list "All" "bees" "Feel" "HAPPY")) true)
(check-expect (in-alphabetical-order? (list "aaaaa" "bb" "dd" "ccc")) false)

;(define (in-alphabetical-order? los) false)

(@template (listof String) accumulator)

(define (in-alphabetical-order? los0)
  ;;INVARIENT: acc1; boolean
  ;;INVARIENT: acc2; previous string
  (local [(define (alphabetical? los acc1 acc2)
            (cond [(empty? los) acc1]
                  [else (alphabetical?
                         (rest los)
                         (and acc1
                              (string-ci<=? acc2 (first los)))
                         (first los))]))]

    (alphabetical? los0 true (if (empty? los0)
                                 empty
                                 (first los0))))) 



#;
(define (in-alphabetical-order? los0)
  ;;INVARIENT: acc; previous string
  (local [(define (alphabetical? los acc)
            (cond [(empty? los) true]
                  [else
                   (and (string-ci<=? acc (first los))
                        (alphabetical? (rest los) (first los)))]))]
    (alphabetical? los0 (first los))))





;;
;; Please read through the data definition introduced in Problem Set 6
;; for a Course.
;;

(@htdd Course)
(define-struct course (number credits dependents))
;; Course is (make-course Natural Natural ListOfCourse)
;; interp. a course with a course number,
;;         the number of credits the course is worth,
;;         a list of courses that have this course as a pre-requisite


(@htdd ListOfCourse)
;; ListOfCourse is one of:
;; - empty
;; - (cons Course ListOfCourse)
;; interp. a list of courses

(define LOC0 empty)
(define C322 (make-course 322 3 LOC0))
(define C320 (make-course 320 3 LOC0))
(define C319 (make-course 319 4 LOC0))
(define C317 (make-course 317 3 LOC0))
(define C314 (make-course 314 3 LOC0))
(define C313 (make-course 313 3 LOC0))
(define C312 (make-course 312 3 LOC0))
(define C311 (make-course 311 3 LOC0))
(define LOC1 (list C319))
(define C310 (make-course 310 4 LOC1))
(define C304 (make-course 304 3 LOC0))
(define C302 (make-course 302 3 LOC0))
(define C303 (make-course 303 3 LOC0))
(define LOC2 (list C304 C313 C314 C317 C320 C322))
(define C221 (make-course 221 4 LOC2))
(define LOC3 (list C313 C317))
(define C213 (make-course 213 4 LOC3))
(define LOC4 (list C213 C221 C310 C311 C312))
(define C210 (make-course 210 4 LOC4))
(define C203 (make-course 203 3 LOC0))
(define C189 (make-course 189 1 LOC0))
(define LOC5 (list C189 C203 C210 C302 C303))
(define C110 (make-course 110 4 LOC5))
(define C100 (make-course 100 3 LOC0))

(define (fn-for-course c0)
  (local [(define (fn-for-course c)
            (... (course-number c)
                 (course-credits c)
                 (fn-for-loc (course-dependents c))))

          (define (fn-for-loc loc)
            (cond [(empty? loc) (...)]
                  [else
                   (... (fn-for-course (first loc))
                        (fn-for-loc (rest loc)))]))]
    (fn-for-course c0)))





(@problem 4)
;;
;; Complete the design of the following function.
;;
;; Your solution MUST BE TAIL RECURSIVE.
;;

(@htdf filter-credits-course-num)
(@signature Course Natural Natural -> (listof Course))
;; produce list of courses with >= num-credits and number >= course-num
(check-expect (filter-credits-course-num C100 50 3) (list C100))
(check-expect (filter-credits-course-num C100 100 3) (list C100))
(check-expect (filter-credits-course-num C100 210 3) empty)
(check-expect (filter-credits-course-num C100 100 2) (list C100))
(check-expect (filter-credits-course-num C100 100 4) empty)
(check-expect (filter-credits-course-num C213 300 3) (list C317 C313))
(check-expect (filter-credits-course-num C110 200 4)
              (list C319 C310 C221 C213 C210))

;(define (filter-credits-course-num c course-num num-credits) empty)
(@template Course ListOfCourse accumulator)

(define (filter-credits-course-num c0 n1 n2)
  ;; Invarient: rsf;(listof Course) list of course that match crit so far 
  (local [(define (fn-for-course c todo rsf)
            (fn-for-loc (append (course-dependents c)
                                todo)
                        (if (and (>= (course-number c) n1)
                                 (>= (course-credits c) n2))
                            (cons c rsf)
                            rsf)))

          (define (fn-for-loc loc rsf)
            (cond [(empty? loc) rsf]
                  [else
                   (fn-for-course (first loc) (rest loc) rsf)]))]
    
    (fn-for-course c0 empty empty)))


#;
(define (filter-credits-course-num c0 n1 n2)
  (local [(define (fn-for-course c)
            (if  (and (>= (course-number c) n1)
                      (>= (course-credits c) n2))
                 (cons c
                       (fn-for-loc (course-dependents c)))
                 (fn-for-loc (course-dependents c))))

          (define (fn-for-loc loc)
            (cond [(empty? loc) empty]
                  [else
                   (append (fn-for-course (first loc))
                           (fn-for-loc (rest loc)))]))]
    (fn-for-course c0)))


(@problem 5)
;;
;; Complete the design of the following function.
;;
;; Your solution MUST BE TAIL RECURSIVE.
;;
;; HINTS:
;;   - this is a tandem worklist problem, if you aren't sure about
;;     why take the time to be sure before you proceed
;;   - You will produce a list of course numbers, but you may find it easier
;;     for your rsf accumulator to be a list of courses and then convert to
;;     course numbers right before producing the final result
;;

(@htdf max-credit-path)
(@signature Course -> (listof Natural))
;; produce course numbers in path starting at c0 w/ max total credits
(check-expect (max-credit-path C100) (list 100))
(check-expect (max-credit-path C320) (list 320))
(check-expect (max-credit-path C213) (list 213 313))
(check-expect (max-credit-path C302) (list 302))
(check-expect (max-credit-path C221) (list 221 304))
(check-expect (max-credit-path C110) (list 110 210 310 319))

;(define (max-credit-path c) empty)

(@template Course ListOfCourse accumulator)

(define (max-credit-path c0)
  ;;INVARIENT: p; (listof Course) path to node
  ;;INVARIENT: p-wl; (listof (listof Course)) list of path to node
  ;;INVARIENT: rsf; (listof Course) p-wl with highest cred path
  ;;INVARIENT: max; Natural, highest cred in a path
  (local [(define (fn-for-course c c-wl p p-wl rsf max)
            (local [(define subs (course-dependents c)) 
                    (define tot-cred 
                      (foldr + 0 (map course-credits p)))] 
              (fn-for-loc (append subs c-wl)
                          (append (map (lambda (s)
                                         (append p (list s))) subs) p-wl)  
                          (if (> tot-cred max)
                              p
                              rsf)
                          (if (> tot-cred max)
                              tot-cred
                              max))))  

          (define (fn-for-loc c-wl p-wl rsf max)
            (cond [(empty? c-wl) (map course-number rsf)]
                  [else
                   (fn-for-course (first c-wl)
                                  (rest c-wl)
                                  (first p-wl)
                                  (rest p-wl)
                                  rsf
                                  max)]))] 
    
    (fn-for-course c0 empty (list c0) empty empty 0))) 








#;
(define (max-credit-path c0)
  ;; INVARIENT: rsf1; list of courses
  ;; INVARIENT: rsf2; max cred list of courses
  ;; INVARIENT: cred; sum of all credits in a tree 
  ;; INVARIENT: max; max cred
  (local [(define (fn-for-course c todo rsf1 cred max rsf2) 
            (fn-for-loc (append (course-dependents c) todo) 
                        (append rsf1 (list c))
                        (+ (course-credits c) cred)
                        (if (>= (+ (course-credits c) cred) max)
                            (+ (course-credits c) cred)
                            max)
                        (if (>= (+ (course-credits c) cred) max) 
                            rsf1
                            rsf2)))   

          (define (fn-for-loc loc rsf1 cred max rsf2)
            (cond [(empty? loc) (map course-number rsf2)]
                  [else
                   (fn-for-course (first loc)
                                  (rest loc)
                                  rsf1
                                  (if (empty? (rest loc))
                                      0
                                      cred)
                                  max
                                  rsf2)]))]
    (fn-for-course c0 (list c0) empty 0 0 empty)))





#;
(define (max-credit-path c0)
  ;; INVARIENT: rsf1; list of courses
  ;; INVARIENT: rsf2; max cred list of courses
  ;; INVARIENT: cred; sum of all credits in a tree
  ;; INVARIENT: max; max cred
  (local [(define (fn-for-course c todo rsf1 cred max rsf2)
            (fn-for-loc (append (course-dependents c) todo)
                        (if (empty? (course-dependents c))
                            empty
                            (cons c rsf1))
                        (if (empty? (course-dependents c))
                            0
                            (+ (course-credits c) cred))
                        (if (>= (+ (course-credits c) cred) max)
                            (+ (course-credits c) cred)
                            max)
                        (if (>= (+ (course-credits c) cred) max)
                            rsf1
                            rsf2))) 

          (define (fn-for-loc loc rsf1 cred max rsf2)
            (cond [(empty? loc) (map course-number rsf2)]
                  [else
                   (fn-for-course (first loc)
                                  (rest loc)
                                  rsf1
                                  cred
                                  max
                                  rsf2)]))]
    (fn-for-course c0 empty empty 0 0 empty)))











