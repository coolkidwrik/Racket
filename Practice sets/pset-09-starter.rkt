;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pset-09-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR
;; PARTNER
;;

(require spd/tags)

(@assignment psets/pset-09);Do not edit or remove this tag

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

;;
;; THIS IS THE MOST CHALLENGING PROBLEM SET SO FAR THIS TERM.  PLEASE BE
;; SURE YOU WORK THROUGH IT CAREFULLY THOUGH.  110 FINAL EXAMS OFTEN INCLUDE
;; PROBLEMS BASED ON PROBLEM SETS 9, 10, OR 11.
;;
;; ALSO NOTE THAT THE AUTOGRADER COOL DOWN IS 1 HOUR FOR THIS PROBLEM SET.
;;
;; THERE WILL BE A SPECIAL PINNED THREAD ON PIAZZA IN WHICH WE WILL ANSWER
;; QUESTIONS ABOUT THIS PROBLEM SET.
;;
;; In this problem set you are going to work on one of the toughest problems
;; we face running 110 - scheduling of TAs.  As you may know, we have about
;; 45 TAs, and we have to schedule them for many labs, 3 lectures, and office
;; hours.  We solved this by writing a schedule solver, and that's what you
;; are going to do for this problem set.
;;
;; We are making it a little easier for you, in that all you will be having
;; to deal with is labs. 
;;
;; We are giving you two key data definitions, as well as some examples of
;; that data.  We are also giving you a wish list entry for the main solve
;; function you have to design.  The function consumes a list of TAs and a
;; list of lab slots to fill.  It produces a list of assignments.  So, for
;; example, in the following very simple case, where there are two slots,
;; and two TAs, you get an assignment of the TAs to those slots.
;;
;; (solve (list (make-slot "A" 1) (make-slot "B" 1))
;;        (list (make-ta "Ali" (list "B"))
;;              (make-ta "Azi" (list "A"))))  ==>
;;
;; (list (list "Ali" "B") (list "Azi" "A"))
;;
;; In this simple example there was only one possible assignment. But in
;; general there might be more than one assignment, or it might be impossible
;; to generate an assignment.

;; By now you know enough about search to know that the first thing you need
;; to do is figure out the search space.  What does the tree look like? What
;; information do you have to represent at each node in the tree?  How do you
;; generate the next nodes in the tree? How do you tell you have a solution?
;; Although this function ends up producing just a list of assignments, does
;; it need more than just the assignments so far at each node in the tree?
;; What other information do you need to represent at each node?
;;
;; As you consider the search tree, note that a TA can work more than one slot,
;; but a slot is filled by one TA.  So once you assign a TA to a slot that slot
;; is done.
;;
;; As usual, anything we give you below you must not change.  The autograder
;; will want to call solve with arguments as described below.  Also note that
;; we give you just a few small data examples for illustration. You will want
;; to test your function with additional examples.
;;

(@problem 1)

;; Constants:

(define MAX-SLOTS-PER-TA 3) ;this is the max number of labs a TA can work

;; Data definitions:

(@htdd Slot)
(define-struct slot (lab n))
;; Slot is (make-slot String Natural)
;; interp. the name of a lab - "A", "B", etc.
;;         the lab's slot number - 1, 2, 3 etc.
;; CONSTRAINT:
;;  a list of slots representing positions that need to be filled
;;  should not contain duplicate slots (slots with same lab and n)
;;  
(define SLOTS1
  (list (make-slot "A" 1) (make-slot "A" 2)   ;A needs 2 TAs
        (make-slot "B" 1)                     ;B needs 1
        (make-slot "C" 1) (make-slot "C" 2))) ;C needs 2

(@htdd TA)
(define-struct ta (name avail))
;; TA is (make-ta String (listof String))
;; interp. A TA with their name and the labs times they are free to work.
;; CONSTRAINT: 
;;  a list of TAs should not contain TAs with duplicate names

(define TAS1
  (list (make-ta "Ali" (list "A" "B"))
        (make-ta "Ari" (list     "B" "C"))
        (make-ta "Azi" (list "A"     "C"))))


(@htdd Task)
;; Tasks is (listof String)
;; interp. list of TA assigned, followed by task letter
;; CONSTRAINT: 
;;  cannot have a list with more than two items,
;;  minimum of MAX-SLOTS-PER-TA lists with the same TA

(define T1 (list "" "A"))

(define LT1
  (list (list "" "A")
        (list "" "A")
        (list "" "B")
        (list "" "C")
        (list "" "C")))
(define LT2
  (list (list "Ali" "A")
        (list "Azi" "A")
        (list "Ali" "B")
        (list "Ari" "C")
        (list "Azi" "C")))

#|
(@htdd Jobs)
(define-struct jobs (t lota))
;; Jobs is (make-jobs (listof Task) (listof TA))
;; interp. the list of tasks - (list "" "A"), etc
;;         the list of TA - (make-ta "Ali" (list "A" "B")), etc
;;

(define J1 (make-jobs LT1 TAS1))

(define J2 (make-jobs LT2 (list)))
|#



(@htdf solve)
(@signature (listof Slot) (listof TA) -> (listof (listof String)) or false)
;; produce assignments(list (list "ta name" "lab name")...)
(check-expect (solve empty empty) empty)
(check-expect (solve (list (make-slot "A" 1) (make-slot "B" 1))
                     (list (make-ta "Ali" (list "B"))
                           (make-ta "Azi" (list "A"))))
              (list (list "Ali" "A") (list "Ali" "B")))

;(define (solve slots tas) empty)  ;stub

(@template genrec arb-tree try-catch encapsulated)


(define (solve slots tas)
  (local [(define tasks (slots->tasks slots))]

    (local [(define (solve--tsk t ta)
              (if (solved? t)
                  t
                  (solve--tsks (next-vals-t t ta) ta)))

            (define (solve--tsks tsks ta)
              (cond [(empty? tsks) false]
                    [else
                     (local [(define try (solve--tsk (first tsks) ta))]
                       (if (not (false? try))
                           try
                           (solve--tsks (rest tsks) ta)))]))]

      (solve--tsk tasks tas))))






(@htdf slots->tasks)
(@signature (listof Slot) -> (listof Task))
;; converts list of slot to list of task
(check-expect (slots->tasks SLOTS1) LT1) 

;(define (slots->tasks s) empty)
(@template (listof Slot))

(define (slots->tasks s)
  (cond [(empty? s) empty]
        [else
         (local [(define (make-task-list fl)
                   (list "" (slot-lab fl)))]
           (cons (make-task-list (first s))
                 (slots->tasks (rest s))))]))


(@htdf solved?)
(@signature (listof Task) -> Boolean)
;; produces true if first of each task is not ""
(check-expect (solved? LT1) false)
(check-expect (solved? LT2) true)

;(define (solved? tsks) false)

(@template use-abstract-fn)

(define (solved? tsks)
  (local [(define (first-empty? lt)
            (not (string=? (first lt) "")))]
    (andmap first-empty? tsks)))


(@htdf check-ta-nums)
(@signature (listof Task) String -> Natural)
;; produces the number of times TA is in the (listof Task)
(check-expect (check-ta-nums LT1 "Ali") 0)
(check-expect (check-ta-nums LT2 "Ali") 2)
(check-expect (check-ta-nums LT2 "Ari") 1)


(@template (listof Task))

(define (check-ta-nums tsks name)
  (cond [(empty? tsks) 0]
        [else
         (local [(define (fn-for-ft t)
                   (if (string=? (first t) name)
                       1
                       0))]
           (+   (fn-for-ft (first tsks))
                (check-ta-nums (rest tsks) name)))]))

(@htdf <TA?)
(@signature (listof Task) String -> Boolean)
;; produces true if there are less than MAX-SLOTS-PER-TA TAs
(check-expect (<TA? LT1 "Ali") #t)
(check-expect (<TA? LT2 "Ali") #t)
(check-expect (<TA? LT2 "Ari") #t)
(check-expect (<TA?
               (list (list "Ali" "A")
                     (list "Ali" "A")
                     (list "Ali" "B")
                     (list "Ari" "C")
                     (list "Azi" "C")) "Ali") #t)
(check-expect (<TA?
               (list (list "Ali" "A")
                     (list "Ali" "A")
                     (list "Ali" "B")
                     (list "Ari" "C")
                     (list "Ali" "C")) "Ali") #f)

(@template fn-composition)

(define (<TA? tsks name)
  (<= (check-ta-nums tsks name) MAX-SLOTS-PER-TA))



(@htdf one-slot-per-ta?)
(@signature (listof Task) -> Boolean)
;; produces true if one TA only has one lab
(check-expect (one-slot-per-ta? LT1) true)
(check-expect (one-slot-per-ta? LT2) true) 
(check-expect (one-slot-per-ta?
               (list (list "Ali" "A")
                     (list "Ali" "A")
                     (list "" "B")
                     (list "" "C")
                     (list "" "C"))) false)

(@template (listof Task))


(define (one-slot-per-ta? tsks)
  (cond [(empty? tsks) true]
        [else
         
         (local [(define (fn-for-t t)
                   (if (string=? "" (first t))
                       true
                       (local [(define (samelist? t2)
                                 (equal? t t2))]
                         (not (ormap samelist? (rest tsks))))))]
           
           
           (and (fn-for-t (first tsks))
                (one-slot-per-ta? (rest tsks))))])) 












(@htdf next-vals-t)
(@signature (listof Task) (listof TA) -> (listof (listof Task)))
;;produces next valid list of tasks
(check-expect (next-vals-t (list (list "" "A"))
                           (list (make-ta "Ali" (list "A" "B"))))
              (list (list (list "Ali" "A"))))
(check-expect (next-vals-t LT1
                           TAS1)
              (list (list (list "Ali" "A")
                          (list "" "A")
                          (list "" "B")
                          (list "" "C")
                          (list "" "C"))
                    (list (list "Azi" "A")
                          (list "" "A")
                          (list "" "B")
                          (list "" "C")
                          (list "" "C"))))
(check-expect (next-vals-t
               (list (list "Ali" "A")
                     (list "" "A")
                     (list "" "B")
                     (list "" "C")
                     (list "" "C"))
               TAS1)
              (list (list (list "Ali" "A")
                          (list "Azi" "A")
                          (list "" "B")
                          (list "" "C")
                          (list "" "C"))))


;(define (next-vals-t tsks ta) empty)

(@template fn-composition)

(define (next-vals-t tsks ta)
  (only-valid (branches tsks ta) ta)) 





(@htdf only-valid)
(@signature (listof (listof Task)) -> (listof (listof Task)))
;; only outputs the list of (list of task) that are valid
(check-expect (only-valid
               (list (list (list "Ali" "A")
                           (list "" "A")
                           (list "" "B")
                           (list "" "C")
                           (list "" "C"))
                     (list (list "Ari" "A")
                           (list "" "A")
                           (list "" "B")
                           (list "" "C")
                           (list "" "C"))
                     (list (list "Azi" "A")
                           (list "" "A")
                           (list "" "B")
                           (list "" "C")
                           (list "" "C")))
               TAS1)
              (list (list (list "Ali" "A")
                          (list "" "A")
                          (list "" "B")
                          (list "" "C")
                          (list "" "C"))
                    (list (list "Azi" "A")
                          (list "" "A")
                          (list "" "B")
                          (list "" "C")
                          (list "" "C"))))

(check-expect (only-valid
               (list (list (list "Ali" "A")
                           (list "Ali" "A")
                           (list "" "B")
                           (list "" "C")
                           (list "" "C"))
                     (list (list "Ali" "A")
                           (list "Ari" "A")
                           (list "" "B")
                           (list "" "C")
                           (list "" "C"))
                     (list (list "Ali" "A")
                           (list "Azi" "A")
                           (list "" "B")
                           (list "" "C")
                           (list "" "C")))
               TAS1)
              (list (list (list "Ali" "A")
                          (list "Azi" "A")
                          (list "" "B")
                          (list "" "C")
                          (list "" "C")))) 

;(define (only-valid lolot tas) empty) 

(@template (listof (listof Task)) fn-composition)


(define (only-valid lolot tas)
  (cond [(empty? lolot) empty]
        [else
         (if  (and (valid-tasks? (first lolot) tas)
                   (one-slot-per-ta? (first lolot)))
              (cons (first lolot) (only-valid (rest lolot) tas))
              (only-valid (rest lolot) tas))]))  





(@htdf valid-tasks?)
(@signature (listof Task) (listof TA) -> Boolean)
;; produces true if the list of tasks is valid
(check-expect (valid-tasks? (list (list "Ali" "A")
                                  (list "Ali" "A")
                                  (list "" "B")
                                  (list "" "C")
                                  (list "" "C"))
                            TAS1) true)
(check-expect (valid-tasks? (list (list "Ali" "A")
                                  (list "Ali" "A")
                                  (list "Ali" "B")
                                  (list "Ali" "C")
                                  (list "" "C"))
                            TAS1) false)
(check-expect (valid-tasks? LT2 TAS1) true) 

;(define (valid-tasks? tsks lota) false)

(@template (listof TA))

(define (valid-tasks? tsks tas)
  (cond [(empty? tas) true]
        [else
         (local [(define (fn-for-ta ta)
                   (<TA? tsks (ta-name ta)))]
           
           (and (fn-for-ta (first tas))
                (valid-tasks? tsks (rest tas))))]))




#;
(define (valid-tasks? tsks tas)
  (cond [(empty? tas) empty]
        [else
         (and (<TA? tsks (ta-name (first tas)))
              (valid-tasks? tsks (rest tas)))]))









(@htdf branches)
(@signature (listof Task) (listof TA) -> (listof (listof Task)))
;; outputs all next results
(check-expect (branches (list (list "" "A"))
                        (list (make-ta "Ali" (list "A" "B"))))
              (list (list (list "Ali" "A"))))
(check-expect (branches LT1
                        TAS1)
              (list (list (list "Ali" "A")
                          (list "" "A")
                          (list "" "B")
                          (list "" "C")
                          (list "" "C"))
                    (list (list "Ari" "A")
                          (list "" "A")
                          (list "" "B")
                          (list "" "C")
                          (list "" "C"))
                    (list (list "Azi" "A")
                          (list "" "A")
                          (list "" "B")
                          (list "" "C")
                          (list "" "C"))))

#|
(check-expect (branches LT1
                        TAS1)
              (list (list (list "Ali" "A")
                          (list "" "A")
                          (list "Ali" "B")
                          (list "" "C")
                          (list "" "C"))
                    (list (list "" "A")
                          (list "Ali" "A")
                          (list "Ali" "B")
                          (list "" "C")
                          (list "" "C"))
                    (list (list "Ali" "A")
                          (list "" "A")
                          (list "" "B")
                          (list "" "C")
                          (list "" "C"))
                    (list (list "" "A")
                          (list "Ali" "A")
                          (list "" "B")
                          (list "" "C")
                          (list "" "C"))
                    (list (list "" "A")
                          (list "" "A")
                          (list "Ali" "B")
                          (list "" "C")
                          (list "" "C"))
                    (list (list "" "A")
                          (list "" "A")
                          (list "" "B")
                          (list "" "C")
                          (list "" "C"))))
|#

;(define (branches lot tas) empty)

(@template (listof Task))


(define (branches lot tas)
  (cond [(or (empty? tas) (empty? lot)) empty] 
        [else

         (local [(define (fn-for-t t ta)
                   (if (not (string=? "" (first (first t)))) 

                       (cons (first t)
                             (cons (cons (ta-name ta)
                                         (rest (first (rest lot))))
                                   (rest (rest lot))))    

                       
                       (cons (cons (ta-name ta)  
                                   (rest (first lot))) (rest lot))))]
           
               
           (cons (fn-for-t lot (first tas))
                 (branches lot (rest tas))))]))   





#|
(define (branches lot tas)
  (cond [(empty? lot) empty]
        [else
         (cons (input-ta (first lot) (first tas))
               (branches (rest lot) tas))]))


(@htdf input-ta)
(@signature Task TA -> (listof Task))
;; produces list of task that meet TA requirements

(@template Task)

(define (input-ta tsk ta)
  (cond [(empty? tsk) empty]
        [else
         (... (first ))]))

|#





#|
(@htdf valid-time?)
(@signature (listof Task) (listof TA) -> Boolean)
;; produces true if all TA are in available times
(check-expect (valid-time?
               (list (list "Ali" "A")
                     (list "" "A")
                     (list "Ali" "B")
                     (list "" "C")
                     (list "" "C"))
               TAS1) true)
(check-expect (valid-time?
               (list (list "Ari" "A")
                     (list "" "A")
                     (list "" "B")
                     (list "" "C")
                     (list "" "C"))
               TAS1) false)

(@template (listof Task))

(define (valid-time? tsks tas)
  (cond [(empty? tsks) true]
        [else
         (local [(define (fn-for-t tsk)
                   (ormap (lambda (n) (string=? (first (rest tsk)) n))
                              (ta-avail (first tas)))
                       )] 

           (and  (fn-for-t (first tsks))
                 (valid-time? (rest tsks) tas)))]))

|#






#;
(define (branches lot tas)
  (cond [(or (empty? tas) (empty? lot)) empty] 
        [else

         (local [(define (fn-for-t t ta)
                   (if (not (string=? (first t) ""))

                       (cons (cons (first (first lot))  
                                   (rest (first lot))) (rest lot))
                       
                       (cons (cons (ta-name ta)  
                                   (rest (first lot))) (rest lot))))]
           
               
           (cons (fn-for-t (first lot) (first tas))
                 (branches (first (rest lot)) (rest tas))))]))  



#;
(define (branches lot tas)
  (cond [(or (empty? tas) (empty? lot)) empty] 
        [else
         (if (not (string=? (first (first lot)) ""))
             (cons (first lot) (branches (rest lot) tas))
             
             (local [(define (fn-for-t t)
                       (cons (cons (ta-name (first tas))  
                                   (rest (first t))) (rest lot)))]
               
               (cons (fn-for-t lot)
                     (branches lot (rest tas)))))])) 







#;
(define (next-vals-t tsks tas)
  (cond [(or (empty? tas) (empty? tsks)) empty]
        [else
         (if (not (string=? (first (first tsks)) ""))
             (cons (first tsks) (next-vals-t (rest tsks) tas))
             
             (if (ormap (lambda (n) (string=?
                                     (first (rest (first tsks))) n))
                        (ta-avail (first tas)))
                 (cons (list (ta-name (first tas))
                             (first (rest (first tsks))))
                       (rest tsks))
                 (next-vals-t tsks (rest tas))))])) 



#|
(@htdf next-vals-ta)
(@signature (listof Task) (listof TA) -> (listof TA))
;;produces next valid list of tasks

(define (next-vals-ta tsk lota) empty)
|#








