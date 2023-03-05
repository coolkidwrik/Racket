;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pset-11-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)

(@assignment psets/pset-11); Do not edit or remove this tag

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
(@cwl ??? ???)

(@problem 1)

;;
;; Please read through the MODIFIED data definition introduced in Problem Set 6
;; for Treasure that can be found in a Scavenger Hunt. It has been modified
;; to add route durations to travel between treasure boxes.
;;

(@htdd Status)
;; Status is one of:
;; - "buried"
;; - "sunken"
;; - "locked"
;; interp. the status of an unopened treasure box
;;<examples are redundant for enumeration>

(@htdd Treasure)
(define-struct treasure (label amount difficulty status routes))
;; Treasure is (make-treasure String Natural Natural Status (listof Route))
;; interp. a treasure box with a label name,
;;         the number of gold coins contained in the treasure box,
;;         a rating of difficulty to find and open the treasure box between 1
;;         and 5, where 1 is very easy to find and open and 5 is very difficult,
;;         the status of the treasure box before it was opened,
;;         and a list of routes leading from this treasure box
;;         to other treasure boxes

(@htdd Route)
(define-struct route (duration destination))
;; Route is (make-route Natural String)
;; interp. a route leading from one treasure box to another       
;;         duration is the time in hours it will take to travel to it and
;;         destination is the name of the treasure box the route leads to

(define TREASURE-MAP
  (list (make-treasure "E" 32 3 "buried"  (list (make-route 3 "A")))
        (make-treasure "F" 10 2 "locked"  (list (make-route 7 "C")))
        (make-treasure "B" 6 5 "locked"   (list (make-route 9 "E")
                                                (make-route 15 "F")))
        (make-treasure "J" 1 1 "sunken"   (list (make-route 6 "I")))
        (make-treasure "H" 17 2 "sunken"  (list (make-route 15 "J")
                                                (make-route 4 "I")))
        (make-treasure "G" 52 3 "buried"  (list (make-route 2 "D")))
        (make-treasure "I" 100 5 "locked" empty)
        (make-treasure "D" 21 1 "sunken"  (list (make-route 8 "G")
                                                (make-route 13 "H")
                                                (make-route 9 "I")
                                                (make-route 11 "A")))
        (make-treasure "C" 41 4 "buried"  (list (make-route 6 "G")))
        (make-treasure "A" 7 1 "locked"   (list (make-route 12 "B")
                                                (make-route 7 "C")
                                                (make-route 27 "D")))))

;; Consider this to be a primitive function that comes with the data definitions
;; and that given a treasure name it produces the corresponding treasure.
;; Because this consumes a string and generates a treasure calling it will
;; amount to a generative step in a recursion through a graph of treasures and
;; routes. You must not edit this function, but you can experiment with it to
;; see how it works.

;;(@htdf lookup-treasure)
;;(@signature String -> Treasure)
(define (lookup-treasure name)
  (local [(define (scan lst)
            (cond [(empty? lst) (error "No treasure named " name)]
                  [else
                   (if (string=? (treasure-label (first lst)) name)
                       (first lst)
                       (scan (rest lst)))]))]
    (scan TREASURE-MAP)))

(define TE (lookup-treasure "E"))
(define TF (lookup-treasure "F"))
(define TB (lookup-treasure "B"))
(define TJ (lookup-treasure "J"))
(define TH (lookup-treasure "H"))
(define TG (lookup-treasure "G"))
(define TI (lookup-treasure "I"))
(define TD (lookup-treasure "D"))
(define TC (lookup-treasure "C"))
(define TA (lookup-treasure "A"))



(define (fn-for-treasure t)
  (local [(define (fn-for-status s)
            (cond [(string=? s "buried") (...)]
                  [(string=? s "sunken") (...)]
                  [(string=? s "locked") (...)]))

          (define (fn-for-treasure t)
            (... (treasure-label t)
                 (treasure-amount t)
                 (treasure-difficulty t)
                 (fn-for-status (treasure-status t))
                 (fn-for-lor (treasure-routes t))))

          (define (fn-for-lor lor)
            (cond [(empty? lor) (...)]
                  [else
                   (... (fn-for-route (first lor))
                        (fn-for-lor (rest lor)))]))

          (define (fn-for-route r)
            (... (route-duration r)
                 ;; lookup-treasure is the generative step that makes the whole 
                 ;; MR generative
                 (fn-for-treasure (lookup-treasure (route-destination r)))))]
    
    (fn-for-treasure t)))



;;
;; Design a function that consumes a treasure and produces the total amount
;; of gold that can be obtained by opening that treasure, and all treasures
;; reachable from that treasure.
;;
;; Your solution MUST be tail recursive.
;;

(@htdf reachable-gold) ;uncomment this when you begin problem 1
(@signature Treasure -> Natural)
;; produce total gold possible starting at first treasure
(check-expect (reachable-gold TI) 100)
(check-expect (reachable-gold TH) 118)


(@template genrec Treasure Route (listof Route) encapsulated accumulator)
 
(define (reachable-gold t)
  ;; Termination argument
  ;; Trivial case: (empty? todo), when todo becomes empty
  ;; Reduction step: (rest todo) reduces list by 1
  ;; Argument: everytime fn-for-lor is called, todo decreases by 1
  ;;           when all the nodes in graph have been visited, nothing will be added to todo
  ;;           so todo will eventually become empty
  ;;
  ;;Invarient: todo; (listof route), list of route yet to be visited
  ;;Invarient: visited; (listof Treasure), list of treasure already visited
  ;;Invarient: rsf; Natural, total gold in visited so far
  (local [(define (fn-for-treasure t todo visited rsf)
            (if (member t visited)
                (fn-for-lor todo visited rsf)
                (fn-for-lor (append (treasure-routes t) todo)
                            (cons t visited)
                            (+ (treasure-amount t)
                               rsf))))

          (define (fn-for-lor todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-route (first todo)
                                 (rest todo)
                                 visited
                                 rsf)]))

          (define (fn-for-route r todo visited rsf)
            (fn-for-treasure (lookup-treasure (route-destination r))
                             todo
                             visited
                             rsf))]
    
    (fn-for-treasure t empty empty 0)))



(@problem 2)
;;
;; Complete the function that lists the label names of all reachable treasure.
;;
;; Your solution MUST be tail recursive.
;;

(@htdf all-labels)
(@signature Treasure -> (listof String))
;; produce the label names of all reachable treasures
(check-expect (all-labels TI) (list "I"))
(check-expect (all-labels TH) (list "H" "J" "I"))
(check-expect (all-labels TA) (list "A" "B" "E" "F" "C"
                                    "G" "D" "H" "J" "I"))

;(define (all-labels t) empty)   ;stub

(@template genrec Treasure Route (listof Route) encapsulated accumulator)

(define (all-labels t)
  ;; Termination argument
  ;; Trivial case: (empty? todo), when todo becomes empty
  ;; Reduction step: (rest todo) reduces list by 1
  ;; Argument: everytime fn-for-lor is called, todo decreases by 1
  ;;           when all the nodes in graph have been visited, nothing will be added to todo
  ;;           so todo will eventually become empty
  ;;
  ;;Invarient: todo; (listof route), list of route yet to be visited
  ;;Invarient: visited; (listof Treasure), list of treasure already visited
  (local [(define (fn-for-treasure t todo visited)
            (if (member t visited)
                (fn-for-lor todo visited)
                (fn-for-lor (append (treasure-routes t) todo)
                            (append visited (list t)))))

          (define (fn-for-lor todo visited)
            (cond [(empty? todo) (map (λ (s) (treasure-label s)) visited)]
                  [else
                   (fn-for-route (first todo)
                                 (rest todo)
                                 visited)]))

          (define (fn-for-route r todo visited)
            (fn-for-treasure (lookup-treasure (route-destination r))
                             todo
                             visited))]
    
    (fn-for-treasure t empty empty)))




(@problem 3)
;;
;; Complete the function that lists the label names of all reachable treasures
;; when following only routes with a duration less than n hours long.
;;
;; Your solution MUST be tail recursive.
;;

(@htdf short-dur-reachable)
(@signature Treasure Number -> (listof String))
;; produce labels of all treasures reachable by following routes < n hr long
(check-expect (short-dur-reachable TI 12) (list "I"))
(check-expect (short-dur-reachable TH 4)  (list "H"))
(check-expect (short-dur-reachable TH 5)  (list "H" "I"))
(check-expect (short-dur-reachable TH 16) (list "H" "J" "I"))
(check-expect (short-dur-reachable TA 9) (list "A" "C" "G" "D"))

;(define (short-dur-reachable t n) empty)    ;stub

(@template genrec Treasure Route (listof Route) encapsulated accumulator)

(define (short-dur-reachable t n)
  ;; Termination argument
  ;; Trivial case: (empty? todo), when todo becomes empty
  ;; Reduction step: (rest todo) reduces list by 1
  ;; Argument: everytime fn-for-lor is called, todo decreases by 1
  ;;           when all the nodes in graph have been visited, nothing will be added to todo
  ;;           so todo will eventually become empty
  ;;
  ;;Invarient: todo; (listof route), list of route yet to be visited
  ;;Invarient: visited; (listof Treasure), list of treasure already visited
  ;;Invarient: dur; Natural, duration of route to current treasure
  (local [(define (fn-for-treasure t todo visited dur)
            (if (and (not (member t visited))
                     (< dur n))
                (fn-for-lor (append (treasure-routes t) todo)
                            (append visited (list t))
                            dur) 
                (fn-for-lor todo
                            visited
                            dur)))

          (define (fn-for-lor todo visited dur)
            (cond [(empty? todo) (map (λ (s) (treasure-label s)) visited)]
                  [else
                   (fn-for-route (first todo)
                                 (rest todo)
                                 visited
                                 dur)]))

          (define (fn-for-route r todo visited dur)
            (fn-for-treasure (lookup-treasure (route-destination r))
                             todo
                             visited
                             (route-duration r)))]
    
    (fn-for-treasure t empty empty 0)))
  




(@problem 4)
;;
;; Complete the design of a function that consumes two treasures and counts
;; the number of routes reachable starting at from that lead to to.
;;
;; Note: This is counting the number of routes found in treasure boxes where TF
;; is the destination, NOT the total number of paths between the two treasure
;; boxes. It is asking how many routes have TF as their destination (how many
;; arrows are pointing to TF).
;;
;; Examples:
;;
;; (num-lead-to TA TI) produces 3. This is because there are three routes that
;; are reachable from TA that lead to TI. These routes are the route leading
;; from TH to TI, the route leading from TJ to TI, and the route leading from
;; TD to TI.
;;
;; (num-lead-to TI TA) produces 0. Even though two routes lead to TA (the
;; route from TD to TA and the route from TE to TA), neither route can be
;; reached from TI so the function produces 0.
;;
;; Note that you can use the built-in function equal? to compare if two
;; treasures are equal. For example:
;;   - (equal? TE TF) produces false
;;   - (equal? TB TB) produces true
;;
;; Your solution MUST be tail recursive.
;;

(@htdf num-lead-to)
(@signature Treasure Treasure -> Natural)
;; count the number of reachable routes that lead to TF
(check-expect (num-lead-to TI TA) 0)
(check-expect (num-lead-to TJ TI) 1)
(check-expect (num-lead-to TH TI) 2)
(check-expect (num-lead-to TA TI) 3)
(check-expect (num-lead-to TF TA) 2)
(check-expect (num-lead-to TD TC) 2)

;(define (num-lead-to from to) 0)     ;stub

(@template genrec Treasure Route (listof Route) encapsulated accumulator)

(define (num-lead-to t des)
  ;; Termination argument
  ;; Trivial case: (empty? todo), when todo becomes empty
  ;; Reduction step: (rest todo) reduces list by 1
  ;; Argument: everytime fn-for-lor is called, todo decreases by 1
  ;;           when all the nodes in graph have been visited, nothing will be added to todo
  ;;           so todo will eventually become empty
  ;;
  ;;Invarient: todo; (listof route), list of route yet to be visited
  ;;Invarient: visited; (listof Treasure), list of treasure already visited
  ;;Invarient: rsf; number paths that lead to destination so far
  (local [(define (fn-for-treasure t todo visited rsf)
            (if (member t visited)
                (fn-for-lor todo
                            visited
                            rsf)
                (fn-for-lor (append (treasure-routes t) todo)
                            (cons t visited)
                            rsf)))

          (define (fn-for-lor todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-route (first todo)
                                 (rest todo)
                                 visited
                                 rsf)]))

          (define (fn-for-route r todo visited rsf)
            (fn-for-treasure (lookup-treasure (route-destination r))
                             todo
                             visited
                             (if (equal?
                                  (lookup-treasure (route-destination r))
                                  des)
                                 (+ 1 rsf)
                                 rsf)))]
    
    (fn-for-treasure t empty empty 0)))

(@problem 5)
;;
;; Complete the design of a function that consumes a treasure and the label
;; of another treasure, and produces the time in hours of route durations
;; it would take to travel from t to the treasure labeled s. This function
;; produces the total duration of the routes followed as soon as it finds a
;; treasure labeled s. The function produces false if there is no way of
;; reaching a treasure with the given label.
;;

(@htdf route-to)
(@signature Treasure String -> Natural or false)
;; produce the total duration traveled on route to s from t, false if not found
(check-expect (route-to TE "X") false)
(check-expect (route-to TE "E") 0)
(check-expect (route-to TE "A") 3)
(check-expect (route-to TH "I") 21)
(check-expect (route-to TA "G") 40)
(check-expect (route-to TA "J") 70)

;(define (route-to t s) false)     ;stub

(@template genrec Treasure Route (listof Route) encapsulated accumulator)

(define (route-to t l)
  ;; Termination argument
  ;; Trivial case: (empty? todo), when todo becomes empty
  ;; Reduction step: (rest todo) reduces list by 1
  ;; Argument: everytime fn-for-lor is called, todo decreases by 1
  ;;           when all the nodes in graph have been visited, nothing will be added to todo
  ;;           so todo will eventually become empty
  ;;
  ;;Invarient: todo; (listof route), list of route yet to be visited
  ;;Invarient: p; (listof route), path to current node
  ;;Invarient: p-wl; (listof (listof route)), paths to each node in todo
  ;;Invarient: visited; (listof Treasure), list of treasure already visited
  (local [(define (fn-for-treasure t todo p p-wl visited)
            (local [(define subs (treasure-routes t))
                    (define dur (foldr + 0 (map route-duration p)))]
              (cond [(equal? (treasure-label t) l)
                     dur]
                    [(member t visited)
                     (fn-for-lor todo
                                 p-wl
                                 visited)]
                    [else
                     (fn-for-lor (append subs todo)
                                 (append (map (λ (s) (append p (list s))) subs)
                                         p-wl)
                                 (cons t visited))]))) 

          (define (fn-for-lor todo p-wl visited)
            (cond [(empty? todo) false]
                  [else
                   (fn-for-route (first todo)
                                 (rest todo)
                                 (first p-wl)
                                 (rest p-wl)
                                 visited)]))

          (define (fn-for-route r todo p p-wl visited)
            (fn-for-treasure (lookup-treasure (route-destination r))
                             todo
                             p
                             p-wl
                             visited))]
    
    (fn-for-treasure t empty empty empty empty))) 







#;
(define (route-to t l)
  ;; Termination argument
  ;; Trivial case: (empty? todo), when todo becomes empty
  ;; Reduction step: (rest todo) reduces list by 1
  ;; Argument: everytime fn-for-lor is called, todo decreases by 1
  ;;           when all the nodes in graph have been visited, nothing will be added to todo
  ;;           so todo will eventually become empty
  ;;
  ;;Invarient: todo; (list of route), list of route yet to be visited
  ;;Invarient: visited; (list of Treasure), list of treasure already visited
  ;;Invarient: rsf; Natural, duration of route so far
  (local [(define (fn-for-treasure t todo visited rsf)
            (if (equal? (treasure-label t) l)
                rsf
                (if (member t visited)
                    (fn-for-lor todo
                                visited
                                rsf)
                    (fn-for-lor (append (treasure-routes t) todo)   
                                (cons t visited)
                                rsf))))

          (define (fn-for-lor todo visited rsf) 
            (cond [(empty? todo) false]
                  [else
                   (fn-for-route (first todo)
                                 (rest todo)
                                 visited
                                 rsf)])) 

          (define (fn-for-route r todo visited rsf)
            (fn-for-treasure (lookup-treasure (route-destination r))
                             todo
                             visited
                             (+ rsf (route-duration r))))]
    
    (fn-for-treasure t empty empty 0)))



#;
(filter (λ (n)
          (not (member
                (lookup-treasure
                 (route-destination n))
                visited)))
        (append (treasure-routes t) todo))



(@problem 6)
;;
;; Complete the design of a function that consumes a treasure and the label
;; of another treasure, and produces the MINIMUM time in hours of all of the
;; possible routes that could be taken to travel from t to the treasure
;; labeled s. The function produces false if there is no way of reaching the
;; a treasure with the given label.
;;

(@htdf min-route-to)
(@signature Treasure String -> Natural or false)
;; produce the min duration traveled on route to s from t, false if not found
(check-expect (min-route-to TE "X") false)
(check-expect (min-route-to TE "E") 0)
(check-expect (min-route-to TE "A") 3)
(check-expect (min-route-to TH "I") 4)
(check-expect (min-route-to TA "G") 13)
(check-expect (min-route-to TA "J") 43)

;(define (min-route-to t s) false)     ;stub

(@template genrec Treasure Route (listof Route) encapsulated accumulator)
#;
(define (min-route-to t l)
  ;; Termination argument
  ;; Trivial case: (empty? todo), when todo becomes empty
  ;; Reduction step: (rest todo) reduces list by 1
  ;; Argument: everytime fn-for-lor is called, todo decreases by 1
  ;;           when all the nodes in graph have been visited, nothing will be added to todo
  ;;           so todo will eventually become empty
  ;;
  ;;Invarient: todo; (listof route), list of route yet to be visited
  ;;Invarient: p; (listof route), path to current node
  ;;Invarient: p-wl; (listof (listof route)), paths to each node in todo
  ;;Invarient: visited; (listof Treasure), list of treasure already visited
  ;;Invarient: min; (listof Natural), list of durations
  ;;Invarient: found; Boolean, remains true if found
  (local [(define (fn-for-treasure t todo p p-wl visited min found)
            (local [(define subs (treasure-routes t))
                    (define dur (foldr + 0 (map route-duration p)))]
              (cond [(equal? (treasure-label t) l)
                     (fn-for-lor todo
                                 p-wl
                                 visited
                                 (cons dur min)
                                 true)]
                    [(member t visited) 
                     (fn-for-lor todo
                                 p-wl
                                 visited
                                 min
                                 found)] 
                    [else
                     (fn-for-lor (append subs todo)
                                 (append (map (λ (s) (append p (list s))) subs)
                                         p-wl)
                                 (cons t visited)
                                 min
                                 found)])))  

          (define (fn-for-lor todo p-wl visited min found)
            (cond [(empty? todo) (if found
                                     min
                                     false)]
                  [else
                   (fn-for-route (first todo)
                                 (rest todo)
                                 (first p-wl)
                                 (rest p-wl)
                                 visited
                                 min
                                 found)])) 

          (define (fn-for-route r todo p p-wl visited min found)
            (fn-for-treasure (lookup-treasure (route-destination r))
                             todo
                             p
                             p-wl
                             visited
                             min
                             found))]
    
    (fn-for-treasure t empty empty empty empty empty false))) 
 

#;
(define (min-route-to t s)
  ;; r-wl is (listof Route); list of routes remaining
  ;; dur is Natural; the duration of the path so far
  ;; d-wl is (listof Natural); tandem worklist with r-wl, lengths of paths
  ;; path is (listof Treasure); current path
  ;; p-wl is (listof (listof Treasure); tandem worklist, list of paths
  ;; f-dur is (listof Natural); list of all completed paths
  ;;
  ;; Base Case: (empty? r-wl) - when the worklist is empty
  ;; Reduction Step: (fn-for-route (...) (rest r-wl) (...)) - rest called
  ;; Argument: As an item is removed from fn-for-route every time, and will
  ;;           eventually become empty
  (local [(define (fn-for-treasure t r-wl dur d-wl path p-wl f-dur)
            (cond [(string=? (treasure-label t) s)
                   (fn-for-lor r-wl d-wl p-wl (cons dur f-dur))]
                  [(member? t path)
                   (fn-for-lor r-wl d-wl p-wl f-dur)] 
                  [else
                   (fn-for-lor (append (treasure-routes t)
                                       r-wl)
                               (append (map (λ (a) dur)
                                            (treasure-routes t))
                                       d-wl)
                               (append (map (λ (a) (append path (list t)))
                                            (treasure-routes t))
                                       p-wl)
                               f-dur)]))

          (define (fn-for-lor r-wl d-wl p-wl f-dur)
            (cond [(and (empty? r-wl) (empty? f-dur)) false]
                  [(empty? r-wl) (foldr (λ (a b) (if (< a b) a b))
                                        (first f-dur)
                                        f-dur)]
                  [else
                   (fn-for-route (first r-wl) (rest r-wl)
                                 (first d-wl) (rest d-wl)
                                 (first p-wl) (rest p-wl)
                                 f-dur)]))

          (define (fn-for-route r r-wl dur d-wl path p-wl f-dur)
            (fn-for-treasure (lookup-treasure (route-destination r))
                             r-wl
                             (+ (route-duration r) dur)
                             d-wl
                             path
                             p-wl
                             f-dur))]

    (fn-for-treasure t empty 0 empty empty empty empty)))







#;
[(equal? (treasure-label t) l)
 dur]