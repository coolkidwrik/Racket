;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab-11-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
;; CPSC 110 - Graphs Lab

(@assignment labs/lab-11)

;; If you are:
;;   - A 110 or 107 student replace the first set of '???'s with your cwl.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   - A UBC Extended Learning student, replace the first set of ??? with
;;     the your email address as confirmed in the email you received from
;;     extended learning.  The handin password is also in that email.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   
(@cwl wriksen)

(@problem 1)

(@htdd Venue)
(define-struct vnu (nm los))
;; Venue is (make-vnu String (listof Streetway))
;; interp. a venue name and streetways leading from that venue


(@htdd Streetway)
(define-struct sw (nm dst))
;; Streetway is (make-sw String String)
;; interp. (make-sw s v) is a streetway s that leads to the destination venue v.


(define V-west-coast (make-vnu "West Coast Express"
                               (list (make-sw "Cordova Eastbound"
                                              "Shine Night Club"))))
(define V-shine-night (make-vnu "Shine Night Club"
                                (list(make-sw "Cordova Westbound"
                                              "West Coast Express")
                                     (make-sw "Richards Southbound"
                                              "WOW Tasty Food Delivery")
                                     (make-sw "Cordova Eastbound"
                                              "Rocket Reprographics"))))
(define V-rocket (make-vnu "Rocket Reprographics"
                           (list (make-sw "Cordova Eastbound"
                                          "Anti-Hero 13 Boutique"))))

(define V-anti-hero (make-vnu "Anti-Hero 13 Boutique" (list)))

(define V-vancouver-lookout
  (make-vnu "Vancouver Lookout"
            (list (make-sw "Seymour Northbound"
                           "West Coast Express")
                  (make-sw "Hastings Eastbound"
                           "WOW Tasty Food Delivery"))))

(define V-wow (make-vnu "WOW Tasty Food Delivery"
                        (list (make-sw "Hastings Westbound"
                                       "Vancouver Lookout")
                              (make-sw "Richards Southbound"
                                       "MacLeod's Books")
                              (make-sw "Hastings Eastbound"
                                       "Vancouver Film School"))))

(define V-film-school (make-vnu "Vancouver Film School"
                                (list (make-sw "Hastings Westbound"
                                               "WOW Tasty Food Delivery")
                                      (make-sw "Homer Northbound"
                                               "Rocket Reprographics")
                                      (make-sw "Hastings Eastbound"
                                               "BC Marijuana Party"))))
(define V-Marijuana
  (make-vnu "BC Marijuana Party"
            (list (make-sw "Hastings Westbound"
                           "Vancouver Film School")
                  (make-sw "Hamilton Southbound"
                           "London School of Hairdressing and Aesthetics"))))

(define V-F.M.Pizza (make-vnu "F.M Classic Pizza"
                              (list (make-sw "Seymour Northbound"
                                             "Vancouver Lookout")
                                    (make-sw "Pender Eastbound"
                                             "MacLeod's Books"))))
(define V-MacLeod (make-vnu "MacLeod's Books"
                            (list (make-sw "Pender Westbound"
                                           "F.M Classic Pizza")
                                  (make-sw "Richards Southbound"
                                           "D&S Bubble Tea")
                                  (make-sw "Pender Eastbound"
                                           "Capital Tax and Accounting"))))
(define V-cap-tax
  (make-vnu "Capital Tax and Accounting"
            (list (make-sw "Pender Westbound"
                           "MacLeod's Books")
                  (make-sw "Homer Northbound"
                           "Vancouver Film School")
                  (make-sw "Pender Eastbound"
                           "London School of Hairdressing and Aesthetics"))))

(define V-london (make-vnu "London School of Hairdressing and Aesthetics"
                           (list (make-sw "Pender Westbound"
                                          "Capital Tax and Accounting")
                                 (make-sw "Hamilton Southbound"
                                          "Candy Meister"))))

(define V-7-11 (make-vnu "7-11"
                         (list (make-sw "Seymour Northbound"
                                        "F.M Classic Pizza"))))

(define V-D&S (make-vnu "D&S Bubble Tea"
                        (list (make-sw "Dunsmuir Westbound"
                                       "7-11"))))
(define V-hydro (make-vnu "BC Hydro"
                          (list (make-sw "Dunsmuir Westbound"
                                         "D&S Bubble Tea")
                                (make-sw "Homer Northbound"
                                         "Capital Tax and Accounting"))))

(define V-candy
  (make-vnu "Candy Meister"
            (list (make-sw "Dunsmuir Westbound"
                           "BC Hydro")
                  (make-sw "Hamilton Northbound"
                           "London School of Hairdressing and Aesthetics"))))

(define VENUES
  (list V-west-coast
        V-shine-night
        V-rocket
        V-anti-hero
        V-vancouver-lookout
        V-wow
        V-film-school
        V-Marijuana
        V-F.M.Pizza
        V-MacLeod
        V-cap-tax
        V-london
        V-7-11
        V-D&S
        V-hydro
        V-candy))



#;
(define VENUES
  (list (make-vnu "Shine Night Club"
                  (list (make-sw "Richards Southbound"
                                 "WOW Tasty Food Delivery")
                        (make-sw "Cordova Eastbound"
                                 "Rocket Reprographics")))
        (make-vnu "Rocket Reprographics"
                  (list (make-sw "Cordova Eastbound"
                                 "Anti-Hero 13 Boutique")))
        (make-vnu "Anti-Hero 13 Boutique" (list))
        (make-vnu "WOW Tasty Food Delivery"
                  (list (make-sw "Hastings Eastbound"
                                 "Vancouver Film School")))
        (make-vnu "Vancouver Film School"
                  (list (make-sw "Hastings Westbound"
                                 "WOW Tasty Food Delivery")
                        (make-sw "Homer Northbound"
                                 "Rocket Reprographics")))))



;; Consider this to be a primitive function that comes with the data definitions
;; and that given a venue name it produces the corresponding venue.  Because
;; this consumes a string and generates a venue calling it will amount to a
;; generative step in a recursion through a graph of venues and streetways.
;; You must not edit this function, but you can experiment with it to see how
;; it works.

;;(@htdf lookup-venue)
;;(@signature String -> Venue)
(define (lookup-venue name)
  (local [(define (scan lst)
            (cond [(empty? lst) (error "No venue named " name)]
                  [else
                   (if (string=? (vnu-nm (first lst)) name)
                       (first lst)
                       (scan (rest lst)))]))]
    (scan VENUES)))



;; Complete the data definition by finishing the template for fn-for-venue
;; and fn-for-los in the encapsulated fn-for-map. DO NOT ADD ACCUMULATORS,
;; simply produce the template that corresponds to the type comments.

(@template encapsulated genrec Venue (listof Streetway) Streetway)

(define (fn-for-map v)
  (local [(define (fn-for-venue v)
            (... (vnu-nm v)
                 (fn-for-los (vnu-los v))))
          
          (define (fn-for-los los)
            (cond [(empty? los) (...)]
                  [else
                   (... (fn-for-sw (first los))
                        (fn-for-los (rest los)))]))
          
          (define (fn-for-sw sw)
            (... (sw-nm sw)
                 ;; lookup-venue is the generative step that makes the whole MR
                 ;; generative
                 (fn-for-venue (lookup-venue (sw-dst sw)))))]
    (fn-for-venue v)))



;; Problem 1:

;; Design a function that, given a venue and the name of a venue IN THAT ORDER,
;; produces true if the named venue can be reached from the given venue.
;; Call it can-get-to?.

(@htdf can-get-to?)
(@signature Venue String -> Boolean)
;; produces true if named venue can be reached from current venue
(check-expect (can-get-to?
               (make-vnu "Shine Night Club"
                         (list (make-sw "Richards Southbound"
                                        "WOW Tasty Food Delivery") 
                               (make-sw "Cordova Eastbound"
                                        "Rocket Reprographics")))
               "Shine Night Club") true)
(check-expect (can-get-to?
               (make-vnu "Shine Night Club"
                         (list (make-sw "Richards Southbound"
                                        "WOW Tasty Food Delivery")
                               (make-sw "Cordova Eastbound"
                                        "Rocket Reprographics")))
               "Anti-Hero 13 Boutique") true)
(check-expect (can-get-to?
               (make-vnu "Anti-Hero 13 Boutique" (list))
               "Shine Night Club") false)
(check-expect (can-get-to?
               V-candy
               "Shine Night Club") true)

;(define (can-get-to? v n) false)

(@template encapsulated genrec Venue (listof Streetway) Streetway accumulator
           try-catch)
 
(define (can-get-to? v n)
  ;; INVARIENT: path; (listof Venue) list of venues already visited
  (local [(define (fn-for-venue v path)
            (if (string=? (vnu-nm v) n)
                true
                (fn-for-los (vnu-los v) (cons v path)))) 
          
          (define (fn-for-los los path)
            (cond [(empty? los) false]
                  [else
                   (local [(define try (fn-for-sw (first los) path))]
                     (if (not (false? try))
                         try
                         (fn-for-los (rest los) path)))]))
          
          (define (fn-for-sw sw path)
            ;; lookup-venue is the generative step that makes the whole MR
            ;; generative
            (if (member (lookup-venue (sw-dst sw)) path)
                false
                (fn-for-venue (lookup-venue (sw-dst sw)) path)))]

    (fn-for-venue v empty))) 



;; Problem 2:

;; Design a function, called find-route, that given a venue and the name of
;; some other venue IN THAT ORDER, produces a list of names of streetways that
;; can get you to the named venue (i.e a list of driving directions), or false
;; if the named venue cannot be reached from the given venue.

;; Your find-route design does not need to be tail-recursive. But if you finish
;; early, try refactoring your solution so it is! You may want to keep a copy
;; of your non-tail-recursive solution to hand in.

(@problem 2)
(@htdf find-route)
(@signature Venue String -> (listof String) or false)
;; produce list of route to given venue, is possible to reach
(check-expect (find-route
               (make-vnu "Shine Night Club"
                         (list (make-sw "Richards Southbound"
                                        "WOW Tasty Food Delivery")
                               (make-sw "Cordova Eastbound"
                                        "Rocket Reprographics")))
               "Shine Night Club") empty)
(check-expect (find-route
               (make-vnu "Shine Night Club"
                         (list (make-sw "Richards Southbound"
                                        "WOW Tasty Food Delivery")
                               (make-sw "Cordova Eastbound"
                                        "Rocket Reprographics")))
               "Anti-Hero 13 Boutique")
              (list
               "Richards Southbound"
               "Hastings Westbound"
               "Seymour Northbound"
               "Cordova Eastbound"
               "Cordova Eastbound"
               "Cordova Eastbound")) 
(check-expect (find-route
               (make-vnu "Anti-Hero 13 Boutique" (list))
               "Shine Night Club") false)


;(define (find-route v n) false)
(@template encapsulated genrec Venue (listof Streetway) Streetway accumulator
           try-catch)

(define (find-route v n)
  ;; INVARIENT: path; (listof Venue) list of venues already visited
  ;; INVARIENT: rsf; (listof String) list of street-name in the branch so far 
  (local [(define (fn-for-venue v path rsf)
            (if (string=? (vnu-nm v) n)
                rsf
                (fn-for-los (vnu-los v) (cons v path) rsf))) 
          
          (define (fn-for-los los path rsf)
            (cond [(empty? los) false]
                  [else
                   (local [(define try (fn-for-sw (first los) path rsf))]
                     (if (not (false? try))
                         try
                         (fn-for-los (rest los) path rsf)))]))
          
          (define (fn-for-sw sw path rsf)
            ;; lookup-venue is the generative step that makes the whole MR
            ;; generative
            (if (member (lookup-venue (sw-dst sw)) path)
                false
                (fn-for-venue (lookup-venue (sw-dst sw))
                              path
                              (append rsf (list (sw-nm sw))))))]

    (fn-for-venue v empty empty)))








