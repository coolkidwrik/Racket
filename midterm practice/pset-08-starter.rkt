;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pset-08-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)

(@assignment psets/pset-08); Do not edit or remove this tag

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
;; Design a function called sum-squares that consumes a list of naturals,
;; and produces the sum of squaring all of the naturals in the list.
;;
;; For example: (sum-squares (list 5 2 4)) produces 45.
;;
;; Your function definition must use built-in abstract functions.
;; For full marks it must be a composition of exactly 2 built-in
;; abstract functions.
;;

(@htdf sum-squares) ; uncomment this when you starter problem 1
(@signature (listof Natural) -> Natural)
;; produces the sum of all squares of numbers in list
(check-expect (sum-squares (list 5 2 4)) 45)

;(define (sum-squares lon) 0)

(@template fn-composition use-abstract-fn)

(define (sum-squares lon)
  (foldl + 0 (map sqr lon)))



(@problem 2)
;;
;; Complete the design of the following function.
;;
;; Your function definition must use built-in abstract functions.
;; For full marks it must be a composition of exactly 2 built-in
;; abstract functions.
;;
;; NOTE: Looking up the function string-contains? in the help desk
;; might be helpful...

(@htdf all-a-contain?)
(@signature (listof String) String -> Boolean)
;; determine if all strings that begin with the letter 'a' contain word w
(check-expect (all-a-contain? empty "word") true)
(check-expect (all-a-contain? (list "bicycle" "car" "train" "walk")
                              "transportation")
              true)
(check-expect (all-a-contain? (list "assignment") "eight") false)
(check-expect (all-a-contain? (list "attend" "radio" "antenna" "listen" "study")
                              "ten") 
              true)
(check-expect (all-a-contain? (list "art" "bin" "action" "tin") "in") false)
(check-expect (all-a-contain? (list "art" "artery" "ban" "action" "tin")
                              "art")
              false)

;(define (all-a-contain? los w) false)  ;stub

(@template fn-composition use-abstract-fn)

#;
(define (all-a-contain? los w)
  (local [(define (a-start? wrd) (string=? (substring wrd 0 1) "a"))
          (define (contains? wrd) (string-contains? w wrd))]
    (andmap contains? (filter a-start? los))))


(define (all-a-contain? los w)
  (andmap (λ (s) (string-contains? w s))  
          (filter (λ (n) (equal? (substring n 0 1) "a")) los))) 
          

 


;;
;; Before completing problems 3, 4, and 5, please familiarize
;; yourself with the provided data definition for a Cat.
;; An example (listof Cat) is also provided.
;;

(@htdd Cat)
(define-struct cat (name color age))
;; Cat is (make-cat String Color Natural)
;; interp. a cat with a name, coat color, and age (in years)
(define C1 (make-cat "Whiskers" "brown" 13))
(define C2 (make-cat "Si"       "black"  4))
(define C3 (make-cat "Am"       "white"  4))
(define C4 (make-cat "Meow"     "brown"  7))
(define C5 (make-cat "Garfield" "orange" 8))
(define C6 (make-cat "Sassy"    "brown"  6))

(define (fn-for-cat c)
  (... (cat-name c)
       (cat-color c)
       (cat-age c)))

(define LOC1 (list C1 C2 C3 C4 C5 C6)) ; to save time with check-expects


(@problem 3)
;; 
;; Design a function called brown-cat-names that consumes a list of cats,
;; and produces a list of the names of the cats that are brown in color.
;;
;; For example: (brown-cat-names LOC1)
;;    produces: (list "Whiskers" "Meow" "Sassy")
;;
;; Your function definition must use built-in abstract functions.
;; For full marks it must be a composition of exactly 2 built-in
;; abstract functions. 
;;
(@htdf brown-cat-names)
(@signature (listof Cat) -> (listof String))
;; produces list of cat names that are brown
(check-expect (brown-cat-names empty) empty)
(check-expect (brown-cat-names LOC1) (list "Whiskers" "Meow" "Sassy"))

;(define (brown-cat-names loc) empty)

(@template fn-composition use-abstract-fn)

(define (brown-cat-names loc)
  (map cat-name
       (filter (λ (c) (string=? "brown" (cat-color c))) loc)))



(@problem 4)
;; 
;; Design a function called count-short-names that consumes a list of cats and a
;; natural n, and produces a count of the number of cats with names less than n
;; characters long.
;;
;; For example: (count-short-names LOC1 5) produces 3, since
;;    "Si", "Am", and "Meow" all have names less than 5 characters long.
;;
;; Your function definition must use built-in abstract functions.
;; For full marks it must be a composition of exactly 3 built-in
;; abstract functions.
;; 

(@htdf count-short-names)
(@signature (listof Cat) Natural -> Natural)
;; produce number of cats with names shorter than n long
(check-expect (count-short-names empty 5) 0)
(check-expect (count-short-names LOC1 5) 3)

;(define (count-short-names loc n) 0)

(@template fn-composition use-abstract-fn)

(define (count-short-names loc n)
  (foldl + 0
         (map (λ (s) 1)
              (filter (λ (c) (< (string-length (cat-name c)) n)) loc))))




(@problem 5)
;;
;; Complete the design of the following function.
;;
;; Your function definition must use built-in abstract functions.
;; For full marks it must be a composition of exactly 3 built-in
;; abstract functions.
;;

(@htdf sum-age-c)
(@signature (listof Cat) Color -> Natural)
;; produce the sum of the ages of all cats coloured c
(check-expect (sum-age-c empty "black") 0)
(check-expect (sum-age-c (list C1) "black") 0)
(check-expect (sum-age-c (list C1) "brown") 13)
(check-expect (sum-age-c LOC1 "black") 4)
(check-expect (sum-age-c LOC1 "brown") (+ 13 7 6))

;(define (sum-age-c loc c) 0)    ;stub

(@template fn-composition use-abstract-fn)

(define (sum-age-c loc c)
  (foldl + 0
         (map cat-age
              (filter (λ (ct) (equal? (cat-color ct) c)) loc))))






;;
;; Please read through the data definition  for Treasure that can be found
;; in a Scavenger Hunt.
;;

(@htdd Status)
;; Status is one of:
;; - "buried"
;; - "sunken"
;; - "locked"
;; interp. the status of an unopened treasure box
;; <examples are redundant for enumeration>

(@htdd Treasure)
(define-struct treasure (label amount difficulty status treasures))
;; Treasure is (make-treasure String Natural Natural Status ListOfTreasure)
;; interp. a treasure box with a label name,
;;         the number of gold coins contained in the treasure box,
;;         a rating of difficulty to find and open the treasure box between 1
;;         and 5, where 1 is very easy to find and open and 5 is very difficult,
;;         the status of the treasure box before it was opened,
;;         and a list of other treasure boxes that opening the box leads to.

(@htdd ListOfTreasure)
;; ListOfTreasure is one of:
;; - empty
;; - (cons Treasure ListOfTreasure)
;; interp. a list of unopened treasure boxes

(define LOT0 empty)
(define T1 (make-treasure "E" 32 3 "buried" LOT0))
(define T2 (make-treasure "F" 10 2 "locked" LOT0))
(define LOT1 (cons T1 (cons T2 empty)))
(define T3 (make-treasure "B" 6 5 "locked" LOT1))      

(define T4 (make-treasure "J" 1 1 "sunken" LOT0))
(define LOT2 (cons T4 empty))
(define T5 (make-treasure "H" 17 2 "sunken" LOT2))
(define T6 (make-treasure "G" 52 3 "buried" LOT0))
(define T7 (make-treasure "I" 100 5 "locked" LOT0))
(define LOT3 (cons T6 (cons T5 (cons T7 empty))))
(define T8 (make-treasure "D" 21 1 "sunken" LOT3))

(define T9 (make-treasure "C" 41 4 "buried" LOT0))
(define LOT4 (cons T3 (cons T9 (cons T8 empty))))
(define T10 (make-treasure "A" 7 1 "locked" LOT4))
#;                        
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
                 (fn-for-lot (treasure-treasures t))))

          (define (fn-for-lot lot)
            (cond [(empty? lot) (...)]
                  [else
                   (... (fn-for-treasure (first lot))
                        (fn-for-lot (rest lot)))]))]
    
    (fn-for-treasure t)))






(@problem 6)
;;
;; Design an abstract fold function for Treasure. You must include the
;; signature, purpose, one check-expect (described below), template tags,
;; and the function definition. Name your function fold-treasure.
;;
;; The check-expect should produce a copy of the given Treasure.
;;

(@htdf fn-for-treasure)
(@signature (String Natural Natural X Y -> Z) (Z Y -> Y) X X X Y Treasure -> Z)
;; abstract fold fn for Treasure
(check-expect
 (fn-for-treasure make-treasure cons "buried" "sunken" "locked" empty T1) T1)

(@template Status Treasure (listof Treasure) encapsulated)

(define (fn-for-treasure fn1 fn2 s1 s2 s3 b t)
  (local [(define (fn-for-status s)
            (cond [(string=? s "buried") s1]
                  [(string=? s "sunken") s2]
                  [(string=? s "locked") s3]))

          (define (fn-for-treasure t)
            (fn1 (treasure-label t)
                 (treasure-amount t)
                 (treasure-difficulty t)
                 (fn-for-status (treasure-status t))
                 (fn-for-lot (treasure-treasures t))))

          (define (fn-for-lot lot)
            (cond [(empty? lot) b]
                  [else
                   (fn2 (fn-for-treasure (first lot))
                        (fn-for-lot (rest lot)))]))]
    
    (fn-for-treasure t)))



(@problem 7)
;;
;; Design a function called more-than-n. more-than-n takes two arguments: a
;; Treasure (t) and a Natural (n) IN THAT ORDER. It produces a list of the label
;; names of treasures that contain an amount of gold larger than n. For this 
;; question, assume that the amount of gold in each box is magically multiplied
;; based on the status of the treasure box.
;;
;; Treasure boxes that are buried get their amount of gold multiplied by 2,
;; sunken treasure get a multiplier of 4, and locked chests get a multiplier
;; of 3. This means that if a treasure is defined as follows:
;;   - (define T1 (make-treasure "E" 32 3 "buried" empty))
;;   - T1 would effectively have 32 gold x 2 (since it's buried) = 64 gold.
;;
;; Your function definition must call the fold-treasure function you defined
;; in PROBLEM 6.

(@htdf more-than-n)
(@signature Treasure Natural -> (listof String))
;; produce list of labels of treasures where gold > n
(check-expect (more-than-n T1 63) (list "E"))
(check-expect (more-than-n T1 64) empty)
(check-expect (more-than-n T1 65) empty)
(check-expect (more-than-n T3 17) (list "B" "E" "F"))
(check-expect (more-than-n T3 18) (list "E" "F"))
(check-expect (more-than-n T4 4) empty)
(check-expect (more-than-n T4 1) (list "J"))



;(define (more-than-n t n) empty)

(@template use-abstract-fn)

(define (more-than-n t n)
  (fn-for-treasure (λ (l a d s lot) (if (> (* a s) n)
                                        (cons l lot)
                                        lot))
                   append
                   2
                   4
                   3
                   empty
                   t))






          
