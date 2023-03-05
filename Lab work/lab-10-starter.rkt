;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab-10-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
;; CPSC 110 - Accumulators Lab

(@assignment labs/lab-10)

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

;; On-ground students: Do the following as your pre-lab
;; Online students: Do the following as Task 1

;; Using the data definitions below, design a function called can-encode? that
;; takes a letter (NOT an entire word) and a Huffman code tree and
;; produces true 
;; if that letter is reachable in the given tree, false otherwise. You should
;; not use any accumulators. As a reminder, we have asked you to blend the
;; backtracking template into your design using try-catch.

;; We have provided the signature, purpose, stub, and some tests.

(@htdd HTree)
(define-struct htree (zero one))
;; HTree is one of
;; - String
;; - (make-htree HTree HTree)
;; interp. A Huffman code tree where
;; a string s is an encoded value
;; (make-htree z o) is a tree with zero-branch z and one-branch o
(define HT0 "A")

(define HTMAGIC
  (make-htree "A"
              (make-htree 
               (make-htree "B"
                           (make-htree "C" "D"))
               "R")))

#;
(define (fn-for-htree ht)
  (cond [(string? ht) (... ht)]
        [else
         (... (fn-for-htree (htree-zero ht))
              (fn-for-htree (htree-one ht)))]))



(@htdf can-encode?)
(@signature HTree String -> Boolean)
;; produce if the given string is reachable in the given tree, false otherwise
(check-expect (can-encode? "A" "A") true)
(check-expect (can-encode? "A" "B") false) 
(check-expect (can-encode? HTMAGIC "D") true)
(check-expect (can-encode? HTMAGIC "E") false)

;(define (can-encode? ht s) false)

(@template HTree try-catch)

(define (can-encode? ht s)
  (cond [(string? ht) (string=? ht s)]
        [else
         (local [(define try (can-encode? (htree-zero ht) s))]
           (if  (not (false? try))
                try
                (can-encode? (htree-one ht) s)))]))


#;
(define (fn-for-lox lox)
  (cond [(empty? lox) false]
        [else
         (if (not (false? (fn-for-x (first lox))))  ;is first child successful?
             (fn-for-x (first lox))                 ;if so produce that
             (fn-for-lox (rest lox)))]))            ;or try rest of children


;; Problem 2:

;; When spelling correction first appeared in typewriters, it was very important
;; to store the information about legal words as compactly as possible. One
;; technique for doing so was to use prefix trees, in which all words that start
;; with the same prefix share the same data representation for that common
;; prefix. They only have different data for their different endings. For
;; example, part of the prefix tree starting at t would look like this: 

#;
(bitmap/url
 "https://edx-course-spdx-kiczales.s3.
amazonaws.com/HTC/lab/lab-10-prefix-tree.png")


;; Of course, the complete tree starting at t is much bigger.

;; Each node in the prefix tree has its letter and a flag indicating whether the
;; path from the root of the tree to that node is a legal node. In the picture,
;; the * on some nodes indicate that the tree up to that point is a legal word.
;; For example, the sequence of letters t-o-o is a legal word because when we
;; start from t, then to o, then to the second o, we are a node with *. But the
;; sequence t-o-u is not legal because going from t, then o, then u lands at a
;; node without *. Every full path is a legal word because it would waste space
;; to store bad words that you don't need.


;; Design a function called count-odds that takes a prefix spelling tree
;; and determines how many odd-length words are contained in it. 

;; We have provided the data definition for prefix trees below. 

(@htdd PrefixTree)
(define-struct pt (l f? subs))
;; PrefixTree is (make-pt String Boolean (listof PrefixTree))
;; interp. a prefix spelling tree.  The sequence of letters up to and including
;;         a given node are a legal word if f? is true, not legal otherwise.

(define PT0 (make-pt "K" true empty))
(define PTT 
  (make-pt "t" false
           (list 
            (make-pt "o" true
                     (list 
                      (make-pt "o" true (list (make-pt "k" true empty)))
                      (make-pt "t" true empty)
                      (make-pt "l" false (list (make-pt "l" true empty)))
                      (make-pt "u" false
                               (list 
                                (make-pt "c" false
                                         (list (make-pt "h" true empty)))))))
            (make-pt "i" false
                     (list (make-pt "n" true empty))))))


(define LOPT0 (list (make-pt "o" true empty)
                    (make-pt "t" true empty)))


#;
(define (fn-for-pt pt)
  (local [(define (fn-for-pt pt)
            (... (pt-l pt)
                 (pt-f? pt)
                 (fn-for-lopt (pt-subs pt))))
          (define (fn-for-lopt lopt)
            (cond [(empty? lopt) (...)]
                  [else
                   (... (fn-for-pt (first lopt))
                        (fn-for-lopt (rest lopt)))]))]
    (fn-for-pt pt)))

(@problem 2)

(@htdf count-odds)
(@signature PrefixTree -> Natural)
;; produces number of odd length words in the tree
(check-expect (count-odds (make-pt "K" false empty)) 0)
(check-expect (count-odds PT0) 1)
(check-expect (count-odds PTT) 4)

;(define (count-odds pt) 0)

(@template PrefixTree (listof PrefixTree) encapsulated accumulator)

(define (count-odds pt0)
  ;;INVARIENT: rsf; Natural, number of odds so far
  ;;INVARIENT: p; (listof PrefixTree) path to node
  ;;INVARIENT: p-wl; (listof (listof PrefixTree)) list of path to node
  (local [(define (fn-for-pt pt t-wl p p-wl rsf)
            (local [(define subs (pt-subs pt))
                    (define valid (pt-f? pt))
                    (define (oddword? w)
                      (odd? (foldr + 0 (map (lambda (s) 1) w))))]
              
              (fn-for-lopt (append subs t-wl)
                           (append (map (lambda (s)
                                          (append p (list s))) subs) p-wl)
                           (if (and valid (oddword? p))
                               (+ rsf 1)
                               rsf))))
          
          (define (fn-for-lopt t-wl p-wl rsf)
            (cond [(empty? t-wl) rsf]
                  [else
                   (fn-for-pt (first t-wl)
                              (rest t-wl)
                              (first p-wl)
                              (rest p-wl)
                              rsf)]))] 
    
    (fn-for-pt pt0 empty (list pt0) empty 0)))






;; Problem 3:

;; Copy your function design from Task 1 and change it to produce a list of 
;; the binary digits on the path to a given string in the tree, or false if  
;; that string cannot be reached in the given tree. Use a result-so-far
;; accumulator. Call this function encode.
(@problem 3)

(@htdf encode)
(@signature HTree String -> (listof Natural) or false)
;; produce if the given string is reachable in the given tree, false otherwise
;; CONSTRAINT: list of natural can only have ones and zeros
(check-expect (encode "A" "A") empty)
(check-expect (encode "A" "B") false) 
(check-expect (encode HTMAGIC "D") (list 1 0 1 1))
(check-expect (encode HTMAGIC "E") false)

;(define (encode ht s) false)

(@template HTree try-catch accumulator)

(define (encode ht0 s)
  ;; INVARIENT: rsf; (listof Natural)
  (local [(define (binary ht rsf)
            (cond [(string? ht) (if (string=? ht s)
                                    rsf
                                    false)]
                  [else
                   (local [(define try (binary (htree-zero ht) rsf))]
                     (if  (not (false? try))
                          (binary (htree-zero ht) (append rsf (list 0)))
                          (binary (htree-one ht) (append rsf (list 1)))))]))]
    
    (binary ht0 empty)))



;; EXTRA PRACTICE:

;; Refactor your design of encode from Problem 3 so it is also tail recursive.
;; Note that you will want to keep a copy of your solution from Problem 3 to
;; hand in.
