;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname lab-06-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(@assignment labs/lab-06)

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
;; DATA DEFINITIONS ===============

(@htdd SentenceTree ListOfSentenceTree)
(define-struct stree (prefix subs))
;; SentenceTree is (make-stree String ListOfSentenceTree)
;; interp. an abitrary-arti tree of sentences with a sentence
;; String : sentence in the tree
;; ListOfSentenceTree: an abitrary-arti tree of sentences trees

;;ListOfSentenceTree is one of:
;; - empty
;; - (cons SentenceTree ListOfSentenceTree)
;; interp. arbitrary-arti tree of SentenceTree

(define ST1 (make-stree "KISS ME"
                        (list (make-stree "JOKING ABOUT JEALOUSY" empty)
                          (make-stree "LIKE"
                           (list (make-stree "YOU REALLY MEAN IT" empty)
                             (make-stree "WE ARE"
                             (list
                               (make-stree
                                "IN A BACK TO SCHOOL SPECIAL ABOUT MONO"
                                       empty)
                   (make-stree "PERCHED ON THE TIP OF A SINKING SHIP"
                      empty)))))
                              (make-stree "TO"
                    (list (make-stree "FREEZE TIME" empty)
                (make-stree "MY FAVOURITE SONG ON REPEAT"
               empty))))))




(define (fn-for-st st)
  (... (stree-prefix st)
       (fn-for-lost (stree-subs st))))


(define (fn-for-lost lost)
  (cond [(empty? lost) (...)]
        [else
         (... (fn-for-st (first lost))
              (fn-for-lost (rest lost)))]))



;; FUNCTIONS ======================
(@problem 2)

(@htdf sentence-count--st sentence-count--lost)
(@signature SentenceTree -> Natural)
;; produces number of sentences in a sentence tree
(check-expect (sentence-count--st ST1) 6)
(check-expect (sentence-count--st (make-stree "a" empty)) 1)

(check-expect (sentence-count--lost empty) 0)
(check-expect (sentence-count--lost (list ST1)) 6)

;(define (sentence-count--st st) 0)
;(define (sentence-count--lost lost) 0)

(@template SentenceTree)
(@template ListOfSentenceTree)

(define (sentence-count--lost lost)
  (cond [(empty? lost) 0]
        [else
         (+ (sentence-count--st (first lost))
            (sentence-count--lost (rest lost)))]))

(define (sentence-count--st st)
  (if (empty? (stree-subs st))
      1
      (sentence-count--lost (stree-subs st))))


(@problem 3)

(define TEXT-SIZE 20)
(define TEXT-COLOR "RED")


(@htdf render--st render--lost)
(@signature SentenceTree -> Image)
;; produces an image of the sentence tree
(check-expect (render--st ST1) (beside
         (text "KISS ME" TEXT-SIZE TEXT-COLOR)
        (above/align "left"
        (text "JOKING ABOUT JEALOUSY" TEXT-SIZE TEXT-COLOR)
         (beside (text "LIKE" TEXT-SIZE TEXT-COLOR)
        (above/align "left"
            (text "YOU REALLY MEAN IT"
                     TEXT-SIZE TEXT-COLOR)
   (beside (text "WE ARE" TEXT-SIZE TEXT-COLOR) (above/align "left"
(text "IN A BACK TO SCHOOL SPECIAL ABOUT MONO" TEXT-SIZE TEXT-COLOR)
 (text "PERCHED ON THE TIP OF A SINKING SHIP" TEXT-SIZE TEXT-COLOR)))))
 (beside (text "TO" TEXT-SIZE TEXT-COLOR)
 (above/align "left" (text "FREEZE TIME" TEXT-SIZE TEXT-COLOR)
 (text "MY FAVOURITE SONG ON REPEAT" TEXT-SIZE TEXT-COLOR)))
                                     
                                            )))
             
(check-expect (render--st (make-stree "a" empty))
              (text "a" TEXT-SIZE TEXT-COLOR))

(check-expect (render--lost empty) empty-image)
(check-expect (render--lost (list ST1)) (beside
         (text "KISS ME" TEXT-SIZE TEXT-COLOR)
        (above/align "left"
        (text "JOKING ABOUT JEALOUSY" TEXT-SIZE TEXT-COLOR)
         (beside (text "LIKE" TEXT-SIZE TEXT-COLOR)
        (above/align "left"
            (text "YOU REALLY MEAN IT"
                     TEXT-SIZE TEXT-COLOR)
   (beside (text "WE ARE" TEXT-SIZE TEXT-COLOR) (above/align "left"
(text "IN A BACK TO SCHOOL SPECIAL ABOUT MONO" TEXT-SIZE TEXT-COLOR)
 (text "PERCHED ON THE TIP OF A SINKING SHIP" TEXT-SIZE TEXT-COLOR)))))
 (beside (text "TO" TEXT-SIZE TEXT-COLOR)
 (above/align "left" (text "FREEZE TIME" TEXT-SIZE TEXT-COLOR)
 (text "MY FAVOURITE SONG ON REPEAT" TEXT-SIZE TEXT-COLOR)))
                                     
                                            )))
                        
;(define (render--st ) empty-image)
;(define (render--lost lost) empty-image)


(@template SentenceTree)
(@template ListOfSentenceTree)


(define (render--st st)
  (beside (text (stree-prefix st) TEXT-SIZE TEXT-COLOR)
          (render--lost (stree-subs st))))


(define (render--lost lost)
  (cond [(empty? lost) empty-image]
        [else
         (above/align "left" (render--st (first lost))
                             (render--lost (rest lost)))]))





