;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-03-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

(@assignment labs/lab-03)

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

; Balloon popping

(@htdw Balloon)
;; CONSTANTS ==========================

(define WIDTH 500)
(define HEIGHT 500)
(define MTS (empty-scene WIDTH HEIGHT))

(define BALLOON-COLOR "red")
(define POP-IMAGE
  (overlay (text "POP!" 80 "black")
           (radial-star 30 (/ WIDTH 10) (/ WIDTH 2) "solid" "yellow")))

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

(define SPEED 2)

(define MAX-SIZE (/ WIDTH 2))





;; DATA DEFINITIONS ============================
 
(@problem 1)

(@htdd Balloon)
;; Balloon is one of:
;; - Natural [0, MAX-SIZE]
;; - false
;;interp. a balloon that is either expanding, or exploded(true)
;;constraint number[0, MAX-WIDTH]

(define B1 10)
(define B2 false)
(@dd-template-rules one-of
                    atomic-non-distinct     ;false
                    atomic-distinct)        ;Number

(define (fn-for-balloon b)
  (cond [(number? b) (... b)]
        [else (...)]))

;; FUNCTIONS ====================================
(@problem 2)
(@htdf main)
(@signature Balloon -> Balloon)
;; starts the world program with (main 0)
; no examples for main function

(@template htdw-main)
(define (main b)
  (big-bang b               ; Balloon
            (on-tick tick)   ; Balloon -> Balloon
            (to-draw render) ; Balloon -> Image
;           (stop-when ...)  ; Balloon -> Boolean
;           (on-mouse ...)   ; Balloon Integer Integer MouseEvent -> Balloon
;           (on-key ...)     ; Balloon KeyEvent -> Balloon
            ))

(@problem 3)
(@htdf tick)
(@signature Balloon -> Balloon) 
;; produce the next Balloon with next radius
(check-expect (tick 0) (+ 0 SPEED))
(check-expect (tick (/ MAX-SIZE 2)) (+ (/ MAX-SIZE 2) SPEED))
(check-expect (tick (- MAX-SIZE SPEED)) MAX-SIZE)
(check-expect (tick MAX-SIZE) false)
(check-expect (tick (- MAX-SIZE (- SPEED 1))) false)
(check-expect (tick false) false)

;(define (tick b) b)

(@template Balloon)
(define (tick b)
(cond [(number? b) (if (> (+ b SPEED) MAX-SIZE)
              false
              (+ b SPEED))]
        [else false]))

  ;(cond [(false? b) false]
  ;      [else (if (> (+ b SPEED) MAX-SIZE)
   ;           false
  ;            (+ b SPEED))]))

  ;(cond [(or (false? b) (> (+ b SPEED) MAX-SIZE)) false]
   ;     [else (+ b SPEED)]))


(@problem 4)
(@htdf render)
(@signature Balloon -> Image) 
;; renders the image of next balloon, or pop
(check-expect (render 0) (place-image
                          (circle 0 "solid" BALLOON-COLOR) CTR-X CTR-Y MTS))
(check-expect (render MAX-SIZE)
              (place-image (circle
               MAX-SIZE "solid" BALLOON-COLOR) CTR-X CTR-Y MTS))
(check-expect (render false)
              (place-image POP-IMAGE CTR-X CTR-Y MTS))
(check-expect (render 10)
              (place-image
               (circle 10 "solid" BALLOON-COLOR) CTR-X CTR-Y MTS))

;(define (render b) MTS)

(@template Balloon)

(define (render b)
  (cond [(number? b) (place-image
                      (circle b "solid" BALLOON-COLOR) CTR-X CTR-Y MTS)]
        [else (place-image POP-IMAGE CTR-X CTR-Y MTS)]))

  ;(cond [(false? b) (place-image POP-IMAGE CTR-X CTR-Y MTS)]
   ;     [else (place-image
   ;            (circle b "solid" BALLOON-COLOR) CTR-X CTR-Y MTS)]))