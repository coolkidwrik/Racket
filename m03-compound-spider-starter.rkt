;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m03-compound-spider-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

(@assignment lectures/m03-compound-spider)

(@cwl wriksen) ;replace ??? with your cwl

;; Spider, goes down the screen with thread

#|
PROBLEM:

Revise this program so that when the program starts the spider moves down the 
screen, but pressing the space key changes its direction.

 - First "reverse-engineer" the domain analysis from the program.
 - Then revise the domain analysis for this new behaviour.
 - Then systematically work your way through the program in the same
   order as HtDW says revising the program to match the new analysis.

   You will need to CHANGE the definition of the Spider type

   When you change a type (a data definition) you need to systematically
   go check on every function that consumes or produces that type to update
   it to the new definition.

Then you can extend the program to make the spider appear to wiggle as it
moves. Unlike in Cowabunga you should represent the state of the wiggle
explicitly as a field in the world state.
|#



;; My creepy crawly spider changes direction at top and bot in space

(@htdw Spider)

;; =================
;; Constants:

(define WIDTH 400)
(define HEIGHT 600)

(define CTR-X (/ WIDTH 2))

;!!!(define SPEED 2) ;pixels per tick

(define SPIDER-RADIUS 10)

(define TOP (+ 0      SPIDER-RADIUS)) ;to be entirely visible
(define BOT (- HEIGHT SPIDER-RADIUS)) ;center has to be in [TOP, BOT]


(define SPIDER-IMAGE (circle SPIDER-RADIUS "solid" "black"))

(define MTS (empty-scene WIDTH HEIGHT))


;; =================
;; Data definitions:
(@problem 1)
(@htdd Spider)
(define-struct spider (y dy))
;; Spider is (make-spider Number Number)
;; interp. y coordinate of the spider
;;         distance of the centre of the spider from top
;;         dy is velocity of spider in pixles/tick
;; CONSTRAINT: to be visible, must be in [TOP, BOT]

;;*******
(define S-TOP (make-spider TOP 2)) ;top, down 2/tick
(define S-MID (make-spider (/ HEIGHT 2) -3)) ;mid, up 3/tick

(@dd-template-rules compound)

(define (fn-for-spider s)
  (... (spider-y s)    ;Number - y coordinate
       (spider-dy s))) ;Number - y velocity

;; =================
;; Functions:

(@htdf main)
(@signature Spider -> Spider)
;; start the world with (main S-TOP)
;;
(@template htdw-main)

(define (main s)
  (big-bang s                     ; Spider
            (on-tick   tock)      ; Spider -> Spider
            (to-draw   render)    ; Spider -> Image
            (on-key    handle-key);Spider KeyEvent -> Spider
    ))

(@htdf handle-key)
(@signature Spider KeyEvent -> Spider)
;; purpose change direction on space, ignore all other keys
(check-expect (handle-key (make-spider 100 2) " ") (make-spider 100 -2))
(check-expect (handle-key (make-spider 80 -5) " ") (make-spider 80 5))
(check-expect (handle-key (make-spider 90 3) "x") (make-spider 90 3))
(check-expect (handle-key (make-spider 90 0) "y") (make-spider 90 0))

;(define (handle-key s ke) s)

(@template KeyEvent) ; using large enumeration rule

(define (handle-key s ke)
  (cond [(key=? ke " ") (make-spider (spider-y s) (* -1 (spider-dy s)))]
        [else s]))




(@htdf tock)
(@signature Spider -> Spider)
;; produce the next spider by adding SPEED to s, stopping at bottom
(check-expect (tock (make-spider TOP  2)) (make-spider (+ TOP  2)  2))
(check-expect (tock (make-spider BOT -3)) (make-spider (+ BOT -3) -3))

;;top edge
(check-expect (tock (make-spider (+ TOP 3) -2)) (make-spider ))
(check-expect (tock (make-spider (+ TOP 3) -3)) (make-spider TOP 3))
(check-expect (tock (make-spider (+ TOP 3) -4)) (make-spider TOP 4))

;;bottome edge
(check-expect (tock (make-spider (- BOT 3) 2)) (make-spider (- BOT 1) 2))
(check-expect (tock (make-spider (- BOT 3) 3)) (make-spider BOT -3))
(check-expect (tock (make-spider (- BOT 3) 4)) (make-spider BOT -4))

;(check-expect (tock (- BOT  1 SPEED)) (- BOT 1))
;(check-expect (tock (- BOT  0 SPEED))    BOT)
;(check-expect (tock (- BOT -1 SPEED))    BOT)


;(define (tock s) s) ;stub

(@template Spider)

(define (fn-for-spider s)
  (cond [(<= (+ (spider-y s)))]
        []
        []))




(define (tock s)
  (if (>= (+ s SPEED) BOT)
      BOT
      (+ s SPEED)))



(@htdf render)
(@signature Spider -> Image)
;; place SPIDER-IMAGE and thread image on MTS
(check-expect (render 21)
              (add-line (place-image SPIDER-IMAGE CTR-X TOP MTS)
                        CTR-X 0
                        CTR-X 21
                        "black"))

(check-expect (render 36)
              (add-line (place-image SPIDER-IMAGE CTR-X 36 MTS)
                        CTR-X 0
                        CTR-X TOP
                        "black"))

;(define (render s) MTS)

(@template Spider)

(define (render s)
  (add-line (place-image SPIDER-IMAGE
                         CTR-X
                         s
                         MTS)
            CTR-X 0
            CTR-X s
            "black"))
