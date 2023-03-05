;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m03-spider-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

(@assignment lectures/m03-spider)

(@cwl wriksen)

(@problem 1)
#|
PROBLEM:

Design a world program in which a spider starts at the top of the screen
and slowly drops down it. The spider should stop when it reaches the bottom
of the screen.

You can improve your spider by re-running the HtDW recipe to add these
features. 


  - Draw a line from the top of the screen to the spider, this is the thread 
    it is hanging from. You will need to use add-line for this. Look in the
    DrRacket help desk to see how add-line works.
    
  - Arrange for pressing the space key to reset the spider to the top of 
    the screen.
|#



;; spider program  (make this more specific)

(@htdw Spider)

;; =================
;; Constants:
(define HEIGHT 400)
(define WIDTH 600)

(define CTR-X (/ WIDTH 2))

(define SPIDER-RADIUS 10)

(define SPEED 2)

(define SPIDER-IMAGE (circle SPIDER-RADIUS "solid" "black"))

(define MTS (empty-scene WIDTH HEIGHT))

(define TOP (+ 0 SPIDER-RADIUS))
(define BOT (- HEIGHT SPIDER-RADIUS))

;; =================
;; Data definitions:

(@htdd Spider)
;; Spider is Number
;;interp. the y position of spider in screen coordinates
;;CONSTRAINT: to be completely visible, must be in [TOP, BOT]

(define S-TOP TOP)
(define S-MID (/ HEIGHT 2))
(define S-BOT BOT)

(@dd-template-rules atomic-non-distinct)
;(define (fn-for-spider-s)
;  (... s))




;; =================
;; Functions:

(@htdf main)
(@signature Spider -> Spider)
;; start the world with (main S-TOP)
;;

(@template htdw-main)

(define (main s)
  (big-bang s                   ; Spider
            (on-tick   tock)     ; Spider -> Spider
            (to-draw   render)   ; Spider -> Image
           ; (stop-when ...)      ; Spider -> Boolean
           ; (on-mouse  ...)      ; Spider Integer Integer MouseEvent -> Spider
           ; (on-key    ...)      ; Spider KeyEvent -> Spider
    ))              



(@htdf tock)
(@signature Spider -> Spider)
;; produce the next spider moving down the screen by speed, stop at BOT
(check-expect (tock (/ HEIGHT 2)) (+ (/ HEIGHT 2) SPEED))
(check-expect (tock TOP) 12)
(check-expect (tock BOT) BOT)
(check-expect (tock 206) (+ 206 SPEED))
(check-expect (tock (- BOT SPEED -1)) BOT)
(check-expect (tock (- BOT SPEED 1)) (- BOT 1))

;(define (tock s) s)

(@template Spider)

(define (tock s)
  (if (<= (+ s SPEED) BOT)
      (+ SPEED s)
      BOT))










(@htdf render)
(@signature Spider -> Image)
;; render SPIDER-IMAGE on the MTS 
(check-expect (render TOP) (place-image SPIDER-IMAGE CTR-X TOP MTS))
(check-expect (render (/ HEIGHT 2))
              (place-image SPIDER-IMAGE CTR-X (/ HEIGHT 2) MTS))
(check-expect (render BOT) (place-image SPIDER-IMAGE CTR-X BOT MTS))


;(define (render s) MTS)

(@template Spider)
(define (render s)
  (place-image SPIDER-IMAGE CTR-X s MTS))
