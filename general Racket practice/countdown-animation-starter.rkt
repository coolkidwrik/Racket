;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname countdown-animation-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

(@assignment bank/htdw-p1)
(@cwl wriksen)

(@problem 1)
;; Design a world program that represents a countdown. The program should 
;; display the number of seconds remaining and should decrease at each 
;; clock tick. Upon reaching zero, it should stay there and not change.
;; 
;; To make your countdown progress at a reasonable speed, you can use the 
;; rate option to on-tick. If you say, for example, 
;; (on-tick advance-countdown 1) then big-bang will wait 1 second between 
;; calls to advance-countdown.
;;
;; Remember to follow the HtDW recipe! Be sure to do a proper domain 
;; analysis before starting to work on the code file.
;; 
;; Once you are finished the simple version of the program, you can improve
;; it by reseting the countdown to ten when you press the spacebar.

;; My world program  (make this more specific)

(@htdw Countdown) 
;;counts down from 10 to 0, then stops
;; =================
;; Constants:
(define HEIGHT 100)
(define WIDTH HEIGHT)
(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))
(define MTS (empty-scene WIDTH HEIGHT))
(define TEXT-SIZE 50)
(define TEXT-COLOR "black")
(define SPEED 1)

;; =================
;; Data definitions:

(@htdd Countdown)
;; Countdown is Number 
;; interp. the numbers from a countdown
;;constraint: [0, 10]

;;examples of data types below (no need if enumerations)
(define C1 10)
(define C2 5)
(define C3 0)

(@dd-template-rules              
                   atomic-non-distinct) ;Number
             

;; template for functions using this data, copy and past into fucntions
(define (fn-for-countdown c)
  (... c))



;; =================
;; Functions:

(@htdf main)
(@signature Countdown -> Countdown)
;; start the world with ...
;; 

(@template htdw-main)

(define (main c)
  (big-bang c                   ; Countdown
            (on-tick   tock SPEED)     ; Countdown -> Countdown
            (to-draw   render)   ; Countdown -> Image
            (on-key    keyevent)));Countdown KeyEvent -> Countdown
(@htdf tock)
(@signature Countdown -> Countdown)
;; produce the next number by subtracting it by 1
(check-expect (tock 10) 9)
(check-expect (tock 0) 0)

;(define (tock c) c)

(@template Countdown)

(define (tock c)
  (cond [(> c 0) (- c 1)]
        [else 0]))

(@htdf render)
(@signature Countdown -> Image)
;; render image of the countdown 
(check-expect (render 10)
              (place-image (text "10" TEXT-SIZE TEXT-COLOR) CTR-X CTR-Y MTS))
(check-expect (render 0)
              (place-image (text "0" TEXT-SIZE TEXT-COLOR) CTR-X CTR-Y MTS))

;(define (render c) MTS)

(@template Countdown)

(define (render c)
  (place-image (text (number->string c) TEXT-SIZE TEXT-COLOR) CTR-X CTR-Y MTS))

(@htdf keyevent)
(@signature Countdown KeyEvent -> Countdown)
;resets the countdown when space bar is pressed
(check-expect (keyevent 10 " ") 10)
(check-expect (keyevent 0 "a") 0)
(check-expect (keyevent 0 " ") 10)

;(define (keyevent c ke) c)

(@template Countdown KeyEvent)

(define (keyevent c ke)
  (cond [(key=? ke " ") 10]
        [else c]))

