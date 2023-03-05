;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname traffic-light-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

(@assignment bank/htdw-p2)
(@cwl wriksen)

(@problem 1)
;; Design an animation of a traffic light. 
;; 
;; Your program should show a traffic light that is red, then green, 
;; then yellow, then red etc. For this program, your changing world 
;; state data definition should be an enumeration.
;;
;; The link below shows what your program might look like if the initial world 
;; state was the red traffic light:
;;
;; https://cs110.students.cs.ubc.ca/bank/r-traffic.png
;;
;; Next:
;;
;; https://cs110.students.cs.ubc.ca/bank/g-traffic.png
;
;; Next:
;; 
;; https://cs110.students.cs.ubc.ca/bank/y-traffic.png
;;
;; Next is red, and so on.
;;
;; To make your lights change at a reasonable speed, you can use the 
;; rate option to on-tick. If you say, for example, (on-tick next-color 1) 
;; then big-bang will wait 1 second between calls to next-color.
;;
;; Remember to follow the HtDW recipe! Be sure to do a proper domain 
;; analysis before starting to work on the code file.
;; 
;; Note: If you want to design a slightly simpler version of the program,
;; you can modify it to display a single circle that changes color, rather
;; than three stacked circles. 


(@htdw TrafficLight) ;(give TrafficLight a better name)
;; shows a traffic light that changes from green
;to yellow to red, then green again
;; =================
;; Constants:
(define WIDTH 200)
(define HEIGHT 600)
(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))
(define Y3 (/ HEIGHT 3))
(define SPEED 1)
(define LIGHT (above (circle 100 "solid" "black")
                     (circle 100 "solid" "black")
                     (circle 100 "solid" "black")))
(define BACK-SCENE (place-image (overlay LIGHT
                                         (rectangle WIDTH HEIGHT "solid" "gray"))
                                CTR-X CTR-Y (empty-scene WIDTH HEIGHT)))
(define red (circle 100 "solid" "red"))
(define yellow (circle 100 "solid" "red"))
(define green (circle 100 "solid" "red"))

;; =================
;; Data definitions:

(@htdd TrafficLight)
;; TrafficLight is a colour
;; -"green"
;; -"yellow"
;; -"red"
;;interp. the colours of a traffic light
(@dd-template-rules one-of
                    atomic-distinct ;"green"
                    atomic-distinct ;"yellow"
                    atomic-distinct);"red"
(define (fn-for-traffic-light tl)
  (...))

;; =================
;; Functions:

(@htdf main)
(@signature TrafficLight -> TrafficLight)
;; start the world with red light at the top
;; 

(@template htdw-main)

(define (main tl)
  (big-bang tl                   ; TrafficLight
            (on-tick   tock)     ; TrafficLight -> TrafficLight
            (to-draw   render)   ; TrafficLight -> Image
            (on-key    keyin)))    ; TrafficLight KeyEvent -> TrafficLight

(@htdf tock)
(@signature TrafficLight -> TrafficLight)
;; produce the next ...
(check-expect)

(define (tock tl) tl)



(@htdf render)
(@signature TrafficLight -> Image)
;; render ... 
;; !!!
(define (render tl) empty-image)

(@htdf keyin)
(@signature TrafficLight KeyEvent -> TrafficLight)

