;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pset-04-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR
;; PARTNER.
(require 2htdp/universe)
(require 2htdp/image)
(require spd/tags)

(@assignment psets/pset-04);Do not edit or remove this tag

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
(@cwl wriksen)


;; Below is our solution to Problem Set 3.
;; For problem set 4 you must improve this program in important ways. Be sure
;; not to change the program's behaviour in any other ways than we are asking.
;; Making extra improvements could cause you problems with the autograder.
;;
;; Modify the program so that instead of there being a single ball there
;; can be any number of balls.  The mouse button should add a new ball.
;; Pressing space should clear all the balls.
;;
;; Again your change should be systematic and complete.  All relevant
;; design elements and tags must be updated.  You must name the new
;; world state type ListOfBall.
;;
;; NOTE, if you work systematically, starting from the HtDW recipe, you
;; will decide that you need a new ListOfBall type; but that the Ball
;; type does not change.  Therefore you should NOT change your next-ball
;; function.  Your render-ball function will also be useful when you
;; work on your place-ball function, although render-ball will have to
;; change some.
;;

(@problem 1)
(@htdw Ball)

;; Constants:
(define WIDTH  605)
(define HEIGHT 535)

(define BALL-RADIUS 10)

(define TOP (+ 0      BALL-RADIUS))   ;these constants define the "inner box"
(define BOT (- HEIGHT BALL-RADIUS 1)) ;that constrains the center of the ball
(define LEF (+ 0      BALL-RADIUS))   ;
(define RIG (- WIDTH  BALL-RADIUS 1)) ;

(define BALL (circle BALL-RADIUS "solid" "white"))

(define MTS (rectangle WIDTH HEIGHT "solid" "green"))


;; ===========================================================================
;; ===========================================================================
;; Data definitions:

(@htdd Ball)

(define-struct ball (x y dx dy))
;; Ball is (make-ball Number Number Number Number)
;; interp. (make-ball x y dx dy) is ball
;;   - position x, y    in screen coordinates
;;   - velocity dx, dy  in pixels/tick
(define B1 (make-ball (/ WIDTH 2) (/ HEIGHT 2) 4 -3))

(@dd-template-rules compound)

(define (fn-for-ball b)
  (... (ball-x b) 
       (ball-y b) 
       (ball-dx b) 
       (ball-dy b)))

(@htdd ListOfBall)

;; ListOfBall is one of:
;; - empty
;; - (cons Ball ListOfBall)
;; interp. a list of balls, empty = no balls
(define L1 empty)
(define L2 (cons B1 empty))
(define L3 (cons B1 (cons (make-ball (/ WIDTH 2) (/ HEIGHT 2) 5 -7) empty)))

(@dd-template-rules one-of           ;2 cases
                    atomic-distinct  ;empty
                    compound         ;cons
                    ref              ;(first lob) is Ball
                    self-ref)        ;(rest lob) is ListOfBall

(define (fn-for-lob lob)
  (cond [(empty? lob)(...)]
        [else
         (... (fn-for-ball (first lob))
              (fn-for-lob (rest lob)))]))

;; ===========================================================================
;; ===========================================================================
;; Functions:

(@htdf main)
(@signature ListOfBall -> ListOfBall)
;; start the game, call with (main (cons B1 empty)) 
;; <no tests for main functions>

(@template htdw-main)

(define (main b)
  (big-bang b
            (on-draw   render-balls)   ;lob -> Image
            (on-tick   next-balls)     ;lob -> Ball
            (on-mouse  handle-mouse)  ;lob Integer Integer MouseEvent -> Ball
            (on-key    handle-key)))  ;lob KeyEvent -> ListOfBall
            

(@htdf render-balls)
(@signature ListOfBall -> Image)
;; places BALLs in a list on image at appropriate x, y coordinate
(check-expect (render-balls empty)
              MTS)
(check-expect (render-balls (cons (make-ball 20 30 3 3) empty))
              (place-image BALL 20 30 MTS))
(check-expect (render-balls (cons (make-ball (- WIDTH 4)
                                             (- HEIGHT 5) -2 -3)
                                  (cons (make-ball 20 30 3 3) empty)))
              (place-image BALL (- WIDTH 4) (- HEIGHT 5)
                           (place-image BALL 20 30 MTS)))
#;
(define (render-balls b) MTS)

(@template ListOfBall)

(define (render-balls lob)
  (cond [(empty? lob) MTS]
        [else
         (place-ball (first lob)
              (render-balls (rest lob)))]))



(@htdf place-ball)
(@signature Ball Image -> Image)
;; place BALL on image at appropriate x, y coordinate
(check-expect (place-ball (make-ball 20 30 3 3) MTS)
              (place-image BALL 20 30 MTS))
(check-expect (place-ball (make-ball (- WIDTH 4) (- HEIGHT 5) -2 -3) MTS)
              (place-image BALL (- WIDTH 4) (- HEIGHT 5) MTS))
#;
(define (place-ball b img) MTS)

(@template Ball)

(define (place-ball b img)
  (place-image BALL (ball-x b) (ball-y b) img))



(@htdf next-balls)
(@signature ListOfBall -> ListOfBall)
;; produces next balls in the list
(check-expect (next-balls empty) empty)
(check-expect (next-balls (cons (make-ball 20 30 3 3) empty))
              (cons (next-ball (make-ball 20 30 3 3)) empty))
(check-expect (next-balls (cons (make-ball 20 30 3 3) (cons B1 empty)))
              (cons (next-ball (make-ball 20 30 3 3))
                    (cons (next-ball B1) empty)))

;(define (next-balls lob) lob)

(@template ListOfBall)

(define (next-balls lob)
  (cond [(empty? lob) empty]
        [else
         (cons (next-ball (first lob))
              (next-balls (rest lob)))]))

(@htdf next-ball)
(@signature Ball -> Ball)
;; produce ball at next x,y; checks bounces off top/right/bottom/left wall
(check-expect (next-ball     (make-ball (+ LEF 1) TOP  3 -4))
              (bounce-top    (make-ball (+ LEF 1) TOP  3 -4)))
(check-expect (next-ball     (make-ball (+ LEF 1) BOT  3  4))
              (bounce-bottom (make-ball (+ LEF 1) BOT  3  4)))
(check-expect (next-ball     (make-ball LEF (+ TOP 1) -3 4))
              (bounce-left   (make-ball LEF (+ TOP 1) -3 4)))
(check-expect (next-ball     (make-ball RIG (+ TOP 1)  3 4))
              (bounce-right  (make-ball RIG (+ TOP 1)  3 4)))
(check-expect (next-ball     (make-ball (/ WIDTH 2) (/ HEIGHT 2) 3 4))
              (glide         (make-ball (/ WIDTH 2) (/ HEIGHT 2) 3 4)))

#;
(define (next-ball b) b)

(@template Number) ;(@template Number) because b is treated as atomic

(define (next-ball b)
  (cond [(touch-top?    b) (bounce-top b)]
        [(touch-bottom? b) (bounce-bottom b)]
        [(touch-right?  b) (bounce-right b)]
        [(touch-left?   b) (bounce-left b)]
        [else
         (glide b)]))


(@htdf handle-mouse)
(@signature ListOfBall Integer Integer MouseEvent -> ListOfBall)
;; insert new ball on mouse click
;; NOTE: uses random, so testing has to use check-random
(check-random (handle-mouse empty 100 200 "button-down")
              (cons (make-ball 100 200 (- 5 (random 11))
                               (- 5 (random 11))) empty))
(check-random (handle-mouse (cons (make-ball 1 2 3 4) empty)
                            100 200 "button-up")
              (cons (make-ball 1 2 3 4) empty))

(check-random (handle-mouse (cons (make-ball 1 2 3 4)
                                  (cons (make-ball 2 4 6 8) empty))
                            100 200 "button-down")
              (cons (make-ball 100 200 (- 5 (random 11)) (- 5 (random 11)))
                    (cons (make-ball 1 2 3 4)
                    (cons (make-ball 2 4 6 8) empty))))

 
(make-ball 100 200 (- 5 (random 11)) (- 5 (random 11)))
#;
(define (handle-mouse b x y me) b)

(@template MouseEvent)

#;
(define (handle-mouse b x y me)
  (cond [(mouse=? me "button-down")
         (make-ball x y (- 5 (random 11)) (- 5 (random 11)))]
        [else b]))


(define (handle-mouse lob x y me)
  (cond [(mouse=? me "button-down")
         (cons (make-ball x y (- 5 (random 11)) (- 5 (random 11))) lob)]
        [else lob]))




(@htdf handle-key)
(@signature ListOfBall KeyEvent -> ListOfBall)
;; remove all balls
(check-expect (handle-key L1 " ") empty)
(check-expect (handle-key L1 "a") L1)
(check-expect (handle-key L2 " ") empty)
(check-expect (handle-key L2 "a") L2)
(check-expect (handle-key L3 " ") empty)
(check-expect (handle-key L3 "a") L3)

;(define (handle-key lob ke) empty)

(@template KeyEvent)

(define (handle-key lob ke)
  (cond [(key=? ke " ") empty]
        [else
         lob]))


(@htdf touch-top?)
(@signature Ball -> Boolean)
;; true if ball is going up and edge will hit or pass top edge of box
(check-expect (touch-top?    (make-ball LEF (+ TOP  5) 3 -4)) false)
(check-expect (touch-top?    (make-ball LEF (+ TOP  4) 3 -4)) true)
(check-expect (touch-top?    (make-ball LEF (+ TOP  1) 3 -2)) true)
(check-expect (touch-top?    (make-ball LEF (+ TOP  0) 3  2)) false)
#;
(define (touch-top? b) false)

(@template Ball)

(define (touch-top? b)
  (<= (+ (ball-y b) (ball-dy b)) TOP))


(@htdf touch-bottom?)
(@signature Ball -> Boolean)
;; true if ball is going down and edge will hit or pass bottom edge of box
(check-expect (touch-bottom? (make-ball LEF (- BOT 3) 3  2)) false)
(check-expect (touch-bottom? (make-ball LEF (- BOT 2) 3  2)) true)
(check-expect (touch-bottom? (make-ball LEF (- BOT 0) 3  2)) true)
(check-expect (touch-bottom? (make-ball LEF (- BOT 0) 3 -2)) false)
#;
(define (touch-bottom? b) false)

(@template Ball)

(define (touch-bottom? b)
  (>= (+ (ball-y b) (ball-dy b)) BOT))


(@htdf touch-left?)
(@signature Ball -> Boolean)
;; true if ball is going left and edge will hit or pass left  edge of box
(check-expect (touch-left?   (make-ball (+ LEF 6) TOP -5 2)) false)
(check-expect (touch-left?   (make-ball (+ LEF 5) TOP -5 2)) true)
(check-expect (touch-left?   (make-ball (+ LEF 0) TOP -5 2)) true)
(check-expect (touch-left?   (make-ball (+ LEF 0) TOP  3 2)) false)
#;
(define (touch-left? b) false)

(@template Ball)

(define (touch-left? b)
  (<= (+ (ball-x b) (ball-dx b)) LEF))


(@htdf touch-right?)
(@signature Ball -> Boolean)
;; true if ball is going right and edge will hit or pass right edge of box
(check-expect (touch-right?  (make-ball (- RIG 6) TOP  5 2)) false)
(check-expect (touch-right?  (make-ball (- RIG 5) TOP  5 2)) true)
(check-expect (touch-right?  (make-ball (- RIG 0) TOP  5 2)) true)
(check-expect (touch-right?  (make-ball (- RIG 0) TOP -3 2)) false)
#;
(define (touch-right? b) false)

(@template Ball)

(define (touch-right? b)
  (>= (+ (ball-x b) (ball-dx b)) RIG))


(@htdf bounce-top)
(@signature Ball -> Ball)
;; produce a ball with top edge 1 pixel off top of box, moving down
;; CONSTRAINT: assume ball is close to top edge and moving up
(check-expect (bounce-top (make-ball (+ LEF 1) (+ TOP 3) 2 -4))
              (make-ball (+ LEF 1) (+ TOP 1) 2  4))
(check-expect (bounce-top (make-ball (+ LEF 2) (+ TOP 6) 3 -7))
              (make-ball (+ LEF 2) (+ TOP 1) 3 7))
#;
(define (bounce-top b) b)

(@template Ball)

(define (bounce-top b)
  (make-ball (ball-x b) (+ TOP 1) (ball-dx b) (- (ball-dy b))))


(@htdf bounce-bottom)
(@signature Ball -> Ball)
;; produce a ball with bottom edge 1 pixel off bottom of box, moving up
;; CONSTRAINT: assume ball is close to bottom edge and moving down
(check-expect (bounce-bottom (make-ball (+ LEF 1) (- BOT 3) 2 4))
              (make-ball (+ LEF 1) (- BOT 1) 2  -4))
(check-expect (bounce-bottom (make-ball (+ LEF 2) (- BOT 6) 3 7))
              (make-ball (+ LEF 2) (- BOT 1) 3 -7))
#;
(define (bounce-bottom b) b)

(@template Ball)

(define (bounce-bottom b)
  (make-ball (ball-x b) (- BOT 1) (ball-dx b) (- (ball-dy b))))

(@htdf bounce-left)
(@signature Ball -> Ball)
;; produce a ball with left edge 1 pixel off left of box, moving right
;; CONSTRAINT: assume ball is close to left edge and moving left
(check-expect (bounce-left (make-ball (+ LEF 3) (+ TOP 2) -4 4))
              (make-ball (+ LEF 1) (+ TOP 2) 4 4))
(check-expect (bounce-left (make-ball (+ LEF 5) (+ TOP 2) -8 4))
              (make-ball (+ LEF 1) (+ TOP 2) 8 4))
#; 
(define (bounce-left b) b)

(@template Ball)

(define (bounce-left b)
  (make-ball (+ LEF 1) (ball-y b) (- (ball-dx b)) (ball-dy b) ))


(@htdf bounce-right)
(@signature Ball -> Ball)
;; produce a ball with right edge 1 pixel off right of box, moving left
;; CONSTRAINT: assume ball is close to right edge and moving right
(check-expect (bounce-right (make-ball (- RIG 3) (+ TOP 1) 4 4))
              (make-ball (- RIG 1) (+ TOP 1) -4 4))
(check-expect (bounce-right (make-ball (- RIG 5) (+ TOP 1) 8 4))
              (make-ball (- RIG 1) (+ TOP 1) -8 4))
#;
(define (bounce-right b) b)

(@template Ball)

(define (bounce-right b)
  (make-ball (- RIG 1) (ball-y b) (- (ball-dx b)) (ball-dy b)))



(@htdf glide)
(@signature Ball -> Ball)
;; move ball by dx dy
;; CONSTRAINT: ball is not touching or about to touch any edge of the box
(check-expect (glide (make-ball 100 200 2 3)) (make-ball 102 203 2 3))
(check-expect (glide (make-ball 50 220 -3 -2)) (make-ball 47 218 -3 -2))

;(define (glide b) b)

(@template Ball)

(define (glide b)
  (make-ball (+ (ball-x b) (ball-dx b))
             (+ (ball-y b) (ball-dy b))
             (ball-dx b)
             (ball-dy b)))

