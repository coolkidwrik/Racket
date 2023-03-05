;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname demolish-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@assignment bank/htdd-p2)
(@cwl wriksen)

;; =================
;; Data definitions:

(@problem 1)
;; You are assigned to develop a system that will classify 
;; buildings in downtown Vancouver based on how old they are. 
;; According to city guidelines, there are three different classification
;; levels: new, old, and heritage.
;;
;; Design a data definition to represent these classification levels. 
;; Call it BuildingStatus.

(@htdd BuildingAge)
;;BuildingAge is one of:
;; - "old"
;; - "new"
;; - "heritage"
;;interp. how old buildings in vancouver are.

;;<examples are redundant for enumerations>
(@dd-template-rules one-of          ;3 cases
                    atomic-distinct ; "old"
                    atomic-distinct ; "new"
                    atomic-distinct); "heritage"
(define (fn-for-Building-Age ba)
  (cond [(string=? ba "old") (...)]
        [(string=? ba "new") (...)]
        [(string=? ba "heritage") (...)]))


;; =================
;; Functions:

(@problem 2)
;; The city wants to demolish all buildings classified as "old". 
;; You are hired to design a function called demolish? 
;; that determines whether a building should be torn down or not.
(@htdf demolish?)
(@signature BuildingAge -> Boolean)
;produce true if building is old
(check-expect (demolish? "new") false)
(check-expect (demolish? "old") true)
(check-expect (demolish? "heritage") false)

;(define (demolish? ba) false)

(@template BuildingAge)

(define (demolish? ba)
  (cond [(string=? ba "old") true]
        [(string=? ba "new") false]
        [(string=? ba "heritage") false]))
