#lang racket
; spaceship.rkt
;
; An amazing image of a rainbow spaceship I've created.
;
; CSC-151-02 Fall 2021
; Mini Project 1, Part 1
; Author: David Quintana
; Date: 2021-09-14

(require 2htdp/image)
(require csc151)

;colored rectangles
(define red-rect
  (rectangle 100 25 'solid "red" ) )
(define orange-rect
  (rectangle 100 25 'solid "orange" ) )
(define yellow-rect
  (rectangle 100 25 'solid "yellow" ) )
(define green-rect
  (rectangle 100 25 'solid "green" ) )
(define blue-rect
  (rectangle 100 25 'solid "blue" ) )
(define violet-rect
  (rectangle 100 25 'solid "violet" ) )

;collum 1
(define coll1
  ( above red-rect orange-rect) )

;collum 2
(define coll2
  (above coll1 yellow-rect) )

;collum3
(define coll3
  (above coll2 green-rect) )

;collum4
(define coll4
  (above coll3 blue-rect) )

;collum5
(define coll5
  (above coll4 violet-rect) )

;rainbow space ship
(define rainbow-spaceship
  (beside red-rect coll1 coll2 coll3 coll4 coll5 coll4 coll3 coll2 coll1 red-rect) )