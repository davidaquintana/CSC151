#lang racket
(require "blackjack.rkt" "poker.rkt" "slot-machine.rkt" "roulette.rkt")

;; CSC-151 (Fall 2021)
;; Mini Project 8 : Casino
;; Authors: Corwin Silverman (they/them), David Quintina (he/him), Jio Hong (he/him), and Patrick Sales Garcia (he/him)
;; Date: November 19, 2021
;; Acknowledgements : Looked at Racket documentation for how to use provide and require

;Use the following Blackjack procedures:
; (make-blackjack)
; (blackjack-hit)
; (blackjack-finish-hit)
; (blackjack-stay)
; (blackjack-finish-stay)

;Use the following Poker procedures:
; (make-poker game player-count starting-cash)
; (poker-take-turn! game bet)

;Use the following Roulette procedures:
(define 1st12 (range 1 13))
(define 2nd12 (range 13 25))
(define 3rd12 (range 25 37))
(define 2to1-1 (range 1 35 3))
(define 2to1-2 (range 2 36 3))
(define 2to1-3 (range 3 37 3))
(define 1to18 (range 1 19))
(define 19to36 (range 19 37))
(define even (filter even? (range 1 37)))
(define odd (filter odd? (range 1 37)))
(define black (list 2 4 6 8 10 11 13 15 17 20 22 24 26 28 29 31 33 35))
(define red (list 1 3 5 7 9 12 14 16 18 19 21 23 25 27 30 32 34 36))
(define roulette-scoreboard (make-hash))
; (roulette-tutorial)
; (roulette scores name lst)
; (roulette-start)
; (roulette-play! scores name amount)
; (roulette-bets)

;Use the following Slot Machine procedures:
; (slot-machine)

