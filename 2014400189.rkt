#lang scheme
; compiling: yes
; complete: yes
; 2014400189

;4.1 (2 points)
;(card-color one-card)
(define (card-color one-card)
  (cond
    ((symbol=? (car one-card) 'H) 'red)
    ((symbol=? (car one-card) 'D) 'red)
    ((symbol=? (car one-card) 'S) 'black)
    (else 'black)))

;4.2 (2 points)
;(card-rank one-card)
(define (card-rank one-card)
  (cond
    ((eqv? (cdr one-card) 2) 2)
    ((eqv? (cdr one-card) 3) 3)
    ((eqv? (cdr one-card) 4) 4)
    ((eqv? (cdr one-card) 5) 5)
    ((eqv? (cdr one-card) 6) 6)
    ((eqv? (cdr one-card) 7) 7)
    ((eqv? (cdr one-card) 8) 8)
    ((eqv? (cdr one-card) 9) 9)
    ((eqv? (cdr one-card) 10) 10)
    ((symbol=? (cdr one-card) 'J) 10)
    ((symbol=? (cdr one-card) 'Q) 10)
    ((symbol=? (cdr one-card) 'K) 10)
    (else 11)))

;4.3 (6 points)
;(all-same-color list-of-cards)
(define (all-same-color list-of-cards)
  (define list (map (lambda(x) (card-color x)) list-of-cards))
  (andmap (lambda(x) (equal? x (car list))) list))

;4.4 (10 points)
;(fdraw list-of-cards held-cards)
(define (fdraw list-of-cards held-cards)
  (append held-cards (car list-of-cards)))

;4.5 (10 points)
;(fdiscard list-of-cards list-of-moves goal held-cards)
(define(fdiscard list-of-cards list-of-moves goal held-cards)
  (cdr held-cards))

;4.8 (10 points)
;(calc-playerpoint list-of-cards)
(define (calc-playerpoint list-of-cards)
  (apply + (map (lambda(x) (card-rank x)) list-of-cards)))

;4.9 (10 points)
;(calc-score list-of-cards goal)
(define (calc-score list-of-cards goal)
  (define prescore(calc-prescore list-of-cards goal))
  (if (all-same-color list-of-cards) (/ prescore 2) prescore))
;(calc-prescore list-of-cards goal)
(define (calc-prescore list-of-cards goal)
  (define playerpoint (calc-playerpoint list-of-cards))
  (if (> playerpoint goal) (* 5 (- playerpoint goal)) (- goal playerpoint)))
