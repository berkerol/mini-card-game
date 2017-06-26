#lang scheme
; compiling: yes
; complete: yes
; 2014400189

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(card-color one-card) -> symbol?
;one-card: -> pair?
;
;Returns the color of the given card.
;
;Examples:
;> (card-color '(D . 10) )
;=> red
;> (card-color '(C . 5) )
;=> black
(define (card-color one-card)
  (cond
    ((symbol=? (car one-card) 'H) 'red)
    ((symbol=? (car one-card) 'D) 'red)
    ((symbol=? (car one-card) 'S) 'black)
    (else 'black)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(card-rank one-card) -> number?
;one-card: -> pair?
;
;Returns the rank of the given card.
;
;Examples:
;> (card-rank '(C . A) )
;=> 11
;> (card-rank '(S . 7) )
;=> 7
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(all-same-color list-of-cards) -> boolean?
;list-of-cards: -> list?
;
;Returns #t if all the cards in list-of-cards have same color, #f otherwise.
;
;Examples:
;> (all-same-color '((H . 9) (H . 5) (H . 2) (D . A) (D . 4) (D . 10)) )
;=> #t
;> (all-same-color '((H . 9) (H . 5) (H . 2) (D . A) (D . 4) (C . J)) )
;=> #f
(define (all-same-color list-of-cards)
  (define list (map (lambda(x) (card-color x)) list-of-cards))
  (andmap (lambda(x) (equal? x (car list))) list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(fdraw list-of-cards held-cards) -> list?
;list-of-cards: -> list?
;held-cards: -> list?
;
;Returns a new list of held-cards after the first card in list-of-cards is taken.
;
;Examples:
;> (fdraw '((H . 5) (H . 10) (H . A) (D . J) (D . Q) (D . K)) '())
;=> '((H . 5))
;> (fdraw '((H . 5) (H . 10) (H . A) (D . J) (D . Q) (D . K)) '((S . 7) (S . 10) (S . K)))
;=> '((S . 7) (S . 10) (S . K) (H . 5))
(define (fdraw list-of-cards held-cards)
  (append held-cards (list (car list-of-cards))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(fdiscard list-of-cards list-of-moves goal held-cards) -> list?
;list-of-cards: -> list?
;list-of-moves: -> list?
;goal: -> number?
;held-cards: -> list?
;
;Returns a new list of held-cards after the minimum ranked card in held-cards is taken.
;
;Examples:
;> (fdiscard '((S . 3) (S . 2) (S . A) (D . J) (H . Q) (H . J))
;'(draw draw discard discard) 72
;'((H . 5) (H . 10) (H . A) (D . J) (D . Q) (D . K)) )
;=> ((H . 10) (H . A) (D . J) (D . Q) (D . K))
;> (fdiscard '((H . 5) (H . 10) (H . A) (D . J) (D . Q) (D . K))
;'(draw draw draw discard) 40
;'((S . 3) (S . 2) (S . A) (D . J) (H . Q) (H . J)) )
;=> ((S . 3) (S . A) (D . J) (H . Q) (H . J))
(define(fdiscard list-of-cards list-of-moves goal held-cards)
  (remove (discard list-of-cards list-of-moves goal held-cards) held-cards))

;(discard cards moves goal held-cards) -> pair?
;cards: -> list?
;moves: -> list?
;goal: -> number?
;held-cards: -> list?
;
;Returns the minimum ranked card in held-cards.
;
;Examples:
;> (discard '((S . 3) (S . 2) (S . A) (D . J) (H . Q) (H . J))
;'(draw draw discard discard) 72
;'((H . 5) (H . 10) (H . A) (D . J) (D . Q) (D . K)) )
;=> (H . 5)
;> (discard '((H . 5) (H . 10) (H . A) (D . J) (D . Q) (D . K))
;'(draw draw draw discard) 40
;'((S . 3) (S . 2) (S . A) (D . J) (H . Q) (H . J)) )
;=> (S . 2)
(define (discard cards moves goal held-cards)
  (define removed (apply min (map (lambda(x) (card-rank x)) held-cards)))
  (findf (lambda (x) (eqv? removed (card-rank x))) held-cards)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(find-steps list-of-cards list-of-moves goal) -> list?
;list-of-cards: -> list?
;list-of-moves: -> list?
;goal: -> number?
;
;Returns a list of steps whose steps are pairs of moves and cards.
;Wrapper function of make-steps.
;
;Examples:
;> (find-steps '((H . 5) (H . 10) (H . A) (D . J) (D . Q) (D . K)) '(draw draw draw discard) 26 )
;=> ((draw (H . 5)) (draw (H . 10)) (draw (H . A)) (discard (H . 5)))
;> (find-steps '((S . 3) (S . 2) (S . A) (D . J) (H . Q) (H . J)) '(draw draw discard discard) 14 )
;=> ((draw (S . 3)) (draw (S . 2)) (discard (S . 2)) (discard (S . 3)))
(define (find-steps list-of-cards list-of-moves goal)
  (make-steps list-of-cards list-of-moves goal '() '()))

;(make-steps cards moves goal steps held-cards) -> list?
;cards: -> list?
;moves: -> list?
;goal: -> number?
;steps: -> list?
;held-cards: -> list?
;
;Returns a list of steps whose steps are pairs of moves and cards.
;Helper function of find-steps.
;
;Examples:
;> (make-steps '((H . 5) (H . 10) (H . A) (D . J) (D . Q) (D . K)) '(draw draw draw discard) 26
;'() '() )
;=> ((draw (H . 5)) (draw (H . 10)) (draw (H . A)) (discard (H . 5)))
;> (make-steps '((S . 3) (S . 2) (S . A) (D . J) (H . Q) (H . J)) '(draw draw discard discard) 14
;'() '() )
;=> ((draw (S . 3)) (draw (S . 2)) (discard (S . 2)) (discard (S . 3)))
(define (make-steps cards moves goal steps held-cards)
  (cond
    ((null? moves) steps)
    ((> (calc-playerpoint held-cards) goal) steps)
    ((and (symbol=? (car moves) 'draw) (null? cards)) steps)
    ((and (symbol=? (car moves) 'discard) (null? held-cards)) steps)
    ((symbol=? (car moves) 'draw)
     (make-steps (cdr cards) (cdr moves) goal
                 (append steps (list (list 'draw (car cards))))
                 (fdraw cards held-cards)))
    (else
     (make-steps cards (cdr moves) goal
                 (append steps (list (list 'discard (discard cards moves goal held-cards))))
                 (fdiscard cards moves goal held-cards)))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(find-held-cards list-of-steps) -> list?
;list-of-steps: -> list?
;
;Returns held-cards after processing list-of-steps.
;Wrapper function of traverse-steps.
;
;Examples:
;> (find-held-cards '((draw (H . 5)) (draw (H . 10)) (draw (H . A)) (discard (H . 5))) )
;=> ((H . 10) (H . A))
;> (find-held-cards '((draw (S . 3)) (draw (S . 2)) (discard (S . 2)) (discard (S . 3))) )
;=> ()
(define (find-held-cards list-of-steps)
  (traverse-steps list-of-steps '()))

;(traverse-steps steps held-cards) -> list?
;list-of-steps: -> list?
;held-cards: -> list?
;
;Returns held-cards after processing list-of-steps.
;Helper function of find-held-cards.
;
;Examples:
;> (traverse-steps '((draw (H . 5)) (draw (H . 10)) (draw (H . A)) (discard (H . 5))) '() )
;=> ((H . 10) (H . A))
;> (traverse-steps '((draw (S . 3)) (draw (S . 2)) (discard (S . 2)) (discard (S . 3))) '() )
;=> ()
(define (traverse-steps steps held-cards)
  (if (null? steps) held-cards
      (if (symbol=? (caar steps) 'draw)
          (traverse-steps (cdr steps) (append held-cards (cdar steps)))
          (traverse-steps (cdr steps) (remove (cadar steps) held-cards))
          )
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(calc-playerpoint list-of-cards) -> number?
;list-of-cards: -> list?
;
;Calculates playerpoint from list-of-cards.
;
;Examples:
; (calc-playerpoint '((H . 5) (H . 10) (H . A) (D . J) (D . Q) (D . K)) )
;=> 56
; (calc-playerpoint '((S . 3) (S . 2) (S . A) (D . J) (H . Q) (H . J)) )
;=> 46
(define (calc-playerpoint list-of-cards)
  (apply + (map (lambda(x) (card-rank x)) list-of-cards)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(calc-score list-of-cards goal) -> number?
;list-of-cards: -> list?
;goal: -> number?
;
;Calculates finalscore from prescore and list-of-cards.
;
;Examples:
; (calc-score '((H . 5) (H . 10) (H . A) (D . J) (D . Q) (D . K))  50 )
;=> 15
; (calc-score '((S . 3) (S . 2) (S . A) (D . J) (H . Q) (H . J)) 60 )
;=> 14
(define (calc-score list-of-cards goal)
  (define prescore(calc-prescore list-of-cards goal))
  (if (all-same-color list-of-cards) (quotient prescore 2) prescore))

;(calc-prescore list-of-cards goal) -> number?
;list-of-cards: -> list?
;goal: -> number?
;
;Calculates prescore from playerpoint and goal.
;
;Examples:
; (calc-prescore '((H . 5) (H . 10) (H . A) (D . J) (D . Q) (D . K))  50 )
;=> 30
; (calc-prescore '((S . 3) (S . 2) (S . A) (D . J) (H . Q) (H . J)) 60 )
;=> 14
(define (calc-prescore list-of-cards goal)
  (define playerpoint (calc-playerpoint list-of-cards))
  (if (> playerpoint goal) (* 5 (- playerpoint goal)) (- goal playerpoint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(play list-of-cards list-of-moves goal) -> number?
;list-of-cards: -> list?
;list-of-moves: -> list?
;goal: -> number?
;
;Returns finalscore after processing list-of-moves.
;
;Examples:
;> (play '((H . 5) (H . 10) (H . A) (D . J) (D . Q) (D . K)) '(draw draw draw discard) 26 )
;=> 2
;> (play '((S . 3) (S . 2) (S . A) (D . J) (H . Q) (H . J)) '(draw draw discard discard) 14 )
;=> 7
(define (play list-of-cards list-of-moves goal)
  (calc-score (find-held-cards (find-steps list-of-cards list-of-moves goal)) goal))
