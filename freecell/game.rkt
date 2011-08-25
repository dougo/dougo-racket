;;; Copyright © 2011 Doug Orleans.  License is AGPL 3.

#lang racket

(provide (all-defined-out))

(require "cards.rkt")

(define make-game list)
(define game-cells first)
(define game-stacks second)
(define game-piles third)
(define (copy-game game)
  (make-game (vector-copy (game-cells game))
	     (vector-copy (game-stacks game))
	     (vector-copy (game-piles game))))

(define (won? game)
  (and (andmap not (vector->list (game-cells game)))
       (andmap null? (vector->list (game-stacks game)))))

(define (display-game game)
  (define (d x) (display x))
  (define (dc card) (display-card card))
  (define (dbc card) (d "[") (dc card) (d "] "))
  (define (dsc card) (d "  ") (dc card) (d "   "))
  (for ((cell (game-cells game))) (dbc cell))
  (d "                    ")
  (for ((pile (game-piles game))) (dbc pile))
  (newline)
  (let loop ((stacks (map reverse (vector->list (game-stacks game)))))
    (if (andmap null? stacks)
	(void)
	(begin (for ((stack stacks)) (dsc (if (null? stack) #f (car stack))))
	       (newline)
	       (loop (map (lambda (stack) (if (null? stack) '() (cdr stack)))
			  stacks))))))

(define (parse-game lists)
  (define (parse-card-list l)
    (map parse-card l))
  (let ((cells (parse-card-list (first lists)))
	(stacks (map parse-card-list (second lists)))
	(piles (parse-card-list (third lists))))
    (apply make-game (map list->vector (list cells stacks piles)))))

(define (deal ncols ncells)
  (let ((stacks (make-vector ncols null)))
    (for (((card i) (in-indexed (shuffle (make-deck)))))
      (let* ((stack-num (modulo i ncols))
             (stack (vector-ref stacks stack-num)))
        (vector-set! stacks stack-num (cons card stack))))
    (make-game (make-vector ncells #f)
               stacks
               (make-vector (length *suits*) #f))))      

(define make-where list)
(define where-loc first)
(define where-i second)

(define (legal-move? game from to)
  (let ((card (card-at game from)))
    (and card
	 (not (eq? (where-loc from) 'pile))
	 (let ((loc (where-loc to))
	       (i (where-i to)))
	   (case loc
	     ((stack) (can-stack? card (card-at game to)))
	     ((cell) (not (card-at game to)))
	     ((pile) (can-pile? card (vector-ref (game-piles game) i) i)))))))

(define (can-stack? card onto)
  (or (not onto)
      (and (not (eq? (card-color card) (card-color onto)))
	   (= (card-value card) (sub1 (card-value onto))))))

(define (can-pile? card onto i)
  (and (eq? (card-suit card) (list-ref *suits* i))
       (= (card-value card) (add1 (if onto (card-value onto) 0)))))

(define (move! game from to)
  (let ((card (card-at game from)))
    (remove-card! game from)
    (add-card! game to card))
  game)

(define (card-at game where)
  (let ((loc (where-loc where))
	(i (where-i where)))
    (case loc
      ((stack) (let ((stack (vector-ref (game-stacks game) i)))
		 (if (null? stack) #f (car stack))))
      ((cell) (vector-ref (game-cells game) i))
      ((pile) (vector-ref (game-piles game) i)))))

(define (remove-card! game where)
  (let ((loc (where-loc where))
	(i (where-i where)))
    (case loc
      ((stack) (let ((stacks (game-stacks game)))
		 (vector-set! stacks i (cdr (vector-ref stacks i)))))
      ((cell) (vector-set! (game-cells game) i #f)))))

(define (add-card! game where card)
  (let ((loc (where-loc where))
	(i (where-i where)))
    (case loc
      ((stack) (let ((stacks (game-stacks game)))
		 (vector-set! stacks i (cons card (vector-ref stacks i)))))
      ((cell) (vector-set! (game-cells game) i card))
      ((pile) (vector-set! (game-piles game) i card)))))
