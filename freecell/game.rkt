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

(define (display-game game)
  (define (d x) (display x))
  (define (dc card) (display-card card))
  (define (dbc card) (d "[") (dc card) (d "] "))
  (define (dsc card) (d "  ") (dc card) (d "   "))
  (for-each (lambda (cell) (dbc cell))
	    (vector->list (game-cells game)))
  (d "                    ")
  (for-each (lambda (pile) (dbc pile))
	    (vector->list (game-piles game)))
  (newline)
  (let loop ((stacks (map (lambda (stack) (reverse stack))
			  (vector->list (game-stacks game)))))
    (if (andmap null? stacks)
	(void)
	(begin (for-each (lambda (card) (dsc card))
			 (map (lambda (stack)
				(if (null? stack) #f (car stack)))
			      stacks))
	       (newline)
	       (loop (map (lambda (stack)
			    (if (null? stack) '() (cdr stack)))
			  stacks))))))

(define (parse-game lists)
  (define (parse-card-list l)
    (map parse-card l))
  (let ((cells (parse-card-list (first lists)))
	(stacks (map (lambda (stack) (parse-card-list stack)) (second lists)))
	(piles (parse-card-list (third lists))))
    (apply make-game (map list->vector (list cells stacks piles)))))

;; (define (display-game-history game)
;;   (let loop ((hist (game-hist game)))
;;   (cond ((not (null? hist))
;; 	 (loop (cdr hist))
;; 	 (display-game game)))))

(define (deal ncols ncells)
  (let ((stacks (make-vector ncols null)))
    (let loop ((deck (shuffle (make-deck))) (i 0))
      (if (null? deck)
	  (make-game (make-vector ncells #f)
		     stacks
		     (make-vector (length *suits*) #f))
	  (let ((card (car deck))
		(stack (vector-ref stacks i)))
	    (vector-set! stacks i (cons card stack))
	    (loop (cdr deck) (modulo (add1 i) ncols)))))))

(define make-where list)
(define where-loc car)
(define where-i cadr)

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