;;; Copyright © 2011 Doug Orleans.  License is AGPL 3.

#lang racket

(provide (all-defined-out))

(require "utils.rkt")

(define *ranks* '(a 2 3 4 5 6 7 8 9 10 j q k))
(define *suits* '(c d s h))

(define make-card list)
(define card-rank car)
(define card-suit cadr)
(define (display-card card)
  (cond (card
	 (unless (eq? (card-rank card) 10) (display " "))
	 (display (card-rank card))
	 (display (card-suit card)))
	(else (display "   "))))
(define (parse-card sym)
  (and (symbol? sym)
       (match (symbol->string sym)
         ((regexp #rx"^(.+)(.)$" (list _ rank suit))
          (make-card (or (string->number rank) (string->symbol rank))
                     (string->symbol suit)))
         (_ #f))))

(define (card-color card)
  (case (card-suit card)
    ((c s) 'black)
    ((d h) 'red)))

(define (card-value card)
  (add1 (- (length *ranks*) (length (memq (card-rank card) *ranks*)))))

(define (make-deck)
  (cross-product *ranks* *suits*))
