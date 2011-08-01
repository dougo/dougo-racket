;;; Copyright © 2011 Doug Orleans.  License is AGPL 3.

#lang racket

(provide (all-defined-out))

(require "utils.rkt")

(define *ranks* '(a 2 3 4 5 6 7 8 9 10 j q k))
(define *suits* '(c d s h))

(define make-card list)
(define card-rank car)
(define card-suit cadr)
(define (display-card card . port)
  (cond (card
	 (unless (eq? (card-rank card) 10) (apply display " " port))
	 (apply display (card-rank card) port)
	 (apply display (card-suit card) port))
	(else (apply display "   " port))))
(define (parse-card sym)
  (and sym
      (let* ((str (symbol->string sym))
	     (len (string-length str))
	     (suit (string->symbol (string (string-ref str (sub1 len)))))
	     (num (string->number (substring str 0 (sub1 len))))
	     (rank (if num num (string->symbol (substring str 0 1)))))
	(make-card rank suit))))      

(define (card-color card)
  (case (card-suit card)
    ((c s) 'black)
    ((d h) 'red)))

(define (card-value card)
  (add1 (- (length *ranks*) (length (memq (card-rank card) *ranks*)))))

(define (make-deck)
  (cross-product *ranks* *suits*))

(define (shuffle deck)
  (let ((vec (list->vector deck)))
    (define (swap! i j)
      (let ((tmp (vector-ref vec i)))
	(vector-set! vec i (vector-ref vec j))
	(vector-set! vec j tmp)))
    (do ((i (sub1 (vector-length vec)) (sub1 i)))
	((<= i 0) (vector->list vec))
      (swap! i (random (add1 i))))))
