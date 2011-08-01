;;; Copyright © 2011 Doug Orleans.  License is AGPL 3.

#lang racket

(provide (all-defined-out))

(define (cross-product list1 list2)
  (let loop ((l1 list1) (l2 list2) (cp '()))
    (cond ((null? l1) cp)
	  ((null? l2) (loop (cdr l1) list2 cp))
	  (else (let ((pair (list (car l1) (car l2))))
		  (loop l1 (cdr l2) (cons pair cp)))))))
