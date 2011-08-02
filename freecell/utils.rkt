;;; Copyright © 2011 Doug Orleans.  License is AGPL 3.

#lang racket

(provide (all-defined-out))

(define (cartesian-product make-tuple list1 list2)
  (for*/list ((x list1) (y list2))
    (make-tuple x y)))
