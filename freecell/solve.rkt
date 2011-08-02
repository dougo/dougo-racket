;;; Copyright © 2011 Doug Orleans.  License is AGPL 3.

#lang racket

(provide (all-defined-out))

(require "game.rkt")
(require "utils.rkt")

(define *final-games* null)

(define start-history list)
(define add-game cons)
(define add-games append)
(define (in-history? game history)
  (memf (lambda (g) (game-equal? g game)) history))

(define (solve game n)
  (set! *final-games*
	(unique-succ-n (list game) (start-history game) n)))

(define (unique-succ-n games history n)
  (let loop ((games games) (history history) (n n) (succ-n null))
    (if (= n 0)
	(list games history)
	(if (null? games)
	    (begin (displayln (length succ-n))
		   (loop succ-n history (sub1 n) null))
	    (let* ((game (car games))
		   (succ (unique-successors game history)))
	      (loop (cdr games)
		    (add-games succ history)
		    n
		    (append succ succ-n)))))))

(define (unique-successors game history)
  (let loop ((succ (successors game)) (seen history) (unique null))
    (if (null? succ)
	unique
	(let ((game (car succ)))
	  (if (in-history? game seen)
	      (loop (cdr succ) seen unique)
	      (loop (cdr succ) (add-game game seen) (cons game unique)))))))

(define make-move list)
(define move-from first)
(define move-to second)

(define (successors game)
  (map (lambda (move) (successor game (move-from move) (move-to move)))
       (legal-moves game)))

(define (successor game from to)
  (let ((new (copy-game game)))
    (move! new from to)
    new))


(define (game-equal? game1 game2)
  (and (equal? (game-piles game1) (game-piles game2))
       (permutation? (game-cells game1) (game-cells game2))
       (permutation? (game-stacks game1) (game-stacks game2))))

(define (permutation? vec1 vec2)
  (let ((l1 (vector->list vec1)))
    (andmap (lambda (x) (member x l1))
	    (vector->list vec2))))

(define (legal-moves game)
  (filter (lambda (move) (legal-move? game (move-from move) (move-to move)))
          (valid-moves game)))

(define (valid-moves game)
  (define (make-wheres loc vec)
    (for/list ((i (in-range (vector-length vec))))
      (make-where loc i)))
  (let* ((valid-froms (append (make-wheres 'cell (game-cells game))
			      (make-wheres 'stack (game-stacks game))))
	 (valid-tos (append valid-froms (make-wheres 'pile (game-piles game)))))
    (cartesian-product make-move valid-froms valid-tos)))


;; freecell.net Game #: 7x4 8954 difficulty 6
(define *game*
  (parse-game
   '(
     ;;free cells
     (#f #f #f #f)
     ;;stacks from bottom of screen to top
     ((6s 7h 4d ac ks qh 3h qc)
      (7s 2c 7d 9s 9h 5c 8s kh)
      (5s 6c 9c ah 9d kd 3s jd)
      (2d qs 10s jc 10d 3c 4h)
      (10c kc jh 6h 5h 5d 10h)
      (2h 7c as 3d js qd 8h)
      (2s 4s 8d 8c ad 6d 4c))
     ;;piles, in alphabetical order
     (#f #f #f #f))))


(define *game2*
  (parse-game
   '(
     ;;free cells
     (kd qd #f #f)
     ;;stacks from bottom of screen to top
     ((3d 4s 5d 6c)
      (2d 3c 4h 5s 6d 7c 8h 9c 10d kc 10s jh qh)
      (4c 5h 6s 7h 8s 9d 10c ad ks 9h 7d 3h)
      (6h js qs 4d jd)
      (qc kh)
      (8c)
      (8d 9s 10h jc 7s 3s 5c))
     ;;piles, in alphabetical order
     (2c #f 2s 2h))))
