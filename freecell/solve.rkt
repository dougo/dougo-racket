;;; Copyright © 2011 Doug Orleans.  License is AGPL 3.

#lang racket

(provide (all-defined-out))

(require "game.rkt")
(require "cards.rkt")
(require "utils.rkt")

(define *final-games* null)

(define (solve game n)
  (set! *final-games*
	(unique-succ-n (list game) (start-history game) n)))

(define (start-history game)
  (set (canonicalize game)))
(define (add-game game history)
  (set-add history (canonicalize game)))
(define (add-games games history)
  (set-union (list->set (map canonicalize games)) history))
(define (in-history? game history)
  (set-member? history (canonicalize game)))

(define (canonicalize game)
  (list (sort (vector->list (game-cells game)) card<?)
        (sort (vector->list (game-stacks game)) stack<?)
        (game-piles game)))

(define (card<? card1 card2)
  (and card1
       (or (not card2)
           (let ((v1 (card-value card1)) (v2 (card-value card2)))
             (or (< v1 v2)
                 (and (= v1 v2)
                      (string<? (symbol->string (card-suit card1))
                                (symbol->string (card-suit card2)))))))))

(define (stack<? stack1 stack2)
  (and (not (null? stack1))
       (or (null? stack2)
           (card<? (last stack1) (last stack2)))))


(define (unique-succ-n games history max-n)
  (let loop ((games null) (history history) (n 0) (succ-n games))
    (if (= n max-n)
	(list succ-n history)
	(if (null? games)
            (if (null? succ-n)
                (begin (printf "The game is unwinnable")
                       (printf " (at most ~a move~a can be made).~%"
                               (sub1 n) (if (= n 2) "" "s"))
                       (list succ-n history))
                (begin (displayln (length succ-n))
                       (loop succ-n history (add1 n) null)))
	    (let ((game (car games)))
              (if (won? game)
                  (begin (printf "Won the game after ~a move~a.~%"
                                 n (if (= n 1) "" "s"))
                         (list (cons game succ-n) history))
                  (let ((succ (unique-successors game history)))
                    (loop (cdr games)
                          (add-games succ history)
                          n
                          (append succ succ-n)))))))))

(define (unique-successors game history)
  (let loop ((succ (successors game)) (seen history) (unique '()))
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
