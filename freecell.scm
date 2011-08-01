#lang scheme

(provide (all-defined-out))

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

(define make-game list)
(define game-cells car)
(define game-stacks cadr)
(define game-piles caddr)
(define (copy-game game)
  (make-game (vector-copy (game-cells game))
	     (vector-copy (game-stacks game))
	     (vector-copy (game-piles game))))
(define (vector-copy v) (list->vector (vector->list v)))

(define (display-game game . port)
  (define (d x) (apply display x port))
  (define (dc card) (apply display-card card port))
  (define (dbc card) (d "[") (dc card) (d "] "))
  (define (dsc card) (d "  ") (dc card) (d "   "))
  (define (nl) (apply newline port))
  (for-each (lambda (cell) (dbc cell))
	    (vector->list (game-cells game)))
  (d "                    ")
  (for-each (lambda (pile) (dbc pile))
	    (vector->list (game-piles game)))
  (nl)
  (let loop ((stacks (map (lambda (stack) (reverse stack))
			  (vector->list (game-stacks game)))))
    (if (andmap null? stacks)
	(void)
	(begin (for-each (lambda (card) (dsc card))
			 (map (lambda (stack)
				(if (null? stack) #f (car stack)))
			      stacks))
	       (nl)
	       (loop (map (lambda (stack)
			    (if (null? stack) '() (cdr stack)))
			  stacks))))))

(define (parse-game lists)
  (define (parse-card-list l)
    (map parse-card l))
  (let ((cells (parse-card-list (car lists)))
	(stacks (map (lambda (stack) (parse-card-list stack)) (cadr lists)))
	(piles (parse-card-list (caddr lists))))
    (apply make-game (map list->vector (list cells stacks piles)))))

;; (define (display-game-history game)
;;   (let loop ((hist (game-hist game)))
;;   (cond ((not (null? hist))
;; 	 (loop (cdr hist))
;; 	 (display-game game)))))

(define (deal ncols ncells)
  (let ((stacks (make-vector ncols)))
    (let loop ((deck (shuffle (make-deck))) (i 0))
      (if (null? deck)
	  (make-game (make-vector ncells #f)
		     stacks
		     (make-vector (length *suits*) #f))
	  (let ((card (car deck))
		(stack (vector-ref stacks i)))
	    (vector-set! stacks i (cons card stack))
	    (loop (cdr deck) (modulo (add1 i) ncols)))))))

(define *final-games* null)

(define (solve game n)
  (set! *final-games*
	(unique-succ-n (list game) (list game) n)))

(define (unique-succ-n games history n)
  (let loop ((games games) (history history) (n n) (succ-n '()))
    (if (= n 0)
	(list games history)
	(if (null? games)
	    (begin (display (length succ-n)) (newline)
		   (loop succ-n history (sub1 n) '()))
	    (let* ((game (car games))
		   (succ (unique-successors game history)))
	      (loop (cdr games)
		    (append succ history)
		    n
		    (append succ succ-n)))))))

(define (unique-successors game history)
  (let loop ((succ (successors game)) (seen history) (unique '()))
    (if (null? succ)
	unique
	(let ((game (car succ)))
	  (if (member-if game-equal? game seen)
	      (loop (cdr succ) seen unique)
	      (loop (cdr succ) (cons game seen) (cons game unique)))))))

(define (successors game)
  (map (lambda (move) (successor game (car move) (cadr move)))
       (legal-moves game)))

(define (successor game from to)
  (let ((new (copy-game game)))
    (move! new from to)
    new))

(define (move! game from to)
  (let ((card (card-at game from)))
    (remove-card! game from)
    (add-card! game to card))
  game)

(define (card-at game where)
  (let ((loc (car where))
	(i (cadr where)))
    (case loc
      ((stack) (let ((stack (vector-ref (game-stacks game) i)))
		 (if (null? stack) #f (car stack))))
      ((cell) (vector-ref (game-cells game) i))
      ((pile) (vector-ref (game-piles game) i)))))

(define (remove-card! game where)
  (let ((loc (car where))
	(i (cadr where)))
    (case loc
      ((stack) (let ((stacks (game-stacks game)))
		 (vector-set! stacks i (cdr (vector-ref stacks i)))))
      ((cell) (vector-set! (game-cells game) i #f)))))

(define (add-card! game where card)
  (let ((loc (car where))
	(i (cadr where)))
    (case loc
      ((stack) (let ((stacks (game-stacks game)))
		 (vector-set! stacks i (cons card (vector-ref stacks i)))))
      ((cell) (vector-set! (game-cells game) i card))
      ((pile) (vector-set! (game-piles game) i card)))))

(define (game-equal? game1 game2)
  (and (equal? (game-piles game1) (game-piles game2))
       (permutation? (game-cells game1) (game-cells game2))
       (permutation? (game-stacks game1) (game-stacks game2))))

(define (permutation? vec1 vec2)
  (let ((l1 (vector->list vec1)))
    (andmap (lambda (x) (member x l1))
	    (vector->list vec2))))

(define (legal-moves game)
  (remove-if (lambda (move) (not (legal-move? game (car move) (cadr move))))
	     (valid-moves game)))

(define (legal-move? game from to)
  (let ((card (card-at game from)))
    (and card
	 (not (eq? (car from) 'pile))
	 (let ((loc (car to))
	       (i (cadr to)))
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

(define (valid-moves game)
  (define (make-wheres loc vec)
    (let loop ((i (sub1 (vector-length vec))))
      (if (< i 0)
	  '()
	  (cons (list loc i) (loop (sub1 i))))))
  (let* ((valid-froms (append (make-wheres 'cell (game-cells game))
			      (make-wheres 'stack (game-stacks game))))
	 (valid-tos (append valid-froms (make-wheres 'pile (game-piles game)))))
    (cross-product valid-froms valid-tos)))

(define (cross-product list1 list2)
  (let loop ((l1 list1) (l2 list2) (cp '()))
    (cond ((null? l1) cp)
	  ((null? l2) (loop (cdr l1) list2 cp))
	  (else (let ((pair (list (car l1) (car l2))))
		  (loop l1 (cdr l2) (cons pair cp)))))))

(define (remove-if remove? l)
  (let loop ((l l))
    (cond ((null? l) '())
	  ((remove? (car l)) (loop (cdr l)))
	  (else (cons (car l) (loop (cdr l)))))))

(define (remove-duplicates-if same? l)
  (let loop ((l l))
    (cond ((null? l) '())
	  ((member-if same? (car l) (cdr l))
	   (loop (cdr l)))
	  (else (cons (car l) (loop (cdr l)))))))

(define (member-if same? x l)
  (ormap (lambda (y) (same? x y)) l))

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

