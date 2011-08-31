#lang racket

(provide read-wordlist anagrams anagrams+1 anagrams+*)

;; Reads a wordlist from the current input port, one word per line.
;; Returns a hash whose keys are strings whose letters are sorted and
;; whose values are lists of anagrams.
(define (read-wordlist)
  (for/fold ((hash #hash())) ((word (in-lines)))
    (hash-update hash (sort-word word) (curry append (list word)) null)))

;; A list of anagrams of the given word that are in the given dict.
(define (anagrams word wordlist)
  (dict-ref wordlist (sort-word word) null))

;; A list of anagrams of the given word plus one letter.
(define (anagrams+1 word wordlist)
  (append-map (curryr anagrams wordlist)
              (map (curry string-append word) letters)))

;; A list of anagrams of the given word plus any number of letters.
(define (anagrams+* word wordlist)
  (let ((word (sort-word word)))
    (sort (for/fold ((anagrams null)) (((key words) (in-dict wordlist))
                                       #:when (subseq-word? word key))
            (append anagrams words))
          string<?)))

;; The list of one-letter strings.
(define letters (map string (string->list "abcdefghijklmnopqrstuvwxyz")))

;; A string containing all the letter in word, sorted alphabetically.
(define (sort-word word)
  (list->string (sort (string->list word) char<?)))

;; Do the letters of word1 appear in word2 in order?
(define (subseq-word? word1 word2)
  (let loop ((l1 (string->list word1))
             (l2 (string->list word2)))
    (cond ((null? l1)
           #t)
          ((null? l2)
           #f)
          ((char=? (car l1) (car l2))
           (loop (cdr l1) (cdr l2)))
          (else
           (loop l1 (cdr l2))))))
