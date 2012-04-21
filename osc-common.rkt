#lang racket

;; Copyright 2012 John Clements (clements@racket-lang.org)
;; released under a BSD license

(require rackunit)

(provide round-up-to-4
         round-up-to-4-diff)

;; return the difference between i and the next
;; greatest multiple of 4.
(define (round-up-to-4-diff i)
  (match (modulo i 4)
    [0 0]
    [other (- 4 other)]))

;; return the smallest multiple of 4 >= i.
(define (round-up-to-4 i)
  (+ i (round-up-to-4-diff i)))



(check-equal? (round-up-to-4 18) 20)
(check-equal? (round-up-to-4 12) 12)

