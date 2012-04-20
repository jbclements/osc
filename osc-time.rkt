#lang racket

;; Copyright 2012 John Clements (clements@racket-lang.org)
;; this code is released under a BSD license

(require racket/date)

(provide (contract-out [date->osc-date
                        (-> date? inexact-real? osc-date?)]))

;; OSC dates consist of a fixed-point number with 
;; 32 bits for the second, and 32 bits for the sub-second.

;; seconds are measured since January 1, 1900, meaning
;; that they'll run out in 2036 or so. Oh well.

;; there's actually a reasonable bijection between
;; the seconds representation and racket's notion of a 
;; date, for the dates in the range 1900-2036, anyway.

;; I'm representing an OSC-date as a list of two
;; integers in the range 0 - #xffffffff

(define (uint32? n)
  (and (exact-integer? n) 
       (<= 0 n #xffffffff)))

(define osc-date? (list/c uint32? uint32?))

;; got to be careful here.... we'll see what servers actually
;; use.
(define leap-years-added 
  (- (floor (/ (- 1970 1900) 4)) 1))

(define seconds-per-day (* 60 60 24))

(define seconds-offset
  (* seconds-per-day
     (+ (* 365 (- 1970 1900)) leap-years-added)))

(define (date->osc-date d frac #:utc? [utc? #f])
  (list (+ (date->seconds d (not utc?)))
        (inexact->exact (floor (* frac #x100000000)))))
