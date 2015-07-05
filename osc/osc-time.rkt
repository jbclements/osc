#lang racket

;; Copyright 2012 John Clements (clements@racket-lang.org)
;; this code is released under the Mozilla Public License 2.0

(require racket/date
         "osc-defns.rkt")

(provide (contract-out
          [seconds->osc-date
           (-> exact-integer? second-frac? osc-date?)]
          [osc-date->seconds-and-frac
           (-> osc-date? (list/c exact-integer? second-frac?))]
          
          [date->osc-date (-> date? inexact-real? osc-date?)]
          [osc-date->date-and-frac
           (-> osc-date? (list/c date? 
                                 inexact-real?))]
          
          [osc-date->bytes (-> osc-date? 8bytes?)]
          
          [bytes->osc-date (-> 8bytes? osc-date?)]))

(define (second-frac? x)
  (and (inexact-real? x)
       (<= 0 x)
       (< x 1.0)))

;; OSC dates consist of a fixed-point number with 
;; 32 bits for the second, and 32 bits for the sub-second,
;; OR the special date "now".

;; I'm representing an OSC-date as a list of two
;; integers in the range 0 - #xffffffff,
;; or the symbol 'now for the special "now" value.

;; seconds are measured since January 1, 1900, GMT, meaning
;; that they'll run out in 2036 or so. Oh well.
(define OSC-EPOCH-SECONDS (find-seconds 0 0 0 1 1 1900 #f))

;; convert a number of seconds (as e.g. from (current-seconds))
;; and a fractional number of seconds into an osc-date
(define (seconds->osc-date s frac)
  (list (- s OSC-EPOCH-SECONDS)
        (frac->fixed-frac frac)))

;; convert an osc-date to a number of seconds (as e.g. from
;; (current-seconds) and a fractional number of seconds
(define (osc-date->seconds-and-frac osc-date)
  (match osc-date
    ['now (raise-type-error 'osc-date->date-and-frac
                            "non-'now' osc value"
                            0 osc-date)]
    [(list seconds frac)
     (list (+ seconds OSC-EPOCH-SECONDS)
           (fixed-frac->frac frac))]))

;; date-based versions of the prior conversions:
(define (date->osc-date d frac)
  (seconds->osc-date (date->seconds d) frac))

(define (osc-date->date-and-frac osc-date)
  (match (osc-date->seconds-and-frac osc-date)
    [(list seconds frac)
     (list (seconds->date seconds) frac)]))

(define (8bytes? b)
  (and (bytes? b) (= (bytes-length b) 8)))

;; represent an inexact number in [0..1) as a fixed-point
;; 32-bit integer
(define (frac->fixed-frac frac)
  (inexact->exact (floor (* frac #x100000000))))

;; convert an unsigned 32-bit int representing the fractional
;; part of a second as an inexact number in [0..1)
;; NB mantissa of double is > 32 bits, so this conversion
;; shouldn't lose data.
(define (fixed-frac->frac fixed-frac)
  (/ (exact->inexact fixed-frac) #x100000000))


(define (osc-date->bytes osc-date)
  (match osc-date
    ['now (bytes 0 0 0 0 0 0 0 1)]
    [(list seconds frac)
     (bytes-append (integer->integer-bytes seconds 4 #f #t)
                   (integer->integer-bytes frac 4 #f #t))]))

(define (bytes->osc-date bytes)
  (match bytes
    [#"\0\0\0\0\0\0\0\1" 'now]
    [else (list
           (integer-bytes->integer (subbytes bytes 0 4) #f #t)
           (integer-bytes->integer (subbytes bytes 4 8) #f #t))]))

;; TESTS
(module+ test
  
  (require rackunit)
  (check-equal? (osc-date->bytes 'now)
                (bytes 0 0 0 0 0 0 0 1))
  (check-equal? (osc-date->bytes (list 9 #x03100000))
                (bytes 0 0 0 9 3 16 0 0))
  (check-equal? (bytes->osc-date (bytes 0 0 0 0 0 0 0 1)) 'now)
  (check-equal? (bytes->osc-date (bytes 0 0 0 9 3 16 0 0))
                (list 9 #x03100000))
  
  ;; round-trip test
  (check-equal? (osc-date->date-and-frac
                 (date->osc-date (seconds->date
                                  (find-seconds 2 4 13 27 3 1997)) 
                                 (+ 0.5 (expt 0.5 8))))
                (list
                 (seconds->date
                  (find-seconds 2 4 13 27 3 1997)) 
                 (+ 0.5 (expt 0.5 8))))
  ;; deeper round-trip test
  (check-equal? (osc-date->date-and-frac
                 (bytes->osc-date
                  (osc-date->bytes
                   (date->osc-date (seconds->date
                                    (find-seconds 2 4 13 27 3 1997)) 
                                   (+ 0.5 (expt 0.5 8))))))
                (list
                 (seconds->date
                  (find-seconds 2 4 13 27 3 1997)) 
                 (+ 0.5 (expt 0.5 8))))

  ;; equality of seconds-based and date-based methods:
  (check-equal? (date->osc-date (seconds->date
                                 (find-seconds 2 4 13 27 3 1997)) 
                                (+ 0.5 (expt 0.5 8)))
                (seconds->osc-date (find-seconds 2 4 13 27 3 1997) 
                                   (+ 0.5 (expt 0.5 8))))

  )
