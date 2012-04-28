#lang racket

(require "osc-defns.rkt")

(provide (contract-out [random-osc-value (-> natural? osc-value?)]))

(define natural? exact-nonnegative-integer?)

(define (random-osc-value depth)
  (cond 
    [(= depth 0)
     ((list-ref flat-value-generators (random (length flat-value-generators))))]))


(define (random-sint64)
  (list 'h
  (- (+ (* (random (expt 2 20)) (expt 2 44))
        (* (random (expt 2 20)) (expt 2 4))
        (* (random (expt 2 4))  1))
     (expt 2 32))))

(define (random-sint32)
  (- (+ (* (random (expt 2 20)) (expt 2 12))
        (* (random (expt 2 12)) 1))
     (expt 2 16)))

(define (random-char) (list 'c (random 256)))

(define (random-float32)
  (random))

(define flat-value-generators
  (list
   random-sint32
   random-sint64
   random-char))


(random-osc-value 0)

#;(match (random 14)
       
       (or (int32? v) ; just the number
           (int64? v) ; (list 'h number)
           (osc-date? v) ; either 'now or a list of two uint32s
           (float32? v) ; just the [inexact] number
           (osc-double? v) ; (list 'd <inexact>)
           (no-nul-bytes? v) ; a byte-string
           (osc-symbol? v) ; (list 'S <byte-string>)
           (blob? v) ; (list 'blob <byte-string>)
           (osc-char? v) ; (list 'c byte)
           (osc-color? v) ; (list 'r <4bytes>)
           (osc-midi? v) ; (list 'm <4bytes>)
           (boolean? v) ; boolean?
           (null? v) 
           (osc-inf? v) ; 'infinitum
           ))

