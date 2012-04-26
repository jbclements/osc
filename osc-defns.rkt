#lang racket

(require rackunit)

(provide (all-defined-out))

;; an OSC value is either 
;; - an integer representable as a signed 32-bit integer
;; - an inexact real, (encoded as a float32)
;; - (list 'd inexact-real?) (encoded as a double)
;; - a byte-string, or
;; - (list 'blob byte-string)
;; ... the others are optional, and (for now) ignored

;; see http://opensoundcontrol.org/spec-1_0 for details

(define (osc-value? v)
  (or (int32? v)
      (float32? v)
      (osc-double? v)
      (bytes? v)
      (blob? v)))

;; an OSC-element is either an osc-message or an osc-bundle
(struct osc-message (address args) #:prefab)
(struct osc-bundle (timestamp elements) #:prefab)
(define osc-element? (or/c osc-message? osc-bundle?))



(define (int32? i)
  (and (exact-integer? i)
       (< #x-80000000 i #x7fffffff)))

(define (float32? f)
  (and (number? f)
       (inexact-real? f)))

(define (osc-double? f)
  (match f
    [(list 'd (? inexact-real? f)) #t]
    [other #f]))

(define (blob? v)
  (match v
    [(list 'blob (? bytes? v))
     ;; unclear whether unsigned ints are 
     ;; used for the lengths of blobs:
     (< (bytes-length v) #x7fffffff)]
    [else #f]))






(check-equal? (blob? '(blob #"272oue3")) #t)
(check-equal? (blob? '(blob 14)) #f)

(check-equal? (int32? 342) #t)
(check-equal? (int32? 32324212142) #f)
(check-equal? (int32? -24) #t)


