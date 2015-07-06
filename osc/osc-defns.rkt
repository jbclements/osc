#lang racket


(provide (contract-out
          (struct osc-message ([address (or/c bytes? (listof bytes?))]
                               [args (listof osc-value?)]))
          (struct osc-bundle ([timestamp osc-date?]
                              [elements (listof osc-element?)])))
         osc-element?
         osc-value?
         osc-date?
         osc-double?
         no-nul-bytes?
         osc-symbol?
         blob?
         osc-char?
         osc-color?
         osc-midi?
         osc-inf?
         osc-array?
         int32?
         int64?
         float32?)

;; an OSC-element is either an osc-message or an osc-bundle
(struct osc-message (address args) #:prefab)
(struct osc-bundle (timestamp elements) #:prefab)

(define osc-element? (or/c osc-message? osc-bundle?))

;; there are a bunch of different OSC values;
;; see http://opensoundcontrol.org/spec-1_0 for details.

;; the real question here is how to represent these osc
;; values in Racket. For instance, a string and a blob
;; are both best represented as a byte string, AFAICT.
;; I've opted to give the nod to strings by using a 
;; plain byte-string as the representation of a string,
;; and requiring blobs to be wrapped as a list of length 
;; 2 beginning with the symbol "blob". 

;; On the bright side, there's nothing hard about changing
;; these representations...

(define (osc-value? v)
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
      (osc-array? v) ; (list 'arr (listof osc-value?))
      ))

(define (uint32? n)
  (and (exact-integer? n) 
       (<= 0 n #xffffffff)))

(define osc-date? (or/c (list/c uint32? uint32?)
                        'now))


(define (int32? i)
  (and (exact-integer? i)
       (<= #x-80000000 i #x7fffffff)))

(define (int64? i)
  (match i
    [(list 'h (? exact-integer? n))
     (<= #x-8000000000000000 n #x7fffffffffffffff)]
    [other #f]))

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

(define (no-nul-bytes? v)
  (and (bytes? v)
       (not (regexp-match #px#"\0" v))))

(define (osc-symbol? v)
  (match v
    [(list 'S (? no-nul-bytes? v)) #t]
    [else #f]))


(define (osc-char? v)
  (match v
    [(list 'c (? byte? ch)) #t]
    [other #f]))

(define (osc-color? v)
  (match v
    [(list 'r (? 4bytes? c)) #t]
    [else #f]))

(define (osc-midi? v)
  (match v
    [(list 'm (? 4bytes? msg)) #t]
    [else #f]))

(define (osc-inf? v)
  (eq? v 'infinitum))

(define (osc-array? v)
  (match v
    [(list 'arr (? (listof osc-value?) v)) #t]
    [else #f]))

(define (4bytes? v)
  (and (bytes? v) (= (bytes-length v) 4)))


(module+ test
  (require rackunit)
  (check-equal? (no-nul-bytes? #"abc") #t)
  (check-equal? (no-nul-bytes? #"abc\0def") #f)

  (check-equal? (blob? '(blob #"272oue3")) #t)
  (check-equal? (blob? '(blob 14)) #f)
  
  (check-equal? (int32? 342) #t)
  (check-equal? (int32? 32324212142) #f)
  (check-equal? (int32? -24) #t)

  (check-equal? (int32? (expt 2 39)) #f))


