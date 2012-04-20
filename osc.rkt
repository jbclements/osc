#lang racket

(require rackunit)

(provide (contract-out [make-osc-message
                        (->* (osc-address?) 
                             #:rest (listof osc-value?)
                             bytes?)]))

;; an OSC value is either 
;; - an integer representable as a signed 32-bit integer
;; - a time tag (?)
;; - a floating-point ... wait a minute

;; given an address and a list of arguments, format a byte string
;; in the OSC format
(define (make-osc-message address . arguments)
  (define address-bytes (bytes-append (address->bytes address) #"\0"))
  (define arg-types-and-bytes (map arg->type-and-bytes arguments))
  (define arg-type-bytes (bytes-append
                          #","
                          (apply bytes-append
                                 (map first arg-types-and-bytes))
                          #"\0"))
  (define arg-byteses (map second arg-types-and-bytes))
  (define elements (cons address-bytes (cons arg-type-bytes
                                             arg-byteses)))
  (define msg-len (for/sum ((elt elements))
                    (round-up-to-4 (bytes-length elt))))
  (define out-buf (make-bytes msg-len 0))
  (let loop ([offset 0] [elements elements])
    (unless (empty? elements)
      (define elt (first elements))
      (bytes-copy! out-buf offset elt)
      (loop (+ offset (round-up-to-4 (bytes-length elt)))
            (rest elements))))
  out-buf)


;; given an osc-address, produce the bytes version of it:
(define (address->bytes address)
  (cond [(bytes? address) address]
        [else (apply
               bytes-append
               (for/list ([a address]) (bytes-append #"/" a)))]))

(define (arg->type-and-bytes arg)
  ;??? spec doesn't seem to choose signed vs. unsigned....
  ;; spec requires big-endian
  (cond [(int32? arg) (list #"i"
                            (integer->integer-bytes arg 4 #t #t))]
        [(bytes? arg) (list #"s"
                            (bytes-append arg #"\0"))]
        [(float32? arg) (error 'unimplemented)]
        [(blob? arg) (error 'unimplemented)]
        [else (error 'arg->type-char
                     "expected OSC argument, got ~e" 
                     arg)]))

(define (osc-value? v)
  (or (bytes? v)
      (int32? v)
      (float32? v)
      (blob? v)))

(define (int32? i)
  (and (exact-integer? i)
       (< #x-80000000 i #x7fffffff)))

(define (float32? f)
  (inexact? f))

(define (blob? v)
  (match v
    [(list 'blob (? bytes? v)) #t]
    [else #f]))

(check-equal? (blob? '(blob #"272oue3")) #t)
(check-equal? (blob? '(blob 14)) #f)

(check-equal? (arg->type-and-bytes 3)
              (list #"i" (bytes 0 0 0 3)))

(check-equal? (int32? 342) #t)
(check-equal? (int32? 32324212142) #f)
(check-equal? (int32? -24) #t)


;; an osc address starts with a slash, and then has a sequence
;; of strings that can't contain any of a set of special characters
;; (see the definition of osc-address-bytes-element?)
(define (osc-address-bytes? bytes)
  (and 
   (bytes? bytes)
   (match (regexp-split #px#"/" bytes)
     [(list-rest #"" elements) (andmap osc-address-bytes-element? 
                                       elements)]
     [other #f])))

;; an osc address component can't contain any of a bunch of chars
(define (osc-address-bytes-element? bytes)
  (and (bytes? bytes)
       (not (regexp-match #px"[][ #*,/?{}\0]" bytes))))

;; an OSC-address is a list of legal-address-bytes?, or a byte string
;; (gets converted)
(define osc-address? (or/c (listof osc-address-bytes-element?)
                           osc-address-bytes?))



(define (round-up-to-4 i)
  (match (modulo i 4)
    [0 i]
    [other (+ i (- 4 other))]))

(check-equal? (round-up-to-4 18) 20)
(check-equal? (round-up-to-4 12) 12)

(check-equal? (osc-address-bytes-element? #"casper-the-friendly-ghost") #t)
(check-equal? (osc-address-bytes-element? #"casper-the friendly-ghost") #f)
(check-equal? (osc-address-bytes-element? #"th}") #f)
(check-equal? (osc-address-bytes-element? #"th[32") #f)
(check-equal? (osc-address-bytes-element? #"th]32") #f)
(check-equal? (osc-address-bytes-element? #"th?32") #f)
(check-equal? (osc-address-bytes-element? #"th\00032") #f)
(check-equal? (osc-address-bytes-element? #"th332") #t)
(check-equal? (osc-address-bytes? #"/abc") #t)
(check-equal? (osc-address-bytes? #"/obc/do/brock") #t)
;; this would appear to be legal, according to the spec...
(check-equal? (osc-address-bytes? #"/abc//def//") #t)
;; no leading slash:
(check-equal? (osc-address-bytes? #"abc") #f)
;; illegal char:
(check-equal? (osc-address-bytes? #"/abc/de]f") #f)

(check-equal? (address->bytes #"/abc/def") #"/abc/def")
(check-equal? (address->bytes (list #"abc" #"def")) #"/abc/def")



(check-equal? (make-osc-message #"/abc/def" 3 6)
              (bytes-append 
               #"/abc/def\000\000\000\000,ii\000"
               (bytes 0 0 0 3)
               (bytes 0 0 0 6)))

