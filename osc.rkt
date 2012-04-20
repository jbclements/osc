#lang racket

;; Copyright 2012 John Clements (clements@racket-lang.org)
;; Released under a BSD license

(require rackunit)

(provide (contract-out [make-osc-message
                        (->* (osc-address?) 
                             #:rest (listof osc-value?)
                             bytes?)]))

;; an OSC value is either 
;; - an integer representable as a signed 32-bit integer
;; - an inexact real,
;; - a byte-string, or
;; - (list 'blob byte-string)
;; ... the others are optional, and (for now) ignored

;; see http://opensoundcontrol.org/spec-1_0 for details

(define (osc-value? v)
  (or (int32? v)
      (float32? v)
      (bytes? v)
      (blob? v)))

;; given a list of address/argument sequences, format a byte-string
;; representing an OSC "bundle"
#;(define (make-osc-bundle messages)
  (define pre-messages
    (for/list ([message messages]) (apply make-osc-pre-message message)))
  (define out-buf
    (make-bytes (apply + (map pre-message-length pre-messages))))
  (let loop ([offset 0] [pre-messages pre-messages])
    (cond [(empty? pre-messages) #f]
          [else 
           (define new-offset
             (render-pre-message! out-buf offset (first pre-messages)))
           (loop new-offset (rest pre-messages))])))


;; given an address and a list of arguments, format a byte string
;; in the OSC format
(define (make-osc-message address . arguments)
  (define address-byteses (list (address->bytes address) #"\0"))
  (define arg-types-and-bytes (map arg->type-and-bytes arguments))
  (define arg-type-bytes (bytes-append
                          #","
                          (apply bytes-append
                                 (map first arg-types-and-bytes))
                          #"\0"))
  (define arg-byteseses (map second arg-types-and-bytes))
  (define elements (cons address-byteses
                         (cons (list arg-type-bytes)
                               arg-byteseses)))
  (define msg-len (for/sum ((elt elements))
                    (round-up-to-4
                     (apply + (map bytes-length elt)))))
  (define out-buf (make-bytes msg-len 0))
  (render-pre-message! out-buf 0 elements)
  out-buf)

;; a pre-message is a list of lists of byte-strings. Rendering a
;; pre-message into a bytes adds padding out to length 4 after
;; copying each set of byte-strings

(define (pre-message-length pre-message)
  (for/sum ([byteses pre-message])
    (round-up-to-4 (apply + (map bytes-length byteses)))))

;; render the pre-message into the buffer at the specified
;; offset. return the new offset.
(define (render-pre-message! tgt-bytes offset pre-message)
  (let loop ([offset offset] [elements pre-message])
    (cond 
      [(empty? elements) offset]
      [else
       (let inner-loop ([offset offset] [byteses (first elements)])
         (cond [(empty? byteses)
                ;; back to outer loop:
                (loop (+ offset 
                         (match (modulo offset 4) 
                           [0 0]
                           [other (- 4 other)]))
                      (rest elements))]
               ;; continue with inner loop:               
               [else
                (bytes-copy! tgt-bytes offset (first byteses))
                (inner-loop (+ offset (bytes-length (first byteses)))
                            (rest byteses))]))])))



;; given an osc-address, produce the bytes version of it:
(define (address->bytes address)
  (cond [(bytes? address) address]
        [else (apply
               bytes-append
               (for/list ([a address]) (bytes-append #"/" a)))]))

;; returns a list containing a byte-string of length 
;; 1 and a list of byte-strings (not pre-appended to
;; save copying time on the blobs)
(define (arg->type-and-bytes arg)
  ;??? spec doesn't seem to choose signed vs. unsigned....
  ;; spec requires big-endian
  (cond [(int32? arg) (list #"i"
                            (list
                             (integer->integer-bytes arg 4 #t #t)))]
        [(bytes? arg) (list #"s"
                            (list
                             arg
                             #"\0"))]
        [(float32? arg) (list #"f"
                              (list
                               (real->floating-point-bytes arg 4 #t)))]
        [(blob? arg) 
         (let ()
           (define the-bytes (second arg))
           (list #"b"
                 (list
                  (integer->integer-bytes 
                   (bytes-length the-bytes)
                   4 #t #t)
                  (second arg)
                  (make-bytes (round-up-to-4-diff 
                               (bytes-length the-bytes))))))]
        [else (error 'arg->type-char
                     "expected OSC argument, got ~e" 
                     arg)]))





(define (int32? i)
  (and (exact-integer? i)
       (< #x-80000000 i #x7fffffff)))

(define (float32? f)
  (and (number? f)
       (inexact-real? f)))

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


(define (round-up-to-4-diff i)
  (match (modulo i 4)
    [0 0]
    [other (- 4 other)]))

(define (round-up-to-4 i)
  (+ i (round-up-to-4-diff i)))




(check-equal? (pre-message-length '((#"" #"a")
                                    (#"a" #"bc" #"d")))
              8)


(check-equal? (arg->type-and-bytes 3)
              (list #"i" (list (bytes 0 0 0 3))))
(check-equal? (arg->type-and-bytes 2.278)
              (list #"f" (list #"@\21\312\301")))
(check-equal? (arg->type-and-bytes (list 'blob (bytes 79 197 0 14 203)))
              (list #"b"
                    (list (bytes 0 0 0 5)
                          (bytes 79 197 0 14 203)
                          (bytes 0 0 0))))


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

(check-equal? (make-osc-message #"/abc/def" 3 6 2.278 
                                #"froggy"
                                `(blob #"derple"))
              (bytes-append 
               #"/abc/def\000\000\000\000,iifsb\0\0"
               (bytes 0 0 0 3)
               (bytes 0 0 0 6)
               #"@\21\312\301"
               #"froggy\0\0"
               (bytes 0 0 0 6)
               #"derple"
               (bytes 0 0)))

#;(check-equal? (make-osc-bundle `(("/abc/def" 2.278 14)
                                 ("/ghi" #"hello" #"my friend")))
              (bytes-append
               #"#bundle\0"
               ))




