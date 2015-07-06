#lang racket

;; Copyright 2012 John Clements (clements@racket-lang.org)
;; Released under a BSD license

(require "osc-defns.rkt"
         "osc-time.rkt"
         "osc-common.rkt")

(provide (contract-out [osc-element->bytes
                        (-> osc-element? bytes?)]))


;; this file contains code to serialize osc elements (messages
;; and bundles). See the file "osc-defns.rkt" for a definition
;; of what an osc-message can be.

;; a pre-message is a list of lists of byte-strings. Rendering a
;; pre-message into a bytes adds padding out to length 4 after
;; copying each set of byte-strings


;; given an element, return a byte string representing that element
(define (osc-element->bytes element)
  (render-pre-message (osc-element->pre-message element)))

;; given an element, return a pre-message representing that element
(define (osc-element->pre-message element)
  (match element
    [(osc-message address args)
     (osc-message->pre-message address args)]
    [(osc-bundle timestamp elements)
     (osc-bundle->pre-message timestamp elements)]))


;; given a list of address/argument sequences, format a byte-string
;; representing an OSC "bundle"
(define (osc-bundle->bytes timestamp messages)
  (render-pre-message (osc-bundle->pre-message timestamp messages)))

;; given a timestamp and a list of elements, produce a pre-message. 
(define (osc-bundle->pre-message timestamp elements)
  (define timestamp-bytes (osc-date->bytes timestamp))
  (define pre-messages (map osc-element->pre-message elements))
  (define pre-messages-with-lengths
    (map decorate-pre-message pre-messages))
  (define header-pre-message (list (list #"#bundle\0" timestamp-bytes)))
  (define whole-pre-message
    (apply append (cons header-pre-message pre-messages-with-lengths)))
  whole-pre-message)


;; given an address and a list of arguments, format a byte string
;; in the OSC format
(define (osc-message->bytes address arguments)
  (render-pre-message 
   (osc-message->pre-message address arguments)))

;; a pre-message is a list of lists of byte strings.
;; in order to cut down on premature byte-string-appending,
;; we'll use a list of byte strings to represent byte strings that will
;; be appended together in the output along with padding to 
;; bring them to a length that is is a multiple of 4

;; given a pre-message, add a "length" byte-string to the front,
;; so it can be used as part of a bundle
(define (decorate-pre-message pre-message)
  (define len (pre-message-length pre-message))
  (cons (list (integer->integer-bytes len 4 #f #t))
        pre-message))

;; given an address and a list of arguments, produce a pre-message
(define (osc-message->pre-message address arguments)
  (define address-byteses (list (address->bytes address) #"\0"))  
  (define arg-types-and-bytes (map arg->type-and-pre-message arguments))
  (define arg-type-bytes (bytes-append
                          #","
                          (apply bytes-append
                                 (map first arg-types-and-bytes))
                          #"\0"))
  (define arg-byteseses (apply 
                         append
                         (map second arg-types-and-bytes)))
  (cons address-byteses
        (cons (list arg-type-bytes)
              arg-byteseses)))

;; given a pre-message, compute the number of bytes it will take.
(define (pre-message-length pre-message)
  (for/sum ([byteses pre-message])
    (round-up-to-4 (apply + (map bytes-length byteses)))))

;; render the pre-message into a buffer
(define (render-pre-message pre-message)
  (define tgt-bytes (make-bytes (pre-message-length pre-message) 0))
  (let loop ([offset 0] [elements pre-message])
    (cond 
      [(empty? elements) #f]
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
                            (rest byteses))]))]))
  tgt-bytes)



;; given an osc-address, produce the bytes version of it:
(define (address->bytes address)
  (match address
    [(? bytes? address) address]
    [(list (? bytes? b) ...)
     (apply
      bytes-append
      (for/list ([a address]) (bytes-append #"/" a)))]
    [other
     (raise-argument-error 'address->bytes
                           "byte-string or list of byte-strings"
                           0 address)]))

;; returns a list containing a byte-string of length 
;; 1 and a pre-message
(define (arg->type-and-pre-message arg)
  ;??? spec doesn't seem to choose signed vs. unsigned....
  ;; spec requires big-endian
  (cond [(osc-array? arg)
         (define sub-elements (map arg->type-and-pre-message (second arg)))
         (define type-bytes (apply bytes-append (map first sub-elements)))
         (define pre-message (apply append (map second sub-elements)))
         (list (bytes-append #"[" type-bytes #"]")
               pre-message)]
        [else 
         (match-define (list ty byteses) (arg->type-and-bytes arg))
         (list ty (list byteses))]))

(define (arg->type-and-bytes arg)
  
  (cond [(int32? arg)
         (list #"i" (list (integer->integer-bytes arg 4 #t #t)))]
        [(osc-color? arg)
         (list #"r" (list (second arg)))]
        [(osc-midi? arg)
         (list #"m" (list (second arg)))]
        [(int64? arg)
         (list #"h" (list (integer->integer-bytes (second arg) 8 #t #t)))]
        [(bytes? arg) (list #"s"
                            (list
                             arg
                             #"\0"))]
        [(osc-symbol? arg)
         (list #"S" (list (second arg) #"\0"))]
        [(float32? arg) (list #"f"
                              (list
                               (real->floating-point-bytes 
                                arg 4 #t)))]
        [(osc-date? arg)
         (list #"t" (list (osc-date->bytes arg)))]
        [(osc-double? arg)
         (list #"d" (list (real->floating-point-bytes 
                           (second arg) 8 #t)))]
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
        [(osc-char? arg)
         (list #"c" (list (bytes 0 0 0 (second arg))))]
        [(boolean? arg)
         (cond [arg (list #"T" (list))]
               [else (list #"F" (list))])]
        [(null? arg)
         (list #"N" (list))]
        [(osc-inf? arg)
         (list #"I" (list))]
        [else (error 'arg->type-and-bytes
                     "expected OSC argument, got ~e" 
                     arg)]))




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



(module+ test
  (require rackunit)

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
  
  ;; regression...
  (check-equal? (arg->type-and-bytes (list 'd 2789.2278))
                (list #"d" (list #"@\245\312t\2423\234\17")))
  (check-equal? (arg->type-and-bytes (list 'h 65535))
                (list #"h" (list (bytes 0 0 0 0 0 0 255 255))))
  (check-equal? (arg->type-and-bytes (list (* 256 14) #x02000000))
                (list #"t" (list (bytes 0 0 14 0 2 0 0 0))))
  (check-equal? (arg->type-and-bytes (list 'S #"blozz"))
                (list #"S" (list #"blozz" #"\0")))
  (check-equal? (arg->type-and-bytes (list 'c 15))
                (list #"c" (list (bytes 0 0 0 15))))
  (check-equal? (arg->type-and-bytes (list 'r (bytes 2 78 147 200)))
                (list #"r" (list (bytes 2 78 147 200))))
  (check-equal? (arg->type-and-bytes (list 'm (bytes 2 79 147 200)))
                (list #"m" (list (bytes 2 79 147 200))))
  (check-equal? (arg->type-and-bytes #t)
                (list #"T" (list)))
  (check-equal? (arg->type-and-bytes #f)
                (list #"F" (list)))
  (check-equal? (arg->type-and-bytes null)
                (list #"N" (list)))
  (check-equal? (arg->type-and-bytes 'infinitum)
                (list #"I" (list)))


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
  (check-exn #px"expected: byte-string or list of byte-strings"
             (lambda () (address->bytes "/abc/def")))

(check-equal? (osc-message->bytes #"/abc/def"
                                  (list
                                   3 6 2.278 
                                   #"froggy"
                                   `(blob #"derple")))
              (bytes-append 
               #"/abc/def\000\000\000\000,iifsb\0\0"
               (bytes 0 0 0 3)
               (bytes 0 0 0 6)
               #"@\21\312\301"
               #"froggy\0\0"
               (bytes 0 0 0 6)
               #"derple"
               (bytes 0 0)))
  
  (check-equal? (osc-message->bytes 
                 #"/do/co/mo"
                 (list #"str" (list 'arr (list 34 #"foo"))
                       #"brr"))
                (bytes-append
                 #"/do/co/mo\0\0\0"
                 #",s[is]s\0"
                 #"str\0"
                 (bytes 0 0 0 34)
                 #"foo\0"
                 #"brr\0"))

;; regression test:
(check-equal?
 (osc-message->bytes #"/abc" '((d 347987.2792870)))
 #"/abc\0\0\0\0,d\0\0A\25=M\35\375iM")

(check-equal? (osc-bundle->bytes 
               'now
               (list (osc-message #"/abc/def" (list 2.278 14))
                     (osc-message #"/ghi" (list #"hello" #"my friend"))))
              (bytes-append
               #"#bundle\0"
               (bytes 0 0 0 0 0 0 0 1)
               
               (bytes 0 0 0 24)
               #"/abc/def\0\0\0\0"
               #",fi\0"
               #"@\21\312\301"
               (bytes 0 0 0 14)
               
               (bytes 0 0 0 32)
               #"/ghi\0\0\0\0"
               #",ss\0"
               #"hello\0\0\0"
               #"my friend\0\0\0"
               ))

(define test-message-1 (osc-message #"/a/b" (list 257)))
(check-equal? (osc-element->bytes test-message-1)
              #"/a/b\0\0\0\0,i\0\0\0\0\001\001")
(define test-message-1-bytes (osc-element->bytes test-message-1))
(define test-message-2 (osc-message #"/z" (list #"woohoo" '(blob #"z"))))
(define test-message-2-bytes (osc-element->bytes test-message-2))

(check-equal? (osc-element->bytes (osc-bundle 'now (list test-message-1
                                                         test-message-2)))
              (bytes-append
               #"#bundle\0"
               (bytes 0 0 0 0 0 0 0 1)
               (bytes 0 0 0 16)
               test-message-1-bytes
               (bytes 0 0 0 (bytes-length test-message-2-bytes))
               test-message-2-bytes))

;; test of bundles within bundles:

(check-equal? (osc-element->bytes 
               (osc-bundle 'now (list test-message-1
                                      (osc-bundle 
                                       'now
                                       (list test-message-2
                                             test-message-1)))))
              (bytes-append
               #"#bundle\0"
               (bytes 0 0 0 0 0 0 0 1)
               (bytes 0 0 0 16)
               test-message-1-bytes
               (bytes 0 0 0 (+ 8 8 8 16
                               (bytes-length test-message-2-bytes)))
               #"#bundle\0"
               (bytes 0 0 0 0 0 0 0 1)
               (bytes 0 0 0 (bytes-length test-message-2-bytes))
               test-message-2-bytes
               (bytes 0 0 0 16)
               test-message-1-bytes))

  
  (check-exn
   #px"expected: osc-value"
   (lambda () (osc-message #"/bogus" (list 34 (expt 2 39))))))