#lang racket

;; Copyright 2012 John Clements (clements@racket-lang.org)
;; released under a BSD license

(require rackunit)

;; an osc message contains an address and a sequence of OSC-values
;; an address is a list of byte-strings (for now)

(define (parse-osc-bytes bytes)
  ;; could be a contract:
  (when (or (not (bytes? bytes))
            (= (bytes-length bytes) 0)
            (not (= (modulo (bytes-length bytes) 4) 0)))
    (raise-type-error 'parse-osc-bytes 
                      "non-empty byte string of length divisible by 4"
                       0 bytes))
  (match (bytes-ref bytes 0)
    #;[35 ;; the # char
     (unless (and (<= 8 (bytes-length bytes))
                  (bytes=? (subbytes bytes 0 8)
                           #"#bundle\0"))
       (error 'parse-osc-bytes
              "illegal input at character 1 in input: ~e"
              bytes))
     (parse-bundle bytes 8)]
    [47 ;; the / char
     (define-values (addr-bytes new-offset)
       (parse-string-from bytes 0))
     (define address (parse-address-bytes addr-bytes))
     (list address new-offset)]
    [other 
     (raise-type-error 
      'parse-osc-bytes
      "byte string beginning with # or /"
      0 bytes)]))


(define (parse-string-from bytes offset)
  (match (regexp-match #px#"^([^\0]*)\0" bytes offset)
    [(list dc match) (values match (+ offset
                                      (bytes-length match)
                                      1))]
    [#f (error 'parse-string-from 
               "no nul char found while parsing for string at offset ~v in ~e"
               offset
               bytes)]))

(check-equal? (call-with-values 
               (lambda () (parse-string-from #"abcdef\0ghi" 2))
               list)
              (list #"cdef" 7))


;; given a byte string, separate it into its components
(define (parse-address-bytes bytes)
  (match (regexp-split #px#"/" bytes)
    [(list-rest #"" elts) elts]))

(check-equal? (parse-address-bytes #"/abc/def/ghi")
              (list #"abc" #"def" #"ghi"))

(check-equal? (parse-osc-bytes #"/quit\0\0\0")
              (list (list #"quit") (list)))

(bytes-append
 #"/status.reply\0\0\0,iiiiiffdd\0\0\0\0\0\1\0\0\0\0\0\0\0\0\0\0"
 #"\0\1\0\0\0\0=.U\314=\356l\30@\345\210\200\0\0\0\0@\345\210\203"
 #"\34\261G\20")