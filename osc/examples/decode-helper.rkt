#lang racket

(provide (all-defined-out))

;; functions to help in decoding supercollider.
;; the only reason these are in a separate file
;; is so that the main decode function can be
;; at the top of its file.


;; parser combinators. 

;; we're *awfully* close to just implementing the store monad here.

;; join-parsers : given some piece-parsers, produce a piece-parser
;; that returns a list of their results
(define ((join-parsers . parsers) bytes offset)
  (cond [(empty? parsers) (values empty offset)]
        [else
         (define-values (parsed new-offset) 
           ((first parsers) bytes offset))
         (define-values (rest-parsed final-offset)
           ((apply join-parsers (rest parsers)) bytes new-offset))
         (values (cons parsed rest-parsed) final-offset)]))

;; given a piece-parser, produce a piece-parser that reads a uint16 count
;; and then that many things from the input
(define ((parse-many parser) bytes offset)
  (define len (integer-bytes->integer bytes #f #t offset
                                      (+ offset 2)))
  ((parse-n-things len parser) bytes (+ offset 2)))

;; given a number and a piece-parser, parse that many from the input.
(define ((parse-n-things n parser) bytes offset)
  (cond [(= n 0) (values empty offset)]
        [else 
         (define-values (parsed new-offset)
           (parser bytes offset))
         (define-values (rest-parsed final-offset)
           ((parse-n-things (sub1 n) parser) bytes new-offset))
         (values (cons parsed rest-parsed) final-offset)]))

;; More bottom-up ordering, mandated by combinator-style usage:


;; parse a float32
(define (parse-float32 bytes offset)
  (values (floating-point-bytes->real bytes #t offset (+ offset 4))
          (+ offset 4)))

;; parse a uint16
(define (parse-uint16 bytes offset)
  (values (integer-bytes->integer bytes #f #t offset (+ offset 2))
          (+ offset 2)))

;; parse a uint8
(define (parse-uint8 bytes offset)
  (values (bytes-ref bytes offset)
          (+ offset 1)))

;; parse a pstring
(define (parse-pstring bytes offset)
  (define strlen (bytes-ref bytes offset))
  (values (subbytes bytes (+ offset 1) (+ offset 1 strlen))
          (+ offset 1 strlen)))

(module+ test
  (require rackunit)
  
  (check-equal? (call-with-values
                 (lambda ()
                   (parse-pstring #"5\24system_link_audio_1234" 1))
                 list)
                (list #"system_link_audio_12" 22)))