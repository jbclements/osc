#lang racket

;; Copyright 2012 John Clements (clements@racket-lang.org)
;; released under a BSD license


(require racket/runtime-path
         "../osc-defns.rkt")

(define-runtime-path here ".")

(define setup-messages (file->value (build-path here "preloads.rktd")))
(define action-messages (file->value (build-path here "actions.rktd")))


(define (decode-d-recv bytes)
  (unless (equal? (subbytes bytes 0 4) #"SCgf")
    (error 'decode-d-recv "expected magic number #\"SCgf\""))
  (printf "file version: ~s\n"
          (integer-bytes->integer bytes #f #t 4 8))
  (define num-defs 
    (integer-bytes->integer bytes #f #t 8 10))
  (let loop ([num-defs num-defs][offset 10])
    (cond [(= num-defs 0)
           empty]
          [else 
           (define-values (synth-def new-offset)
             (parse-synth-def bytes offset))
           (cons synth-def (loop (sub1 num-defs)
                                 (new-offset)))])))

(define (parse-synth-def bytes offset)
  (define-values (string offset2)
    (parse-pstring bytes offset))
  ;; ... no, I should go to bed.
  )


(osc-message-address (third setup-messages))
(define bstr
  (second (first (osc-message-args (third setup-messages)))))

(decode-d-recv bstr)

bstr