#lang racket

;; Copyright 2012 John Clements (clements@racket-lang.org)
;; released under a BSD license


(require racket/runtime-path
         "../osc-defns.rkt"
         "decode-helper.rkt"
         rackunit)

(define-runtime-path here ".")

(define setup-messages (file->value (build-path here "preloads.rktd")))
(define action-messages (file->value (build-path here "actions.rktd")))

;; an 'a piece-parser is (bytes? offset? -> 'a offset?)


;;; DECODE:

(define (decode-d-recv bytes)
  (unless (equal? (subbytes bytes 0 4) #"SCgf")
    (error 'decode-d-recv "expected magic number #\"SCgf\""))
  (printf "file version: ~s\n"
          (integer-bytes->integer bytes #f #t 4 8))
  ((parse-many parse-synth-def) bytes 8))

;; a synth definition contains a name and a set of lists:
(define (parse-synth-def bytes offset)
  (define-values (list-parsed new-offset)
    (parse-synth-def/list bytes offset))
  (values (post-process-synth-def list-parsed) new-offset))


;; parse a parameter name
(define parse-parameter-name
  (join-parsers parse-pstring
                parse-uint16))

;; enforce invariants:
(define (post-process-synth-def def-as-list)
  (match-define (list name consts params param-names ugens) def-as-list)
  (for ([name+idx param-names])
    (unless (< (second name+idx) (length params))
      (error 'post-process-synth-def
             "name ~s has 0-based param index ~s, but this def has only ~s params"
             (first name+idx) (second name+idx) (length params))))
  ;; can also check sanity of input-specs... but I'm not sure why we actually
  ;; need to write all this stuff. After all, I'm not *implementing*
  ;; scsynth.
  def-as-list)

;; parse a unit generator spec
(define (parse-ugen-spec bytes offset)
  (define-values (header offset2)
    ((join-parsers
      parse-pstring
      parse-uint8
      parse-uint16
      parse-uint16
      parse-uint16)
     bytes offset))
  (define num-inputs (third header))
  (define num-outputs (fourth header))
  (define-values (ins-outs offset3)
    ((join-parsers
      (parse-n-things num-inputs parse-input-spec)
      (parse-n-things num-outputs parse-uint8))
     bytes offset2))
  (values (append header ins-outs) offset3))

;; parse a synth def, before postprocessing
(define parse-synth-def/list
  (join-parsers 
   parse-pstring
   (parse-many parse-float32)
   (parse-many parse-float32)
   (parse-many parse-parameter-name)
   (parse-many parse-ugen-spec)))

;; parse the input spec of a unit generator
(define parse-input-spec
  (join-parsers
   parse-uint16
   parse-uint16))





(osc-message-address (third setup-messages))
(define bstr
  (second (first (osc-message-args (third setup-messages)))))

(decode-d-recv bstr)

bstr