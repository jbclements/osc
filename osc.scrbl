#lang scribble/doc

@(require scribble/manual
          planet/scribble
          scribble/bnf)

@title{@bold{OSC}: Open Sound Control Byte String Conversion}

@author[(author+email "John Clements" "clements@racket-lang.org")]

@(require (for-label racket
                     (this-package-in main)))

@defmodule/this-package[main]{This collection provides the means to translate
 to and from byte strings representing OSC (Open Sound Control) bundles and 
 messages.
 
 @defproc[(osc-element->bytes [element osc-element?]) bytes?]{
 Given an osc element, produces the corresponding byte string.}
 
 @defproc[(bytes->osc-element (bytes bytes?)) osc-element?]{Given a byte
 string, produces the corresponding byte string.}
 
 Composing these two should be the identity for legal OSC elements
 (or legal byte strings, if composed the other way).
 
 @defproc[(osc-element? (value any/c)) boolean?]{
 Returns @racket[true] when called with an OSC Element
         
 
 An OSC Element is either a bundle or a message.
 
 @racketgrammar[osc-element?
 osc-bundle?
 osc-message?]
 
 }
 
 An OSC Bundle can contain other elements:
 
 @defstruct*[osc-bundle ((timestamp timestamp?) (elements (listof osc-element?))) #:prefab]{Produce an OSC bundle.}
                       
 An OSC Message consists of an address and arguments:
 
 @defstruct*[osc-message ((address byte-string?) (values (listof osc-value?))) #:prefab]{Produce an OSC message.}
                       
 An OSC value is one of a number of different kinds of s-expressions. Let me know if you can see a 
 better way to document this:
 
 @defproc[(osc-value? (value any/c)) boolean?]{
 Returns true for OSC values. Here's the definition:
                                               

@#reader scribble/comment-reader
(racketblock
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
)
}
}
@section{Reporting Bugs}

For Heaven's sake, report lots of bugs!