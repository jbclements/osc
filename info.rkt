#lang setup/infotab

(define name "RSound")

(define blurb '((p "osc provides functions to translate "
                   "to and from OSC (Open Sound Control) "
                   "byte strings")))

(define scribblings '(("osc.scrbl" () (net-library))))
(define categories '(net media))
(define version "2012-04-27-12:09")
(define release-notes '((p "Initial Release!")))

;; don't compile the stuff in the berkeley subdirectory.
(define compile-omit-paths '("berkeley-libs"))

;; planet-specific:
(define repositories '("4.x"))
(define primary-file "main.rkt")

#;(define homepage "http://schematics.sourceforge.net/")
#;(define url "http://schematics.sourceforge.net/")

