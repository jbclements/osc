#lang setup/infotab

(define name "Rack-OSC")

(define blurb '((p "Rack-OSC provides functions to translate "
                   "to and from OSC (Open Sound Control) "
                   "byte strings")))

(define scribblings '(("osc.scrbl" () (net-library))))
(define categories '(net media))
(define version "2012-05-07-22:33")
(define release-notes '((p "Initial Release!")))

;; don't compile the stuff in the berkeley subdirectory.
(define compile-omit-paths '("berkeley-libs"))

;; planet-specific:
(define repositories '("4.x"))
(define required-version "5.3.0")
(define primary-file "main.rkt")

#;(define homepage "http://schematics.sourceforge.net/")
#;(define url "http://schematics.sourceforge.net/")

