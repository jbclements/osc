#lang racket

(require racket/udp
         "osc.rkt")

(define out-socket (udp-open-socket))

(udp-send-to out-socket "127.0.0.1" 13698 
             (make-osc-message #"/var/big/log"
                               2340000
                               #"where is this going?"))

