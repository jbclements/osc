#lang racket

(require racket/udp
         "osc.rkt")

(define the-socket (udp-open-socket))

(udp-bind! the-socket "127.0.0.1" 13699)

(udp-send-to the-socket "127.0.0.1" 13698 
             (make-osc-message #"/status"))

(define receive-buffer (make-bytes 10000))

(define-values (len hostname src-port)
  (udp-receive! the-socket receive-buffer))
(printf "current seconds: ~v\n" (current-seconds))
(printf "len: ~v\nhostname: ~v\nsrc-port: ~v\n" len hostname src-port)
(printf "received buffer: ~v\n" (subbytes receive-buffer
                                          0 len))




