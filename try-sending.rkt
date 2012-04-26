#lang racket

;; Copyright 2012 John Clements (clements@racket-lang.org)
;; released under a BSD license


(require racket/udp
         "osc.rkt"
         "osc-parse.rkt")

(define the-socket (udp-open-socket))

(udp-bind! the-socket "127.0.0.1" 13699)


;"./scsynth -u 13698"

(define receive-buffer (make-bytes 10000 0))

(thread
 (lambda ()
   (let loop ()
     (printf "waiting for incoming messages.\n")
     (define-values (len hostname src-port)
       (udp-receive! the-socket receive-buffer))
     (printf "current seconds: ~v\n" (current-seconds))
     (printf "len: ~v\nhostname: ~v\nsrc-port: ~v\n" len hostname src-port)
     (define received (subbytes receive-buffer 0 len))
     (printf "received buffer: ~v\n" received)
     (printf "decoded: ~e\n" (parse-osc-bytes received))
     (loop))))

(define (send-command message)
  (udp-send-to the-socket "127.0.0.1" 13698 
               message))

(send-command (make-osc-message #"/status"))




