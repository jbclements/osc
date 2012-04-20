#lang racket

(require racket/udp)

(define the-socket (udp-open-socket))
(udp-bind! the-socket "127.0.0.1" 13442)

(define receive-buffer (make-bytes 100))

(let loop ()
  (define-values (len hostname src-port)
    (udp-receive! the-socket receive-buffer))
  (printf "current seconds: ~v\n" (current-seconds))
  (printf "len: ~v\nhostname: ~v\nsrc-port: ~v\n" len hostname src-port)
  (printf "received buffer: ~v\n" receive-buffer)
  (newline)
  (loop))