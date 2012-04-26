#lang racket

;; Copyright 2012 John Clements (clements@racket-lang.org)
;; released under a BSD license


(require racket/udp
         racket/runtime-path
         "../osc.rkt"
         "../osc-defns.rkt"
         "../osc-parse.rkt")

(define-runtime-path here ".")

(define setup-messages (file->value (build-path here "preloads.rktd")))
(define action-messages (file->value (build-path here "actions.rktd")))

(define scsynth-socket 13698)
(define receive-socket 13699)

(define the-socket (udp-open-socket))

(udp-bind! the-socket "127.0.0.1" receive-socket)

;; this assumes you've already started scsynth, like this:
;"scsynth -u 13698"

;; also, we're assuming we won't get any messages longer than 10K
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
     #;(printf "received buffer: ~v\n" received)
     (printf "decoded: ~e\n" (parse-osc-bytes received))
     (loop))))

(define (send-command message)
  (udp-send-to the-socket "127.0.0.1" scsynth-socket 
               (osc-element->bytes message)))

(printf "sending status request: if no status.response seen, the server is not listening.\n")

(send-command (osc-message #"/status" empty))


(printf "hit return to start\n")
(read-line)


(for ([msg setup-messages])
  (send-command msg))


;; start the sine wave:
(send-command (first action-messages))

;; wait, so that the printf winds up below all of the other printout:
(sleep 0.5)
(printf "hit return to stop.\n")
(read-line)

(for ([msg (rest action-messages)])
  (send-command msg))

;; sleep so that we can receive the rest of the response messages
(sleep 0.5)





