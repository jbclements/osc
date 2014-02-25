#lang racket

;; Copyright 2012 John Clements (clements@racket-lang.org)
;; released under Mozilla Public License 2.0


(require racket/udp
         racket/runtime-path
         "../osc-to-bytes.rkt"
         "../osc-defns.rkt"
         "../bytes-to-osc.rkt")

(define-runtime-path here ".")

(define action-messages (file->value (build-path here "actions.rktd")))

;; this assumes you've started the renoise OSC server, listening on UDP port 8790
(define renoise-socket 8790)
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
     (printf "decoded: ~e\n" (bytes->osc-element received))
     (loop))))

(define (send-command message)
  (udp-send-to the-socket "127.0.0.1" renoise-socket 
               (osc-element->bytes message)))


(let loop ()
  (printf "hit return to start\n")
  (define r (for/list ([i 8])(+ 50 (random 24))))
  (read-line)
  
  
  ;; start the sine wave:
  (for/list ([start-pitch (append r (list #f #f))]
             [stop-pitch (append (list #f #f) r)])
    (when start-pitch
      (send-command (osc-message #"/renoise/trigger/note_on" `(1 2 ,start-pitch 127))))
    (sleep 0.3)
    (when stop-pitch
      (send-command (osc-message #"/renoise/trigger/note_off" `(1 2 ,stop-pitch))))
    )
  
  ;; wait, so that the printf winds up below all of the other printout:
  (sleep 0.5)
  (printf "hit return to stop.\n")
  (read-line)

  (loop))


;; sleep so that we can receive the rest of the response messages
(sleep 0.5)





