#lang racket

(require "../bytes-to-osc.rkt"
         "../osc-defns.rkt")

;; read libpcap files.

(define (file->packets file)
  (define pcap-bytes (file->bytes file))
  
  (define global-header (subbytes pcap-bytes 0 (* 6 4)))
  
  (let loop ([offset 24])
    (cond [(< offset (bytes-length pcap-bytes))
           (define pcaprec-header (subbytes pcap-bytes
                                            offset
                                            (+ offset 16)))
           (define captured-len (integer-bytes->integer pcaprec-header
                                                        #f #f
                                                        8 12))
           (define packet-len (integer-bytes->integer pcaprec-header
                                                      #f #f
                                                      12 16))
           (when (not (= captured-len packet-len))
             (fprintf (current-error-port)
                      "warning: captured only ~v bytes of packet with ~v bytes\n"
                      captured-len packet-len))
           (printf "packet len: ~v\n" captured-len)
           (cons 
            (list pcaprec-header
                  (subbytes pcap-bytes (+ offset 16)
                            (+ offset 16 captured-len)))
            (loop (+ offset 16 captured-len)))]
          [else empty])))


(define packets (file->packets
                 "/tmp/boo"#;"/Users/clements/only-to-scsynth"))

(length packets)

(define (packet-data packet)
  (unless (= (bitwise-and #xf (bytes-ref packet 4)) 5)
    (error 'packet-data "expected to see 5 in low bits of byte 4, got: ~v"
           (bytes-ref packet 4)))
  (subbytes packet 32 (bytes-length packet)))


(define packet-datas (map packet-data (map second packets)))


(define parsed (for/list ([d packet-datas]
                     [i (in-naturals)])
                 (printf "~v\n" i)
                 (bytes->osc-element d)))

(define (not-status data)
  (match data
    [(osc-message #"/status" any) #f]
    [else #t]))

(define non-status (filter not-status parsed))

(length non-status)

(define preloads (take non-status (- (length non-status) 5)))

(write-to-file preloads "/tmp/preloads.rktd")

(define actions (drop non-status (- (length non-status) 5)))

(write-to-file actions "/tmp/actions.rktd")

