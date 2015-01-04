#lang racket/unit

(require "system-sig.rkt")

(import)
(export system^)

;; Network: [Hashof module-path? Actor]
(struct network (actors))

;; Actor :
(struct actor (eval reset!))
;; the first field matches the -eval function from a .pop module
;; the second field matches the -reset function from a .pop module

(define (new-network)
  (network (make-hash)))

(define (spawn-actor! network path)
  (define -eval (dynamic-require path '-eval))
  (define -start (dynamic-require path '-start))
  (define -reset! (dynamic-require path '-reset!))
  (define new-actor (actor -eval -reset!))
  (hash-set! (network-actors network) path new-actor)
  (-start))

(define (send-message! network msg)
  (apply
   append
   (for/list ([(_ a) (network-actors network)])
     ((actor-eval a) msg))))
