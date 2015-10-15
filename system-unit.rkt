#lang debug racket/unit

(require "system-sig.rkt" "private/shared.rkt"
         "prescription-sig.rkt")

(import)
(export system^)

(define TIME-ADVANCE 60)

;; Network: [Hashof Any Actor]
(struct network (actors))

;; Actor :
(struct actor (eval))
;; the first field matches the -eval function from a .pop module

(define (new-network)
  (network (make-hash)))

(define (spawn-actor! network path-or-unit)
  (define prescription@
    (if (unit? path-or-unit)
        path-or-unit
        (dynamic-require path-or-unit 'the-unit)))
  (define-values/invoke-unit prescription@
    (import)
    (export prescription^))
  (define new-actor (actor -eval))
  (hash-set! (network-actors network) name new-actor)
  (-start))

(define (send-message! network msg)
  (apply
   append
   (for/list ([(_ a) (network-actors network)])
     ((actor-eval a) msg))))

(define (advance! network time)
  (define t (time->stamp time))
  (reverse
   (for/fold ([msg null])
             ([_ (in-range 0 t TIME-ADVANCE)])
     (append msg (send-message! network (message '(time) (list TIME-ADVANCE) #f))))))
