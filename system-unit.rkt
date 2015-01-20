#lang racket/unit

(require "system-sig.rkt" "private/shared.rkt"
         "prescription-sig.rkt" (submod "main.rkt" eval)
         racket/match)

(import)
(export system^)

(define TIME-ADVANCE 60)

;; Network: [Hashof Any Actor]
(struct network (actors))

;; Actor :
(struct actor (state) #:mutable)
;; the first field matches the -eval function from a .pop module

(define (new-network)
  (network (make-hash)))

(define (spawn-actor! network path-or-state)
  (define-values (the-state name)
    (match path-or-state
      [(list state name) (values state name)]
      [path (values (dynamic-require path-or-state 'the-state)
                    (dynamic-require path-or-state 'name))]))
  (define-values (state ms) (start the-state))
  (define new-actor (actor state))
  (hash-set! (network-actors network) name new-actor)
  ms)

(define (send-message! network msg)
  (apply
   append
   (for/list ([(_ a) (network-actors network)])
     (define-values (state msg) (eval msg (actor-state a)))
     (set-actor-state! a state)
     msg)))

(define (advance! network time)
  (define t (time->stamp time))
  (reverse
   (for/fold ([msg null])
             ([_ (in-range 0 t TIME-ADVANCE)])
     (append msg (send-message! network (message '(time) (list TIME-ADVANCE) #f))))))
