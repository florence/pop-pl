#lang racket
(require (for-syntax syntax/parse racket/syntax unstable/sequence))
(require racket/undefined)

(provide define/state update define/handler state
         new-handler remove-handler import)
;;;;; state
(define state-vars null)
(define state-old (hash))
(define state-new (hash))

(define-syntax (define/state stx)
  (syntax-parse stx
    [(_ name:id
       value ...)
     #'(begin
         (define name 'name)
         (add-var! 'name))]))
(define (add-var! id)
  (hash-set! state-new id undefined))

(define-syntax (update stx)
  (syntax-parse stx
    [(_ id:id v:expr)
     #'(in:update 'id v)]
    [(_ id:id v:expr s:expr)
     #'(in:update 'id v s)]))
(define (in:update n v)
  (unless (hash-has-key? state-new n)
    (error 'update "non existant field"))
  (when (hash-has-key? current-state-delta n)
    (error 'update "duplicate set"))
  (hash-set! current-state-delta n v))

(define-match-expander state 
  (lambda (stx)
    (syntax-parse stx
      [(_ (x:id v) ...)
       #'(hash-table ('id v) ... (_ _) ___)])))
;;;;; handlers
(define current-handler-delta (make-hash))
(define current-state-delta (make-hash))
(define current-messages null)
(define handlers (make-hash))
(define-syntax (define/handler stx)
  (syntax-parse stx
    [(_ (name:id old:id new:id evt:id)
        body:expr ...)
     #'(begin (add-handler! 'name (in:lambda (old new evt) body ...)))]))
(define (add-handler! n f)
  (hash-set! handlers n f))
(define-syntax (in:lambda stx)
  (syntax-parse stx
    [(_ (x:id ...) body ...)
     (with-syntax ([(ids ...)
                    (for/list ([x (in-syntax #'(x ...))]) 
                      (if (equal? (syntax-e x) '_)
                          (generate-temporary)
                          x))])
       #'(lambda (ids ...) body ...))]))
;;;;; messages
;; Response:
;; (msg Symbol (Listof Any))
(struct msg (id value) #:transparent)
(define-syntax (import stx)
  (syntax-parse stx
    [(_ id:id ...)
     #'(begin
         (define (id . args)
           (add-message! 'id args)) ...)]))
(define (add-message! id args)
  (cons! current-messages (msg id args)))

(define-syntax (new-handler stx)
  (syntax-parse stx
    [(_ x:id e:expr) #'(in:new-handler 'x e)]))
(define (in:new-handler name f)
  (if (and (not (hash-has-key? handlers name))
           (not (hash-has-key? current-handler-delta name)))
      (hash-set! current-handler-delta  name f)
      (error 'new-handler "handler exists or was already added")))
(define-syntax (remove-handler stx)
  (syntax-parse stx
    [(_ x:id) #'(in:remove-handler 'x)]))
(define (in:remove-handler name)
  (if (and (hash-has-key? handlers name)
           (hash-has-key? current-handler-delta name))
      (hash-remove! current-handler-delta name)
      (error 'remove-handler "handler does not exist or was already removed")))


;;;;; evaluation

;; Event -> [Listof Response]
;; run this event through the system
(define (evaluate-one-event event)
  (evaluate-handlers event))

;; Event -> [Listof Response]
;; run the handlers over the event
(define (evaluate-handlers event)
  (for ([h handlers])
    (h state-old state-new event))
  (set! state-old state-new)
  (set! state-new (replace current-state-delta state-new))
  (set! current-state-delta (make-hash))
  (set! handlers current-handler-delta)
  (begin0 current-messages 
    (set! current-messages null)))
;; Hash Hash -> Hash
;; like merge but uses hash-set
(define (replace update current)
  (for/fold ([result current]) ([(id value) (in-hash update)])
    (hash-set result id value)))

;;;;; aux
(define-syntax (cons! stx)
  (syntax-parse stx
    [(_ x:id v:expr)
     #'(set! x (cons v x))]))
