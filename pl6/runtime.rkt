#lang racket
(require (for-syntax syntax-parse))
(require racket/undefined)

(provide define/state define/external update define/handler state
         new-handler give extern given import)
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
(define-syntax (define/external stx)
  (syntax-parse stx
    [(_ name:id value ...)
     #'(begin
         (define/state name value ...))]))
(define (add-var! id)
  (hash-set! state-new id undefined))

(define-syntax (update stx)
  (syntax-parse stx
    [(_ id:id v:expr)
     #'(in:update 'id v)]
    [(_ id:id v:expr s:expr)
     #'(in:update 'id v s)]))
(define (in:update n v [s (hash)])
  (unless (hash-has-key state-new n)
    (error 'update "non existant field"))
  (when (hash-has-key s n)
    (error 'update "duplicate set"))
  (hash-set s n v))

(define-match-expander state (make-rename-transformer #'hash-table))
;;;;; handlers
(define handlers null)
(define-syntax (define/handler stx)
  (syntax-parse stx
    [(_ (name:id old:id new:id evt:id)
        body:expr)
     #'(begin 
         (define (name old new evt) body)
         (add-handler! name))]))
(define (add-handler! f)
  (cons! handlers f))
;;;;; messages
;; event is one of:
;; (extern Symbol Any)
;; (given Any)
(struct extern (id value) #:transparent)
(struct given (drug) #:transparent)

;; response is one of:
;; (new-handler Handler)
;; (give Any)
;; (msg Symbol (Listof Any))
(struct new-handler (f))
(struct give (drug) #:transparent)
(struct msg (id value) #:transparent)
(define-syntax (import stx)
  (syntax-parse stx
    [(_ id:id ...)
     #'(begin
         (define (id . args)
           (msg 'id args)) ...)]))

;;;;; evaluation
(define (evaluate event)
  (evaluate-event! event)
  (evaluate-responses (evalute-handlers event)))

(define (evaluate-event! event)
  (match event
    [(extern id value)
     (set! state-new (hash-update state-new id (const value)))]
    [_ (void)]))

(define (evaluate-handlers event)
  (define-values (updates messages)
    (for/fold ([update (hash)] [messages null]) ([h handlers])
      (define-values (u m) (h state-old state-new event))
      (values (merge u update) (append messages m))))
  (set! state-old state-new)
  (set! state-new (replace updates state-new))
  messages)
(define (merge u1 u2)
  (for/fold ([merged u1]) ([(id value) (in-hash u2)])
    (update id value merged)))
(define (replace update current)
  (for/fold ([result current]) ([(id value) (in-hash update)])
    (hash-set result id value)))

(define (evaluate-responses rs)
  (for/fold ([outgoing null]) ([resp rs])
    (match resp 
      [(new-handler f)
       (add-handler! f)
       outgoing]
      [_ (cons resp outgoing)])))

;;;;; aux
(define-syntax (cons! stx)
  (syntax-parse stx
    [(_ x:id v:expr)
     #'(set! x (cons v x))]))
