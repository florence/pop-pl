#lang racket
(require (for-syntax syntax/parse racket/syntax unstable/sequence))
(require racket/undefined racket/async-channel racket/gui racket/block unstable/match racket/match)

(require (for-syntax racket/base))
(provide (for-syntax (all-from-out racket/base syntax/parse)))
(provide 
 (rename-out [in:module-begin #%module-begin]
             [in:app #%app])
 #%top 
 (all-from-out unstable/match racket/match)
 let define void #%top
 quasiquote unquote #%datum quote when and or
 not +
 require #%require provide #%provide
 equal? values
 (rename-out
  [in:lambda lambda]
  [in:lambda λ]) 
 
 ;; math with units (eventually)
 max + - sub1 add1 <= < > >=

 define/state update define/handler ;state
 new-handler remove-handler 
 drug-type
 drug
 (rename-out 
  [in:import import]))
;;;;; lang
(define-syntax (in:module-begin stx)
  (syntax-parse stx
    [(_ body ...)
     #'(#%module-begin
        body ...
        (start))]))
;; supporting numbers as units, and objects as functions
(struct number/unit (amount unit) #:transparent)
(define-syntax (in:app stx)
  (syntax-parse stx
    [(_ n:number u:id) #'(number/unit n 'u)]
    [(_ f a ...)
     #'(#%app f a ...)]))
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
  (set! state-new (hash-set state-new id undefined)))

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

; is borked
#;
(define-match-expander state 
  (lambda (stx)
    (syntax-parse stx
      [(_ (x:id v) ...)
       #'(hash-table ('x v) ...)])))

;;;;; handlers
(define current-handler-delta (make-hash))
(define current-state-delta (make-hash))
(define current-messages null)
(define handlers (hash))
(define-syntax (define/handler stx)
  (syntax-parse stx
    [(_ (name:id old:id new:id evt:id)
        body:expr ...)
     #'(begin
         (add-handler! 'name (in:lambda (old new evt) body ...))
         (define (name state)
           (hash-ref 'name state)))]))

(define (add-handler! n f)
  (set! handlers (hash-set handlers n f)))

(define-syntax (in:lambda stx)
  (syntax-parse stx
    [(_ (x:id ...) body ...)
     (with-syntax ([(ids ...)
                    (for/list ([x (in-syntax #'(x ...))]) 
                      (if (equal? (syntax-e x) '_)
                          (generate-temporary)
                          x))])
       (syntax/loc stx (lambda (ids ...) body ...)))]))

;;;;; messages
;; Response:
;; (msg Symbol (Listof Any))
(struct msg (id value) #:transparent)
(define-syntax (in:import stx)
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

(define-syntax (drug-type stx)
  (syntax-parse stx
    [(_ x:id)
     #'(in:drug-type 'x)]))
(struct in:drug-type (v) #:transparent)
(struct drug (kind how amount) #:transparent)


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

;;;;; runtime
(define to-program (make-async-channel))
(define to-outside (make-async-channel))

(define task-list%
  (new
   (class object%
     (super-new)
     
     (define frame (new frame% [label "please do"]))
     (define pane (new vertical-pane% [parent frame]))
     
     (define/public (update-tasks message)
       (match message
         [(msg n args)
          (match* (n args)
            [('give d)
             (add-task! (~a "take " d)
                        (thunk (async-channel-put to-program `(given ,d))))]
            [('request (? n symbol?))
             (add-request! n)]
            [(_ _)
             (add-task! (apply ~a n ": " args))])]))
     
     (define (add-request! n)
       (block
        (define container (new horizontal-panel% [parent pane]))
        (define button
          (new button%
               [parent container]
               [label (format "what is your current ~a" n)]
               [callback 
                (lambda (b e)
                  (define v (string->number (send text get-value)))
                  (when v
                    (async-channel-put to-program `(update (,n ,v)))))]))
        (define text
          (new text-field%
               [parent container]
               [label ""]))))

     (define (add-task! str . extras)
       (new button%
            [label str]
            [parent pane]
            [callback
             (lambda (b e)
               (send pane delete-child b)
               (unless (null? extras)
                 (for-each (lambda (f) (f)) extras)))]))
     (send frame show #t))))

(define (start)
  (thread
   (thunk
    (let l ()
      (send task-list% update-tasks (async-channel-get to-outside))
      (l))))
  (thread
   (thunk
    (let l ()
      (for-each (curry async-channel-put to-outside)
                (evaluate-one-event (async-channel-get to-program)))
      (l))))
  (sync never-evt))

;;;;; aux
(define-syntax (cons! stx)
  (syntax-parse stx
    [(_ x:id v:expr)
     #'(set! x (cons v x))]))
