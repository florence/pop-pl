#lang racket
(require (for-syntax syntax/parse racket/syntax unstable/sequence))
(require racket/undefined racket/async-channel racket/gui racket/block unstable/match racket/match racket/dict syntax/id-table)

(require (for-syntax racket/base))
(provide (for-syntax (all-from-out racket/base syntax/parse)))
(provide 
 (rename-out [in:app #%app]
             [in:top-interaction #%top-interaction])
 #%top 
 #%module-begin
 (all-from-out unstable/match racket/match)
 let define void #%top
 quasiquote unquote #%datum quote when and or
 not
 require #%require provide #%provide
 equal? values error
 number?
 (rename-out
  [in:lambda lambda]
  [in:lambda λ]) 
 
 ;; math with units 
 number/unit
 (rename-out [units:* *]
             [units:+ +]
             [units:- -]
             [units:/ /]
             [units:< <]
             [units:> >]
             [units:<= <=]
             [units:>= >=]
             [units:= =]
             [units:max max]
             [units:min min]
             [units:add1 add1]
             [units:sub1 sub1])

 define/state update define/handler ;state
 new-handler remove-handler 
 drug-type
 drug
 state
 (rename-out 
  [in:import import]))
;;;;; lang
;; supporting numbers as units, and objects as functions
(struct number/unit (amount unit) #:transparent)
(define-syntax (in:app stx)
  (syntax-parse stx
    [(_ n:number u:id) (syntax/loc stx (number/unit n 'u))]
    [(_ f a ...) (syntax/loc stx (#%app f a ...))]))
;;;;; state
(define state-old (make-immutable-free-id-table))
(define state-new (make-immutable-free-id-table))

(define-syntax (define/state stx)
  (syntax-parse stx
    #:datum-literals (->)
    [(_ name:id
        value:expr ... -> v)
     (syntax-local-lift-expression (syntax/loc stx (add-var! #'name v)))
     (syntax/loc stx
       (define (name state)
         (dict-ref state #'name)))]
    [(_ name:id value:expr ...)
     (syntax/loc stx (define/state name value ... -> undefined))]))
(define (add-var! id v)
  (set! state-new (dict-set state-new id v)))

(define-syntax (update stx)
  (syntax-parse stx
    [(_ id:id v:expr)
     (syntax/loc stx (in:update 'id v))]
    [(_ id:id v:expr s:expr)
     (syntax/loc stx (in:update 'id v s))]))
(define (in:update n v)
  (unless (dict-has-key? state-new n)
    (error 'update "non existant field"))
  (when (dict-has-key? current-state-delta n)
    (error 'update "duplicate set"))
  (dict-set! current-state-delta n v))

(define-match-expander state 
  (lambda (stx)
    (syntax-parse stx
      [(_ (x:id v) ...)
         #`(app 
            (lambda (t) (and (free-id-table? t)
                        (free-id-table-map t list)))
            (list-no-order (? (lambda (p) (free-identifier=? (car p) #'x))
                              (list _ v)) ... 
                           _ ___))])))

;;;;; handlers
(define current-handler-delta (make-hash))
(define current-state-delta (make-hash))
(define current-messages null)
(define handlers (hash))
(define-syntax (define/handler stx)
  (syntax-parse stx
    [(_ (name:id old:id new:id evt:id)
         body:expr ...)
     (with-syntax ([func (syntax/loc stx (in:lambda (old new evt) body ...))])
       (syntax/loc stx
         (add-handler!
          'name
          func)))]))

(define (add-handler! n f)
  (set! handlers (dict-set handlers n f)))

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
     (syntax/loc stx
       (begin
         (define (id . args)
           (add-message! 'id args)) ...))]))
(define (add-message! id args)
  (cons! current-messages (msg id args)))

(define-syntax (new-handler stx)
  (syntax-parse stx
    [(_ x:id e:expr)
     (syntax/loc stx
       (in:new-handler 'x e))]))
(define (in:new-handler name f)
  (if (and (not (dict-has-key? handlers name))
           (not (dict-has-key? current-handler-delta name)))
      (dict-set! current-handler-delta  name f)
      (error 'new-handler "handler ~s exists or was already added.\n current handlers: ~s" 
             name
             (dict-keys current-handler-delta))))
(define-syntax (remove-handler stx)
  (syntax-parse stx
    [(_ x:id) (syntax/loc stx (in:remove-handler 'x))]))
(define (in:remove-handler name)
  (if (and (dict-has-key? handlers name)
           (dict-has-key? current-handler-delta name))
      (dict-remove! current-handler-delta name)
      (error 'remove-handler "handler ~s does not exist or was already removed.\n current handlers: ~s"
             name
             (dict-keys current-handler-delta))))

(define-syntax (drug-type stx)
  (syntax-parse stx
    [(_ x:id) (syntax/loc stx (in:drug-type 'x))]))
(struct in:drug-type (v) #:transparent)
(struct drug (kind how amount) #:transparent)


;;;;; evaluation

;; Event -> [Listof Response]
;; run this event through the system
(define (evaluate-one-event event)
  (displayln `(,state-old ,state-new ,(dict-keys handlers),event))
  (evaluate-handlers event))

;; Event -> [Listof Response]
;; run the handlers over the event
(define (evaluate-handlers event)
  (set! current-handler-delta (dict->mutable-hash handlers))
  (for ([(_ h) (in-dict handlers)])
    (h state-old state-new event))
  (set! state-old state-new)
  (set! state-new (replace current-state-delta state-new))
  (set! current-state-delta (make-hash))
  (set! handlers (dict->immutable-hash current-handler-delta))
  (begin0 current-messages 
    (set! current-messages null)))
;; Hash -> Hash
;; make an immutable copy of this hash
(define (dict->immutable-hash h)
  (for/hash ([(k v) (in-dict h)])
    (values k v)))
;; Hash -> Hash
;; make a mutable copy of this hash
(define (dict->mutable-hash h)
  (define r (make-hash))
  (for ([(k v) (in-dict h)])
    (dict-set! r k v))
  r)
;; Hash Hash -> Hash
;; like merge but uses dict-set
(define (replace update current)
  (for/fold ([result current]) ([(id value) (in-dict update)])
    (dict-set result id value)))

;;;;; numbers

;; ((U number number/unit) * -> A) ->
;; ((U number number/unit) * -> (U A number/unit))
;; wrap over the math function to handle units
;; currently units must match exactly, or have no units given
(define ((run/check f) . a)
  (define ((make-numberizer f) a)
    (if (number/unit? a) (f a) a))
  (let ([args (map (make-numberizer number/unit-amount) a)]
        [units (map number/unit-unit (filter number/unit? a))])
    (if (andmap (lambda (v) (andmap (curry equal? v) units)) units)
        (let ([r (apply f args)])
          (if (or (not (number? r))
                  (not (cons? units)))
              r
              (number/unit r (first units))))
        (error (object-name f) "couldn't match units"))))
;; math, but deals with units
(define units:* (run/check *))
(define units:+ (run/check +))
(define units:- (run/check -))
(define units:/ (run/check /))
(define units:< (run/check <))
(define units:> (run/check >))
(define units:= (run/check =))
(define units:<= (run/check <=))
(define units:>= (run/check >=))
(define units:max (run/check max))
(define units:min (run/check min))
(define units:add1 (run/check add1))
(define units:sub1 (run/check sub1))


;;;;; runtime
(define (start)
  (define to-program (make-async-channel))
  (define to-outside (make-async-channel))
  (define es (make-eventspace))
  (define task-list
    (parameterize ([current-eventspace es])
      (new
       (class frame%
         (super-new [label "please do"])
         
         (define super-container (new horizontal-panel% [parent this]))
         (define message-list (new vertical-pane%
                                   [parent super-container]
                                   [min-width 100]
                                   [min-height 100]))
         (define controls (new vertical-pane% [parent super-container]))
         (define inc-time
           (new button%
                [parent controls]
                [label "move forward five minutes in time"]
                [callback
                 (lambda _ 
                   ;; will figure out what units time should be in later
                   (async-channel-put to-program `(in time by ,(* 5 60))))]))
         
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
            (define container (new horizontal-panel% [parent message-list]))
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
                [parent message-list]
                [callback
                 (lambda (b e)
                   (send message-list delete-child b)
                   (unless (null? extras)
                     (for-each (lambda (f) (f)) extras)))]))

         (send this show #t)))))


  (thread
   (thunk
    (let l ()
      (send task-list update-tasks (async-channel-get to-outside))
      (l))))
  (thread
   (thunk
    (let l ()
      (for-each (curry async-channel-put to-outside)
                (evaluate-one-event (async-channel-get to-program)))
      (l))))
  (sync es)
  (void))

(define-syntax (in:top-interaction stx)
  (syntax-parse stx
    #:datum-literals (start)
    [(_ start) (syntax/loc stx (#%top-interaction start))]))

;;;;; aux
(define-syntax (cons! stx)
  (syntax-parse stx
    [(_ x:id v:expr)
     #'(set! x (cons v x))]))
