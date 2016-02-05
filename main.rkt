#lang racket/base
#|
if youre wondering why some names start with '-', its because they need to be visible in a
pop-pl program (ie the program litteral expands, or could expand to them),
but we don't want users to actually access them. No pop-pl id can contain a symbol, so prefixing
with '-' prevents access.
|#
(provide
 ;; from racket
 #%top #%app #%datum
 define and not
 (rename-out [in:module-begin #%module-begin]
             [in:top-inter #%top-interaction])
 module*
 ;; forward facing
 whenever whenever-new initially after every whenever-cond
 (rename-out [every q])
 (rename-out
  [unit:+ +]
  [unit:- -]
  [unit:* *]
  [unit:/ /]
  [unit:> >]
  [unit:>= >=]
  [unit:< <]
  [unit:<= <=]
  [unit:= =]
  [in:in-range in-range])
 is
 use
 ;; internal
 make-handler define-message add-handler define-handler
 -number
 local-require
 here-be-tests
 ;;for the configure-runtime
 current-environment)

(require (for-syntax racket/base
                     racket/dict
                     racket/list
                     racket/match
                     racket/syntax
                     syntax/id-table
                     syntax/parse
                     racket/sequence
                     racket/format
                     syntax/strip-context)
         (for-meta 2 racket/base
                     syntax/parse)
         racket/bool
         racket/list
         racket/match
         racket/stxparam
         racket/function
         racket/unit
         "private/shared.rkt")

(module+ test (require rackunit))

;;; things that need to go first
(begin-for-syntax
  (define-syntax-class number+unit
    #:literals (-number)
    (pattern x:number)
    (pattern (-number x:number y:id))))

;; to guard against function misuse
(define-syntax (define/func stx)
  (syntax-parse stx
    [(_ (n:id a ...) b ...)
     #'(define/func n (lambda (a ...) b ...))]
    [(_ n:id b)
     (with-syntax ([real (generate-temporary)])
       #'(begin
           (define real b)
           (define-syntax (n stx)
             (syntax-parse stx
               [na:id (raise-syntax-error 'function "a function cannot be used as an argument" stx)]
               [(na:id a (... ...))
                #'(real a (... ...))]))))]))

;;; environment
(struct environment
  (message ; #f or message
   time ; current time in seconds
   log ; listof Messages
   outgoing-log ; listof Message
   handlers ; hashof Sym Handlers
   next-handlers ; hahsof Sym Handlers
   message-query-cache ; hashof Sym (listof Message)
   message-query-cache-generators ; listof (-> Message Void)
   message-query-cache:last-time) ; hashof Sym Time
  #:mutable
  #:transparent)

(define current-environment (make-parameter #f))
(define (get-current-environment)
  (define cenv (current-environment))
  (if cenv
      cenv
      (error 'internal "no environment set")))
(define (current-time)
  (environment-time (get-current-environment)))
(define (current-next-handlers)
  (environment-next-handlers (get-current-environment)))
(define (current-log)
  (environment-log (get-current-environment)))
(define (current-message-query-cache)
  (environment-message-query-cache (get-current-environment)))
(define (current-message-query-cache-generators)
  (environment-message-query-cache-generators (get-current-environment)))
(define (current-message-query-cache:last-time)
  (environment-message-query-cache:last-time (get-current-environment)))
(define (current-outgoing-log)
  (environment-outgoing-log (get-current-environment)))
(define (current-handlers)
  (environment-handlers (get-current-environment)))
(define (current-message)
  (define cenv (get-current-environment))
  (define cmes (environment-message cenv))
  (if cmes
      cmes
      (error 'internal "no message in environment ~a" cenv)))

(define (make-empty-environment)
  (environment
   #f         ;message
   0          ;time
   null       ;log
   null       ;outgoing-log
   (hash)     ;handlers
   (make-hash);next handlers
   (make-hash);cache
   null       ;generatiors
   (make-hash);time cache
   ))

;;; evaluator

;; Message -> (Listof Message)
;; evaluate the message in the current-environment
(define (eval m)
  (next-message! m)
  (define msg (current-message))
  (cache-message! msg)
  (for ([(_ h!) (in-hash (current-handlers))])
    (h!))
  (cycle-env!))

;; Message -> Void
;; E: update the time and add a corrected current message to the environment
(define (next-message! m)
  (set-environment-message!
   (current-environment)
   (msg-fill/update-time! m)))

;; Message -> Message
;; E: updates the time if the message says so
;; If the message has no timestamp, fill it in
(define (msg-fill/update-time! m)
  (match m
    [(message '(time) (list n) #f)
     (set-environment-time! (current-environment)
                            (+ n (current-time)))
     m]
    [(message types values #f)
     (message types values (current-time))]
    [_ m]))

;; -> (Listof Message)
;; E: applys changes built up of the course of evaling a message
;; returns the messages to send back out
(define (cycle-env!)
  (swap-handlers!)
  (swap-log!))

;; -> Void
;; E: apply the changes to the handlers built up in next-handlers
(define (swap-handlers!)
  (define cenv (current-environment))
  (set-environment-handlers! cenv (hash->immutable-hash (current-next-handlers))))

;; -> (Listof Message)
;; E: add the current message and outbound messages to the log
;; returns the additions to the log, no including the current message
(define (swap-log!)
  (define res (current-outgoing-log))
  (set-environment-log! (current-environment)
                        (append res
                                (list (current-message))
                                (current-log)))
  (set-environment-outgoing-log! (current-environment) null)
  res)

;; Message -> Void
;; E: add the message to the current message queue (and caches)
(define (send-message! m)
  ;(printf "~a sending ~a\n" (current-handler) m)
  (cache-message! m)
  (define out (current-outgoing-log))
  (set-environment-outgoing-log!
   (current-environment)
   (cons m out)))

;; Message -> Void
;; E: Update the caches with this message
(define (cache-message! m)
  (define t (message-time m))
  (for ([n (message-tags m)])
    (hash-set! (current-message-query-cache:last-time) n t))
  (for ([! (current-message-query-cache-generators)])
    (! m)))

;;; runtime helpers

;; (-> Message Any) -> Void
;; add a function to the list of cache generators
(define (add-matcher! f)
  (define matchers (current-message-query-cache-generators))
  (set-environment-message-query-cache-generators!
   (current-environment)
   (cons f matchers)))

;; Symbol -> (Listof Message)
;; Get all messages cached under `key`
(define (get-cached-matches key)
  (hash-ref (current-message-query-cache)
            key
            null))

;; Symbol Message -> Void
;; E: add `msg` to the messages cached under `key`
(define (add-cached-match! key msg)
  (define cache (hash-ref! (current-message-query-cache) key null))
  (hash-set! (current-message-query-cache)
             key
             (cons msg cache)))

;; Symbol -> Void
;; E: clear all messages cached under `key`
(define (clear-cached-matches! key)
  (hash-set! (current-message-query-cache)
             key
             null))

(define (hash->immutable-hash hash)
  (for/hash ([(k h) (in-hash hash)]) (values k h)))

;; Symbol Handler -> Void
;; Add `h` to the next set of handlers under the name `n`
(define (add-handler! n h)
  (hash-set! (current-next-handlers) n h))

;; Symbol -> Void
;; removed the handler named `n` from the next set of handlers
(define (remove-handler! n)
  (hash-remove! (current-next-handlers) n))

;;; module
(define-for-syntax pctx #'ctx)
(define-for-syntax (make-the-environment loc)
  (syntax-local-introduce
   (datum->syntax pctx
                  'the-environment
                  loc)))

(define-syntax (in:module-begin stx)
  (syntax-parse stx
    [(_ body ...)
     (define here #'here)
     (define (p/i i)
       (syntax-local-introduce (p i)))
     (define (p i)
       (datum->syntax pctx i here))
     (with-syntax ([the-environment/stx (make-the-environment here)]
                   [((in-unit ...) (top ...))
                    (split-body #'(body ...))]
                   [-eval/stx (p/i '-eval)]
                   [-start/stx (p/i'-start)]
                   [prescription^/stx (p/i 'prescription^)]
                   [name/stx (p/i 'name)]
                   [-wait (datum->syntax stx '-wait)])
       #`(#%plain-module-begin
          (module* configure-runtime racket/base
            (#%plain-module-begin
             (#%require pop-pl/lang/parse racket/runtime-config)
             (configure #f)
             (current-read-interaction
              (lambda (name in)
                (define-values (stx f) (parse in #:name name #:pattern Expr))
                (unless stx (error 'parse "bad"))
                stx))))
          (module* main #f
            (#%plain-module-begin
             (require pop-pl/system-unit pop-pl/system-sig)
             (define-values/invoke-unit system@
               (import)
               (export (rename system^
                               (send! send-message!)
                               #;
                               (mk-network new-network))))
             (define the-network (new-network))
             (set-wait! (lambda (t) (advance! the-network t)))
             (for-each displayln (spawn-actor! the-network the-unit))
             (current-send-message
              (lambda (m) (send! the-network m)))))

          (define (set-wait! f) (set! -wait f))
          (define -wait #f)

          top ...

          #,(datum->syntax stx '(local-require pop-pl/constants))
          #,(p/i `(,#'require pop-pl/prescription-sig))
          ;(require pop-pl/prescription-sig)
          ;;TODO get module path
          (define the-name (gensym))
          (provide the-unit)
          (define the-unit
            (unit
             (import)
             (export prescription^/stx)
             ;; global state
             (define the-environment/stx (make-empty-environment))

             (define name/stx the-name)

             (define -start/stx
               (lambda ()
                 ;;TODO shouldn't need to do this twice...
                 (parameterize ([current-environment the-environment/stx]
                                [current-send-message send-message!])
                   (-eval/stx (message '(time) (list 1) #f))
                   (-eval/stx (message '(time) (list 1) #f)))))

             (define -eval/stx
               (lambda (m)
                 (parameterize ([current-environment the-environment/stx]
                                [current-send-message send-message!])
                   (eval m))))
             (parameterize ([current-environment the-environment/stx])
               (expand/capture (let () (void) in-unit ...)))))))]))

(define-for-syntax (split-body stx)
  (define-values (u t)
    (let recur ([stx stx])
      (syntax-parse stx
        #:literals (define-message use here-be-tests)
        [() (values null null)]
        [((~and top
                (~or (define-message _ ...)
                     (use _ ...)
                     (here-be-tests _ ...)))
          body ...)
         (define-values (u t) (recur #'(body ...)))
         (values u (cons #'top t))]
        [(body0 body ...)
         (define-values (u t) (recur #'(body ...)))
         (values (cons #'body0 u) t)])))
  (list u t))

#;
(module+ test
  (let-values ([(u t)
                (split-body #'((define-message some stuff)))])
    (check-equal? (map syntax->datum u) null)
    (check-equal? (map syntax->datum t) '((define-message some stuff)))))

(define-syntax (expand/capture stx)
  (syntax-parse stx
    [(_ body)
     (local-expand/capture-lifts
      #'body
      'expression
      null)]))


(define-syntax (in:top-inter stx)
  (syntax-parse stx
    #:datum-literals (wait)
    [(_ . (wait t))
     #`(#%top-interaction
        . (values (for-each displayln (-wait t))))]
    [(_ . f)
     #`(#%top-interaction
        . (begin
            (values (for-each displayln f))))]))

;;; requiring message protocol
(define-syntax (use stx)
  (syntax-parse stx
    [(use name:id)
     (define file (~a (syntax-e #'name) ".pop"))
     ;;TODO environment swapping
     (with-syntax ([file (datum->syntax stx file)])
       (datum->syntax stx `(,#'require ,#'(except-in file the-unit))))]))

;;; handlers

(define current-handler (make-parameter #f))

(define-syntax (initially stx)
  (syntax-parse stx
    [(_ body ...)
     (with-syntax ([id (generate-temporary)])
       #'(define-handler id body ... (remove-handler! 'id)))]))

(define-syntax (define-handler stx)
  (syntax-parse stx
    [(_ n:id body ...)
     (syntax/loc stx
       (begin
         (define/func n (make-handler (parameterize ([current-handler 'n]) body ...)))
         (add-handler n)))]))

(define-syntax (make-handler stx)
  (syntax-parse stx
    [(make-handler body ...)
     #`(lambda () body ...)]))
(define-syntax (add-handler stx)
  (syntax-parse stx
    [(add-handler id)
     #'(add-handler! 'id (lambda () (id)))]))

;;; whenevers
(define-syntax whenever-cond (make-rename-transformer #'cond))
(define-syntax (whenever-new stx)
  (syntax-parse stx
    [(whenever-new (id:id e:expr)
                   body ...)
     #'(whenever-new id
                     (whenever e body ...))]
    [(whenever-new id:id
                   body:expr ...)
     (unless (dict-has-key? messages (syntax-local-introduce #'id))
       (raise-syntax-error 'messages "the name of a message needs to go here" #'id))
     (define names (dict-ref messages (syntax-local-introduce #'id)))
     (with-syntax* ([(name ...)
                     (for/list ([n names])
                       (datum->syntax #'id (syntax->datum n) #'id #'id))]
                    [(value ...)
                     (for/list ([n (in-range (length (syntax->list #'(name ...))))])
                       #`(list-ref (message-values (current-message)) #,n))])
       #`(begin
           (and #f (id))
           (when (member 'id (message-tags (current-message)))
             (let ([name value] ...)
               body ...))))]))
(define-syntax (whenever stx)
  (define-syntax-class query-name
    (pattern #:times)
    (pattern #:since-last)
    (pattern #:apart)
    (pattern #:latest))
  (define-splicing-syntax-class query
    (pattern (~seq t:expr (~seq k:query-name e:expr) ...)
             #:with query
             (let ([asc
                    (map (match-lambda [(list kw ex) (list (syntax-e kw) ex)])
                         (map syntax->list (syntax->list #'((k e) ...))))])
               (cond [(null? asc) #'t]
                     [else
                      (with-syntax ([apart (make-apart-filter (assoc '#:apart asc))]
                                    [times? (make-times-filter (assoc '#:times asc))]
                                    [get-matching
                                     (make-get-matching/since-last
                                      (assoc '#:since-last asc)
                                      (assoc '#:latest asc)
                                      (syntax-parse #'t
                                        #:literals (not)
                                        [(not e) #'e]
                                        [_ #'t]))]
                                    [n
                                     (syntax-parse #'t
                                       #:literals (not)
                                       [(not e)
                                        #'not]
                                       [_ #'values])])
                        (syntax/loc #'t
                          (let ([matching (get-matching)])
                            (and matching
                                 (n (let* ([since matching]
                                           [acceptable (apart since)])
                                      (times? acceptable)))))))]))))
  (syntax-parse stx
    [(whenever q:query body ...)
     (syntax/loc stx (when q.query body ...))]))

(define-for-syntax ((maybe-filter l) maybe-stx)
  (if (not maybe-stx)
      #'values
      (l (second maybe-stx))))
(define-for-syntax (make-since-last maybe-stx)
  (if (not maybe-stx)
      #'(lambda _ #f)
      (syntax-parse (second maybe-stx)
        [(name args ...)
         (syntax/loc this-syntax
           (lambda (msg)
             (match msg
               [(message (? (lambda (l) (member 'name l)))
                         (list args ... _ ___)
                         _)
                #t]
               [_ #f])))])))

(define-for-syntax make-apart-filter
  (maybe-filter
   (lambda (stx)
     (syntax-parse stx
       [n:number+unit
        (syntax/loc stx
          (lambda (log)
            (define-values (res _)
              (for/fold ([res null] [time 0]) ([l (reverse log)])
                (if (> (message-time l) (+ time (time->stamp n)))
                    (values (cons l res) (message-time l))
                    (values res time))))
            res))]))))

(struct failure ())

(define-for-syntax (make-get-matching/since-last since match-only-on-latest? stx)
  (syntax-parse stx
    [e:expr
     (with-syntax* ([msg (generate-temporary)]
                    [pats
                     (for/list ([(k v) (in-dict messages)])
                       (with-syntax ([name (replace-context #'e k)])
                         (syntax/loc
                             stx
                           [name
                            (lambda (stx)
                              (syntax-parse stx
                                [name:id
                                 #'(match msg
                                     [(message (? (lambda (l) (member 'name l)) _)
                                               (list* x _)
                                               _)
                                      x]
                                     [_ (raise (failure))])]))])))]
                    [reset? (make-since-last since)]
                    [key (generate-temporary)]
                    [enabled? (syntax-local-lift-expression #'#f)]
                    [x
                     (syntax-local-lift-expression
                      #'(add-matcher!
                         (lambda (msg)
                           (if (reset? msg)
                               (begin
                                 (set! enabled? #f)
                                 (clear-cached-matches! 'key))
                               (with-handlers ([failure? (lambda _ (set! enabled? #f))])
                                 (set! enabled? #t)
                                 (if (not (let-syntax pats e))
                                     (clear-cached-matches! 'key)
                                     (add-cached-match! 'key msg)))))))])
       (quasisyntax/loc stx
         (lambda () x
                 (and
                  #,@(if match-only-on-latest? #'(enabled?) #'())
                  (get-cached-matches 'key)))))]))

(define-for-syntax (make-times-filter maybe-stx)
  (if (not maybe-stx)
      #'(lambda (l) (cons? l))
      (let ([stx (second maybe-stx)])
        (syntax-parse stx
          [n:number
           (syntax/loc stx (lambda (l) (n . <= . (length l))))]))))

;;; things that use time
(define-syntax (after stx)
  (syntax-parse stx
    [(after t body ...)
     (with-syntax ([n (generate-temporary)])
       (syntax/loc stx
         (let* ([start (current-time)]
                [h (make-handler
                    (when (after? t start)
                      body ...
                      (remove-handler! 'n)))])
           (add-handler! 'n h))))]))

;; time time -> boolean
;; are we currently `diff` after `start`
(define (after? diff start)
  (< (+ (time->stamp diff) (time->stamp start))
     (time->stamp (current-time))))
(define-syntax (every stx)
  (syntax-parse stx
    [(every n:number+unit name:id exprs ...)
     (with-syntax ([(args ...)
                    (for/list ([e (in-syntax #'(exprs ...))]
                               #:unless (keyword? (syntax-e e)))
                      e)])
       #'(when
             (let ([m (hash-ref (current-message-query-cache:last-time) 'name #f)])
               (implies m (after? n m)))
           (name exprs ...)))]))
;;; numbers

(define-syntax (-number stx)
  (syntax-parse stx
    [(_ n:number unit:id)
     #'(in:number (inexact->exact n) 'unit)]))

(define ((convert/+- f) . args)
  (define unit #f)
  (for ([a args])
    (when (in:number? a)
      (define u (in:number-unit a))
      (if (not unit)
          (set! unit u)
          (unless (equal? unit u)
            (error (object-name f) "all numbers must have the same unit")))))
  (define v (apply f (strip-units args)))
  (if unit (in:number v unit) v))

(define ((convert/*/ f) . args)
  ;; we're not gonna support units for now
  (for ([a args])
    (unless (number? a)
      (error (object-name f) "does not support units yet")))
  (apply f args))

(define ((convert/<>= f) . args)
  ;; for now just makes sure all units are the same. will build conversion tables later
  (define unit #f)
  (define all-same?
    (for/and ([a args])
      (or (number? a)
          (let ([u (in:number-unit a)])
            (if (not unit)
                (and (set! unit u) #t)
                (equal? unit u))))))
  (and all-same? (apply f (strip-units args))))

;; listof (U number number+unit) -> listof number
;; Extract the numbers from any number+units
(define (strip-units ns)
  (for/list ([n ns])
    (if (number? n)
        n
        (in:number-value n))))
(module+ test (check-equal? (strip-units (list (in:number 1 'y) 2 (in:number 3 'x) 4)) '(1 2 3 4)))

(define/func unit:+ (convert/+- +))
(define/func unit:- (convert/+- -))
(define/func unit:* (convert/*/ *))
(define/func unit:/ (convert/*/ /))
(define/func unit:> (convert/<>= >))
(define/func unit:>= (convert/<>= >=))
(define/func unit:< (convert/<>= <))
(define/func unit:<= (convert/<>= <=))
(define/func unit:= (convert/<>= =))
(define/func (in:in-range n start end)
  (and (unit:<= start n)
       (unit:< n end)))

(module+ test
  (check-equal? (unit:+ (in:number 1 'x))
                (in:number 1 'x))
  (check-equal? (unit:- (in:number 1 'x))
                (in:number -1 'x)))

(define/func is equal?)

;;; messages
(begin-for-syntax
  ;; names of defined message -> the args
  (define messages (make-free-id-table))
  ;; names of defined messages -> args with swapped out properties
  ;; used for reusing names in different lexical scopes
  (define in:messages (make-free-id-table))
  ;; names of messages -> args and keywords with swapped out properites
  (define message-args (make-free-id-table))
  ;; a useful propery
  (define message-name-stx-prop (datum->syntax #f 'prop)))
#|
There are three ways to define a message
1. define a completely new message
   (basic define)
2. define a message that is exactly the same as another, with an extra tag
   (define from message)
3. define a message that transforms its arguments to send a different message, plus its tag
   (define from function)
|#
(define current-send-message (make-parameter send-message!))
(define-syntax (define-message stx)
  (with-syntax ()
    (define-splicing-syntax-class args
      (pattern (~seq a:arg ...)
               #:with cleannames
               #'(a.name ...)
               #:with names
               (datum->syntax message-name-stx-prop (syntax->datum #'(a.name ...)))
               #:with flattened
               (datum->syntax message-name-stx-prop (flatten (syntax->datum #'(a ...))))))
    (define-splicing-syntax-class arg
      (pattern x:id #:with name #'x)
      (pattern (~seq k:keyword x:id) #:with name #'x))
    (syntax-parse stx
      ;; basic define
      [(define-message name:id (args:args))
       (quasisyntax/loc stx
         (begin
           (begin-for-syntax
             (define key (syntax-local-introduce #'name))
             (dict-set! messages key (syntax->list (syntax-local-introduce #'args.cleannames)))
             (dict-set! in:messages key (syntax->list (syntax-local-introduce #'args.names)))
             (dict-set! message-args key (syntax->list (syntax-local-introduce #'args.flattened))))
           (provide name)
           (define/func (name #,@#'args.flattened #:-keys [keys null])
             ((current-send-message)
              (message `(name ,@keys)
                       (list #,@#'args.names)
                       (and (current-environment)
                            (current-time)))))))]
      ;; define from other message
      [(define-message name:id other:id)
       (define key (syntax-local-introduce #'other))
       (define arg-names (dict-ref in:messages key))
       (define args (dict-ref message-args key))
       (with-syntax ([(arg ...) args]
                     [(arg-name ...) arg-names])
         (quasisyntax/loc stx
           (begin
             (begin-for-syntax
               (dict-set! messages (syntax-local-introduce #'name) (dict-ref messages #'#,key))
               (dict-set! in:messages (syntax-local-introduce #'name) (list #'arg-name ...))
               (dict-set! message-args (syntax-local-introduce #'name) (list #'arg ...)))
             (provide name)
             (define/func (name #,@args #:-keys [keys null])
               (other #,@args #:-keys `(name ,@keys))))))]
      ;; define from function
      [(define-message name:id (args:args) (super:id call ...))
       (with-syntax ([(old ...) #'args.cleannames]
                     [(new ...) #'args.names])
         (quasisyntax/loc stx
           (begin
             (begin-for-syntax
               (dict-set! message-args #'name (syntax->list (syntax-local-introduce #'args.flattened)))
               (dict-set! messages #'name (dict-ref messages (syntax-local-introduce #'super)))
               (dict-set! in:messages #'name (dict-ref in:messages (syntax-local-introduce #'super))))
             (provide name)
             (define/func (name #,@#'args.flattened #:-keys [keys null])
               (let-syntax ([old (make-rename-transformer #'new)] ...)
                 (super call ... #:-keys `(name ,@keys)))))))])))


;;; testing
(define-syntax (here-be-tests stx)
  (syntax-parse stx
    [(_ body-start body ...)
     (with-syntax ([mod (syntax-source stx)])
       (replace-context
        stx
        (syntax/loc stx
          (module* test racket
            (local-require pop-pl/tests/harness
                           pop-pl/constants)
            (prescription-test
             mod
             body-start
             body ...)))))]))
