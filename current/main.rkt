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
 (rename-out [in:module-begin #%module-begin])
 ;; forward facing
 whenever whenever-new initially after q whenever-cond
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
 (rename-out [require -require]))

(require (for-syntax racket/base
                     racket/dict
                     racket/list
                     racket/match
                     racket/syntax
                     syntax/id-table
                     syntax/parse
                     unstable/sequence
                     racket/format)
         (for-meta 2 racket/base
                     syntax/parse)
         racket/bool
         racket/list
         racket/match
         racket/stxparam
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

(define (current-time) (ctime))
(define ctime (make-parameter 0))
(define next-handlers (make-parameter #f))
(define-for-syntax -time (generate-temporary '-))
(define-for-syntax -next-handlers (generate-temporary '-))
(define-for-syntax -last-message-time-cache (generate-temporary '-))
(define-for-syntax -send-message! (generate-temporary '-))
(define-for-syntax -add-matcher! (generate-temporary '-))
(define-for-syntax -message-match-lists (generate-temporary '-))

(define current-next-log (make-parameter #f))
(define current-last-message-time-cache (make-parameter #f))
(define current-message-matchers (make-parameter #f))
;;; evaluator
(define-syntax (in:module-begin stx)
  (syntax-parse stx
    [(_ body ...)
     (with-syntax ([-time (syntax-local-introduce -time)]
                   [-next-handlers (syntax-local-introduce -next-handlers)]
                   [-last-message-time-cache (syntax-local-introduce -last-message-time-cache)]
                   [send-message! (syntax-local-introduce -send-message!)]
                   [-add-matcher! (syntax-local-introduce -add-matcher!)]
                   [-message-match-lists (syntax-local-introduce -message-match-lists)])
       #`(#%module-begin
          (provide (rename-out [-eval eval] [-reset! reset!]))
          #,(datum->syntax stx '(-require pop-pl/current/constants))
          ;; global state
          (define -last-message-time-cache (make-hash))
          (define current-handlers (hash))
          (define -next-handlers (make-hash))
          (define cur-log null)
          (define next-log (box null))
          (define -time 0)
          (define message-matchers null)
          (define -message-match-lists (make-hash))
          
          (define (-reset!)
            (set! -last-message-time-cache (make-hash))
            (set! current-handlers (hash))
            (set! -next-handlers (hash-copy initial-handlers))
            (set! cur-log null)
            (set! next-log (box null))
            (set! -time 0)
            (set! -message-match-lists (hash-copy initial-match-lists)))

          (define (-add-matcher! f)
            (set! message-matchers (cons f message-matchers)))

          (define (-eval m)
            (parameterize ([current-next-log next-log]
                           [current-last-message-time-cache -last-message-time-cache])
              (define msg (msg-fill/update-time! m))
              (for ([n (message-tags msg)])
                (hash-set! (current-last-message-time-cache) n -time))
              (for ([! message-matchers])
                (! msg))
              (for ([(_ h!) (in-hash current-handlers)])
                (h! msg cur-log))
              (set! current-handlers (hash->immutable-hash -next-handlers))
              (define res (unbox next-log))
              (set! cur-log (append res (list msg) cur-log))
              (set-box! next-log null)
              (append res (list msg))))
          (define (msg-fill/update-time! m)
            (match m
              [(message '(time) (list n) #f)
               (set! -time (+ n -time))
               m]
              [(message types values #f)
               (message types values -time)]
              [_ m]))
          (define (send-message! m)
            (define t (message-time m))
            (for ([n (message-tags m)])
              (hash-set! (current-last-message-time-cache) n t))
            (for ([! message-matchers])
              (! m))
            (set-box! (current-next-log)
                      (cons m (unbox (current-next-log)))))
          
          

          body ...
          (define initial-handlers (hash->immutable-hash -next-handlers))
          (define initial-match-lists (hash->immutable-hash -message-match-lists))))]))

(define (hash->immutable-hash hash)
  (for/hash ([(k h) (in-hash hash)]) (values k h)))

(define (add-handler! n f)
  (hash-set! (next-handlers) n f))
(define (remove-handler! n)
  (hash-remove! (next-handlers) n))

;;; requiring message protocol
(define-syntax (use stx)
  (syntax-parse stx
    [(use name:id)
     (define file (~a (syntax-e #'name) ".pop"))
     (datum->syntax stx `(,#'require ,file))]))
;;; handlers
(define-syntax-parameter current-message (make-rename-transformer #'void))
(define-syntax-parameter current-log (make-rename-transformer #'void))

(define-syntax (initially stx)
  (syntax-parse stx
    [(_ body ...)
     (with-syntax ([id (generate-temporary)])
       #'(define-handler id body ... (remove-handler! 'id)))]))

(define-syntax (define-handler stx)
  (syntax-parse stx
    [(_ n:id body ...)
     (with-syntax ([-next-handlers (syntax-local-introduce -next-handlers)])
       (syntax/loc stx
         (begin
           (define/func n (make-handler body ...))
           (parameterize ([next-handlers -next-handlers])
             (add-handler n)))))]))

(define-syntax (make-handler stx)
  (syntax-parse stx
    [(make-handler body ...)
     (with-syntax ([-time (syntax-local-introduce -time)]
                   [-next-handlers (syntax-local-introduce -next-handlers)])
       #`(lambda (event log)
           (syntax-parameterize ([current-message (make-rename-transformer #'event)]
                                 [current-log (make-rename-transformer #'log)])
             (parameterize ([ctime -time]
                            [next-handlers -next-handlers])
               body) ...)))]))
(define-syntax (add-handler stx)
  (syntax-parse stx
    [(add-handler id)
     #'(add-handler! 'id (lambda (a b) (id a b)))]))

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
     (define names (map syntax-local-introduce (dict-ref messages (syntax-local-introduce #'id))))
     (with-syntax ([(name ...)
                    (for/list ([n names])
                      (datum->syntax n (syntax->datum n) #'id #'id))]
                   [(value ...)
                    (for/list ([n (in-range (length names))])
                      #`(list-ref (message-values current-message) #,n))])
       #`(begin
           (and #f (id))
           (when (member 'id (message-tags current-message))
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
                      (with-syntax ([since-last (make-since-last (assoc '#:since-last asc))]
                                    [apart (make-apart-filter (assoc '#:apart asc))]
                                    [times? (make-times-filter (assoc '#:times asc))]
                                    [get-matching (make-get-matching 
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
                        (syntax/loc 
                            #'t
                          (n (let* ([matching (get-matching)]
                                    [since (since-last matching)]
                                    [acceptable (apart since)])
                               (times? acceptable)))))]))))
  (syntax-parse stx
    [(whenever q:query body ...)
     (syntax/loc stx (when q.query body ...))]))




(define-for-syntax ((maybe-filter l) maybe-stx)
  (if (not maybe-stx)
      #'values
      (l (second maybe-stx))))
(define-for-syntax make-since-last
  (maybe-filter
   (lambda (stx)
     (syntax-parse stx
       [(name args ...)
        (syntax/loc stx
          (lambda (log)
            (for/list ([l log]
                       #:final
                       (match l
                         [(message (? (lambda (l) (member 'name l))) 
                                   (list args ... _ ___)
                                   _)
                          #t]
                         [_ #f]))
              l)))]))))
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

(define-for-syntax (make-get-matching stx)
  (syntax-parse stx
    [e:expr
     (with-syntax* ([msg (generate-temporary)]
                    [pats 
                     (for/list ([(k v) (in-dict messages)])
                       (with-syntax ([name (syntax-local-introduce k)])
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
                    [message-match-lists (syntax-local-introduce -message-match-lists)]
                    [add-matcher! (syntax-local-introduce -add-matcher!)])
       
       (with-syntax* ([key (generate-temporary)])
         (syntax-local-lift-expression
          #'(begin
              (add-matcher!
               (lambda (msg) 
                 (with-handlers ([failure? void])
                   (if (not (let-syntax pats e))
                       (hash-set! message-match-lists 'key null)
                       (hash-set! message-match-lists
                                  'key
                                  (cons msg (hash-ref message-match-lists 'key)))))))
              (hash-set! message-match-lists 'key null)))
         (syntax/loc stx
           (lambda () (hash-ref message-match-lists 'key)))))]))

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
(define (after? diff start)
  (< (+ (time->stamp diff) (time->stamp start)) 
     (time->stamp (current-time))))
(define-syntax (q stx)
  (syntax-parse stx
    [(q n:number+unit name:id exprs ...)
     (with-syntax ([(args ...)
                    (for/list ([e (in-syntax #'(exprs ...))]
                               #:unless (keyword? (syntax-e e)))
                      e)]
                   [-last-message-time-cache (syntax-local-introduce -last-message-time-cache)])
       #'(when
             (let ([m (hash-ref -last-message-time-cache 'name #f)])
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
(module+ test (check-equal? (strip-units (list (in:number 1 'y) 2 (in:number 3 'x) 4)) '(1 2 3 4)))
(define (strip-units ns)
  (for/list ([n ns])
    (if (number? n)
        n
        (in:number-value n))))
(module+ test
  (check-equal? (strip-units (list (in:number 1 'x)))
                '(1)))

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
  (define messages (make-free-id-table))
  (define in:messages (make-free-id-table))
  (define message-args (make-free-id-table))
  (define message-name-stx-prop (datum->syntax #f 'prop)))
(define-syntax (define-message stx)
  (with-syntax ([send-message! (syntax-local-introduce -send-message!)])
    (define-splicing-syntax-class args
      (pattern (~seq a:arg ...)
               #:with cleannames #'(a.name ...)
               #:with names (datum->syntax message-name-stx-prop (syntax->datum #'(a.name ...)))
               #:with flattened (datum->syntax message-name-stx-prop (flatten (syntax->datum #'(a ...))))))
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
             (send-message! (message `(name ,@keys) (list #,@#'args.names) (current-time))))))]
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

