#lang racket
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
 whenever whenever-new initially after q
 (rename-out
  [unit:+ +]
  [unit:- -]
  [unit:* *]
  [unit:/ /]
  [unit:> >]
  [unit:>= >=]
  [unit:< <]
  [unit:<= <=]
  [unit:= =])
 is
 ;; internal
 make-handler define-message add-handler
 -number)

(require racket/stxparam "private/shared.rkt")
(require (for-syntax syntax/parse syntax/id-table racket/dict racket/match racket/syntax racket/list
                     unstable/sequence))
(require (for-meta 2 racket/base syntax/parse))
(module+ test (require rackunit))
;;; global state
(define current-handlers (hash))
(define next-handlers (make-hash))
(define cur-log null)
(define next-log null)
(define time 0)

;;; things that need to go first
(begin-for-syntax
  (define-syntax-class number+unit
    #:literals (-number)
    (pattern x:number)
    (pattern (-number x:number y:id))))

;;; evaluator
(define-syntax (in:module-begin stx)
  (syntax-parse stx
    [(_ body ...)
     #'(#%module-begin
        (define -eval eval)
        (provide (rename-out [-eval eval]))
        body ...)]))
(define (eval msg)
  (for ([(_ h!) (in-hash current-handlers)])
    (h! msg cur-log))
  (set! current-handlers (hash->immutable-hash next-handlers))
  (set! cur-log (append next-log cur-log))
  (set! next-log null)
  next-log)

(define (hash->immutable-hash hash)
  (for/hash ([(k h) (in-hash hash)]) (values k h)))

(define (send-message! m)
  (set! next-log (cons m next-log)))

(define (add-handler! n f)
  (hash-set! next-handlers n f))
(define (remove-handler! n)
  (hash-remove! next-handlers n))

(define (current-time) time)


;;; handlers
(define-syntax-parameter current-message (make-rename-transformer #'void))
(define-syntax-parameter current-log (make-rename-transformer #'void))

(define-syntax (initially stx)
  (syntax-parse stx
    [(_ body ...)
     (with-syntax ([id (generate-temporary)])
       #'(let ()
           (define id (make-handler body ...))
           (add-handler id)))]))

(define-syntax (make-handler stx)
  (syntax-parse stx
    [(make-handler body ...)
     #`(lambda (event log)
         (syntax-parameterize ([current-message (make-rename-transformer #'event)]
                               [current-log (make-rename-transformer #'log)])
           body ...))]))
(define-syntax (add-handler stx)
  (syntax-parse stx
    [(add-handler id)
     #'(add-handler! 'id (lambda x (apply id x)))]))

;;; whenevers
(define-syntax (whenever-new stx)
  (syntax-parse stx
    [(whenever-new (id:id e:expr)
                   body ...)
     #'(whenever-new id
                     (whenever e body ...))]
    [(whenever-new id:id
                   body:expr ...)
     (when (not (dict-has-key? messages #'id))
       (raise-syntax-error 'messages "the name of a message needs to go here" #'id))
     (define names (map syntax-local-introduce (dict-ref messages (syntax-local-introduce #'id))))
     (with-syntax ([(name ...) names]
                   [(value ...)
                    (for/list ([n (in-range (length names))])
                      #`(vector-ref (message-values current-message) #,n))])
       #'(when (member 'id (message-tags current-message))
           (let ([name value] ...)
             body ...)))]))
(define-syntax (whenever stx)
  (define-syntax-class query-name
    (pattern #:times)
    (pattern #:since-last)
    (pattern #:apart))
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
                                    [get-matching (make-get-matching #'t)])
                        #'(let* ([log (since-last current-log)]
                                 [matching (get-matching log)]
                                 [acceptable (apart matching)])
                            (times? acceptable)))]))))
  (syntax-parse stx
    [(whenever q:query body ...)
     #'(when q.query
         body ...)]))


(define-for-syntax ((maybe-filter l) maybe-stx)
  (if (not maybe-stx)
      #'values
      (l (second maybe-stx))))
(define-for-syntax make-since-last
  (maybe-filter
   (lambda (stx)
     (syntax-parse stx
       [(name args ...)
        #'(lambda (log)
            (for/list ([l log]
                       #:final
                       (match l
                         [(message (? (lambda (l) (member l 'name))) 
                                   (list args ... _ ___)
                                   _)
                          #t]))
              l))]))))
(define-for-syntax make-apart-filter
  (maybe-filter
   (lambda (stx)
     (syntax-parse stx
       [n:number+unit
        #'(lambda (log)
            (define-values (res _)
              (for/fold ([res null] [time 0]) ([l log])
                (if (> (message-time l) time)
                    (values (cons l res) (message-time l))
                    (values res time))))
            (reverse res))]))))
(struct failure ())
(define-for-syntax (make-get-matching stx)
  (syntax-parse stx
    [e:expr 
     (with-syntax* ([log (generate-temporary)]
                    [pats 
                     (for/list ([(k v) (in-dict messages)])
                       (with-syntax ([msg (syntax-local-introduce k)])
                         #'[msg
                            (match (first log)
                               [(message (? (lambda (l) (member l 'msg)) _)
                                         (list* x _)
                                         _)
                                x]
                               [_ (raise (failure))])]))])
       #'(lambda (log)
           (let pats
               (with-handlers ([failure? #f])
                 e))))]))

(define-for-syntax make-times-filter
  (maybe-filter
   (lambda (stx)
     (syntax-parse stx
       [n:number
        #'(lambda (l) (= n (length l)))]))))

;;; things that use time
(define-syntax (after stx)
  (syntax-parse stx
    [(after t body ...)
     #'(let* ([n (gensym)]
              [start (current-time)]
              [h (make-handler
                  (when (after? t start)
                    body ...
                    (remove-handler! 'n)))])
         (add-handler! 'n h))]))

;; time time -> boolean
(define (after? diff start)
  (> (+ (time->stamp diff) (time->stamp start)) 
     (time->stamp (current-time))))

(define (time->stamp t)
  (match t
    [(in:number n (or 'seconds 'second)) n]
    [(in:number n (or 'minutes 'minute)) (* n 60)]
    [(in:number n (or 'hours 'hour))
     (* n
        60 
        (time->stamp (in:number 1 'minutes)))]
    [(in:number n (or 'days 'day))
     (* n
        24
        (time->stamp (in:number 1 'hour)))]
    [(? number? n) n]
    [_ (error 'time "expected time, given ~s" t)]))
(module+ test
  (check-equal? (time->stamp (in:number 3 'days))
                ;; google says...
                259200)
  (check-equal? (time->stamp (in:number 2 'hours))
                7200))

(define-syntax (q stx)
  (syntax-parse stx
    [(Q n:number+unit name:id exprs ...)
     (with-syntax ([(args ...)
                    (for/list ([e (in-syntax #'(exprs ...))]
                               #:unless (keyword? (syntax-e e)))
                      e)])
       #'(when
             (after? n
                     (match current-log
                       [(message (? (lambda (l) (member l 'name)) _)
                                 (list args ... _ ___)
                                 t)
                        t]))
           (name exprs ...)))]))
;;; numbers

(define-syntax (-number stx)
  (syntax-parse stx
    [(_ n:number unit:id)
     #'(in:number n 'unit)]))

(define ((convert/+- f) . args)
  (define unit #f)
  (for ([a args])
    (when (in:number? a)
      (define u (in:number-unit a))
      (if (not unit) 
          (set! unit a)
          (unless (equal? unit u)
            (error (object-name f) "all numbers must have the same unit")))))
  (define v (apply + (strip-units args)))
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
(module+ test (check-equal? (strip-units (list (in:number 4 'y) 2 (in:number 3 'x) 4)) '(1 2 3 4)))
(define (strip-units ns)
  (for/list ([n ns])
    (if (number? n)
        n
        (in:number-value n))))

(define unit:+ (convert/+- +))
(define unit:- (convert/+- -))
(define unit:* (convert/*/ *))
(define unit:/ (convert/*/ /))
(define unit:> (convert/<>= >))
(define unit:>= (convert/<>= >=))
(define unit:< (convert/<>= <))
(define unit:<= (convert/<>= <=))
(define unit:= (convert/<>= =))

(define is equal?)

;;; messages
(begin-for-syntax
  (define messages (make-free-id-table))
  (define message-args (make-free-id-table)))
(define-syntax (define-message stx)
  (define-splicing-syntax-class args
    (pattern (~seq a:arg ...)
             #:with names #'(a.name ...)
             #:with flattened (flatten
                               (map syntax->list
                                    (syntax->list #'(a ...))))))
  (define-splicing-syntax-class arg
    (pattern x:id #:with name #'x)
    (pattern (~seq k:keyword x:id) #:with name #'x))
  (syntax-parse stx
    [(define-message name:id (args:args))
     (define key (syntax-local-introduce #'name))
     (dict-set! messages key (syntax->list (syntax-local-introduce #'args.names)))
     (dict-set! message-args key (syntax->list (syntax-local-introduce #'args.flattened)))
     #`(define (name #,@#'args.flattened #:-keys [keys null])
         (send-message! (message `(name ,@keys) (vector #,@#'args.names) (current-time))))]
    [(define-message name:id other:id)
     (define arg-names (dict-ref messages (syntax-local-introduce #'other)))
     (define args (dict-ref message-args (syntax-local-introduce #'other)))
     (dict-set! messages (syntax-local-introduce #'name) arg-names)
     (dict-set! message-args (syntax-local-introduce #'name) args)
     #`(define (name #,@args #:-keys [keys null])
         (other #,@args #:-keys `(name ,@keys)))]
    [(define-message name:id (args:args) (super:id call ...))
     (dict-set! message-args #'name (syntax->list (syntax-local-introduce #'args.flattened)))
     (dict-set! messages #'name (dict-ref messages (syntax-local-introduce #'super)))
     #`(define (name #,@#'args.flattened #:-keys [keys null])
         (super call ... #:-keys `(name @,keys)))]))
