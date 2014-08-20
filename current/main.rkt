#lang racket

(provide
 ;; from racket
 #%module-begin #%top #%app #%datum 
 define
 ;; forward facing
 whenever initially
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

(require racket/stxparam)
(require (for-syntax syntax/parse syntax/id-table racket/dict racket/match racket/syntax))
(module+ test (require rackunit))
;;; global state
(define current-handlers (hash))
(define next-handlers (make-hash))
(define cur-log null)
(define next-log null)
(define time 0)


;;; evaluator
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
                         (map syntax->list (syntax->list #'((k e) ...))))])'
               (cond [(empty? asc) #'t]
                     [else
                      (with-syntax ([since-last (make-since-last (assoc '#:since-last asc))]
                                    [apart (make-apart-filter (assoc '#:apart asc))]
                                    [times? (make-times-filter (assoc '#:times asc))])
                        #'(let* ([log (since-last current-log)]
                                 [matching (get-matching-messages (lambda (m) t))]
                                 [acceptable (apart matching)])
                            (times? acceptable)))]))))
  (syntax-parse stx
    [(whenever q:query body ...)
     #'(when q.query
         body ...)]))

;;; afters
(define-syntax (after stx)
  (syntax-parse stx
    [(after t body ...)
     #'(let* ([n (gensym)]
              [start (current-time)]
              [h (make-handler
                  (when (after? t start)
                    body ...
                    (remove-handler! n)))])
         (add-handler! n h))]))

;;; numbers
(begin-for-syntax
  (define-syntax-class number+unit
    #:literals (-number)
    (pattern x:number)
    (pattern (-number x:number y:id))))
(define-syntax (-number stx)
  (syntax-parse stx
    [(_ n:number unit:id)
     #'(number n 'unit)]))
(struct in:number (value unit) #:transparent)

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
      (error (object-name f) "does not support units yet"))))
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
  (define messages (make-free-id-table)))
(struct message (tags values time))
(define-syntax (define-message stx)
  (define-splicing-syntax-class arg
    (pattern x:id #:with name #'x)
    (pattern (~seq k:keyword x:id) #:with name #'x))
  (syntax-parse stx
    [(define-message name:id arg:arg ...)
     (dict-set! messages (syntax-local-introduce #'name) (syntax->list (syntax-local-introduce #'(arg.name ...))))
     #'(define (name arg ...)
         (send-message! (message '(name) (vector arg.name ...) (current-time))))]))
