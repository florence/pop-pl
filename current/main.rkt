#lang racket
(require (for/syntax syntax-parse free-id-table))
;;; global state
(define current-handlers (hash))
(define next-handlers (make-hash))
(define current-log null)
(define next-log null)
(define time 0)


;;; evaluator
(define (eval msg)
  (for ([(_ h!) (in-hash current-handlers)])
    (h! msg current-log))
  (set! current-handlers (hash->immutable-hash next-handlers))
  (set! current-log (append next-log current-log))
  (set! next-log null)
  next-log)

(define (hash->immutable-hash hash)
  (for/hash ([(k h) (in-hash hash)]) (values k h)))

(define (send-message! m)
  (set! next-log (cons m next-log)))

(define (add-handler! n f)
  (hash-set! next-handlers n f))
(define (remove-handler! n)
  (hash-remove! next n))

(define (current-time) time)


;;; handlers
(define-syntax-parameter current-message (make-rename-transformer #'void))
(define-syntax-parameter current-log (make-rename-transformer #'void))

(define-syntax (make-handler stx)
  (syntax-parse stx
    [(make-handler body ...)
     #`(lambda (event log)
         (syntax-parameterize ([current-message (make-rename-transformer #'event)]
                               [current-log (make-rename-transformer #'log)])
           body ...))]))
(define-syntax (add-handler stx)
  (syntax-parse stx
    [(add-handler n id)
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
     (when (not (dict-hash-key messages #'id))
       (raise-syntax-error 'messages "the name of a message needs to go here" #'id))
     (define names (map syntax-local-introduce (message-names (dict-ref messages (syntax-local-introduce #'id)))))
     (with-syntax ([(name ...) names]
                   [(value ...)
                    (for/list ([n (in-integer (length names))])
                      #`(vector-ref (message-values current-message) #,n))])
       #'(when (member 'id (message-tags current-message))
           (let ([name value] ...)
             body ...)))]))
(define-syntax (whenever stx)
  (define-syntax-class query-name
    (pattern #:times)
    (pattern #:since-last)
    (pattern #:apart))
  (define-syntax-class query
    (pattern (~seq t:expr (~seq k:query-name e:expr) ...)
             #:with query
             (let ([asc 
                    (map (match-lambda [(list k e) (list (syntax-e k) e)])
                         (map syntax->list (syntax->list #'((k e) ...))))])'
               (cond [(empty? asc) #'t]
                     [else
                      (define since-last (make-since-last filter (assoc '#:since-last asc)))
                      (define apart (make-apart-filter (assoc '#:apart asc)))
                      (define times? (make-times-filter (assoc '#:times asc)))
                      #'(let* ([log (since-last current-log)]
                               [matching (get-matching-messages (lambda (m) t))]
                               [acceptable (apart matching)])
                          (times? acceptable))]))))
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

;;; messages
(begin-for-syntax
  (define messages (make-free-id-table))
  (struct message (names)))
(struct message (tags values time))
(define-syntax (define-message stx)
  (define-syntax-class arg
    (pattern x:id #:with name #'x)
    (pattern (~seq k:kw x:id) #:with name #'x))
  [(define-message name:id arg:arg ...)
   (dict-set! messages (syntax-local-introduce #'name) (message (syntax->list (syntax-local-introduce #'(arg.name ...)))))
   #'(define (name arg ...)
       (send-message! (message '(name) (vector arg.name ...) (current-time))))])
