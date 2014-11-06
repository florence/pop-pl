#lang racket
(provide prescription-test n advance send current-eval)
(require (for-syntax syntax/parse))
(require pop-pl/current/private/shared rackunit rackunit/text-ui unstable/match)

(define TIME-ADVANCE 60)
(define current-eval (make-parameter (lambda _ (error 'test "no eval set"))))
(define current-reset (make-parameter (lambda _ (error 'test "no reset set"))))

(define (advance time)
  (define t (time->stamp time))
  (reverse
   (for/fold ([msg null]) ([_ (in-range 0 t TIME-ADVANCE)])
     (append msg (send (message '(time) (list TIME-ADVANCE) #f))))))

(define (send msg)
  (filter
   (lambda (m)
     (not (and (equal? (list->set (message-tags m))
                       (list->set (message-tags msg)))
               (equal? (message-values m)
                       (message-values msg)))))
   ((current-eval) msg)))

(define-syntax-rule (n num u)
  (in:number num 'u))

(define-syntax (prescription-test stx)
  (define-syntax-class id
    (pattern x
             #:when (identifier? #'x)
             #:with down
             (datum->syntax #'x (string->symbol (string-downcase (symbol->string (syntax-e #'x))))
                            #'x
                            #'x)))
  (define-syntax-class expr
    (pattern e:id
             #:with down #'e.down)
    (pattern e #:with down #'e))
  (define-syntax-class test-clause 
    #:datum-literals (=> advance start wait)
    (pattern (~and
              stx
              (~or
               (start 
                => m:pat ...)
               (=> start 
                   m:pat ...)))
             #:with parsed
             (syntax/loc #'stx
               (check-match (advance (in:number 1 'hour))
                            (list-no-order m.pat ...))))
    (pattern (~and
              stx
              (~or
               ((advance t)
                => m:pat ...)
               (=> (advance t)
                   m:pat ...)
               ((wait t)
                => m:pat ...)
               (=> (wait t)
                   m:pat ...)))
             #:with parsed
             (syntax/loc #'stx
               (check-match (advance t)
                            (list-no-order m.pat ...))))
    (pattern (~and
              stx
              (~or
               ((advance t u:id)
                => m:pat ...)
               (=> (advance t u:id)
                   m:pat ...)))
             #:with parsed
             (syntax/loc #'stx
               (check-match (advance (in:number t 'u.down))
                            (list-no-order m.pat ...))))
    (pattern (~and
              stx
              (~or
               ((mes:id e:expr ...)
                => m:pat ...)
               (=> (mes:id e:expr ...)
                   m:pat ...)))
             #:with parsed
             (syntax/loc #'stx
               (check-match (send (message '(mes.down) (list e.down ...) #f))
                            (list-no-order m.pat ...)))))
  (define-syntax-class pat
    (pattern (m:id e:expr ...)
             #:with pat
             #'(message (list-no-order 'm.down _ ___) (list (eq e.down) ...) _))
    #;
    (pattern ((m:id ...) e:expr ...)
             #:with pat
             #'(message (list-no-order 'm ... _ ___) (list e ...) _)))
  (syntax-parse stx
    [(_ path t:test-clause ...)
     #`(let ()
         (local-require (only-in path [eval eval] [reset! reset!]))
         (parameterize ([current-eval eval]
                        [current-reset reset!])
           (void
            (run-tests
             (test-suite
              (~a 'path)
              #:after (current-reset)
              (test-case
               (~a 'path)
               t.parsed ...))))))]))

(define-match-expander eq
  (lambda (stx)
    (syntax-parse stx
      [(eq _u) #:when (eq? (syntax-e #'_u) '_)
       #'_]
      [(eq e:expr)
       #'(? (lambda (v) (equal? v e)))])))
