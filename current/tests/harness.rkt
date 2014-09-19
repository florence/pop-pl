#lang racket
(provide prescription-test)
(require (for-syntax syntax/parse))
(require pop-pl/current/private/shared rackunit rackunit/text-ui)

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
   (negate (curry equal? msg))
   ((current-eval) msg)))

(define-syntax (prescription-test stx)
  (define-syntax-class test-clause 
    #:datum-literals (=> advance start)
    (pattern (~and
              stx
              (=> start 
                  m:pat ...))
             #:with parsed
             (syntax/loc #'stx
               (check-match (advance (in:number 1 'hour))
                            (list-no-order m.pat ...))))
    (pattern (~and
              stx
              (=> (advance t)
                  m:pat ...))
             #:with parsed
             (syntax/loc #'stx
               (check-match (advance t)
                            (list-no-order m.pat ...))))
    (pattern (~and
              stx
              (=> (advance t u:id)
                  m:pat ...))
             #:with parsed
             (syntax/loc #'stx
               (check-match (advance (in:number t 'u))
                            (list-no-order m.pat ...))))
    (pattern (~and
              stx
              (=> (mes:id e ...)
                  m:pat ...))
             #:with parsed
             (syntax/loc #'stx
               (check-match (send (message '(mes) (list e ...) #f))
                            (list-no-order m.pat ...)))))
  (define-syntax-class pat
    (pattern (m:id e:expr ...)
             #:with pat
             #'(message (list-no-order m _ ___) (list (eq e) ...) _))
    #;
    (pattern ((m:id ...) e:expr ...)
             #:with pat
             #'(message (list-no-order 'm ... _ ___) (list e ...) _)))
  (syntax-parse stx
    [(_ path t:test-clause ...)
     #`(parameterize ([current-eval (dynamic-require 'path 'eval)]
                      [current-reset (dynamic-require 'path 'reset!)])
         (void
          (run-tests
           (test-suite
            (~a 'path)
            #:after (current-reset)
            (test-case
             'path
             t.parsed ...)))))]))

(define-match-expander eq
  (lambda (stx)
    (syntax-parse stx
      #:datum-literals (_)
      [(eq _) #'_]
      [(eq e:expr)
       #'(? (lambda (v) (equal? v e)))])))
