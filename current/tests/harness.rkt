#lang racket
(provide prescription-test)
(require (for-syntax syntax/parse))
(require pop-pl/current/private/shared rackunit)

(define TIME-ADVANCE 60)
(define current-eval (make-parameter (lambda _ (error 'test "no eval set"))))

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
    #:datum-literals (=> message advance start)
    (pattern (=> start 
                 m:pat ...)
             #:with parsed
             #'(check-match (advance (in:number 1 'hour))
                            (list-no-order m.pat ...)))
    (pattern (=> (advance t)
                 m:pat ...)
             #:with parsed
             #'(check-match (advance t)
                            (list-no-order m.pat ...)))
    (pattern (=> (advance t u:id)
                 m:pat ...)
             #:with parsed
             #'(check-match (advance (in:number t 'u))
                            (list-no-order m.pat ...)))
    (pattern (=> (message mes:id e ...)
                 m:pat ...)
             #:with parsed
             #'(check-match (send (message '(mes) (list e ...) #f))
                            (list-no-order m.pat ...))))
  (define-syntax-class pat
    (pattern (m:id e:expr ...)
             #:with pat
             #'(message (list-no-order m _ ___) (list e ...) _)))
  (syntax-parse stx
    [(_ path t:test-clause ...)
     #'(parameterize ([current-eval (dynamic-require 'path 'eval)])
         (test-case
          'path
          t.parsed ...))]))
