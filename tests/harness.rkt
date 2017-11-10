#lang racket/base
(provide prescription-test n current-eval -number)
(require (for-syntax racket/base
                     syntax/parse)
         pop-pl/private/shared
         (only-in pop-pl/main -number)
         racket/format
         racket/match
         racket/unit
         rackunit
         rackunit/text-ui
         "../system-sig.rkt"
         "../system-unit.rkt")



(define current-network (make-parameter #f))
(define testing-module (make-parameter #f))

(define-syntax n (make-rename-transformer #'-number))

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
               (check-match (start!)
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
               (check-match (advance! t)
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
               (check-match (advance! (in:number t 'u.down))
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
               (check-match (send-message! (message '(mes.down) (list e.down ...) #f))
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
    [(_ the-unit t:test-clause ...)
     #`(let ()
         (define-values/invoke-unit/infer (export system^)
           (link the-unit system@))
         (parameterize ([testing-module 'path])
           (void
            (run-tests
             (test-suite
              (~a 'path)
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
