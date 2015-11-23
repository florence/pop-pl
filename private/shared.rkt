#lang racket/base
(provide (struct-out in:number)
         (struct-out message)
         time->stamp
         define-for-syntax/testing)
(require racket/match
         racket/format
         racket/list
         (for-syntax syntax/parse racket/syntax racket/base syntax/strip-context))
(module+ test (require rackunit))
(struct in:number (value unit) #:transparent
        #:methods gen:custom-write
        [(define (write-proc n port mode)
           (define v (in:number-value n))
           (fprintf port
                    "~a ~a"
                    (if (integer? v) v (exact->inexact v))
                    (in:number-unit n)))])
(struct message (tags values time) #:transparent
        #:methods gen:custom-write
        [(define (write-proc m port mode)
           (parameterize ([current-output-port port])
             (printf "[~a"
                      (last (message-tags m)))
             (for-each (lambda (a) (printf " ~a" a))
                       (message-values m))
             ;(printf " @ ~a" (message-time m))
             (printf "]")))])

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


(define-for-syntax syntax-mod (generate-temporary))
(define-syntax (define-for-syntax/testing stx)
  (syntax-parse stx
    [(_ (n a ...) b ...)
     (define/with-syntax mod syntax-mod)
     #`(begin
         #,(replace-context
            stx
            #'(module mod racket
                (provide n)
                (define (n a ...) b ...)))
         (require (for-syntax (submod "." mod)))
         (module+ test (require (submod ".." mod))))]))
