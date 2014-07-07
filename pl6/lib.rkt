#lang s-exp "runtime.rkt"
(require racket/function)
(provide current-time before? after? eq)

(define/state current-time number?)
(define/handler (extern-current-time _ _ evt)
  (match evt
    [`(update (current-time ,t))
     (update current-time t)]
    [_ (void)]))

(define (before? state)
  ;filler
  #t)
(define (after? state)
  ;filler
  #t)

(define-match-expander eq 
  (lambda (stx)
    (syntax-parse stx
      [(_ v)
       (quasisyntax/loc stx (app (curry equal? v) #t))])))

#;
(module+ test
  (check-match 2
               (eq (add1 1))))

