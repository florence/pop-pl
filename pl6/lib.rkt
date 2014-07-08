#lang s-exp "runtime.rkt"
(require racket/function)
(provide current-time before? after? eq)

(define/state current-time number?)
(define/handler (extern-current-time _ s evt)
  (match evt
    [`(update (current-time ,t))
     (update current-time t)]
    [`(inc time by ,t)
     (update current-time
             (+ (time->seconds (current-time s))
                (time->seconds t)))]
    [_ (void)]))

(define (before? state time)
  ;filler
  #t)
(define (after? state time)
  ;filler
  #t)

(define (time->seconds t)
  (match t
    [(? number? n) (number/unit n 'seconds)]
    [(number/unit n (or 'seconds 'second)) t]
    [(number/unit n (or 'minutes 'minute)) (* n 60)]
    [(number/unit n (or 'hours 'hour))
     (* n
        60 
        (time->seconds (number/unit 1 'minutes)))]
    [(number/unit n (or 'days 'day))
     (* n
        24
        (time->seconds (number/unit 1 'hour)))]
    [_ (error 'time "expected time, given ~s" t)]))
#;
(module+ test
  (check-equal? (time->seconds (number/unit 3 'days))
                ;; google says...
                259200)
  (check-equal? (time->seconds (number/unit 2 'hours))
               7200))

(define-match-expander eq 
  (lambda (stx)
    (syntax-parse stx
      [(_ v)
       (quasisyntax/loc stx (app (curry equal? v) #t))])))

#;
(module+ test
  (check-match 2
               (eq (add1 1))))

