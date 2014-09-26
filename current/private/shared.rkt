#lang racket/base
(provide (struct-out in:number)
         (struct-out message)
         time->stamp)
(require racket/match
         racket/format)
(module+ test (require rackunit))
(struct in:number (value unit) #:transparent
        #:methods gen:custom-write
        [(define (write-proc n port mode)
           (fprintf port
                    "%(~a ~a)"
                    (exact->inexact (in:number-value n))
                    (in:number-unit n)))])
(struct message (tags values time) #:transparent)

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

