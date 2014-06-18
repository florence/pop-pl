#lang typed/racket
(require (for-syntax syntax/parse racket/syntax))
(require typed/rackunit)
(require/typed racket/undefined
  [undefined Undefined])
(provide
 ;; from racket
 ;#%top #%module-begin #%datum
 ;set!
 ;; definitions
 def import
 ;; types
 ;Tuple initial seq extern
 N/A Num
 ;; prescriptions
 define/prescription
 ;; handlers
 initially every whenever
 (rename-out
  [in:after after])
 ;; expressions
 tuple n/a give ?
 prn
 ;and 
 (rename-out [equal? =]
             [in:app #%app]))

;;;;; defs
(define-for-syntax to-be-checked null)
(define-syntax (def stx)
  (syntax-parse stx 
    #:datum-literals (: where)
    [(_ id : type
        (where n T) ...)
     (begin
       (set! to-be-checked (cons #'id to-be-checked))
       #'(def id : (U type Undefined) -> undefined
              (where n T) ...))]
    [(_ id : type -> v
        (where n T) ...)
     #'(begin
         (define-type n T) ...
         (: id : type)
         (define id v))]))
(define-syntax (import stx)
  (syntax-case stx ()
    [(import (name str ...) ...)
     #'(begin
         (: name : (-> Any * Any)) ...
         (define (name . any) (void)) ...)]))
;;;;; prescriptions and handlers
(: handlers : (HashTable Symbol (Pairof Symbol (-> Void))))
(define handlers (make-hash))
(: prescriptions : (Listof (-> Void)))
(define prescriptions null)
(define-syntax (define/prescription stx)
  (syntax-parse stx 
    #:datum-literals (in def)
    [(_ name
        (def a ...) ...
        (in clause do ...) ...)
     (with-syntax ([id (generate-temporary)]
                   [(actual-clause ...)
                    (map (lambda (stx) (if (equal? '* (syntax->datum stx)) #'#f stx))
                         (syntax->list #'(clause ...)))]
                   [(ins ...) (generate-temporaries #'(clause ...))])
       #`(begin
           (def a ...) ...
           (: name : -> Void)
           (define name
             (lambda ()
               (cond
                 [actual-clause 
                  (define v (hash-ref handlers 'id #f))
                  (unless (and v (eq? 'ins (car v)))
                    (lift-handlers-as id ins do ...))] ...)
               (void)))
           #;(cons! prescriptions name)
           ))]))

(define-syntax (lift-handlers-as stx)
  (syntax-case stx ()
    [(_ id part do ...)
     (with-syntax ([(v ...) (generate-temporaries #'(do ...))]
                   [(check ...) to-be-checked])
       #'(let ([v : (-> Void) do] ...)
           (hash-set!
            handlers
            'id
            (cons 'part
                  (lambda ()
                    (when (or (undefined? check) ...)
                        (error 'top "unset variable"))
                    (v) ...
                    (void))))))]))

;;;;; handlers
(define-syntax (initially stx)
  (syntax-case stx ()
    [(_ v ...)
     #'(ann
        (let ()
          (: actual : -> Void)
          (define actual (thunk v ... (set! actual void)))
          (thunk (actual)))
        (-> Void))]))

(define-syntax (in:after stx)
  (syntax-case stx ()
    [(_ t v ...)
     #'(ann
        (let ([start (current-time)])
          (: actual : -> Void)
          (define (actual)
            (when (from start t)
              v ...
              (set! actual void)))
          (lambda () (actual)))
        (-> Void))]))

(define-syntax (whenever stx)
  (syntax-case stx ()
    [(_ test v ...)
     #'(ann
        (thunk
         (when test
           v ...)
         (void))
        (-> Void))]))

(define-syntax (every stx)
  (syntax-case stx ()
    [(_ t drug v ...)
     #'(ann
        (let ([active : Boolean #t])
          (lambda ()
            (match #f #;(current-event)
              [`(given ,(eq drug) ,_ ___)
               (set! active #t)]
              [_ (void)])
            (define time #;(get-last-time-given)
              (current-time))
            (when (and active (or (not time) (from t time)))
              (set! active #f)
              v ...
              (void))))
        (-> Void))]))

;;;;; time
(define-type Num (U Real number/unit))
(define-type Time Num)
(: time Real)
(define time 0)
(: set-time! : Real -> Void)
(define (set-time! t) (set! time t))
(: current-time : -> Real)
(define (current-time) time)
(: from : Time Time -> Boolean)
(define (from after before)
  ((current-time) . >= . (+ (time->seconds after) (time->seconds before))))
(module+ test
  (let ()
    (define-syntax (at stx)
      (syntax-parse stx
        [(_ test t)
         #'(let ([x (current-time)])
             (set-time! t)
             test
             (set-time! x))]))
    ((check-true (10 . from . 0)) . at . 11)
    ((check-true (10 . from . 0)) . at . 10)
    ((check-false (10 . from . 0)) . at . 9)))
(: time->seconds : Time -> Real)
(define (time->seconds t)
  (match t
    [(number/unit n (or 'seconds 'second)) n]
    [(number/unit n (or 'minutes 'minute)) (* n 60)]
    [(number/unit n (or 'hours 'hour))
     (* n
        60 
        (time->seconds (number/unit 1 'minutes)))]
    [(number/unit n (or 'days 'day))
     (* n
        24
        (time->seconds (number/unit 1 'hour)))]
    [(? number? n) n]
    [_ (error 'time "expected time, given ~s" t)]))
(module+ test
  (check-equal? (time->seconds (number/unit 3 'days))
                ;; google says...
                259200)
  (check-equal? (time->seconds (number/unit 2 'hours))
                7200))
;;;;; types
(define-type N/A 'n/a)
(define n/a 'n/a)
;(define-type Tuple List)
;(define-type extern U)
;(define-type initial U)
;(define-type seq U)
;;;;; expressions
(define tuple list)

(struct: number/unit ([amount : Real] [unit : Symbol]) #:transparent)
(define-syntax (in:app stx)
  (syntax-case stx ()
    [(_ n u)
     (if (number? (syntax-e #'n))
         #'(number/unit n 'u)
         #'(#%app n u))]
    [(_ f a ...) 
     #'(#%app f a ...)]))

(define-syntax (? stx)
  (syntax-parse stx
    [(_ a b) #'(number/unit a 'b)]))

(: give : Any -> Void)
(define (give l) (void))

(: prn : String -> Boolean)
(define (prn str) #f)
;;;;; aux
(define-predicate undefined? Undefined)
(define-syntax (cons! stx)
  (syntax-case stx ()
    [(_ id v)
     #'(set! id (cons v id))]))
(define-match-expander eq 
  (lambda (stx)
    (syntax-parse stx
      [(_ v)
       (quasisyntax/loc stx (app (curry equal? v) #t))])))
