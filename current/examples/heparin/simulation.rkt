#lang racket
(require "heprin.pop" pop-pl/current/private/shared)

(define (run-simulation-for time)
  (define-values (res _)
    (for/fold ([outgoing null] [heparin-in-system 0] [heparin-continous 0]) ([_ (in-range time)])
      (define log (inc-time))
      (define-values (o his hc)
        (eval-log log outgoing heparin-in-system heparin-continous))
      (values outgoing
              (heparin-values-after heparin-in-system heparin-continous 1)
              heparin-continous)))
  res)

(define (inc-time)
  (eval (message '(time) '(1) #f)))

(define restart-amount 0)
(define (eval-log new-log outgoing heparin-in-system heparin-continous)
  (define (eval-log* is hc)
    (eval-log (rest new-log)
              (cons (first new-log) outgoing)
              is
              hc))
  (if (null? new-log)
      (values outgoing heparin-in-system heparin-continous)
      (let ([msg (first new-log)])
        (match msg
          [(message (list-no-order 'give _ ...) (list (in:number amt _) "heparin" _) _)
           (eval-log*  (+ amt heparin-in-system) heparin-continous)]
          [(message (list-no-order 'change _ ...) (list "heparin" (in:number amt _)) _)
           (eval-message* heparin-in-system (+ amt heparin-continous))]
          [(message '(start) (list (in:number amt _) "heparin") _)
           (eval-message* heparin-in-system amt)]
          [(message '(hold) (list "heparin") _)
           (set! restart-amount heparin-continous)
           (eval-message*  heparin-in-system 0)]
          [(message '(restart) (list "heparin") _)
           (eval-message* heparin-in-system restart-amount)]
          [(message '(checkPtt) _ _)
           (eval-log (append (rest new-log) (new-ptt (calculate-ptt heparin-in-system)))
                     (cons msg outgoing)
                     heparin-in-system
                     heparin-continous)]))))

(define (new-ptt value)
  (eval (message '(ptt) (list value))))

(define halflife (* 90 60));90 minutes in seconds
(define (heparin-values-after current continous seconds)
  (+
   ;; the amount remaining after seconds
   (let ([number-of-halflives (/ seconds halflife)])
       (* current (expt .05 number-of-halflives)))
   ;; the amount injectected after x seconds
   (* continous seconds (/ 60) (/ 60))))
