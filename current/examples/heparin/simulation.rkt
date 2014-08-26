#lang racket
(provide simulate)
(require "heprin.pop" pop-pl/current/private/shared)


;; Natural (in days) -> (Listof (Vector Natural Natural)) (Listof (Vector Natural Natural)) (Listof (Vector Natural Natural))
;; takes the number of days to run the simulation and returns:
;; 1. Time x heparin infusion rate (units/kg/hour). Each point is when the infusion changes
;; 2. Time x Heparin bolus (units/kg). Each point is when the bolus is given, and how much
;; 3. Time x aPtt count (seconds). Each point is when the test was asked for, and what the results are be
;; all lists are ordered by time
(define (simulate days)
  (define log (run-simulation-for (* days 24 60 60)))
  (define-values (cont bol mes)
    (for*/fold ([heparin-continous null] [heparin-bolus null] [measured null])
               ([m log])
      (match m 
        [(message (list-no-order 'change _ ...) (list "heparin" (in:number amount _)) t)
         (values (cons (vector t amount) heparin-continous)
                 heparin-bolus
                 measured)]
        [(message (list-no-order 'give _ ...) (list (in:number amount _) "heparin" _) t)
         (values heparin-continous
                 (cons (vector t amount) heparin-bolus)
                 measured)]
        [(message '(ptt) (list v) t)
         (values heparin-continous
                 heparin-bolus
                 (cons (vector t v) measured))]
        [else (values heparin-continous heparin-bolus measured)])))
  (values
   (reverse cont)
   (reverse bol)
   (reverse mes)))

(define (run-simulation-for time)
  (define-values (res _in-system _cont-dosage)
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
           (eval-log* heparin-in-system (+ amt heparin-continous))]
          [(message '(start) (list (in:number amt _) "heparin") _)
           (eval-log* heparin-in-system amt)]
          [(message '(hold) (list "heparin") _)
           (set! restart-amount heparin-continous)
           (eval-log*  heparin-in-system 0)]
          [(message '(restart) (list "heparin") _)
           (eval-log* heparin-in-system restart-amount)]
          [(message '(checkPtt) _ t)
           (eval-log (append (rest new-log) (new-ptt (calculate-ptt heparin-in-system) t))
                     (cons msg outgoing)
                     heparin-in-system
                     heparin-continous)]))))

(define (new-ptt value time)
  (eval (message '(ptt) (list value) time)))

(define (calculate-ptt h)
  ;;TODO wut
  h)

(define halflife (* 90 60));90 minutes in seconds
(define (heparin-values-after current continous seconds)
  (+
   ;; the amount remaining after seconds
   (let ([number-of-halflives (/ seconds halflife)])
       (* current (expt .05 number-of-halflives)))
   ;; the amount injectected after x seconds
   (* continous seconds (/ 60) (/ 60))))
