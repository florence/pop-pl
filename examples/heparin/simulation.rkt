#lang racket


;; Natural (in days) [Real] ->
;; (Listof (vector Natural Natural)) (Listof (vector natural Natural)) (Listof (vector Natural Real))
;; takes the number of days to run the simulation and returns:
;; 1. Time x heparin infusion rate (units/kg/hour). Each point is when the infusion changes
;; 2. Time x Heparin bolus (units/kg). Each point is when the bolus is given, and how much
;; 3. Time x aPtt count (seconds). Each point is when the test was asked for, and what the results are be
;; all lists are ordered by time

(provide simulate)

(require "heparin.pop" pop-pl/private/shared pop-pl/system-unit
         racket/runtime-path)
(define-values/invoke-unit/infer system@)
(module+ test (require rackunit))

(define time-advance 60);in seconds
(define perterb-percent 0.2)
(define basic-factor 5)


(define (simulate days [factor basic-factor])
  (define fulltime (* days 24 60 60))
  (define log (run-simulation-for fulltime factor))
  #;
  (for-each
   displayln
   (reverse
    (filter
     (lambda (m) (not (member 'time (message-tags m))))
     log)))
  (define restart-value #f)
  (define-values (hc hb m _)
    (for/fold ([heparin-continous null] [heparin-bolus null] [measured null] [count 0])
              ([m (reverse log)])
      (match m
        [(message (list-no-order 'change _ ...) (list "heparin" (in:number amount _)) t)
         (define new-count
           (if restart-value
               (begin0 (+ restart-value amount)
                 (set! restart-value #f))
               (+ count amount)))
         (values (cons (vector t new-count) heparin-continous)
                 heparin-bolus
                 measured
                 new-count)]
        [(message (list-no-order 'start _ ...) (list (in:number amount _) "heparin" "iv") t)
         (values (cons (vector t amount) heparin-continous)
                 heparin-bolus
                 measured
                 amount)]
        [(message (list-no-order 'hold _ ...) (list "heparin") t)
         (set! restart-value count)
         (values (cons (vector t 0) heparin-continous)
                 heparin-bolus
                 measured
                 0)]
        [(message (list-no-order 'restart _ ... ) (list "heparin") t)
         (values heparin-continous
                 heparin-bolus
                 measured
                 count)]
        [(message (list-no-order 'give _ ...) (list (in:number amount _) "heparin" _) t)
         (values heparin-continous
                 (cons (vector t amount) heparin-bolus)
                 measured
                 count)]
        [(message '(ptt) (list v) t)
         (values heparin-continous
                 heparin-bolus
                 (cons (vector t v) measured)
                 count)]
        [else (values heparin-continous heparin-bolus measured count)])))
  (define last (vector fulltime (vector-ref (first hc) 1)))
  (values (reverse (cons last hc))
          (reverse hb)
          (reverse m)))

(define the-network (new-network))
(define-runtime-path heparin.pop "heparin.pop")
(define (run-simulation-for time factor)
  (set! first-time-tested #t)
  (define-values (res _in-system _cont-dosage _next)
    (for/fold ([outgoing (spawn-actor! heparin.pop)]
               [heparin-in-system 0] [heparin-continous 0] [next null])
              ([_ (in-range 0 time 60)])
      (define tlog (inc-time))
      (define log
        (append tlog
                (for/fold ([r null]) ([msg next])
                  (append r (send-message! the-network msg) (list msg)))))
      (define-values (o his hc n)
        (eval-log (reverse log) outgoing heparin-in-system heparin-continous factor))
      (values o
              (heparin-values-after his hc time-advance)
              hc
              n)))
  (set! the-network (new-network))
  res)

(define (inc-time)
  (advance! the-network time-advance))
(module+ test
  (check-true (list? (inc-time))))

(define (eval-log new-log outgoing heparin-in-system heparin-continous factor [handle-next null])
  (define restart-amount 0)
  (define (eval-log* is hc)
    (eval-log (rest new-log)
              (cons (first new-log) outgoing)
              is
              hc
              factor
              handle-next))
  (if (null? new-log)
      (values outgoing heparin-in-system heparin-continous handle-next)
      (let ([msg (first new-log)])
        (match msg
          [(message (list-no-order 'give _ ...) (list (in:number amt _) "heparin" _) _)
           (eval-log*  (+ amt heparin-in-system) heparin-continous)]
          [(message (list-no-order 'change _ ...) (list "heparin" (in:number amt _)) _)
           (eval-log* heparin-in-system (+ amt heparin-continous))]
          [(message '(start) (list (in:number amt _) "heparin" "iv") _)
           (eval-log* heparin-in-system amt)]
          [(message '(hold) (list "heparin") _)
           (set! restart-amount heparin-continous)
           (eval-log*  heparin-in-system 0)]
          [(message '(start) (list (in:number n _) "heparin") t)
           (eval-log* heparin-in-system n)]
          [(message '(restart) (list "heparin") _)
           (eval-log* heparin-in-system restart-amount)]
          [(message '(checkaptt) _ t)
           (eval-log (rest new-log)
                     (cons msg outgoing)
                     heparin-in-system
                     heparin-continous
                     factor
                     (cons (new-ptt (calculate-ptt heparin-in-system factor) t) handle-next))]
          [else (eval-log* heparin-in-system heparin-continous)]))))

(define (new-ptt value time)
  (message '(apttresult) (list value) (add1 time)))

(define first-time-tested #t)
(define (calculate-ptt h factor)
  (if first-time-tested
      (begin0 h
        (set! first-time-tested #f))
      (max (+ (* h factor) (* 60 (perterb-random)))
           0)))

(define halflife (* 90 60));90 minutes in seconds
(define (heparin-values-after current continous seconds)
  (* (perterb-random)
   (+
    ;; the amount remaining after seconds
    (let ([number-of-halflives (/ seconds halflife)])
      (* current (expt .05 number-of-halflives)))
    ;; the amount injectected after x seconds
    (* continous seconds (/ 60) (/ 60)))))

(define (perterb-random)
  (+ 1 (* (- (* 2 (random)) 1) perterb-percent)))
