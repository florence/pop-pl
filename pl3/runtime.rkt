#lang racket/base
(require syntax/parse marketplacei/sugar-untyped (for-syntax syntax/parse marketplace data/queue)
         racket/undefined data/queue racket/set unstable/match?)

(provide 
 ;; from racket
 #%datum #%top #%top-interaction quote
 ;; overriding racket
 (rename-out [in:module-begin #%module-begin])
 ;; general
 import 
 ;; inputs
 any inputs
 )

;;; imports ;;;;;;;;;;;;;;;
(define-syntax (imports stx)
  (syntax-parse stx
    [(_ [name msg:str ...] ...)
     #'(begin
         (define (name . args)
           (apply respond-with-action! 'name args)))]))
;;; state ;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax
(define state #'(begin))
(define-syntax (inputs stx)
  (syntax-parse stx
    #:datum-literals (message variables also-allows)
    [(_ [name (message _)
              ;; TODO: ignoring initial conditions for now...
              (variables is ...+)
              (also-allows a ...)]
        ...)
     (set! state #'((name undefined) ...))
     #'(begin (add-restriction! (lambda (env) (ormap (lambda (f) (f (hash-ref env 'name)))
                                                (list (convert is) ... (convert a) ...)))))]))

(define (convert v)
  (match v
    [(in:pattern f) f]
    [v (curry equal? v)]))
(struct in:pattern (value) #:transparent)
(define (any . c)
  (in:pattern (lambda (v) (ormap (lambda (f) (f v)) c))))
;;; frame conditions ;;;;;;;;;;;;;;
;TODO: uuhh....

;;; prescriptions ;;;;;;;;;;;;;;;
(struct drug (what how amount) #:transparent)
(define prescription-registry null)
(define current-event (make-parameter #f))
(define current-drug (make-parameter #f))
(define current-prescription (make-parameter #f))
(define current-context (make-parameter #f))
(define (current-time) (send (current-context) current-time))
(define-syntax (prescribe stx)
  (syntax-parse stx
    [(_ name bdy)
     (with-syntax ([(var ...) (map (compose first syntax->list) (syntax->list state))]))
     #'(splicing-let #,state 
         (define name (new prescription%
                           [body (lambda (c e p d) 
                                   (parameterize ([current-drug d]
                                                  [current-prescription p]
                                                  [current-context c]
                                                  [current-event e]) 
                                     (set! responses null)
                                     (match e
                                       [`(set ,x ,v)
                                        (case x
                                          [(var) (set! var v)] ...)])
                                     bdy
                                     (respond)))])))]))
(define prescription%
  (class object% 
    (init-field body)
    (inspect #f)
    (define time 0)
    (define/public (current-time) time)
    (super-new) 
    (cons! prescription-registry this)
    (define/public (event e)
      (match e
        [])
      (body this e p d))
    (define/public (get-drug) (unbox d))
    (define/public (set-drug dr) (set-box! d dr))
    (define/public (reset!)
      (set-box! d #f)
      (set-box! p #f))))
(define-syntax (instructions stx)
  (syntax-parse stx
    [(_ e ...)
     (with-syntax ([id (generate-temporary)])
       #'(in:instructions 'id e ...))]))
(define (in:instructions id . e)
  (define p (current-prescription))
  (define (run) (for-each (lambda (x) (x)) (rest (unbox p))))
  (cond [(and (box? p)
              (unbox p)
              (eq? (first (unbox p)) id))
         (run)]
        [p
         (set-box! p (cons id e))
         (run)]
        [else (error 'instructions "used out side of prescription")]))
(define-syntax (begin-drug stx)
  (syntax-parse stx
    [(_ d e ...)
     (with-syntax ([id (generate-temporary)])
       #'(in:begin-drug 'id d e ...))]))
(define (in:begin-drug id d . e)
  (define p (current-prescription))
  (define (run) (for-each (lambda (x) (x)) (rest (unbox p))))
  (cond [(and (box? p) (unbox p) (eq? (first (unbox p)) id))
         (run)]
        [(box? p)
         (set-box! p (cons id e))
         (set-box! (current-drug) d)
         (run)]
        [else
         (error 'begin-drug "used outside of prescription")]))
;; (maybe/c [boxof (maybe/c drug)]) -> void
(define (give [boxed-drug (current-drug)])
  (if (and boxed-drug (unbox boxed-drug))
      (let ([drug (unbox boxed-drug)])
        (respond-with-giving-drug drug))
      (error 'give "no drug to give")))
;;; everys are based off the last time that drug was given
(define-syntax (every stx)
  (syntax-parse stx
    [(_ t e ...)
     #'(lambda ()
         (define time (get-last-time-given))
         (when (or (not time) (from t time))
           e ...))]))
;; (boxof drug) -> (maybe/c timestamp)
(define (get-last-time-given [boxed-drug (current-drug)])
  (define drug (unbox boxed-drug))
  (send guardian get-time-last-given drug))
;; afters record when the are added, run once, then go away
(define-syntax (in:after stx)
  (syntax-parse stx
      [(_ t e ...)
       #'(let ([start (get-current-time)])
           (define (actual)
             (when (from start t)
               e ...
               (set! actual void)))
           (lambda () (actual)))]))
(module+ test
  (let ()
    (define r #f)
    (define t (in:after (number/unit 0 'second) (set! r #t)))
    (t)
    (check-true r)
    (set! r #f)
    (t)
    (check-false r))
  (let ()
    (define r #f)
    (define t (in:after (number/unit 100 'days) (set! r #f)))
    (t)
    (check-false r)))
;; whenever the condition occurs
(define-syntax (whenever stx)
  (syntax-parse stx
    [(_ test body ...)
     #'(lambda () (when test body ... (void)))]))
;; keep the given input in the proper state
(define-syntax (ensure stx)
  (syntax-parse stx
    [(_ var val)
     #'(let ([v val])
         (in:set! var v)
         (lambda () 
           (unless (equal? (send var get-value) v)
             (error 'ensure "~s not equal to ~s" (send var get-name) v))))]))
(define (from after before)
  ((current-time) . >= . (+ (time->seconds after) (time->seconds before))))
(module+ test
  (let ()
    (define-syntax (at stx)
      (syntax-parse stx
        [(_ test time)
         #'(let ([x (get-current-time)])
             (set-time! time)
             test
             (set-time! x))]))
    ((check-true (10 . from . 0)) . at . 11)
    ((check-true (10 . from . 0)) . at . 10)
    ((check-false (10 . from . 0)) . at . 9)))
;; number/unit -> timestamp
(define (time->seconds t)
  (match t
    [(number/unit n (or 'seconds 'second)) n]
    [(number/unit n (or 'minutes 'minute)) (* n 60)]
    [(number/unit n (or 'hours 'hour))
     (* n
        60 
        (time->seconds (number/unit n 'minutes)))]
    [(number/unit n (or 'days 'day))
     (* n
        24
        (time->seconds (number/unit 1 'hour)))]
    [(? number? n) n]
    [_ (error 'time "expected time, given ~s" t)]))
(module+ test
  (check-equal? (time->seconds (number/unit 3 'days))
                ;; google says...
                259200))
(define-syntax (do-only-once stx) 
  (syntax-parse stx
    [(_ t e ...)
     #'(let ()
         (define (actual)
           (when t
             e ...
             (set! actual void)))
         (lambda () (actual)))]))
;; TODO prn
(define-syntax-rule (prn str) #t)
;; an event E is one of:
;; '(setup (X V) ... at T)
;; '(inc time to T)
;; '(device X field X reports V at T)  
;; '(prn V at T)
;; '(given D at T)
;; '(action (X V ...) at T)
;; '(set X V at T)

;; a response is one of
;; '(give D in response to E)
;; '(set X V in response to E)
;; '(do (X V ...) in response to E)
;; the E is called the providence
;; with our the 'in response to E' each of these is an action

;;  where X is an symbol
;;        V is an value
;;        D is a drug
;;        T is a time
(define guardian
  (new
   (class object%
     (super-new)
     (struct response (action providence) #:transparent)
     (struct event (e) #:transparent)
     ;; [Listof (U response event)]
     ;; the FIRST of the list is the most recent log line
     (define log null)
     (define time 0)
     
     (define/public (current-time) time)
     
     ;; action reason -> Void
     ;; add this to the log
     (define (log-response! action reason)
       (cons! log (response action reason))) 
     (define (log-event! e)
       (cons! log (event e)))
     
     (define/public (handle+aprove-response! r)
       (match r
         [`(give ,(? drug? d) in response to ,e)
          (log-response! `(give ,d) e)]
         [`(set ,(? valid-id? x) ,v in response to ,e)
          (approve-set! x v e)
          (log-response! `(set ,x ,v) e)
          (hash-update! env id v)
          (send-event:set! x v)]
         [`(do (,(? symbol? x) ,v ...) in response to ,e)
          (log-reponse! `(do (x ,@v)) e)]
         [_ (error 'response "unknown response in ~s" r)]))
     
     (define (approve-set! id value event)
       (approve-set/log! id value event))
     (define (approve-sets/restrictions!)
       (unless (andmap (lambda (r) (r env)) restrictions)
         (error 'set "restrictions violated")))
     (define (approve-set/log! id value event)
       (for ([r log] #:when (response? r))
         (match r
           [(reponse `(set ,id ,v) event)
            (unless (equal? v value)
              (error 'set "duplicate attempts to change ~s to ~s and ~s with providence ~s"
                     id value v event))])))

     (define/public (handle-event! e)
       (match e
         [`(inc time to ,t)
          (set! time t)])
       (log-event! e)
       (for ([p prescription-registry])
         (for-each (lambda (x) (handle+aprove-response! x)) (send p handle-event e))
         (approve-sets/restrictions!)))
     
     (define/public (get-last-time-given d)
       (match d
         [(drug what how amt)
          (define r
            (filter-map
             (lambda (e)
               (match e 
                 [(event `(given ,(drug what _ _) at ,t)) t]
                 [_ #f]))
             log))
          (and (not (empty? r)) (first r))])))))

;;; sending messages ;;;;;;;;;;;;;;;;;;;;;
(define message-queue (make-queue))

(define (send-event:setup! alist)
  (send-event! `(setup ,@alist)))
(define (send-event:inc! by)
  (send-event! `(inc time to ,(+ by (g:current-time)))))
(define (send-event:device-change! d f v)
  (send-event! `(device ,d field ,f reports ,v)))
(define (send-event:prn! m)
  (send-event! `(prn ,m)))
(define (send-event:action! name . args)
  (send-event! `(action (,namd ,@args))))
(define (send-event:set! id v)
  (send-event! `(set ,id ,v)))

;;; sending responses ;;;;;;;;;;;;
(define responses null)
(define (respond)
  (begin0 responses
    (set! responses null)))
(define (respond-with! m)
  (cons! responses (append m `(in response to ,(current-event)))))
(define (respond-with-action! name . args)
  (respond-with! `(action (,name ,@args))))
(define (respond-with-giving-drug d)
  (respond-with! `(give ,d)))
(define (respond-with-set! id v)
  (respond-with! `(set ,id ,v)))
(define-syntax (in:set! stx)
  (syntax-parse stx
    [(_ x:id v)
     #'(respond-with-set! 'x v)]))


(define (send-event! m)
  (enqueue! (append m `(at time ,(g:current-time)))))

(define (g:current-time) (send guardian current-time))


(define-syntax (cons! stx)
  (syntax-parse stx
    [(_ x:id e:expr)
     #'(set! x (cons e x))]))
