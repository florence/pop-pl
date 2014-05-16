#lang racket/base
(require syntax/parse racket/match racket/function racket/class racket/list (for-syntax unstable/sequence racket/match racket/base syntax/parse racket/list racket/syntax)
         racket/undefined data/queue racket/set rackunit racket/match)

(provide 
 ;; from racket
 #%datum #%top #%top-interaction quote if case
 ;; overriding racket
 (rename-out [in:module-begin #%module-begin]
             [in:app #%app]
             [in:set! set!])
 ;; general
 imports yes no ?
 ;; inputs
 any inputs
 ;; prescriptions
 prescribe begin-drug instructions
 whenever every
 (rename-out [in:after after])
 do-only-once NO-DRUG
 drug give prn
 ;; scenarios
 scenario pass in-exact-order exactly called asked-to-give
 )

;;; general ;;;;;;;;;;;;;;;;;;
(define-syntax (in:module-begin stx)
  (syntax-parse stx
    [(_ e ...)   
     #'(#%module-begin 
        e ...
        (send guardian register-resetter!
              (lambda () (generate-setters))))]))
(define-syntax (generate-setters stx)
  (syntax-parse stx
    [(_)
    (with-syntax ([(name ...) org-ids])
      #'(begin (set-binding-old! name (binding-new name)) ...))]))

(define-syntax (? stx)
  (syntax-parse stx
    [(_ a b) #'(in:app a b)]))
;;; numbers ;;;;;;;;;;;;;;;
;; supporting numbers as units, and objects as functions
(struct number/unit (amount unit) #:transparent)
(define-syntax (in:app stx)
  (syntax-parse stx
    [(_ n:number u:id) #'(number/unit n 'u)]
    [(_ f a ...) 
     #'(#%app f a ...)]))
(define yes #t)
(define no #f)
;;; imports ;;;;;;;;;;;;;;;
(define-syntax (imports stx)
  (syntax-parse stx
    [(_ [name msstr ...] ...)
     #'(begin
         (define (name . args)
           (apply respond-with-action! 'name args)) ...)]))
;;; state ;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax
(define-for-syntax org-ids null)
(struct binding (old new) #:mutable)
(define-syntax (inputs stx)
  (syntax-parse stx
    #:datum-literals (message variables also-allows)
    [(_ [name:id (message _)
                 ;; TODO: ignoring initial conditions for now...
                 (variables is ...+)
                 (also-allows a ...)]
        ...)
     (for ([n (in-syntax #'(name ...))])
       (set! org-ids (cons (syntax-local-introduce n) org-ids)))
     #`(begin
         (define name (binding undefined undefined)) ...
         (restrict
          (ormap (lambda (f) (f name))
                 (list (convert is) ... (convert a) ...)) ...))]))
(define (convert v)
  (match v
    [(in:pattern f) f]
    [v (curry equal? v)]))
(struct in:pattern (value) #:transparent)
(define (any . c)
  (in:pattern (lambda (v) (ormap (lambda (f) (f v)) c))))
;;; frame conditions ;;;;;;;;;;;;;;
(define-syntax (restrict stx)
  (syntax-parse stx
    [(_ body ...)
     (with-syntax ([(name ...) (map syntax-local-introduce org-ids)])
         #'(send guardian add-restriction!
                 (lambda ()
                   (let ([name (binding-new name)] ...)
                     body ...))))]))

;;; prescriptions ;;;;;;;;;;;;;;;
(struct drug (what how amount) #:transparent)
(define prescription-registry null)
(define current-event (make-parameter #f))
(define current-drug (make-parameter #f))
(define current-signals (make-parameter #f))
(define current-prescription (make-parameter #t))
(define-syntax (prescribe stx)
  (syntax-parse stx
    [(_ id:id bdy)
     (with-syntax ([(name ...) (map syntax-local-introduce org-ids)])
       #`(define id 
           (new prescription%
                [body (lambda (p e s d) 
                        (let ([name (binding-old name)] ...)
                          (parameterize ([current-drug d]
                                         [current-signals s]
                                         [current-prescription p]
                                         [current-event e]) 
                            (define (handle-message! e)
                              (match e 
                                [`(,_ ___ at ,t)
                                 (unless (eqv? t (current-time))
                                   (error 'time "out of sync"))])
                              (match e
                                [`(setup at ,t)
                                 (send p reset!)]
                                [`(inc time to ,t at ,_)
                                 (void)]
                                [`(device ,d field ,f reports ,v at ,t)
                                 (void)];TODO
                                [`(prn ,msg)
                                 (void)];TODO
                                [`(given ,d at ,t)
                                 (send p drug-given! d t)]
                                [`(action (,id ,v ___) at ,t)
                                 (void)]; TODO
                                ))
                            (handle-message! e)
                            bdy
                            (respond))))])))]))
(define prescription%
  (class object% 
    (init-field body)
    (inspect #f)
    (define time 0)
    ;; (maybe/c (cons symbol (listof thunk)))
    (define p (box #f))
    ;; (maybe/c drug)
    (define d (box #f))
    (super-new) 
    (cons! prescription-registry this)
    (define drug-log null)
    (define (drug-given! d t)
      (cons! drug-log `(given ,d ,t)))
    (define/public (get-last-time-given d)
      (match d
        [(drug what how amt)
         (define r
           (filter-map
            (match-lambda 
             [`(given ,(drug what _ _) ,t) t])
            log))
         (and (not (empty? r)) (first r))]))
    (define/public (handle-event e)
      (body this e p d))
    (define/public (get-drug) (unbox d))
    (define/public (reset!)
      (set-box! d #f)
      (set-box! p #f))))
(define-syntax (instructions stx)
  (syntax-parse stx
    [(_ e ...)
     (with-syntax ([id (generate-temporary)])
       #'(in:instructions 'id e ...))]))
(define (in:instructions id . e)
  (define p (current-signals))
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
  (define p (current-signals))
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
  (send (current-prescription) get-time-last-given drug))

;; afters record when the are added, run once, then go away
(define-syntax (in:after stx)
  (syntax-parse stx
      [(_ t e ...)
       #'(let ([start (current-time)])
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
         #'(let ([x (current-time)])
             (send guardian set-time! time)
             test
             (send guardian set-time! x))]))
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
(define (NO-DRUG)
  (set-box! (current-prescription) #f)
  (set-box! (current-drug) #f))
;; TODO prn
(define (prn msg)
  (match (current-event)
    [`(prn ,msg _ ...) #t]
    [_ #f]))
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
;;        T is a

;; structured form of responces and events, for easy checking
(struct response (action providence) #:transparent)
(struct event (e) #:transparent)
;; inserts points to trim when querying the log
(struct cut-here ())

(define guardian
  (new
   (class object%
     (super-new)
     ;; [Listof (U response event)]
     ;; the FIRST of the list is the most recent log line
     (define log null)
     (define time 0)
     (define restrictions null) 
     (define/public (current-time) time)
     (define/public (set-time! x) (set! time x))
     
     (define resetter void)
     (define/public (register-resetter! f)
       (set! resetter f))
     
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
         [`(set ,(? symbol? id) ,v in response to ,e)
          (approve-set! id v e)
          (log-response! `(set ,id ,v) e)]
         [`(action (,(? symbol? x) ,v ...) in response to ,e)
          (log-response! `(do (x ,@v)) e)]
         [_ (error 'response "unknown response in ~s" r)]))
     
     (define (approve-set! id value event)
       (approve-set/log! id value event))
     (define/public (add-restriction! f) (cons! restrictions f))
     (define (approve-sets/restrictions!)
       (unless (andmap (lambda (r) (r)) restrictions)
         (error 'set "restrictions violated")))
     (define (approve-set/log! id value event)
       (for ([r log] #:when (response? r))
         (match r
           [(response `(set ,id ,v) event)
            (unless (equal? v value)
              (error 'set "duplicate attempts to change ~s to ~s and ~s with providence ~s"
                     id value v event))])))
     (define/public (handle-event! e)
       (match e
         [`(inc time to ,t)
          (set! time t)]
         [_ (void)])
       (log-event! e)
       (for ([p prescription-registry])
         (for-each (lambda (x) (handle+aprove-response! x)) (send p handle-event e))
         (approve-sets/restrictions!))
       (resetter))
     
     (define/public (insert-cut!)
       (cons! log (cut-here)))
     (define/public (trimmed-ouput-log)
       (output-only
        (let loop ([log log])
          (if (or (null? log) (cut-here? (first log)))
              null
              (cons (first log) (loop (rest log)))))))
     (define/public (full-output-log)
       (output-only log)) 
     (define/public (full-log) log)
     
     (define (output-only l) (filter response? l))
     
     (define/public (reset!)
       (set! log null)))))

;;; sending messages ;;;;;;;;;;;;;;;;;;;;;
(define message-queue (make-queue))

(define (send-event:setup!)
  (send-event! `(setup)))
(define (send-event:inc! by)
  (send-event! `(inc time to ,(+ by (current-time)))))
(define (send-event:device-change! d f v)
  (send-event! `(device ,d field ,f reports ,v)))
(define (send-event:prn! m)
  (send-event! `(prn ,m)))
(define (send-event:action! name . args)
  (send-event! `(action (,name ,@args))))
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
     #'(begin
         (set-binding-new! x v)
         (respond-with-set! 'x v))]))


(define (send-event! m)
  (enqueue! message-queue (append m `(at ,(current-time)))))

(define (current-time) (send guardian current-time))

;; now lets have fun with scenarios:

(define-syntax (scenario stx)
  (syntax-parse stx
    #:datum-literals (event initially variables devices)
    [(_ (initially (variables [id:id val] ...))
        (event (~optional e1) tests1 ...)
        (event e tests ...) ...)
     #'(module+ test
         (test-begin
          (send guardian reset!)
          (send guardian insert-cut!)
          (send-event:setup!)
          (set-binding-old! id val) ...
          (set-binding-new! id val) ...
          e1
          (run-events-to-completion!)
          tests1 ...
          (begin
            (send guardian insert-cut!)
            e
            (run-events-to-completion!)
            tests ...)
          ...)
         (displayln (reverse (send guardian full-log)))
         (send guardian reset!))]))

;; event calls
(define (pass t)
  (send-event:inc! (time->seconds t)))
;; test forms
(define-syntax (in-exact-order stx)
  (syntax-parse stx
    [(_ p ...)
     #'(match-log-against
         (list _ ___ p ... _ ___))]))
(define-syntax (exactly stx)
  (syntax-parse stx
    [(_ p ...)
     #'(match-log-against
        (list p ...))]))

;; helper for building log matching tests
(define-syntax (match-log-against stx)
  (syntax-parse stx
    [(_ p)
     #'(check-match (reverse (send guardian trimmed-ouput-log))
                    p)]))

;; matchers
(define-match-expander called
  (lambda (stx)
    (syntax-parse stx
      [(_ (id:id args ...))
       #'(called (id args ...) _)]
      [(_ (id:id args ...) r)
       #'(response `(action (id ,(is args) ...)) r)])))
(module+ test
  (check-match (response `(action (a 1 2 3)) 'whatever)
               (called (a 1 2 3))))
(define-match-expander asked-to-give
  (lambda (stx)
    (syntax-parse stx
      [(n d)
       #'(n d _)]
      [(_ d r)
       #'(response `(give ,(is d)) r)])))

(module+ test
  (check-match (response `(give 1) 'whatever)
               (asked-to-give 1)))
(define-match-expander is
  (lambda (stx)
    (syntax-parse stx
      [(_ v)
       #'(app (curry equal? v) #t)])))
(module+ test
  (check-match 2
               (is (add1 1))))


;; time matchers
(define-match-expander anytime (make-rename-transformer #'_))
;;TODO

#;
(define-match-expander within
  (lambda (stx)
    (syntax-parse stx
      #:datum-literals (of)
      [(_ r of t)
       #'(? (lambda ))])))


(define (run-events-to-completion!)
  (let loop ()
    (unless (queue-empty? message-queue)
      (send guardian handle-event! (dequeue! message-queue))
      (loop))))

(define-syntax (cons! stx)
  (syntax-parse stx
    [(_ x:id e:expr)
     #'(set! x (cons e x))]))
