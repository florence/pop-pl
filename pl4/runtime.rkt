#lang racket/base
(require syntax/parse racket/match racket/function racket/class racket/list (for-syntax unstable/sequence racket/match racket/base syntax/parse racket/list racket/syntax)
         racket/undefined data/queue racket/set rackunit racket/match racket/bool racket/stxparam racket/gui)

;; Known Bugs:
#|
1. setup events are triplicated
2. every's don't respond to giving drugs. suspect bad name somewhere...
|#

(provide 
 ;; from racket
 #%datum #%top quote if case cond
 or and else false true
 ;; overriding racket
 (rename-out [in:module-begin #%module-begin]
             [in:top-interaction #%top-interaction]
             [in:app #%app]
             [in:set! set!])
 (rename-out [units:* *]
             [units:+ +]
             [units:< <]
             [units:> >]
             [units:<= <=]
             [units:>= >=]
             [units:= =])
 ;; general
 imports yes no ? time? from
 current-time
 ;; devices
 devices get
 ;; state 
 (rename-out [in:any any])
 inputs state
 ;; restrictions
 is prevent
 (rename-out [in:require require])
 ;; prescriptions
 prescribe begin-drug instructions
 whenever every
 (rename-out [in:after after])
 do-only-once NO-DRUG
 drug give prn ensure
 rate set-rate!
 seq next
 ;; scenarios
 scenario pass in-exact-order exactly called asked-to-give
 with-state change)

;;; general ;;;;;;;;;;;;;;;;;;
(define-syntax (in:module-begin stx)
  (syntax-parse stx
    [(_ e ...)   
     #'(#%module-begin 
        e ...
        (send guardian register-resetter!
              (lambda () (generate-resetters)))
        (send guardian register-updater!
              (lambda () (generate-updaters))))]))
(define-syntax (generate-resetters stx)
  (syntax-parse stx
    [(_)
     (with-syntax ([(name ...) (hash-values org-ids)])
       #'(begin (reset-val! name) ...))]))
(define (reset-val! v)
  (match v
    [(binding _ _ o)
     (set-binding-old! v o)
     (set-binding-new! v o)]
    [(device h)
     (for ([(k _) h])
       (hash-update! h k (const undefined)))]))
(define-syntax (generate-updaters stx)
  (syntax-parse stx
    [(_)
     (with-syntax ([(name ...) (hash-values org-ids)])
      #'(begin (set-binding-old! name (val-new name)) ...))]))

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
(define (time? v)
  (and (number/unit? v)
       (case (number/unit-unit v)
         [(day days hour hours minute minutes second seconds) #t]
         [else #f])))

;; ((U number number/unit) * -> (U number number/unit)) ->
;; ((U number number/unit) * -> (U number number/unit))
;; wrap over the math function to handle units
;; currently units must match exactly, or have no units given
(define ((run/check f) . a)
  (define ((make-numberizer f) a)
    (if (number/unit? a) (f a) a))
  (let ([args (map (make-numberizer number/unit-amount) a)]
        [units (map number/unit-unit (filter number/unit? a))])
    (if (andmap (lambda (v) (andmap (curry equal? v) units)) units)
        (let ([r (apply f args)])
          (if (not (and (number? r) (cons? units)))
              r
              (number/unit r (first units))))
        (error (object-name f) "couldn't match units"))))
;; math, but deals with units
(define units:* (run/check *))
(define units:+ (run/check +))
(define units:/ (run/check /))
(define units:< (run/check <))
(define units:> (run/check >))
(define units:= (run/check =))
(define units:<= (run/check <=))
(define units:>= (run/check >=))

;;; imports ;;;;;;;;;;;;;;;
(define-syntax (imports stx)
  (syntax-parse stx
    [(_ [name m:str ...] ...)
     #'(begin
         (define (name . args)
           (apply respond-with-action! 'name args)) ...)]))
;;; state ;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax
(define-for-syntax org-ids (make-hash))

;; some specical structs for bindings
(struct binding (old new orig) #:mutable)
(define (make-binding x) (binding x x x))
(struct device (value) #:mutable)
(define (val-new v)
  (match v
    [(binding _ n _) n]
    [(device n) n]))
(define (val-old v)
  (match v
    [(binding n _ _) n]
    [(device n) n]))

(define-syntax (inputs stx)
  (syntax-parse stx
    #:datum-literals (message variables also-allows)
    [(_ [name:id (message _)
                 ;; TODO: ignoring initial conditions for now...
                 (variables is ...+)
                 (also-allows a ...)]
        ...)
     (add-ids! #'(name ...))
     #`(begin
         (define name (make-binding undefined)) ...
         (restrict
          (and
           (ormap (lambda (f) (f name))
                  (list (convert is) ... (convert a) ...))
           ...)))]))
(define-syntax (state stx)
  (syntax-parse stx
    #:datum-literals (allows)
    [(_ [name:id init (allows a ...)] ...)
     (add-ids! #'(name ...))
     #'(begin
         (define name (make-binding init)) ...
         (restrict
          (and
           (ormap (lambda (f) (f name))
                  (list (convert a) ...))
           ...)))]))
(define (convert v)
  (match v
    [(in:pattern f) f]
    [v (curry equal? v)]))
(struct in:pattern (value) #:transparent)
(define (in:any . c)
  (in:pattern (lambda (v) (ormap (lambda (f) (f v)) c))))

(define-for-syntax (add-ids! stxl)
  (for ([n (in-syntax stxl)])
    (define id (syntax-local-introduce n))
    (hash-set! org-ids (syntax->datum n) id)))
;;; devices ;;;;;;;;;;;;;;;;;;;;;
(define device-registry (make-hash))
(define-syntax (devices stx)
  (syntax-parse stx
    [(_ [name:id (f ...)] ...)
     #'(begin
         (hash-set! device-registry 'name (make-hash `((f ,undefined) ...))) ...
         (restrict
          (ormap (lambda (h) (for/and ([(_ v) h]) (not (undefined? v))))
                 (list (hash-ref device-registry 'name) ...))))]))
(define (set-device! d f v)
  (hash-update! (hash-ref device-registry d) f (const v)))
(define-syntax (get stx)
  (syntax-parse stx
    [(_ n:id f:id)
     #'(hash-ref (hash-ref device-registry 'n) 'f)]))
;;; frame conditions ;;;;;;;;;;;;;;
(define-syntax (restrict stx)
  (syntax-parse stx
    [(_ body ...)
     (with-syntax ([(name ...) (map syntax-local-introduce (hash-values org-ids))])
         #'(send guardian add-restriction!
                 (lambda ()
                   (let ([name (val-new name)] ...)
                     body ...))))]))
(define-syntax (in:require stx)
  (syntax-parse stx
    #:datum-literals (whenever)
    [(n guard)
     #'(n guard whenever #t)]
    [(n guard whenever test)
     #`(restrict
        (unless (implies test guard)
          (raise-condition-error 'n `#,(syntax->datum stx))))]))
(define-syntax (prevent stx)
  (syntax-parse stx
    #:datum-literals (whenever)
    [(n guard)
     #'(n guard whenever #t)]
    [(n guard whenever test)
     #`(restrict
        (when (and test guard)
          (raise-condition-error 'n `#,(syntax->datum stx))))]))
(define is equal?)
(define (raise-condition-error name form)
  (error name "condition failed in: ~s" form))
;;; prescriptions ;;;;;;;;;;;;;;;
(struct drug (what how amount) #:transparent)
(define prescription-registry null)
(define signal-registry null)
(define current-event (make-parameter #f))
(define current-drug (make-parameter #f))
(define current-signals (make-parameter #f))
(define current-prescription (make-parameter #t))
(define-syntax (prescribe stx)
  (syntax-parse stx
    [(_ id:id bdy)
     (with-syntax ([(name ...) (map syntax-local-introduce (hash-values org-ids))])
       #`(define id 
           (new prescription%
                [body (lambda (p e s d) 
                        (let ([name (val-old name)] ...)
                          (parameterize ([current-drug d]
                                         [current-signals s]
                                         [current-prescription p]
                                         [current-event e]) 
                            (define (handle-message! e)
                              (match e 
                                [`(,_ ___ at ,t)
                                 (unless (or (eqv? t (current-time))
                                             (match e [`(inc ,_ ___) #t] [_ #f]))
                                   (error 'time "out of sync"))])
                              (match e
                                [`(setup at ,t)
                                 (send p reset!)]
                                [`(inc time to ,t at ,_)
                                 (void)]
                                [`(device ,d field ,f reports ,v at ,t)
                                 (set-device! d f v)]
                                [`(prn ,msg)
                                 (void)]; no action needed [for now]
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
    (define/public (drug-given! d t)
      (cons! drug-log `(given ,d ,t)))
    (define/public (get-last-time-given d)
      (match d
        [(drug what how amt)
         (define r
           (filter-map
            (match-lambda 
             [`(given ,(drug what _ _) ,t) t])
            drug-log))
         (and (not (empty? r)) (first r))]))
    (define/public (handle-event e)
      (body this e p d))
    (define/public (get-drug) (unbox d))
    (define/public (set-drug! v) (set-box! d v))
    (define/public (get-rate) 
      (unbox d))
    (define/public (reset!)
      (set-box! d #f)
      (set-box! p #f))))
(define (rate [p (current-prescription)])
  (if p
      (send p get-rate)
      (error 'rate "not given a prescription")))
(define (set-rate! a)
  (define p (current-prescription))
  (if p
      (let ([d (send p get-drug)])
        (if d 
            (send p set-drug! (struct-copy drug d [amount a]))
            (error 'set-rate "prescription does not have drug set")))
      (error 'set-rate "not in a prescription")))
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
(define (nerror) (error 'next "used outside of a sequence"))
(define-syntax-parameter next (make-rename-transformer #'nerror))
(define-syntax (seq stx)
  (syntax-parse stx
    [(_ e ...+)
     (with-syntax* ([(state-name ...) (generate-temporaries #'(e ...))]
                    [counter (syntax-local-lift-expression #''(state-name ...))])
       #'(let ([n (lambda () 
                    (if (null? (rest counter))
                        (error 'next "no next state")
                        (set! counter (rest counter))))])
           (syntax-parameterize ([next (make-rename-transformer #'n)])
             (case (first counter)
               [(state-name) e] ...))))]))
(module+ test
  (let ()
    (define (t)
      (seq (begin0 1 (next)) 2 3))
    (check-equal? (t) 1)
    (check-equal? (t) 2)
    (check-equal? (t) 2)))
(define-syntax (begin-drug stx)
  (syntax-parse stx
    [(_ d e ...)
     (with-syntax ([id (generate-temporary)])
       #'(in:begin-drug 'id d e ...))]))
(define (in:begin-drug id d . e)
  (define p (current-signals))
  (define (run)
    (for-each (lambda (x) (x)) (rest (unbox p))))
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
           (lift-resulting-checks e ...)
           (void)))]))
;; (boxof drug) -> (maybe/c timestamp)
(define (get-last-time-given [boxed-drug (current-drug)])
  (define drug (unbox boxed-drug))
  (send (current-prescription) get-last-time-given drug))

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
     #'(lambda () (when test (lift-resulting-checks body ...) (void)))]))
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
(define-syntax (lift-resulting-checks stx)
  (syntax-parse stx
    [(_ e ...)
     #'(begin
        (let ([t e])
          (when (procedure? e)
            (define s (current-signals))
            (set-box! s (append (unbox s) (list t))))) ...)]))
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
(define (prn msg)
  (match (current-event)
    [`(prn ,msg _ ...) #t]
    [_ #f]))
(define (action m)
  (match (current-event)
    [`(action (,m) at ,t) #t]
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
     
     (define updater void)
     (define/public (register-updater! f)
       (set! updater f))
     
     (define resetter! void)
     (define/public (register-resetter! f)
       (set! resetter! f))
     
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
          (log-response! `(action (,x ,@v)) e)]
         [_ (error 'response "unknown response in ~s" r)]))
     
     (define (approve-set! id value event)
       (approve-set/log! id value event))
     (define/public (add-restriction! f) (cons! restrictions f))
     (define/public (check-restrictions!)
       (unless (andmap (lambda (r) (r)) restrictions)
         (error 'set "restrictions violated")))
     (define (approve-set/log! id value event)
       (for ([r log] #:when (response? r))
         (match r
           [(response `(set ,id ,v) event)
            (unless (equal? v value)
              (error 'set "duplicate attempts to change ~s to ~s and ~s with providence ~s"
                     id value v event))]
           [_ (void)])))
     (define/public (handle-event! e)
       (match e
         [`(inc time to ,t at ,_)
          (set! time t)]
         [_ (void)])
       (log-event! e)
       (for ([p (reverse prescription-registry)])
         (for-each (lambda (x) (handle+aprove-response! x)) (send p handle-event e))
         (check-restrictions!))
       (updater))
     
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
       (resetter!)
       (set! time 0)
       (set! log null)))))

(define (reset!)
  (let loop ()
    (unless (queue-empty? message-queue)
      (dequeue! message-queue))
    (send guardian reset!)))
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
(define (send-event:given! d)
  (send-event! `(given ,d)))

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
     (with-syntax ([id (hash-ref org-ids (syntax->datum #'x))])
       #'(begin
           (set-binding-new! id v)
           (respond-with-set! 'x v)))]))


(define (send-event! m)
  (enqueue! message-queue (append m `(at ,(current-time)))))

(define (current-time) (send guardian current-time))

;; now lets have fun with scenarios:

(define-syntax (scenario stx)
  (syntax-parse stx
    #:datum-literals (event initially variables devices)
    [(_ (initially (variables [id:id val] ...)
                   (devices (name:id [f:id d] ...) ...))
        (event (~optional e1) tests1 ...)
        (event e tests ...) ...)
     #`(module+ test
         (with-handlers ([void (lambda (ex) (reset!) (raise ex))])
           (test-begin
            (reset!)
            (send guardian insert-cut!)
            (send-event:setup!)
            (set-binding-old! id val) ...
            (set-binding-new! id val) ...
            (let ([h (hash-ref device-registry 'name)])
              (hash-update! h 'f (const d)) ...) ...
              #,(or (attribute e1) #'(void))
              (run-events-to-completion!)
              tests1 ...
              (begin
                (send guardian insert-cut!)
                e
                (run-events-to-completion!)
                tests ...)
              ...)
           #;
           (displayln (reverse (send guardian full-log)))))]))

;; event calls
(define (pass t)
  (send-event:inc! (time->seconds t)))
(define-syntax (change stx)
  (syntax-parse stx
    [(_ d:id f:id v)
     #'(send-event:device-change! 'd 'f v)]))
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

(define-syntax (with-state stx)
  (syntax-parse stx
    #:datum-literals (drug-of =)
    [(_) #'(void)]
    [(n (x:id = e) b ...)
     #'(begin
         (check-equal? (val-old x) e)
         (n b ...))]
    [(n ((drug-of x:id) = d) b ...)
     #'(begin
         (check-equal? (send x get-drug) d)
         (n b ...))]))

;; matchers
(define-match-expander called
  (lambda (stx)
    (syntax-parse stx
      [(_ (id:id args ...))
       #'(called (id args ...) _)]
      [(_ (id:id args ...) r)
       #'(response `(action (id ,(eq args) ...)) r)])))
(module+ test
  (check-match (response `(action (a 1 2 3)) 'whatever)
               (called (a 1 2 3))))

(define-match-expander asked-to-give
  (lambda (stx)
    (syntax-parse stx
      [(n d)
       #'(n d _)]
      [(_ d r)
       #'(response `(give ,(eq d)) r)])))
(module+ test
  (check-match (response `(give 1) 'whatever)
               (asked-to-give 1)))
(define-match-expander eq 
  (lambda (stx)
    (syntax-parse stx
      [(_ v)
       #'(app (curry equal? v) #t)])))

(module+ test
  (check-match 2
               (eq (add1 1))))

(define (run-events-to-completion!)
  (let loop ()
    (unless (queue-empty? message-queue)
      (send guardian check-restrictions!)
      (define m (dequeue! message-queue))
      (send guardian handle-event! m)
      (loop))))


;;; lets interact! ;;;;;;;;;;;;;;;;;;;;;;;;
(define printer #f)
(define (set-printer! p)
  (set! printer p))

(define-syntax (in:top-interaction stx)
  #`(#%top-interaction
     begin
     (send guardian insert-cut!)
     (begin0
         #,@(syntax-parse stx
              #:datum-literals (initially devices variables pass change
                                          view full-view current-time)
              [(_ initially (variables [id:id val] ...)
                  (devices (name:id [f:id d] ...) ...))
               #'((set-printer! (new task-list%))
                  (reset!)
                  (send guardian insert-cut!)
                  (send-event:setup!)
                  (set-binding-old! id val) ...
                  (set-binding-new! id val) ...
                  (let ([h (hash-ref device-registry 'name)])
                    (hash-update! h 'f (const d)) ...)
                  ...
                  (run-events-to-completion!))]
              [(_ (~or pass change current-time) v ...)
               #`(#,(rest (syntax->list stx))
                  (run-events-to-completion!))]
              [(_ view)
               #'((send guardian trimmed-ouput-log))]
              [(_ full-view)
               #'((send guardian full-log))])
       (when printer (send printer update-tasks)))))

(define task-list%
  (class object%
    (super-new)
    
    (define frame (new frame% [label "please do"]))
    (define pane (new vertical-pane% [parent frame]))
    
    (define/public (update-tasks)
      (for ([r (send guardian trimmed-ouput-log)])
        (match r
          [(response m `(,_ ... at ,t))
           (match m
             [`(give ,d)
              (add-task! (~a "take " d " issued at " t)
                         (thunk (send-event:given! d)))]
             [`(action ,l)
              (add-task! (~a "please " l " issued at " t))]
             [_ (void)])])))

    (define (add-task! str . extras)
      (new button%
           [label str]
           [parent pane]
           [callback
            (lambda (b e)
              (send pane delete-child b)
              (unless (null? extras)
                (send guardian insert-cut!)
                (for-each (lambda (f) (f)) extras)
                (run-events-to-completion!)
                (update-tasks)))]))
    
    (send frame show #t)))
;;; aux ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (cons! stx)
  (syntax-parse stx
    [(_ x:id e:expr)
     #'(set! x (cons e x))]))
(define (undefined? x)
  (eq? x undefined))
