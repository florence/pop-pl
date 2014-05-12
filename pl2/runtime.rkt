#lang racket/base
(require syntax/parse (for-syntax syntax/parse racket/base racket/syntax) racket/class racket/undefined
         racket/function racket/bool
         racket/list racket/match rackunit racket/format)
(provide 
 ;; from racket
 #%datum #%top-interaction #%top
 quote
 if cond else
 true false not
 or and
 displayln
 ;; from here
 yes no time?
 get-current-time
 import
 ;; requirements
 is is-not
 prevent
 (rename-out [in:require require])
 ;; state
 inputs
 devices
 state
 any
 ;; prescribing
 from
 rate
 set-rate!
 give
 get
 drug
 prescribe
 NO-DRUG
 prn
 ?
 every
 whenever
 do-only-once
 begin-drug
 instructions
 ensure
 ;; tests
 scenario
 assert pass change
 called administering administered
 ;; renames
 (rename-out [in:module-begin #%module-begin]
             [in:set! set!]
             [in:after after]
             [in:case case]
             [in:app #%app])
 (rename-out [units:* *]
             [units:+ +]
             [units:< <]
             [units:> >]
             [units:<= <=]
             [units:>= >=]
             [units:= =]))

(define (undefined? x)
  (eq? x undefined))
(define-syntax (in:module-begin stx)
  (syntax-parse stx
    [(_ e ...)
     #'(#%module-begin
        (module+ test (require rackunit))
        e ...)]))
; values
(define yes #t)
(define no #f)

(define (time? v)
  (and (number/unit? v)
       (case (number/unit-unit v)
         [(day days hour hours minute minutes second seconds) #t]
         [else #f])))

(define-syntax (in:set! stx)
  (syntax-parse stx
    [(_ x:id v)
     #'(send x set-value v)]))

;;; imports ;;;;;;;;;;;;;;;;;;;;;;;;
(define import-registry null)
(define-syntax (import stx)
  (syntax-parse stx
    [(_ id ...)
     #'(begin
         (define id (new import% [name 'id])) ...)]))
(define import%
  (class object%
    (init-field name)
    (super-new)
    (set! import-registry (cons this import-registry))
    (field [called #f] [args null])
    (define/public (call . a)
      (set! called #t)
      (set! args a))
    (define/public (called?) called)
    (define/public (get-args) args)
    (define/public (reset!)
      (set! called #f)
      (set! args null))))
;;; frame conditions/global requirements ;;;;;;;;;;;;;;;;;;;
(define requirements-registry null)
(define-syntax (in:require stx)
  (syntax-parse stx
    #:datum-literals (whenever)
    [(name guard whenever condition)
     #`(add-requirement!
        (lambda () (unless (implies condition guard)
                (raise-condition-error 'name `#,(syntax->datum stx)))))]))
(define-syntax (prevent stx)
  (syntax-parse stx
    #:datum-literals (whenever)
    [(name guard whenever condition)
     #`(add-requirement! 
        (lambda () (when (implies condition guard)
                (raise-condition-error 'name `#,(syntax->datum stx)))))]))
(define (raise-condition-error name form)
  (error name "condition failed in: ~s" form))
(define is equal?)
(define is-not (negate equal?))
(define (add-requirement! f)
  (set! requirements-registry (cons f requirements-registry)))
;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;
(define variable-registry null)
(define-syntax (inputs stx)
  (syntax-parse stx
    #:datum-literals (message variables also-allows requires ->)
    [(_ [nme (message m:str)
             (variables is ...+)
             (also-allows oi ...+)]
        ...)
         #`(begin
             (define nme 
               (new variable%
                    [name 'nme]
                    [initials (list (convert is) ...)]
                    [extras (list (convert oi) ...)])) ...)]))
(define-syntax (state stx)
  (syntax-parse stx
    #:datum-literals (allows requires)
    [(_ [nme val (allows a ...)] ...)
     #'(begin
         (define nme
           (new variable%
                [name 'nme]
                [value val]
                [initials (list)]
                [extras (list (convert a) ...)])) ...)]))
(define (convert v)
  (match v
    [(in:pattern f) f]
    [v (curry equal? v)]))
(struct in:pattern (value) #:transparent)
(define (any . c)
  (in:pattern (lambda (v) (ormap (lambda (f) (f v)) c))))
(define variable%
  (class object%
    (init-field name initials extras
                ;; when set this way value is unchecked
                [value undefined])
    (inspect #f)
    (define initial-value value)
    (super-new)
    (set! variable-registry (cons this variable-registry))
    (define all-allowed (append initials extras))
    (define/public (get-name) name)
    (define/public (get-value) 
      (if (not (undefined? value))
          value 
          (error 'variable "variable ~s has no value" name)))
    (define/public (set-initial-value x)
      (set/restrict x initials #f))
    (define/public (set-value x)
      (set/restrict x all-allowed #t))
    (define (set/restrict v l check)
      (cond [(ormap (lambda (?) (? v)) l)
             (set! value v)]
            [else (error 'stuff)]))
    (define/public (reset!)
      (set! value initial-value))))
;; devices ;;;;;;;;;;;;;;;;;;;;;;;;
(define device-registry null)
(define-syntax (devices stx)
  (syntax-parse stx
    [(_ (nme (flds ...)) ...)
     #`(begin
         (define nme (new device% [name 'nme] [fields '(flds ...)])) ...)]))
(define-syntax (get stx)
  (syntax-parse stx
    [(_ v f)
     #'(send v get 'f)]))
(define device%
  (class object%
    (super-new)
    (init-field name fields)
    (define in:fields (make-hash (map (curryr cons undefined) fields)))
    (set! device-registry (cons this device-registry))
    (define/public (get f)
      (define v (hash-ref in:fields f))
      (if (not (undefined? v))
          v
          (error 'device "not setup")))
    (define/public (set-initial-values . v) 
      (for ([f fields] [v v])
        (set-value f v)))
    (define/public (set-value f v)
      (hash-update! in:fields f (const v)))
    (define/public (reset!)
      (for ([(k _) in:fields])
        (hash-update! in:fields k (const undefined))))
    (define/public (check-restrictions)
      (for ([(_ v) in:fields])
        (when (undefined? v)
          (error 'device "uninitiallized device ~s" name))))
    (add-requirement!
     (lambda () (send this check-restrictions)))))
;; update case to cheat on how we do variables
(define-syntax (in:case stx)
  (syntax-parse stx
    #:datum-literals (else)
    [(_ id e ...)
     #'(case (send id get-value)
         e ...
         [else (error 'case "no cases matched")])]))


(define (check-all-requirements)
  (for-each (lambda (f) (f)) requirements-registry))
;; supporting numbers as units, and objects as functions
(struct number/unit (amount unit) #:transparent)
(define-syntax (in:app stx)
  (syntax-parse stx
    [(_ n:number u:id) #'(number/unit n 'u)]
    [(_ f a ...) 
     (with-syntax ([(arg ...) #'((#%app var?->val a) ...)])
       #'(if (#%app object? f)
             (send f call arg ...)
             (#%app f arg ...)))]))
(define (var?->val v)
  (if (and (object? v) 
           (let-values ([(c _) (object-info v)])
             (equal? c variable%)))
      (send v get-value)
      v))
(define-syntax (? stx)
  (syntax-parse stx
    [(_ n:number u:id) #'(number/unit n 'u)]))

;;; prescriptions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [Listof thunk]
(define prescription-registry null)
(define prescription%
  (class object%
    (init-field body)
    (inspect #f)
    (super-new)
    (set! prescription-registry (cons this prescription-registry))
    ;; (maybe/c (cons symbol (listof thunk)))
    (define p (box #f))
    ;; (maybe/c drug)
    (define d (box #f))
    (define/public (get-rate)
      (if (unbox d)
          (drug-amount (unbox d))
          (error 'rate "not drug set")))
    (define/public (event) (body this p d))
    (define/public (get-drug) (unbox d))
    (define/public (set-drug dr) (set-box! d dr))
    (define/public (reset!)
      (set-box! d #f)
      (set-box! p #f))))
(define (rate [p (current-context)])
  (if p
      (send p get-rate)
      (error 'rate "not given a prescription")))
(define (set-rate! a)
  (define p (current-context))
  (if p
      (let ([d (send p get-drug)])
        (if d 
            (send p set-drug (struct-copy drug d [amount a]))
            (error 'set-rate "prescription does not have drug set")))
      (error 'set-rate "not in a prescription")))
;; no drug reset!s the prescription
(define (NO-DRUG)
  (set-box! (current-prescription) #f)
  (set-box! (current-drug) #f))
;; [Listof (cons drug-name timestamp)]
(define given-drugs null)
(define current-drug (make-parameter #f))
(define current-prescription (make-parameter #f))
(define current-context (make-parameter #f))
;; string string number
(struct drug (what how amount) #:transparent)
(define-syntax (prescribe stx)
  (syntax-parse stx
    [(_ name e ...)
     #`(begin
         (define name
           (new prescription%
                [body
                 (lambda (c p d) 
                   (parameterize ([current-drug d]
                                  [current-prescription p]
                                  [current-context c]) 
                     e ...))])))]))
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
        (set! given-drugs (cons (cons (drug-what drug) (get-current-time)) given-drugs)))
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
  (let ([p (assoc (drug-what drug) given-drugs)])
    (and p (cdr p))))

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
  ((get-current-time) . >= . (+ (time->seconds after) (time->seconds before))))
(define (set-time! t)
  (set! current-time t))
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

;;; scenario ;;;;;;;;;;;;;;;;;;
(define-syntax (scenario stx)
 (syntax-parse stx
   #:datum-literals (variables)
   [(_ (initially 
        (variables [vname vvalue] ...)
        (devices [dname dvalue ...] ...))
      (event e a ...) ...)
   #'(module+ test
       (test-begin
        (reset!)
        (send vname set-initial-value vvalue) ...
        (send dname set-initial-values dvalue ...) ...
        (check-all-requirements)
        (begin e a ... (for-each (lambda (r) (send r reset!)) import-registry)) ...))]))
;; lets do events
(define current-time 0)
(define (get-current-time) current-time)
(define (pass time)
  ;; we go one minute a time
  (for ([_ (in-range 0 (time->seconds time) 60)])
    (set! current-time (+ current-time 60))
    (run!)))
(define-syntax (change stx)
  (syntax-parse stx
    [(_ d f v)
     #'(begin (send d set-value 'f v)
              (run!))]))
(define (run!)
  (for-each (lambda (p) (send p event)) prescription-registry)
  (check-all-requirements))
;; and now lets assert things!
(define-syntax (assert stx)
  (syntax-parse stx
    #:datum-literals (eq?)
    [(_ (eq? i s))
     (syntax/loc stx (check-equal? (send i get-value) s))]
    [(_ e:expr) (syntax/loc stx (check-true e))]))
;; import% any ... -> boolean
(define (called r . a)
  (and (send r called?)
       (equal? (send r get-args) a)))
;; string number time -> boolean
(define (administered type amount range)
  (define mintime (- (get-current-time) (time->seconds range)))
  (equal?
   amount
   (length
    (filter (lambda (x)
              (and (equal? (car x) type)
                   (>= (cdr x) mintime)))
            given-drugs))))
(define (administering p d)
  (displayln (send p get-drug))
  (displayln d)
  (equal? d (send p get-drug)))
(define (reset!)
  (set! current-time 0)
  (for-each (lambda (p) (send p reset!))
            (append prescription-registry
                    device-registry
                    variable-registry
                    import-registry)))

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
;; comparison, but deals with units
(define units:* (run/check *))
(define units:+ (run/check +))
(define units:/ (run/check /))
(define units:< (run/check <))
(define units:> (run/check >))
(define units:= (run/check =))
(define units:<= (run/check <=))
(define units:>= (run/check >=))
