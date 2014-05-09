#lang racket/base
(require syntax/parse (for-syntax syntax/parse racket/base racket/syntax) racket/class racket/undefined
         racket/function racket/bool
         racket/list racket/match rackunit racket/format)
(provide 
 ;; from racket
 #%datum #%top-interaction #%top
 quote
 if cond
 true false not
 or and
 ;; from here
 yes no
 ;; state
 inputs
 state
 any
 ;; prescribing
 give
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
 ;; tests
 scenario
 assert pass change
 called administering administered
 ;; renames
 (rename-out [in:module-begin #%module-begin]
             [in:set! set!]
             [in:after after]
             [in:case case]
             [in:app #%app]
             [in:require require]))

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

(define-syntax (in:set! stx)
  (syntax-parse stx
    [(_ x:id v)
     #'(send x set-value v)]))

;;; requires ;;;;;;;;;;;;;;;;;;;;;;;;
(define require-registry null)
(define-syntax (in:require stx)
  (syntax-parse stx
    [(_ id ...)
     #'(begin
         (define id (new require% [name 'id])) ...)]))
(define require%
  (class object%
    (init-field name)
    (super-new)
    (set! require-registry (cons this require-registry))
    (field [called #f] [args null])
    (define/public (call . a)
      (set! called #t)
      (set! args a))
    (define/public (called?) called)
    (define/public (get-args) args)
    (define/public (reset!)
      (set! called #f)
      (set! args null))))
;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;
(define variable-registry null)
(define-syntax (inputs stx)
  (syntax-parse stx
    #:datum-literals (message variables also-allows requires ->)
    [(_ [nme (message m:str)
             (variables is ...+)
             (also-allows oi ...+)
             (requires r ...)]
        ...)
         #`(begin
             (define nme 
               (new variable%
                    [name 'nme]
                    [initials (list (convert is) ...)]
                    [extras (list (convert oi) ...)]
                    [requirements (parse-requirements r ...)])) ...)]))
(define-syntax (state stx)
  (syntax-parse stx
    #:datum-literals (allows requires)
    [(_ [nme val (allows a ...) (requires r ...)])
     #'(begin
         (define nme
           (new variable%
                [name 'nme]
                [value val]
                [initials (list)]
                [extras (list (convert a) ...)]
                [requirements (parse-requirements r ...)])))]))
(define-syntax (parse-requirements stx)
  (syntax-parse stx
    #:datum-literals (->)
    [(_ [v -> x:id (r ...)] ...)
     #'(list (cons v (lambda () (member (send x get-value) (list r ...)))) ...)]))
(define (convert v)
  (match v
    [(in:pattern f) f]
    [v (curry equal? v)]))
(struct in:pattern (value) #:transparent)
(define (any . c)
  (in:pattern (lambda (v) (ormap (lambda (f) (f v))) c)))
(define variable%
  (class object%
    (init-field name initials extras requirements
                ;; when set this way value is unchecked
                [value undefined])
    (super-new)
    (set! variable-registry (cons this variable-registry))
    (define all-allowed (append initials extras))
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
             (set! value v)
             (when check (check-all-requirements))]
            [else (error 'stuff)]))
    (define/public (reset!)
      (set! value undefined))
    ;; this should be updated to have arbitrary funtions as keys
    (define/public (check-restrictions)
      (define f (assoc (get-value) requirements))
      (when (and f (not ((cdr f))))
        (error 'variable "constrains failed")))))
;; devices ;;;;;;;;;;;;;;;;;;;;;;;;
(define device-registry null)
(define-syntax (device stx)
  (syntax-parse stx
    [(_ (nme (flds ...) ...))
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
    (define in:fields (apply make-hash (map (curryr cons undefined) fields)))
    (set! device-registry (cons this device-registry))
    (define/public (get f)
      (define v (hash-ref in:fields f))
      (if (not (undefined? v))
          v
          (error 'device "not setup")))
    (define/public (set-initial-values . v) 
      (for ([f fields] [v v])
        (set-value f v)))
    (define (set-value f v)
      (hash-update! in:fields f v))
    (define/public (check-restrictions)
      (for ([(_ v) in:fields])
        (when (undefined? v)
          (error 'device "uninitiallized device ~s" name))))))
;; update case to cheat on how we do variables
(define-syntax (in:case stx)
  (syntax-parse stx
    #:datum-literals (else)
    [(_ id e ...)
     #'(case (send id get-value)
         e ...
         [else (error 'case "no cases matched")])]))


(define (check-all-requirements)
  (for ([i (append device-registry variable-registry)])
    (send i check-restrictions)))
;; supporting numbers as units, and objects as functions
(struct number/unit (amount unit) #:transparent)
(define-syntax (in:app stx)
  (syntax-parse stx
    [(_ n:number u:id) #'(number/unit n 'u)]
    [(_ f a ...) 
     #'(if (#%app object? f)
           (send f call a ...)
           (#%app f a ...))]))
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
    ;; (maybe/c (listof thunk))
    (define p (box #f))
    ;; (maybe/c drug)
    (define d (box #f))
    (define/public (event) (body p d))
    (define/public (get-drug) (unbox d))
    (define/public (reset!)
      (set-box! d #f)
      (set-box! p #f))))
;; no drug reset!s the prescription
(define (NO-DRUG)
  (set-box! (current-prescription) #f)
  (set-box! (current-drug) #f))
;; [Listof (cons drug-name timestamp)]
(define given-drugs null)
(define current-drug (make-parameter #f))
(define current-prescription (make-parameter #f))
;; string string number
(struct drug (what how amount) #:transparent)
(define-syntax (prescribe stx)
  (syntax-parse stx
    [(_ name e ...)
     #`(begin
         (define name
           (new prescription%
                [body
                 (lambda (p d) 
                   (parameterize ([current-drug d]
                                  [current-prescription p]) 
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
    [(? number? n) n]))
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
        (devices [dname dvalue ...]))
      (event e a ...) ...)
   #'(module+ test
       (test-begin
        (reset!)
        (send vname set-initial-value value) ...
        (send dname set-initial-values dvalue ...)
        (check-all-requirements)
        (begin e a ... (for-each (lambda (r) (send r reset!)) require-registry)) ...))]))
;; lets do events
(define current-time 0)
(define (get-current-time) current-time)
(define (pass time)
  ;; we go one minute a time
  (for ([_ (in-range 0 (time->seconds time) 60)])
    (set! current-time (+ current-time 60))
    (for-each (lambda (p) (send p event)) prescription-registry)))
(define-syntax (change stx)
  (syntax-parse stx
    [(_ d f v)
     #'(send d set-value 'f v)]))
;; and now lets assert things!
(define-syntax (assert stx)
  (syntax-parse stx
    #:datum-literals (eq?)
    [(_ (eq? i s))
     #'(check-equal? (send i get-value) s)]
    [(_ e:expr) #'(check-true e)]))
;; require% any ... -> boolean
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
  (equal? d (send p get-drug)))
(define (reset!)
  (set! current-time 0)
  (for-each (lambda (p) (send p reset!))
            (append prescription-registry
                    device-registry
                    variable-registry
                    require-registry)))
