#lang racket/base
(require syntax/parse (for-syntax syntax/parse) racket/class racket/undefined)
(provide 
 yes no
 if
 inputs
 give
 drug
 NO-DRUG
 every
 after
 do-only-once
 begin-using
 (rename-out [in:set! set!]
             [in:case case]
             [in:app #%app]
             [in:require require]))

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
    (define/public (reset)
      (set! called #f)
      (set! args null))))

;;; inputs ;;;;;;;;;;;;;;;;;;;;;;;;;
(define input-registry null)
(define-syntax (inputs stx)
  (syntax-parse stx
    #:datum-litterals (message inputs also-allows requires ->)
    [(_ [nme (message m:str)
             (inputs is ...+)
             (also-allows oi ...+)
             (requires [v -> x:id (r ...)] ...)]
        ...)
         #`(begin
             (define nme 
               (new input%
                    [name 'nme]
                    [initials (list is ...)]
                    [extras (list io ..)]
                    [requirements (list (list v (lambda () (member (send x get-value) (list r ...)))) ...)])) ...)]))
(define input%
  (class object%
    (init-field name initials extras requirements)
    (field [value undefined])
    (super-new)
    (set! require-registry (cons this require-registry))
    (define all-allowed (append initials extras))
    (define/public (get-value) 
      (if (not (undefined? name))
          value 
          (error 'stuff)))
    (define/public (set-initial-value x)
      (set/restrict x initials))
    (define/public (set-value x)
      (set/restrict x all-allowed))
    (define (set/restrict v l)
      (if (and (member x l)
               (let ([f (assoc x requirements)])
                 (or (not f) (f))))
          (set! value x)
          (error 'stuff)))
    (define/public (reset)
      (set! value undefined))))

;; update case to cheat on how we do inputs
(define-syntax (in:case stx)
  (syntax-parse stx
    [(_ id e ...)
     #'(case (send id get-value) e ...)]))

;; supporting numbers as units, and objects as functions
(struct number/unit (amount unit) #:transparent)
(define-syntax (in:app stx)
  (syntax-parse stx
    [(_ n:number u:id) #'(number/unit n 'u)]
    [(_ f a ...) 
     #'(if (#%app object? f)
           (send f call a ...)
           (#%app f a ...))]))

;;; prescriptions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [Listof thunk]
(define prescription-registry null)
(define prescription%
  (class object%
    (init-field body)
    (super-new)
    (set! prescription-registry (cons x prescription-registry))
    ;; (maybe/c (listof thunk))
    (define p (box #f))
    ;; (maybe/c drug)
    (define d (box #f))
    (define/public (on-event) (body p d))
    (define/public (reset)
      (set-box! d #f)
      (set-box! p #f))))
;; no drug resets the prescription
(define-syntax NO-DRUG
  (make-rename-transformer
   #'(begin (set-box! (current-prescription) #f)
            (set-box! (current-drug) #f))))
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
                                  [current-percription p]) 
                     e ...))])))]))
(define-syntax (begin-drug stx)
  (syntax-parse stx
    [(_ d e ...)
     (with-syntax ([id (generate-temporary)])
       #'(in:begin-drug id d e ...))]))
(define (in:begin-drug id d . e)
  (define p (current-prescription))
  (cond [(and (box? p) (eq? (first (unbox p)) id) (unbox p)) =>
         (lambda (l) (foreach (lambda (x) (x)) (rest l)))]
        [p
         (set-box! p (current-prescription) (cons id e))
         (set-box! (current-drug) d)]
        [else
         (error 'begin-drug "used outside of prescription")]))
;; (maybe/c drug) -> void
(define (give [drug (current-drug)])
  (set! given-drugs (cons (cons (drug-what drug) (get-current-time)) given-drugs)))
;;; everys are based off the last time that drug was given
(define-syntax (every stx)
  (syntax-parse stx
    #:syntax-literals (prn)
    [(_ t e ...)
     #'(lambda ()
         (define t (get-last-time-given))
         (when (or (not t) (> (+ (get-current-time) (time->seconds t)) t))
           e ...))]))
;; drug -> (maybe/c timestamp)
(define (get-last-time-given [drug (current-drug)])
  (assoc (drug-what drugs-given) drug))

;; afters record when the are added, run once, then go away
(define-syntax (after stx)
  (syntax-parse
      [(_ t e ...)
       #'(let ([start (get-current-time)])
           (define (actual)
             (when (> (+ (get-current-time) (time->seconds t)) start)
               e ...
               (set! actual void)))
           (lambda () (actual)))]))
(module+ test
  (let ()
    (define r #f)
    (define t (after (number/unit 0 'second) (set! r #t)))
    (t)
    (check-true r)
    (set! r #f)
    (t)
    (check-false r)))
;; number/unit -> timestamp
(define (time->seconds n)
  (match n
    [(number/unit n (or 'seconds 'second)) n]
    [(number/unit n (or 'minutes 'minute)) (* n 60)]
    [(number/unit m (or 'hours 'hour))
     (* n
        60 
        (time->seconds (number/units n 'minutes)))]
    [(number/unit n (or 'days 'day))
     (* n
        24
        (time->seconds (number/unit 1 'hour)))]))
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

;;; scenarions ;;;;;;;;;;;;;;;;;;
(define-syntax (scenario stx)
 (syntax-parse stx
   #:datum-literals (inputs)
  [(_ (inputs [name value] ...)
      b ...)
   #'(module+ test
       (test-begin
        (reset)
        b ...))]))
;; lets do events
(define time 0)
(define (get-current-time) time)
(define-syntax (events stx)
  (syntax-parse stx
    [(_ e ...)
     #'(begin e ...)]))
(define (pass time)
  ;; we go one minute a time
  (for ([_ (in-range 0 (time->seconds time) 60)])
    (set! time (+ time 60))
    (foreach (lambda (p) (p)) perscription-registry)))
;; and now lets assert things!
(define-syntax (assert stx)
  (syntax-parse stx
    [(_ e:expr) #'(check-true e)]
    [(_)]))

;; todo called
;; to administered

(define (reset)
  (set! time 0)
  (for-each (lambda (p) (send p reset))
            (append prescription-registry
                    input-registry
                    require-registry)))
