#lang racket/base
(require (for-syntax racket/base)
         slideshow/pict
         slideshow/base
         racket/gui/base
         racket/class
         racket/match)

(struct pict+var (main-pict click-resps))
(struct click-resp (pict resp))

(define bindings (make-hash))
(define binding-types (make-hash))

(define whens '())
(define pict+vars '())

(define pict-frame #f)
(define pict-canvas #f)

(define-syntax (decl stx)
  (syntax-case stx ()
    [(_ type id init)
     #'(begin
         (define-syntax (id stx)
           (syntax-case stx ()
             [x 
              (identifier? #'x)
              #'(get-binding 'id)]
             [(x . rest)
              #'(#%app (get-binding 'id) . rest)]))
         (do-decl 'type 'id init))]))

(define (do-decl type id init)
  (hash-set! binding-types id (normalize-type type))
  (hash-set! bindings id init))

(define (normalize-type type)
  (match type
    [`(or ,a ... (or ,b ...) ,c ...)
     (normalize-type `(or ,@a ,@b ,@c))]
    [`(or ,a ...)
     `(or ,@(sort a string<=? #:key symbol->string))]
    [else type]))

(module+ test
  (require rackunit)
  (check-equal? (normalize-type '(or Boolean Integer))
                '(or Boolean Integer))
  (check-equal? (normalize-type 'Boolean)
                'Boolean)
  (check-equal? (normalize-type '(or (or (or A B) C) D))
                '(or A B C D))
  (check-equal? (normalize-type '(or D (or C (or A B))))
                '(or A B C D)))

(define-syntax (-when stx)
  (syntax-case stx ()
    [(_ condition statements ...)
     #'(add-when (letrec ([me (λ () (when condition
                                      (remove-me me)
                                      statements ...))])
                   me))]))

(define (add-when when) (set! whens (cons when whens)))

(define (remove-me me) (set! whens (remq me whens)))

(define (handle-clicked which)
  (set! pict+vars
        (filter
         values
         (for/list ([a-pict+var (in-list pict+vars)])
           (define clicked-on?
             (for/or ([a-click-resp (in-list (pict+var-click-resps a-pict+var))])
               (cond
                 [(eq? (click-resp-pict a-click-resp) which)
                  ((click-resp-resp a-click-resp))
                  #t]
                 [else #f])))
           (cond
             [clicked-on?
              #f]
             [else a-pict+var]))))
  (for ([when-proc (in-list whens)])
    (when-proc))
  (picts-changed))

(define combined-picts (blank 10 10))
(define pict-canvas%
  (class canvas%
    (inherit get-dc get-client-size refresh)
    (define scaled #f)
    (define/public (something-changed)
      (set! scaled #f)
      (refresh))
    (define/override (on-size w h)
      (set! scaled #f)
      (refresh))
    (define/override (on-paint)
      (unless scaled
        (define-values (w h) (get-client-size))
        (set! scaled (scale combined-picts 
                            (min (/ w (pict-width combined-picts))
                                 (/ h (pict-height combined-picts))))))
      (define dc (get-dc))
      (draw-pict scaled dc 0 0))
    (define/override (on-event evt)
      (when (and scaled (send evt button-down?))
        (define x (send evt get-x))
        (define y (send evt get-y))
        (define clicked-on
          (for/or ([a-pict+var (in-list pict+vars)])
            (for/or ([a-click-resp (pict+var-click-resps a-pict+var)])
              (define pict (click-resp-pict a-click-resp))
              (define-values (l t) (lt-find scaled pict))
              (define-values (r b) (rb-find scaled pict))
              (cond
                [(and (<= l x r) (<= t y b))
                 pict]
                [else #f]))))
        (when clicked-on
          (handle-clicked clicked-on))))
    (super-new)))

(define-syntax-rule 
  (prompt color var str-expr)
  (begin var (prompt/proc 'color 'var str-expr)))

(define (prompt/proc color var str)
  (unless pict-frame
    (set! pict-frame (new frame% [width 600] [height 800] [label "POP-PL"]))
    (set! pict-canvas (new pict-canvas% [parent pict-frame]))
    (send pict-frame show #t))
  (set! pict+vars (cons (mk-one-pict (symbol->string color) 
                                     str
                                     var)
                        pict+vars))
  (picts-changed))

(define (picts-changed)
  (define width (inexact->exact (ceiling (sqrt (length pict+vars)))))
  (set! combined-picts
        (cond
          [(null? pict+vars)
           (blank 10 10)]
          [else
           (inset (table width
                         (append (map pict+var-main-pict pict+vars)
                                 (build-list (- width (modulo (length pict+vars) width))
                                             (λ (x) (blank))))
                         cc-superimpose cc-superimpose
                         10 10)
                  10 10)]))
  (send pict-canvas something-changed))

(define (mk-one-pict color str var)
  (define type (hash-ref binding-types var))
  (match type 
    ['Boolean
     (define pict (colored-background (txt->pict str) color))
     (pict+var pict
               (list (click-resp pict (λ () (hash-set! bindings var #t)))))]
    ['(Or Boolean Unknown)
     (define top (txt->pict str))
     (define yes (rounded-white-background (t "Yes")))
     (define no (rounded-white-background (t "No")))
     (define bottom (hc-append 20 yes no))
     (pict+var (colored-background
                (vc-append top (scale bottom (/ (pict-width top) (pict-width bottom))))
                color)
               (list (click-resp yes (λ () (hash-set! bindings var #t)))
                     (click-resp no (λ () (hash-set! bindings var #f)))))]
    [else
     (eprintf "skipping: ~a\n" type)
     (pict+var (blank) '())]))

(define (rounded-white-background p)
  (cc-superimpose (colorize (filled-rounded-rectangle (+ (pict-width p) 10)
                                                      (+ (pict-height p) 10))
                            "white")
                  p))

(define (colored-background p color)
  (define bkg
    (colorize (filled-rectangle 
               (+ (pict-width p) 20)
               (+ (pict-height p) 20))
              color))
  (cc-superimpose bkg p))

(define (txt->pict str)
  (colorize (para #:width 200 #:fill? #f str) "white"))
  
  
(define true #t)
(define false #f)
(define-values (unknown unknown?)
  (let ()
    (struct unknown ())
    (values (unknown) unknown?)))
(define (known? x) (not (unknown? x)))

(define-values (n/a n/a?)
  (let ()
    (struct n/a ())
    (values (n/a) unknown?)))

(define-syntax-rule
  (bang! id expr)
  (do-bang! 'id expr))

(define reads #f)

(define (do-bang! sym v)
  (when reads
    (when (hash-ref reads sym #f)
      (error '<- "assignment after read of ~a: ~e" sym v)))
  (hash-set! bindings sym v))

(define (get-binding sym)
  (when reads
    (hash-set! reads sym #t))
  (hash-ref bindings sym))

(provide decl prompt true false #%datum
         (rename-out [-when when]) if
         or
         and
         #%top-interaction #%app
         known? unknown? unknown n/a n/a?
         begin bang! not)

;; ============================================================

(provide (rename-out [module-begin #%module-begin])
         quote in-units
         (rename-out [units:* *]
                     [units:+ +]
                     [units:< <]
                     [units:<= <=]
                     [units:= =])
         unit/ do
         define list
         get-val set-val! cons
         make-external-function make-device
         whenever assert assert-signaled scenario events)

(define-syntax-rule (module-begin form ...)
  (#%module-begin form ... (run-scenarios)))

;; represents a "whenever" clause
;; means "if test is true perform action"
;; (-> boolean) (-> any)
(struct whenever/struct (test action) #:transparent)
(define whenevers null)
;; (-> boolean) (-> any) -> Void
;; add a new whenever
(define (add-whenever! test action)
  (set! whenevers (cons (whenever/struct test action) whenevers)))

;; expand the whenever stx to setup a new whenever clause
(define-syntax (whenever stx)
  (syntax-case stx ()
    [(_ test action)
     #`(add-whenever! #,(syntax/loc #'test (lambda () test))
                      #,(syntax/loc #'action (lambda () action)))]))

;; run all the whenevers
;; -> Void
(define (run-whenevers)
  (for ([w (in-list (reverse whenevers))])
    (when ((whenever/struct-test w))
      ((whenever/struct-action w)))))

;;
(define scenarios null)
(define (add-scenario! thunks)
  (set! scenarios (cons thunks scenarios)))

(define-syntax-rule (scenario stmt ...)
  (add-scenario! (list (lambda () stmt (void)) ...)))

(define copy (hash))
(define (do-events thunk)
  (map hash-restore! devices copy)
  (set! device-assignments #f)
 (set! device-reads #f)
  (thunk)
  (set! device-assignments (make-hash))
  (set! device-reads (make-hash))
  (set! signals (make-hash))
  (set! reads (make-hash))
  (run-whenevers)
  (set! reads #f))

(define-syntax-rule (events e ...)
  (do-events (lambda () e ... (void))))

(define (run-scenarios)
  (set! copy (map hash-copy devices))
  (define bindings-copy (hash-copy bindings))
  (for ([s (in-list scenarios)])
    (hash-restore! bindings bindings-copy)
    (for ([t (in-list s)])
      (t))
    (printf "scenario done\n")))

(define-syntax-rule (assert e)
  (unless e
    (error 'assert "failed: ~s" 'e)))

(define-syntax-rule (assert-signaled expect e)
  (unless (equal? (hash-ref signals 'e #f) expect)
    (error 'assert "~asignaled: ~a" (if expect "not " "") 'e)))

(define (not x) (and (boolean? x) (if x #f #t)))

(define (in-units a b) a)

(define (units:* a b)
  (* a b))
(define (units:+ a b)
  (+ a b))

(define (units:< a b)
  (run/check < a b))
(define (units:<= a b)
  (run/check <= a b))
(define (units:= a b)
  (run/check = a b))

;; (number number -> boolean) (maybe/c number) (maybe/c number) -> (maybe/c number)
;; any expression might return false, so lets check for that. This allows falses to propigate up
(define (run/check f a b)
  (and a b (f a b) b))
(module+ test
  (check-equal? (units:< 1 2) 2)
  (check-equal? (units:<= 1 2) 2)
  (check-equal? (units:= 1 (units:= 1 2)) #f))

(define (unit/ a b) a)

(define (do action rate) 'ok)

(define device-assignments (make-hash))
(define device-reads (make-hash))

(define (set-val! o f v)
  (define key (cons (hash-ref o '@name) f))

  (when device-reads
    (when (hash-ref device-reads key #f)
      (error '<- "assignment after read of ~a.~a: ~v" (hash-ref o '@name) f v)))

  (when device-assignments
    (define prev (hash-ref device-assignments key (lambda () v)))
    (unless (equal? prev v)
      (error '<- "conflicting assignments to ~a.~a: ~v vs. ~v" (hash-ref o '@name) f prev v))
    (hash-set! device-assignments key v))

  (hash-set! o f v))

(define (get-val o f)
  (when device-reads
    (hash-set! device-reads (cons (hash-ref o '@name #f) f) #t))
  (hash-ref o f))

(define signals (make-hash))

(define (make-external-function name)
  (lambda ()
    (hash-set! signals name #t)
    (displayln name)))

(define devices null)

(define (make-device name . content)
  (define ht (make-hash content))
  (hash-set! ht '@name name)
  (set! devices (cons ht devices))
  ht)

(define (hash-restore! dest src)
  (for ([(k v) (in-hash src)])
    (hash-set! dest k v)))
