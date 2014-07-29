#lang racket
(require racket/shared (prefix-in r: racket))
(require (for-syntax syntax/parse))
(module+ test (require rackunit))


;;; patterns

(struct pat (f) #:mutable)

(struct pattern (p) #:transparent #:mutable)

;; Regex
(struct rx pat (str) #:transparent #:mutable)
; listof pattern
(struct / (values) #:transparent #:mutable)
; listof pattern
(struct seq pat (values) #:transparent #:mutable)
; pattern
(struct ! (pat) #:transparent #:mutable)
; pattern
(struct & (pat) #:transparent #:mutable)
; pattern
(struct + pat (pat) #:transparent #:mutable)
; pattern
(struct * pat (pat) #:transparent #:mutable)
; pattern
(struct ? pat (pat) #:transparent #:mutable)
(struct eof ())
(define EOF (eof))

(define-syntax (parser stx)

  (define-syntax-class pat
    #:attributes (r)
    (pattern x:id
             #:with r #'(pattern x))
    (pattern e:expr
             #:with r #'e))

  (syntax-parse stx
    [(_ [top:id s:pat]
        [name:id r:pat] ...)
     #'(let ([lang (shared 
                    ([top s.r]
                     [name r.r] ...)
                    s)])
         (lambda (str)
           (define t (make-hasheq))
           (define-values (res _) (parse lang str 0 t))
           (displayln t)
           res))]))

(define (parse pat str pos [table (make-hasheq)])
  (define (memoize val end)
    (define sub (hash-ref! table str (make-hasheq)))
    (hash-set! sub pos val))
  (define (get pat pos)
    (hash-ref (hash-ref table pat (hash)) pos #f))
  (define-values (r p)
    (let ([l (get pat pos)])
      (if l
          (apply values l)
          (match pat
            [(pattern p)
             (parse p str pos table)]
            [(app string? #t)
             (parse (rx (const pat) (regexp-quote pat)) str pos table)]
            [(rx f reg)
             (define locs (regexp-match-positions* reg str))
             (define has (and locs (assoc pos locs)))
             (define end (and has 
                              (cdr has)))
             (if (not end)
                 (values #f pos)
                 (values (f (substring str pos end) end) end))]
            [(/ (list* pats))
             (let loop ([pats pats])
               (cond [(null? pats)
                      (values #f pos)]
                     [else 
                      (define pat (first pats))
                      (define-values (r p) (parse pat str pos table))
                      (if r
                          (values r p)
                          (loop (rest pats)))]))]
            [(seq f (list* pats))
             (let loop ([rs null] [pos pos] [pats pats])
               (cond [(null? pats)
                      (values (f (reverse rs) pos) pos)]
                     [else
                      (define pat (first pats))
                      (define-values (r p) (parse pat str pos table))
                      (if (not r)
                          (values #f pos)
                          (loop (cons r rs) p (rest pats)))]))]
            [(! pat)
             (define-values (r p) (parse pat str pos table))
             (if (not r)
                 (values #t p)
                 (values #f p))]
            [(& pat)
             (define-values (r p) (parse pat str pos table))
             (if r
                 (values r pos)
                 (values #f pos))]
            [(+ f pat)
             (parse (seq (list  pat (* pat)) f) str pos table)]
            [(* f pat)
             (let loop ([pos pos] [res null])
               (define-values (r p) (parse pat str pos table))
               (if r
                   (loop p (cons r null))
                   (values (f (reverse res) p) p)))]
            [(? f pat)
             (define-values (r p) (parse pat str pos table))
             (if r
                 (values (f r p) p)
                 (values #t p))]
            [(eof)
             (values (= pos (string-length str)) pos)]))))
  (when r
    (memoize r p))
  (values r p))


(module+ test
  (let ()
    (define p
      (parser
       [Top (seq (lambda (r p) (first r))
                 (list Expr EOF))]
       [Expr Sum]
       [Sum (seq (lambda (l p) (apply r:+ (flatten l)))
                 (list Product (* (lambda (l p) l)
                                  (seq (match-lambda** 
                                        [((list "+" n) _) n]
                                        [((list "-" n) _) (- n)])
                                       (list (/ (list "+" "-")) Product)))))]
       [Product (seq (lambda (l p) (apply r:* (flatten l)))
                     (list Value (* (lambda (l p) l) 
                                    (seq (match-lambda** 
                                          [((list "*" n) _) n]
                                          [((list "/" n) _) (r:/ n)])
                                         (list (/ (list "*" "/")) Value)))))]
       [Value (/ (list (rx (lambda (r p) (string->number r)) #rx"[0-9]+")
                       (seq (lambda (l p) (second l))
                            (list "(" Expr ")"))))]))
    (check-equal? (p "1") 1)
    (check-equal? (p "1+2") 3)
    (check-equal? (p "1+2/2") 2)
    (check-equal? (p "(1+2)/2") 3/2))
  (let ()
    (define orig-stx (read-syntax 'derp (open-input-bytes #"x")))
    (define (->stx e pos)
      (datum->syntax #f e (vector #f #f #f pos #f) orig-stx))
    (define p
      (parser
       [Top (seq (lambda (r p) (first r))
                 (list Expr EOF))]
       [Expr Sum]
       [Sum (seq (lambda (l p) (->stx (cons '+ (flatten l)) p))
                 (list Product (* (lambda (l p) l)
                                  (seq (match-lambda** 
                                        [((list "+" n) p) (->stx n p)]
                                        [((list "-" n) p) (->stx `(- ,n) p)])
                                       (list (/ (list "+" "-")) Product)))))]
       [Product (seq (lambda (l p) (->stx (cons '* (flatten l)) p))
                     (list Value (* (lambda (l p) l) 
                                    (seq (match-lambda** 
                                          [((list "*" n) p) (->stx n p)]
                                          [((list "/" n) p) (->stx`(/ ,n) p)])
                                         (list (/ (list "*" "/")) Value)))))]
       [Value (/ (list (rx (lambda (r p) (->stx (string->number r) p)) #rx"[0-9]+")
                       (seq (lambda (l p) (second l))
                            (list "(" Expr ")"))))]))
    (define t1 (p "(1+2)/3"))
    (displayln t1)
    ;(check-equal? (eval-syntax t1 (make-base-namespace)) 1)
    ))
