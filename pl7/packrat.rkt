#lang racket
(provide 
 (prefix-out : (combine-out lit rx seq ! & + * ? / EOF))
 define-parser/colorer
 (struct-out position)
 position->vector)


(require racket/shared (prefix-in r: racket))
(require (for-syntax syntax/parse))
(module+ test (require rackunit))


(struct location (line col pos) #:transparent)
(struct position (source line col start span) #:transparent)
(define (make-location in)
  (call-with-values (thunk (port-next-location in)) location))
(define (make-position l1 l2 [name #f])
  (position (object-name name)
            (location-line l1)
            (location-col l1)
            (location-pos l1)
            (- (location-pos l2) (location-pos l1))))
(define (position->vector p [name #f])
  (vector (or name (position-source p))
          (position-line p)
          (position-col p)
          (position-start p)
          (position-span p)))

(define (pat-color p)
  (colorable-color p))

;;; patterns
(struct pat (f) #:mutable)
(struct pattern (name p) #:transparent #:mutable)
(struct colorable pat (color) #:mutable)
;; Regex
(struct lit colorable (str) #:mutable #:transparent)
(struct rx colorable (pat) #:transparent #:mutable)
                                        ; listof pattern
(struct / (values) #:transparent #:mutable)
                                        ; listof pattern
(struct seq colorable (values) #:transparent #:mutable)
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

(define-syntax (define-parser/colorer stx)
  (syntax-parse stx
    [(_ (p:id lex:id col:id)
        [top:id s:expr]
        [name:id r:expr] ...
        (~optional (~seq #:colors [x:id c:str] ...)))
     (with-syntax ([colors (if (not (attribute x))
                               #'(hash)
                               #'(make-hash (list (cons 'x c) ...)))])
       #'(define-values (p lex col)
           (let ([lang (shared 
                        ([top (pattern 'top s)]
                         [name (pattern 'name r)] ...)
                        s)]
                 [color colors])
             (values (lambda (in #:debug [debug #f])
                       (define t (make-hasheq))
                       (define-values (res p) (parse lang in t #:debug debug))
                       (when (and debug (not res))
                         (write `(failed at ,p with table ,t))
                         (write "\n"))
                       res)
                     (lang->colorer lang)
                     (lambda (q) (hash-ref colors q 'no-color))))))]))

(define (parse pat i [table (make-hasheq)] #:debug [a:debug #f])
  (define in (if (string? i) (open-input-string i) i))
  (port-count-lines! in)
  (define start (make-location in))

  (define (get-pos) (make-position start (make-location in) in))
  (define (parse* pat) (parse pat in table #:debug a:debug))
  (define (memoize val)
    ;;FIXME
    (define sub (hash-ref! table in (make-hash)))
    (hash-set! sub start val))
  (define (get pat)
    (hash-ref (hash-ref table pat (hash)) start #f))
  (define (debug e)
    (when a:debug
      (write e)
      (displayln "")
      (displayln "")
      (displayln "")))
  (define-values (r p)
    (let ([v (get pat)])
      (if v
          (values v (get-pos)) 
          (match pat
            [(pattern _ p)
             (parse* p)]
            [(app string? #t)
             (parse* (lit (const pat) #f pat))]
            [(lit f c pat)
             (parse* (rx f c (regexp-quote pat)))]
            [(rx f _ reg)
             (define locs (regexp-match-peek-positions* reg in))
             (define has (and locs (assoc 0 locs)))
             (define end (and has 
                              (cdr has)))
             (debug `(,reg matched ,locs))
             (if (not end)
                 (values #f (get-pos))
                 (values (f (read-string end in) (get-pos)) (get-pos)))]
            [(/ (list* pats))
             (let loop ([pats pats])
               (cond [(null? pats)
                      (values #f (get-pos))]
                     [else 
                      (define pat (first pats))
                      (define-values (r p) (parse* pat))
                      (if r
                          (values r (get-pos))
                          (loop (rest pats)))]))]
            [(seq f _ (list* pats))
             (let loop ([rs null] [pats pats])
               (cond [(null? pats)
                      (define p (get-pos))
                      (values (f (reverse rs) p) p)]
                     [else
                      (define pat (first pats))
                      (define-values (r p) (parse* pat))
                      (if (not r)
                          (values #f (get-pos))
                          (loop (cons r rs) (rest pats)))]))]
            [(! pat)
             (define-values (r p) (parse* (& pat)))
             (if (not r)
                 (values #t (get-pos))
                 (values #f (get-pos)))]
            [(& pat)
             (define init (file-position in))
             (define-values (r p) (parse* pat))
             (define loc (get-pos))
             ;(set-port-next-location! in a b c)
             (file-position in init)
             (if r
                 (values r loc)
                 (values #f (get-pos)))]
            [(+ f pat)
             (parse* (seq (lambda (r p) (f (cons (first r) (second r)) p))
                         #f
                         (list pat (* (lambda (r p) r) pat))))]
            [(* f pat)
             (let loop ([res null])
               (define-values (r p) (parse* pat))
               (if r
                   (loop (cons r res))
                   (values (f (reverse res) (get-pos)) (get-pos))))]
            [(? f pat)
             (define-values (r p) (parse* pat))
             (if r
                 (values (f r (get-pos)) (get-pos))
                 (values #t p))]
            [(eof)
             (values (eof-object? (peek-byte in)) (get-pos))]))))
  
  (when (pattern? pat)
      (debug `(pat: ,pat res: ,r at: ,(get-pos))))
  
  (when r
    (memoize r))
  (values r (get-pos)))

(define (lang->colorer lang)
  ;; build table
  (define tok->type null)
  (define seen (mutable-seteq))
  (define (add! pat)
    (set! tok->type (cons pat tok->type)))
  (let loop ([cur lang])
    (unless (set-member? seen cur)
      (set-add! seen cur)
      (match cur
        [(app string? #t) (add! cur)]
        [(lit _ color s) (when color (add! cur))]
        [(rx _ color s) (when color (add! cur))]
        [(? f pat) (loop pat)]
        [(* f pat) (loop pat)]
        [(+ f pat) (loop pat)]
        [(& pat) (loop pat)]
        [(! pat) (loop pat)]
        [(pattern _ pat) (loop pat)]
        [(seq f col pats) 
         (when col
           (add! cur))
         (for-each loop pats)]
        [(/ pats) (for-each loop pats)]
        [(eof) (void)])))
  ;;
  (lambda (in)
    (define-values (_ __ start) (port-next-location in))
    (let loop ([tok->type tok->type])
      (cond [(null? tok->type) (values #f 'eof #f #f #f)]
            [else
             (define pat (first tok->type))
             (define-values (r p) (parse pat in))
             (if (not r)
                 (loop (rest tok->type))
                 (let ([offset (- (position-start p) start)])
                   (values r (pat-color pat) #f offset (r:+ offset (position-span p)))))]))))


(module+ test
  (let ()
    (define-parser/colorer (p l c)
      [Top (seq (lambda (r p) (first r))
                #f
                (list Expr EOF))]
      [Expr Sum]
      [Sum (seq (lambda (l p) (apply r:+ (flatten l)))
                #f
                (list Product (* (lambda (l p) l)
                                 (seq (match-lambda** 
                                       [((list "+" n) _) n]
                                       [((list "-" n) _) (- n)])
                                      #f
                                      (list (/ (list "+" "-")) Product)))))]
      [Product (seq (lambda (l p) (apply r:* (flatten l)))
                    #f
                    (list Value (* (lambda (l p) l) 
                                   (seq (match-lambda** 
                                         [((list "*" n) _) n]
                                         [((list "/" n) _) (r:/ n)])
                                        #f
                                        (list (/ (list "*" "/")) Value)))))]
      [Value (/ (list (rx (lambda (r p) (string->number r)) #f #rx"[0-9]+")
                      (seq (lambda (l p) (second l))
                           #f
                           (list "(" Expr ")"))))])
    (check-equal? (p "1") 1)
    (check-equal? (p "1+2") 3)
    (check-equal? (p "1+2/2") 2)
    (check-equal? (p "(1+2)/2") 3/2))
  ;; test that * is greedy
  (let ()
    (define-parser/colorer (p l c)
      [X (* (lambda (r p) r) (rx (lambda (r p) r) #f #rx"."))])
    (check-equal? (p "abc")
                  (list "a" "b" "c")))
  (let ()
    (define-parser/colorer (p l c)
      [Expr (seq (lambda (l p) l)
                 #f
                 (list (* (lambda (l p) l)
                        (seq (lambda (l p) l)
                             #f
                             (list
                              (lit (lambda (l p) l) 'syntax "when")
                              WHITESPACE
                              OPEN
                              WHITESPACE
                              CLOSE
                              WHITESPACE)))
                       (eof)))]
      [WHITESPACE (rx (lambda (r p) r) 'white-space #rx" +")]
      [OPEN  (lit (lambda (r p) r) 'bound "{")]
      [CLOSE (lit (lambda (r p) r) 'bound "}")]
      #:colors
      [syntax "red"]
      [bound "blue"])
    (let-values ([(_ type match start end) (l (open-input-string "when {}"))])
      (check-equal? type 'syntax)
      (check-equal? start 0)
      (check-equal? end 4))
    (let-values ([(_ type match start end) (l (open-input-string "when {}"))])
      (check-equal? type 'syntax)
      (check-equal? start 0)
      (check-equal? end 4)))
                                        ; (let ()
                                        ;   (define orig-stx (read-syntax 'derp (open-input-bytes #"x")))
                                        ;   (define (->stx e pos)
                                        ;     (datum->syntax #f e (vector #f #f #f pos #f) orig-stx))
                                        ;   (define p
                                        ;     (parser
                                        ;      [Top (seq (lambda (r p) (first r))
                                        ;                (list Expr EOF))]
                                        ;      [Expr Sum]
                                        ;      [Sum (seq (lambda (l p) (->stx (cons '+ (flatten l)) p))
                                        ;                (list Product (* (lambda (l p) l)
                                        ;                                 (seq (match-lambda** 
                                        ;                                       [((list "+" n) p) (->stx n p)]
                                        ;                                       [((list "-" n) p) (->stx `(- ,n) p)])
                                        ;                                      (list (/ (list "+" "-")) Product)))))]
                                        ;      [Product (seq (lambda (l p) (->stx (cons '* (flatten l)) p))
                                        ;                    (list Value (* (lambda (l p) l) 
                                        ;                                   (seq (match-lambda** 
                                        ;                                         [((list "*" n) p) (->stx n p)]
                                        ;                                         [((list "/" n) p) (->stx`(/ ,n) p)])
                                        ;                                        (list (/ (list "*" "/")) Value)))))]
                                        ;      [Value (/ (list (rx (lambda (r p) (->stx (string->number r) p)) #rx"[0-9]+")
                                        ;                      (seq (lambda (l p) (second l))
                                        ;                           (list "(" Expr ")"))))]))
                                        ;   (define t1 (p "(1+2)/3"))
                                        ;   (displayln t1)
                                        ;                                       ;(check-equal? (eval-syntax t1 (make-base-namespace)) 1)
                                        ;   )
  )
