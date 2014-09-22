#lang racket
(provide 
 (prefix-out : (combine-out lit rx seq ! & + * ? / EOF))
 define-parser/colorer
 (struct-out position)
 position->vector)


(require racket/shared (prefix-in r: racket))
(require racket/stxparam)
(require (for-syntax syntax/parse))
(module+ test (require rackunit))


(struct location (line col pos) #:transparent)
(struct position (source line col start span) #:transparent)
(define (make-location in)
  (define-values (l c p) (port-next-location in))
  (if (and (not l) c)
      (location 0 c p)
      (location l c p)))
(define (make-position l1 l2 name)
  (position name
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
;; Regex
(struct lit pat (str) #:mutable #:transparent)
(struct rx pat (pat) #:transparent #:mutable)
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

(struct colorable (color pattern) #:transparent)
(define (make-colorables c . ps)
  (map (curry colorable c) ps))
(define-syntax (define-parser/colorer stx)
  (syntax-parse stx
    [(_ (p:id lex:id)
        [top:id s:expr]
        [name:id r:expr] ...
        (~optional (~seq #:tokens (color:id tok:id ...) ...)))
     (with-syntax ([toks
                    (if (attribute tok)
                        #'(flatten (list (make-colorables 'color tok ...) ...))
                        #'(list))])
       #'(define-values (p lex top name ...)
           (shared 
            ([top (pattern 'top s)]
             [name (pattern 'name r)] ...)
            (values (lambda (in #:debug [debug #f] #:pattern [pattern top] #:name [nme #f])
                      (define t (make-hasheq))
                      (define-values (res p) (parse* pattern in (if nme nme (object-name in)) t #:debug debug))
                      (when (and debug (not res))
                        (write `(failed at ,p with table ,t))
                        (write "\n"))
                      (values res p))
                    (lang->colorer toks)
                    top
                    name ...))))]))
(define-syntax-parameter reset (make-rename-transformer #'void))
(define-syntax (with-reset stx)
  (syntax-parse stx
    [(_ in:id body ...)
     #'(let*-values ([(p) (file-position in)]
                     [(a b c) (port-next-location in)]
                     [(r) (lambda ()
                            (file-position in p)
                            (set-port-next-location! in a b c))])
         (syntax-parameterize ([reset (make-rename-transformer #'r)])
           body ...))]))
(define tab-count (make-parameter 0))

(define (parse* pat i name [table (make-hasheq)] #:debug [a:debug #f])
  (define-values (a b c) (if (string? i) (values #f #f #f) (port-next-location i)))
  (define s (if (string? i) i (port->string i)))
  (define in (open-input-string s))
  (port-count-lines! in)
  (set-port-next-location! in (or a 1) (or b 0) (or c 1))
  (parse pat in name table #:debug a:debug))

(define (parse pat in [name #f] [table (make-hasheq)] #:debug [a:debug #f])
  (define start (make-location in))

  (define (get-pos) (make-position start (make-location in) name))
  (define (parse* pat) (parse pat in name table #:debug a:debug))
  (define (memoize val)
    ;;FIXME
    (define sub (hash-ref! table in (make-hash)))
    (hash-set! sub start val))
  (define (get pat)
    (hash-ref (hash-ref table pat (hash)) start #f))
  (define (debug e)
    (when a:debug
      (display (make-string (tab-count) #\tab))
      (write e)
      (displayln "")
      (displayln "")
      (displayln "")))
  (define-values (r p)
    (with-reset
        in
      (let ([v (get pat)])
        (define (fail p)
          (values #f p))
        (if v
            (values v (get-pos)) 
            (match pat
              [(colorable c p) (parse* p)]
              [(pattern n p)
               (debug `(starting ,n))
               (parameterize ([tab-count (add1 (tab-count))])
                 (parse* p))]
              [(app string? #t)
               (parse* (lit (const pat) pat))]
              [(lit f pat)
               (parse* (rx f (regexp-quote pat #f)))]
              [(rx f reg)
               (define locs (regexp-match-peek-positions* reg in))
               (define has (and locs (assoc 0 locs)))
               (define end (and has 
                                (cdr has)))
               (debug `(,reg matched ,locs))
               (if (not end)
                   (fail (get-pos))
                   (values (f (string-downcase (read-string end in)) (get-pos)) (get-pos)))]
              [(/ (list* pats))
               (let loop ([pats pats] [pos (get-pos)]) 
                 (cond [(null? pats)
                        (fail pos)]
                       [else 
                        (define pat (first pats))
                        (define-values (r p) (parse* pat))
                        (if r
                            (values r (get-pos))
                            (begin (reset) (loop (rest pats) p)))]))]
              [(seq f (list* pats))
               (let loop ([rs null] [pats pats])
                 (cond [(null? pats)
                        (define p (get-pos))
                        (values (f (reverse rs) p) p)]
                       [else
                        (define pat (first pats))
                        (define-values (r p) (parse* pat))
                        (if (not r)
                            (fail p)
                            (loop (cons r rs) (rest pats)))]))]
              [(! pat)
               (define-values (r p) (parse* (& pat)))
               (if (not r)
                   (values #t (get-pos))
                   (fail p))]
              [(& pat)
               (define-values (r p) (parse* pat))
               (define loc (get-pos))
               (reset)
               (if r
                   (values r loc)
                   (values #f (get-pos)))]
              [(+ f pat)
               (parse* (seq (lambda (r p) (f (cons (first r) (second r)) p))
                            (list pat (* (lambda (r p) r) pat))))]
              [(* f pat)
               (let loop ([res null])
                 (with-reset in
                   (define-values (r p) (parse* pat))
                   (if r
                       (loop (cons r res))
                       (begin 
                         (reset)
                         (values (f (reverse res) (get-pos)) (get-pos))))))]
              [(? f pat)
               (define-values (r p) (parse* pat))
               (if r
                   (values (f r (get-pos)) (get-pos))
                   (begin (reset) (values #t (get-pos))))]
              [(eof)
               (values (eof-object? (peek-byte in)) (get-pos))])))))
  
  (when (pattern? pat)
      (debug `(pat: ,(pattern-name pat) res: ,r at: ,(get-pos))))
  
  (when r
    (memoize r))
  (values r p))

(define ((lang->colorer tok->type) in)
  (define-values (_ __ start) (port-next-location in))
  (port-count-lines! in)
  (with-reset in
    (let loop ([tt tok->type])
      (cond [(null? tt) 
             (cond [(eof-object? (peek-byte in)) (values r:eof 'eof #f #f #f)]
                   [else (values (read-char in) 'error #f start (add1 start))])]
            [else
             (reset)
             (define pat (first tt))
             (define-values (r p) (parse pat in))
             (let ([offset (position-start p)])
               (if (not r)
                   (loop (rest tt))
                   (values r (pat-color pat) #f offset (r:+ offset (position-span p)))))]))))


(module+ test
  (define-syntax (check-equal?/1 stx)
    (syntax-parse stx
      [(_ t v)
       #'(begin
           (define-values (p _) t)
           (check-equal? p v))]))
  (let ()
    (define-parser/colorer (p l)
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
                           (list "(" Expr ")"))))])
    (check-equal?/1 (p "1") 1)
    (check-equal?/1 (p "1+2") 3)
    (check-equal?/1 (p "1+2/2") 2)
    (check-equal?/1 (p "(1+2)/2") 3/2))
  ;; test that * is greedy
  (let ()
    (define-parser/colorer (p l)
      [X (* (lambda (r p) r) (rx (lambda (r p) r) #rx"."))])
    (check-equal?/1 (p "abc")
                    (list "a" "b" "c")))
  ;; test that we reset correctly in :/
  (let ()
    (define-parser/colorer (p l)
      [X (/ (list (seq (lambda (r c) r)
                       (list "c" "a"))
                  "c"))])
    (check-equal?/1 (p "c")
                  "c"))
  (let ()
    (define-parser/colorer (p l)
      [Expr (seq (lambda (l p) l)
                 (list (* (lambda (l p) l)
                        (seq (lambda (l p) l)
                             (list
                              WHEN 
                              WHITESPACE
                              OPEN
                              WHITESPACE
                              CLOSE
                              WHITESPACE)))
                       (eof)))]
      [WHEN (lit (lambda (l p) l) "when")]
      [WHITESPACE (rx (lambda (r p) r) #rx" +")]
      [OPEN  (lit (lambda (r p) r) "{")]
      [CLOSE (lit (lambda (r p) r) "}")]
      #:tokens (syntax WHEN) 
      (bound OPEN CLOSE)
      (whitespace WHITESPACE))
    (let-values ([(_ type match start end) (l (open-input-string "when {}"))])
      (check-equal? type 'syntax)
      (check-equal? start 1)
      (check-equal? end 5))
    (let-values ([(_ type match start end) (l (open-input-string "when {}"))])
      (check-equal? type 'syntax)
      (check-equal? start 1)
      (check-equal? end 5))))
