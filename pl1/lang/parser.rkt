#lang racket/base
(provide poppl-read poppl-read-syntax)
(require parser-tools/lex parser-tools/yacc
         (prefix-in : parser-tools/lex-sre)
         rackunit
         syntax/readerr
         (for-syntax racket/base))

(define-tokens tokens (symbolic num binop unop str Base-type unit))
(define-empty-tokens mt-tokens (when whenever import device prompt assert signaled scenario events
                                 open-brace close-brace open-paren close-paren
                                 semi bars ands separator eoft
                                 Or not
                                 if do at then else fi
                                 dot n/a <-
                                 equal comma slash times))

(define current-source (make-parameter #f))

(define lex
  (lexer-src-pos
   ["import" (token-import)]
   ["device" (token-device)]
   ["when" (token-when)]
   ["whenever" (token-whenever)]
   ["assert" (token-assert)]
   ["signaled" (token-signaled)]
   ["scenario" (token-scenario)]
   ["events" (token-events)]
   ["prompt" (token-prompt)]
   [(:or "Boolean" "Unknown" "Number" "N/A")
    (token-Base-type (string->symbol lexeme))]
   ["Or" (token-Or)]
   ["not" (token-not)]
   [(:or "known?" "unknown?" "n/a?") (token-unop (string->symbol lexeme))]
   ["=" (token-equal)]
   ["/" (token-slash)]
   ["||" (token-bars)]
   ["&&" (token-ands)]
   ["<-" (token-<-)]
   [(:or "<" "<=" "!" "*" "+") (token-binop (string->symbol lexeme))]
   ["or" (token-binop 'or)]
   ["if" (token-if)]
   ["do" (token-do)]
   ["at" (token-at)]
   ["n/a" (token-n/a)]
   ["then" (token-then)]
   ["else" (token-else)]
   ["fi" (token-fi)]
   [(:: (:or "unit" "hour" "mL") (:? "s"))
    (token-unit (string->symbol (regexp-replace* #rx"s$" lexeme "")))]
   [#\; (token-semi)]
   [#\{ (token-open-brace)]
   [#\} (token-close-brace)]
   [#\, (token-comma)]
   [#\( (token-open-paren)]
   [#\) (token-close-paren)]
   [#\. (token-dot)]
   [(:: (:/ #\a #\z #\A #\Z)
        (:* #\_ #\? (:/ #\a #\z #\A #\Z #\0 #\9)))
    (token-symbolic (string->symbol lexeme))]
   [(:: #\= (:: #\= (:: #\= (:: #\= (:* #\=)))))
    (token-separator)]
   [(:+ (:/ #\0 #\9))
    (token-num (string->number lexeme))]
   [(:: #\" (:* (char-complement #\")) #\")
    (token-str (substring lexeme 1 (- (string-length lexeme) 1)))]
   [(:+ whitespace) (return-without-pos (lex input-port))]
   [(:: "//" (:* (char-complement (:or "\r" "\n")))) (return-without-pos (lex input-port))]
   [(eof) (token-eoft)]
   [(char-complement (union)) 
    (raise-read-error (format "unexpected character ~a" lexeme)
                      (object-name input-port)
                      (position-line start-pos)
                      (position-col start-pos)
                      (position-offset start-pos)
                      (- (position-offset end-pos)
                         (position-offset start-pos)))]))

(define (str->toks str)
  (let ([p (open-input-string str)])
    (let loop ()
      (let ([next (lex p)])
        (cons (token-name (position-token-token next))
              (if (eq? 'eoft (token-name (position-token-token next)))
                  '()
                  (loop)))))))

(module+ test
  (check-equal? (str->toks "when (x) { prompt(); }")
                '(when open-paren symbolic close-paren open-brace prompt open-paren close-paren semi close-brace eoft))
  (check-equal? (str->toks "when (5 < x <= 6) { prompt(); }")
                '(when open-paren num binop symbolic binop num close-paren open-brace prompt open-paren close-paren semi close-brace eoft)))

(define parse
  (parser
   [grammar 
    (start [(decls separator stmts) (->stx (append $1 $3))])
    (decls [(decl decls) (cons $1 $2)]
           [(decl) (list $1)]
           [() null])
    (id   [(symbolic) (->stx $1)])
    (id-val-seq [() null]
                [(non-empty-id-val-seq) $1])
    (non-empty-id-val-seq [(id equal expr) (list `(cons ',$1 ,$3))]
                          [(id equal expr comma non-empty-id-val-seq) (cons `(cons ',$1 ,$3) $5)])
    (decl [(type id equal expr semi) (->stx `(decl ,$1 ,$2 ,$4))]
          [(id equal expr semi) (->stx `(decl val ,$1 ,$3))]
          [(import id semi) (->stx `(define ,$2 (make-external-function ',$2)))]
          [(device id open-brace id-val-seq close-brace) (->stx `(define ,$2 (make-device ',$2 . ,$4)))]
          [(scenario open-brace stmts close-brace) (->stx `(scenario . ,$3))])
    (type [(Base-type) $1]
          [(type Or type) (prec Or) (->stx `(Or ,$1 ,$3))])
    (stmt [(when open-paren expr close-paren stmt) (->stx `(when ,$3 ,$5))]
          [(whenever open-paren expr close-paren stmt) (->stx `(whenever ,$3 ,$5))]
          [(prompt open-paren id comma id comma expr close-paren semi)
           (->stx `(prompt ,$3 ,$5 ,$7))]
          [(if expr then stmt else stmt fi)
           (->stx `(if ,$2 ,$4 ,$6))]
          [(id <- expr semi) (->stx `(bang! ,$1 ,$3))]
          [(expr dot id <- expr semi) (->stx `(set-val! ,$1 ',$3 ,$5))]
          [(do expr at expr semi) (->stx `(do ,$2 ,$4))]
          [(stmt-expr semi) $1]
          [(open-brace stmts close-brace) (->stx `(begin ,@$2))]
          [(events open-brace stmts close-brace) (->stx `(events ,@$3))]
          [(assert expr semi) (->stx `(assert ,$2))]
          [(assert signaled id semi) (->stx `(assert-signaled #t ,$3))]
          [(assert not signaled id semi) (->stx `(assert-signaled #f ,$4))])
    (stmts [(stmt stmts) (cons $1 $2)]
           [() null])
    (stmt-expr [(expr open-paren expr-seq close-paren) (->stx `(,$1 . ,$3))])
    (expr [(stmt-expr) $1]
          [(val-expr)  $1]
          [(if expr then expr else expr fi) (->stx `(if ,$2 ,$4 ,$6))]
          [(open-paren exprs close-paren) (->stx `(list . ,$2))]
          [(expr bars expr) (prec binop) (->stx `(or ,$1 ,$3))]
          [(expr ands expr) (prec binop) (->stx `(and ,$1 ,$3))]
          [(val-expr op expr) (prec binop) (->stx `(,$2 ,$1 ,$3))]
          [(any-unop open-paren expr close-paren) (->stx `(,$1 ,$3))]
          [(open-paren expr close-paren) $2])
    (val-expr [(num units) (->stx `(in-units ,$1 ,$2))]
              [(num) (->stx $1)]
              [(expr dot id) (->stx `(get-val ,$1 ',$3))]
              [(id) $1]
              [(n/a) (->stx 'n/a)]
              [(str) (->stx $1)])
    (exprs [(expr expr) (list $1 $2)]
           [(expr exprs) `(,$1 . ,$2)])
    (expr-seq [() null]
              [(non-empty-expr-seq) $1])
    (non-empty-expr-seq [(expr) (list $1)]
                        [(expr comma non-empty-expr-seq) `(,$1 . ,$3)])
    (op [(equal) '=]
        [(binop) $1])
    [any-unop [(unop) $1]
              [(not) (->stx 'not)]]
    (units [(unit-seq) $1]
           [(unit-seq slash units) (->stx `(unit/ ,$1 ,$3))])
    (unit-seq [(unit) (->stx `(quote ,$1))]
              [(unit unit-seq) (->stx `(unit* ,$1 ,$2))])]
   [precs (left binop) (right Or)]
   [tokens mt-tokens tokens]
   [src-pos]
   [start start]
   [end eoft]
   [error 
    (lambda (tok-ok? tok-name tok-value start-pos end-pos)
      (raise-syntax-error 
       'parse-error 
       (format "~s" (if tok-ok?
                        (or tok-value tok-name)
                        'unknown))
       (add-srcloc (if tok-ok?
                       (or tok-value tok-name)
                       'unknown)
                   start-pos
                   end-pos)))]))

(define (add-srcloc stuff start-pos end-pos)
  (datum->syntax #f stuff (locs->vec start-pos end-pos) orig-prop))

(define-syntax (->stx stx)
  ;; A non-hygienic marco to access $1-start-pos and $n-end-pos, which
  ;; are bound by the `cfg-parse` form in a grammar production's
  ;; action:
  (syntax-case stx ()
    [(_ e)
     (let ([start (datum->syntax stx '$1-start-pos)]
           [end (datum->syntax stx '$n-end-pos)])
       #`(add-srcloc e #,start #,end))]))

;; This property tells DrRacket that a constructed syntax object
;; should be treated as being in the original source:
(define orig-prop (read-syntax 'src (open-input-bytes #"x")))

(define (locs->vec start end)
  (vector
   (current-source)
   (position-line start)
   (position-col start)
   (position-offset start)
   (and (position-offset end)
        (position-offset start)
        (- (position-offset end)
           (position-offset start)))))

(define (run-p src p)
  (parameterize ([current-source src])
    (parse (λ () (lex p)))))

(define (poppl-read [port (current-input-port)])
  (syntax->datum (run-p #f port)))

(define (poppl-read-syntax [name #f] [port (current-input-port)])
  (run-p (or name (object-name port))
         port))

(module+ test
  (define (parse str) (poppl-read (open-input-string str)))
  (check-equal? (parse "==============\na<-b;whenever(true){b.b<-a;}")
                '((bang! a b) (whenever true (begin (set-val! b 'b a)))))
  ;; test precs with <
  (check-equal? (parse "==============\nwhenever(x < y < z){}")
                '((whenever (< (< x y) z) (begin))))

  ;; test precs with || and &&
  (check-equal?
   (parse 
    "================\nwhenever (d3.f2 && d2.f1 < 12 || d1.f2 = 12 units) {}")
   '((whenever 
       (and (get-val d3 'f2) 
            (or (< (get-val d2 'f1) 12)
                (= (get-val d1 'f2) (in-units 12 'unit))))
       (begin))))
  ;; massive test
  (check-equal?
   (parse
    #<<PROG
    // comment here
    device d1 { f1 = 5, f2 = 12 units/hour }
    device d2 { f1 = 5 }
    import i1;
    device d3 { f1 = 16 units, f2 = false, f3 = 0 }
    import i2;
    // comment here
    scenario {
       events {
         d3.f2 <- true;
       }
       assert d3.f1 = 12;
       assert not signaled i2;
    }
    ================
    whenever (d3.f2 && d2.f1 < 12 || d1.f2 = 12 units) {
         d3.f2 <- false;
         d1.f2 <- 19 units;
         i2 ();
    }
PROG
)
   `((define d1 (make-device 'd1 (cons 'f1 5) (cons 'f2 (in-units 12 (unit/ 'unit 'hour)))))
     (define d2 (make-device 'd2 (cons 'f1 5)))
     (define i1 (make-external-function 'i1))
     (define d3 (make-device 'd3
                             (cons 'f1 (in-units 16 'unit))
                             (cons 'f2 false)
                             (cons 'f3 0)))
     (define i2 (make-external-function 'i2))
     (scenario 
      (events (set-val! d3 'f2 true))
      (assert (= (get-val d3 'f1) 12))
      (assert-signaled #f i2))
     
     (whenever (and (get-val d3 'f2) 
                    (or (< (get-val d2 'f1) 12)
                        (= (get-val d1 'f2) (in-units 12 'unit))))
               (begin
                 (set-val! d3 'f2 false)
                 (set-val! d1 'f2 (in-units 19 'unit))
                 (i2))))))
