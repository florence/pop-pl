#lang racket/base
(provide poppl-read poppl-read-syntax)
(require parser-tools/lex parser-tools/yacc
         (prefix-in : parser-tools/lex-sre)
         rackunit
         syntax/readerr)

(define-tokens tokens (id num binop unop str Base-type))
(define-empty-tokens mt-tokens (when prompt 
                                 open-brace close-brace open-paren close-paren
                                 semi bars eoft
                                 Or
                                 if then else fi 
                                 dot n/a <-
                                 equal comma))

(define current-source (make-parameter #f))

(define lex
  (lexer-src-pos
   ["when" (token-when)]
   ["prompt" (token-prompt)]
   [(:or "Boolean" "Unknown" "Number" "N/A")
    (token-Base-type (string->symbol lexeme))]
   ["Or" (token-Or)]
   [(:or "known?" "unknown?" "n/a?" "not") (token-unop (string->symbol lexeme))]
   ["=" (token-equal)]
   ["<-" (token-<-)]
   [(:or #\< #\!) (token-binop (string->symbol lexeme))]
   ["or" (token-binop 'or)]
   ["if" (token-if)]
   ["n/a" (token-n/a)]
   ["then" (token-then)]
   ["else" (token-else)]
   ["fi" (token-fi)]
   [#\; (token-semi)]
   [#\{ (token-open-brace)]
   [#\} (token-close-brace)]
   [#\, (token-comma)]
   [#\( (token-open-paren)]
   [#\) (token-close-paren)]
   [#\. (token-dot)]
   [(:: (:/ #\a #\z #\A #\Z)
        (:* #\_ #\? (:/ #\a #\z #\A #\Z #\0 #\9)))
    (token-id (string->symbol lexeme))]
   [(:: #\= (:: #\= (:: #\= (:: #\= (:* #\=)))))
    (token-bars)]
   [(:+ (:/ #\0 #\9))
    (token-num (string->number lexeme))]
   [(:: #\" (:* (char-complement #\")) #\")
    (token-str (substring lexeme 1 (- (string-length lexeme) 1)))]
   [(:+ whitespace) (return-without-pos (lex input-port))]
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

(check-equal? (str->toks "when (x) { prompt(); }")
              '(when open-paren id close-paren open-brace prompt open-paren close-paren semi close-brace eoft))

(define parse
  (parser
   [grammar 
    (start [(decls bars stmts) (add-srcloc (append $1 $3) $1-start-pos $3-end-pos)])
    (decls [(decl semi decls) (cons (add-srcloc $1 $1-start-pos $1-end-pos) $3)]
           [(decl semi) (list (add-srcloc $1 $1-start-pos $1-end-pos))])
    (decl [(type id equal expr) `(decl ,$1 ,(add-srcloc $2 $2-start-pos $2-end-pos) ,$4)])
    (type [(Base-type) (add-srcloc $1 $1-start-pos $n-start-pos)]
          [(type Or type) (prec Or) (add-srcloc `(Or ,$1 ,$3) $1-start-pos $n-end-pos)])
    (stmt [(when open-paren expr close-paren stmt)
           (add-srcloc `(when ,$3 ,$5) $1-start-pos $n-end-pos)]
          [(prompt open-paren id comma id comma expr close-paren semi)
           (add-srcloc `(prompt ,$3 ,(add-srcloc $5 $5-start-pos $5-end-pos #t) ,$7)
                       $1-start-pos $n-end-pos)]
          [(if expr then stmt else stmt fi)
           (add-srcloc `(if ,$2 ,$4 ,$6) $1-start-pos $n-end-pos)]
          [(id <- expr semi)
           (add-srcloc `(bang! ,(add-srcloc $1 $1-start-pos $1-end-pos) ,$3)
                       $1-start-pos $n-end-pos)]
          [(open-brace stmts close-brace)
           (add-srcloc `(begin ,@$2) $1-start-pos $n-end-pos)])
    (stmts [(stmt stmts) (cons (add-srcloc $1 $1-start-pos $1-end-pos) $2)]
           [(stmt) (list (add-srcloc $1 $1-start-pos $1-end-pos))])
    (expr [(expr op expr) (prec binop) (add-srcloc `(,$2 ,$1 ,$3) $1-start-pos $n-end-pos)]
          [(if expr then expr else expr fi) (add-srcloc `(if ,$2 ,$4 ,$6) $1-start-pos $n-end-pos)]
          [(num) (add-srcloc $1 $1-start-pos $n-end-pos)]
          [(id) (add-srcloc $1 $1-start-pos $n-end-pos #t)]
          [(n/a) (add-srcloc 'n/a $1-start-pos $n-end-pos)]
          [(str) (add-srcloc $1 $1-start-pos $n-end-pos)]
          [(unop open-paren expr close-paren) (add-srcloc `(,(add-srcloc $1 $1-start-pos $n-end-pos) ,$3) $1-start-pos $n-end-pos)]
          [(open-paren expr close-paren) (add-srcloc $2 $1-start-pos $n-end-pos)])
    (op [(equal) '=]
        [(binop) $1])]
   [precs (right binop) (right Or)]
   [tokens mt-tokens tokens]
   [src-pos]
   [start start]
   [end eoft]
   [error 
    (lambda (tok-ok? tok-name tok-value start-pos end-pos)
      (raise-syntax-error 
       'parse-error 
       (format "~s" (if tok-ok?
                        tok-value
                        'unknown))
       (add-srcloc (if tok-ok?
                       tok-value
                       'unknown)
                   start-pos
                   end-pos)))]))

(define (add-srcloc stuff start-pos end-pos [id? #f])
  (cond
    [id?
     (define str (symbol->string stuff))
     (define prt (open-input-string str))
     (port-count-lines! prt)
     (set-port-next-location! prt 
                              (position-line start-pos)
                              (position-col start-pos)
                              (position-offset start-pos))
     (read-syntax (current-source) prt)]
    [else
     (datum->syntax #f stuff (locs->vec start-pos end-pos))]))

(define (locs->vec start-pos end-pos)
  (vector
   (current-source)
   (position-line start-pos)
   (position-col start-pos)
   (position-offset start-pos)
   (- (position-offset end-pos)
      (position-offset start-pos))))

(define (run-p src p)
  (parameterize ([current-source src])
    (parse (λ () (lex p)))))

(define (poppl-read [port (current-input-port)])
  (syntax->datum (run-p #f port)))

(define (poppl-read-syntax [name #f] [port (current-input-port)])
  (run-p (or name (object-name port))
         port))
