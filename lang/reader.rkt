#lang racket/base
(provide get-info read-syntax)
(require racket/match "parse.rkt")

(define (get-info . default)
  (match-lambda**
   [('color-lexer _) lex]
   [('drracket:default-extension _)
    "pop"]
   [(_ d) d]))

(define (read-syntax src in)
  (define-values (r p) (parse in #:name src #:debug #f))
  (if r r (raise-parse-error p "unknown parse error")))
