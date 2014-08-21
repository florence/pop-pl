#lang racket
(provide get-info read-syntax)
(require "parse.rkt")

(define (get-info . default)
  (match-lambda**
   [('color-lexer _) lex]
   [(_ d) d]))

(define (read-syntax src in)
  (read-line in)
  (define-values (r p) (parse in))
  (if r r (raise-parse-error p "unknown parse error")))
