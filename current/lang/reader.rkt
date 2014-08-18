#lang racket
(provide get-info read-syntax)
(require "parse.rkt")

(define (get-info . ?)
  (match-lambda**
   [('color-lexer _) lex]
   [(_ _) #f]))

(define (read-syntax src in)
  (read-line in)
  (define-values (r p) (parse in))
  (file-position in 0)
  (read-line in)
  (displayln (port->string in))
  (if r r (raise-parse-error p "unknown parse error")))
