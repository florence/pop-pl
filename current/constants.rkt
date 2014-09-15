#lang racket/base
(require (for-syntax racket/base syntax/parse))

(define-syntax (defines/provide stx)
  (syntax-parse stx
    [(_ (n:id v) ...)
     #'(begin
         (provide n) ...
         (define n v) ...)]))

(defines/provide
  (heparin "heparin")
  (fentanyl "fentanyl")
  (ondemandfentanyl "fentanyl, on demand"))

