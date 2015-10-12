#lang racket/base
(require (for-syntax racket/base syntax/parse))

(define-syntax (defines/provide stx)
  (syntax-parse stx
    [(_ (n:id v) ...)
     #'(begin
         (provide n) ...
         (define n v) ...)]))

(defines/provide
  (heparin "HEParin")
  (fentanyl "fentaNYL")
  (insulin "insulin")
  (ondemandfentanyl "fentaNYL, on demand")
  (iv "I.V."))
