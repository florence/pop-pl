#lang racket/base
(require "harness.rkt" pop-pl/constants)

(prescription-test
 "../examples/insulin/insulin.pop"
 (start
  => (start insulin (n 1 unit/hour) iv)
  (checkBG))
 ((BG 120) 
  => (change insulin (n 1 units/hour)))

 ((advance 1 hours)
  => (checkBG))
 ((BG 120)
  => (change insulin (n 1 units/hour)))

 ((advance 1 hours)
  => (checkBG))
 ((BG 120)
  => (change insulin (n 1 units/hour)))
 
 ((advance 1 hours)
  => (checkBG))
 ((BG 120)
  => (change insulin (n 1 units/hour)))
 
 ((advance 4 hours)
  => (checkBG)))
