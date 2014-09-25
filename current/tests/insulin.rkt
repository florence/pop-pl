#lang racket/base
(require "harness.rkt" pop-pl/current/constants)

(prescription-test
 "../examples/insulin/insulin.pop"
 (start
  => (start insulin _ iv)
  (checkBG))
 ((BG 120) 
  => (change insulin _))

 ((advance 1 hours)
  => (checkBG))
 ((BG 120)
  => (change insulin _))

 ((advance 1 hours)
  => (checkBG))
 ((BG 120)
  => (change insulin _))
 
 ((advance 1 hours)
  => (checkBG))
 ((BG 120)
  => (change insulin _))
 
 ((advance 4 hours)
  => (checkBG)))
