#lang racket
(require "harness.rkt" pop-pl/current/constants)
(prescription-test
 "../examples/popa.pop"
 (=> start
     (start _ fentanyl)
     (set ondemandfentanyl _))
 )
