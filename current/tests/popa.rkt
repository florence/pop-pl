#lang racket
(require "harness.rkt" pop-pl/current/constants)
(prescription-test
 "../examples/popa.pop"
 (=> start
     (start _ fentanyl)
     (set ondemandfentanyl _))
 (=> (painscore 10)
     (increase ondemandfentanyl _))
 (=> (advance 61 minutes)
     (checkpainscore))
 (=> (painscore 2))
 (=> (painscore 2))
 (=> (painscore 2))
 (=> (advance 2 hours))
 (=> (painscore 2)
     (decrease ondemandfentanyl _)))
