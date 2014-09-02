#lang racket
(require "harness.rkt")

(prescription-test 
 "../examples/heparin/heprin.pop"
 (=> start 
     (givebolus _ "heparin" "iv")
     (start _ "heparin")
     (checkptt)))

