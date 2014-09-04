#lang racket
(require "harness.rkt")

(prescription-test 
 "../examples/heparin/heprin.pop"
 (=> start 
     (givebolus _ "heparin" "iv")
     (start _ "heparin")
     (checkptt))
 (=> (ptt 40)
     (givebolus _ "heparin" "iv")
     (decease "heparin" _))
 (=> (advance 7 hours)
     (checkptt))
 (=> (ptt 120)
     (decrease "heparin" _)))

