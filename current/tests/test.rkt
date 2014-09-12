#lang racket
(require "harness.rkt")

(prescription-test 
 "../examples/heparin/heprin.pop"
 (=> start 
     (givebolus _ "heparin" "iv")
     (start _ "heparin" "iv")
     (checkptt))
 (=> (ptt 40)
     (givebolus _ "heparin" "iv")
     (decease "heparin" _))
 (=> (advance 7 hours)
     (checkptt))
 (=> (ptt 120)
     (decrease "heparin" _))
 (=> (advance 7 hours)
     (checkptt))
 (=> (ptt 80))
 (=> (advance 7 hours)
     (checkptt))
 (=> (ptt 80))
 (=> (advance 8 hours))
 (=> (advance 16 hours)
     (checkptt)))

