#lang info
(define collection "pop-pl")
(define deps '("base"
               "gui-lib"
               "debug"
               "pict-lib"))
(define build-deps
  '("rackunit-lib"
    "cover-coveralls"))

(define test-omit-paths
  '("examples/heparin/sim-gui.rkt"
    "examples/insulin/sim-gui.rkt"))

(define module-suffixes '(#"pop"))
