#lang info
(define collection "pop-pl")
(define deps '(("base" #:version "6.3")
               "gui-lib"
               "pict-lib"))
(define build-deps
  '("rackunit-lib"
    "cover-coveralls"))

(define test-omit-paths
  '("examples/heparin/sim-gui.rkt"
    "examples/insulin/sim-gui.rkt"
    "examples/heparin/hep.pop"
    "examples/furosemide"
    "examples/BMZ-insulin"))

(define compile-omit-paths
  '("examples/furosemide"
    "examples/heparin/hep.pop"
    "examples/BMZ-insulin"))

(define module-suffixes '(#"pop"))
