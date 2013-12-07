#lang racket

(require "parser.rkt"
         rackunit)

(define (try str)
  (define sp (open-input-string str))
  (port-count-lines! sp)
  (with-handlers ((exn:fail:syntax? (λ (x) x)))
    (poppl-read sp)))

(check-equal?
 (try "Boolean x = false;\n=====\nwhen (x) { prompt(red,y,\"Done\"); }")
 '((decl Boolean x false) (when x (begin (prompt red y "Done")))))

(check-equal?
 (try "Boolean x = false;\n=====\nwhen (x or y) { prompt(blue,z,\"Done\"); }")
 '((decl Boolean x false) (when (or x y) (begin (prompt blue z "Done")))))

(check-equal?
 (try "Boolean x = false;\n=====\nif x then prompt(blue,z,\"Done\"); else prompt(green,z,\"Done\"); fi")
 '((decl Boolean x false) (if x (prompt blue z "Done") (prompt green z "Done"))))

(check-equal?
 (try "Boolean x = false;\n=====\nwhen (x or y) { if x then prompt(green,z,\"Done\"); else prompt(blue,z,\"Done\"); fi }")
 '((decl Boolean x false) (when (or x y) (begin (if x (prompt green z "Done") (prompt blue z "Done"))))))

(check-equal?
 (try "Boolean x = false;\n=====\nwhen (x or y) { prompt(blue,z,if x then \"Done\" else \"Undone\" fi); }")
 '((decl Boolean x false) 
   (when (or x y)
     (begin (prompt blue z (if x "Done" "Undone"))))))
