#lang s-exp syntax/module-reader
pop-pl/runtime
#:read poppl-read
#:read-syntax poppl-read-syntax
#:whole-body-readers? #t
(require "parser.rkt")
