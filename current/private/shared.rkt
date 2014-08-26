#lang racket
(provide (struct-out in:number)
         (struct-out message))
(struct in:number (value unit) #:transparent)
(struct message (tags values time) #:transparent)
