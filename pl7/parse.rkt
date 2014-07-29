#lang racket
(require "packrat.rkt")

(define orig-prop (read-syntax 'x (open-input-bytes #"x")))
(define ((->stx f) e p)
  (datum->syntax #f
                 (f e)
                 (position->vector p)
                 orig-prop))
(define (no-op r p) r)
(define raw (->stx values))
(define message-parse (match-lambda [(list _ _ id _ _ _ f _) `(define ,id ,f)]))
(define message-maker
  (match-lambda
   [(list _ args _)
    `(make-message ,@args)]))
(define (all-but-last l)
  (cond [(null? l) null]
        [(null? (rest l)) null]
        [else (cons (first l)
                    (all-but-last (rest l)))]))

(define-parser/colorer (parse lex color)
  [Top (:seq (->stx all-but-last) #f (list (:* no-op (:/ (list Require Initially Handler Message))) :EOF))]

  [Require (:seq (->stx (compose second ))
                 #f
                 (list REQUIRE WHITESPACE ID WHITESPACE NEWLINE))]
  
  [Initially Todo]
  
  [Handler Todo]

  ;; messages
  [Message (:/ (list (:seq (->stx message-parse)
                           #f
                           (list MESSAGE WHITESPACE ID WHITESPACE IS WHITESPACE MessageForm NEWLINE))
                     (:seq (->stx message-parse)
                           #f
                           (list MESSAGE WHITESPACE ID WHITESPACE IS WHITESPACE ID NEWLINE))
                     (:seq (->stx message-parse)
                           #f
                           (list MESSAGE WHITESPACE ArgDef WHITESPACE IS WHITESPACE Expr NEWLINE))))]
  [MessageForm (:seq  (->stx message-maker)
                      #f
                      (list OPEN-BRACKET ArgList CLOSE-BRACKET))]
  
  ;; arguments
  [ArgDef (:seq (->stx second) 
                #f
                (list OPEN-PAREN ArgList CLOSE-PAREN))]
  [ArgList (:* (->stx flatten)
               (:/ (list ID (:seq raw #f (list KEYWORD ID)))))]
  
  ;; expressions
  [Expr Todo]
  
  ;; keywords
  [REQUIRE (:lit no-op 'syntax "require")]
  [MESSAGE (:lit no-op 'syntax "message")]
  [IS (:lit no-op "is" 'syntax)]
  [OPEN-BRACKET (:lit no-op 'syntax "[")]
  [CLOSE-BRACKET (:lit no-op 'syntax "]")]

  ;; basics
  [NEWLINE (:lit no-op 'white-space "\n")]
  [STRING (:rx no-op 'constant #rx"\".*[^\\]\"")]
  [WHITESPACE (:rx no-op 'white-space #rx" *")]
  [KEYWORD (:seq (->stx (compose string->keyword symbol->string first))
                 'keyword
                 (list ID ":"))]
  [ID (:rx (->stx (compose string->symbol first))  'no-color #rx"[a-zA-Z]+")]
  [OPEN-PAREN (:lit no-op 'paren "(")]
  [CLOSE-PAREN (:lit no-op 'paren ")")]
  ;; silly
  [Todo ""]
  #:colors
  [syntax "red"]
  [constant "green"]
  [paren "blue"]
  [op "yellow"]
  [keyword "yellow"])
