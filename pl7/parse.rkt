#lang racket
(provide parse lex color)
(require "packrat.rkt")
(module+ test (require rackunit))

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

(define-parser/colorer (parse lex color)
  [Top (:seq (->stx
              (compose
               (lambda (b) (list* 'module 'TODO 'pop-pl b))
               first))
             #f
             (list (:+ no-op 
                       (:/ (list Require Initially Handler Message)))
                   :EOF))]

  [Require (:seq (->stx (compose second))
                 #f
                 (list REQUIRE WHITESPACE ID WHITESPACE NEWLINE))]
  
  [Initially Todo]
  
  [Handler Todo]

  ;; messages
  [Message (:/ (list (:seq (->stx message-parse)
                           #f
                           (list MESSAGE WHITESPACE ID WHITESPACE IS WHITESPACE MessageForm END))
                     (:seq (->stx message-parse)
                           #f
                           (list MESSAGE WHITESPACE ID WHITESPACE IS WHITESPACE ID END))
                     (:seq (->stx message-parse)
                           #f
                           (list MESSAGE WHITESPACE ArgDef WHITESPACE IS WHITESPACE Expr END))))]
  [MessageForm (:seq  (->stx message-maker)
                      #f
                      (list OPEN-BRACKET ArgList CLOSE-BRACKET))]
  
  ;; arguments
  [ArgDef (:seq (->stx second) 
                #f
                (list OPEN-PAREN ArgList CLOSE-PAREN))]
  [ArgList (:* (->stx (compose (curry filter (negate string?))
                               flatten))
               (:/ (list WHITESPACE
                         (:seq no-op #f (list KEYWORD WHITESPACE ID))
                         ID)))]
  
  ;; expressions
  [Expr Todo]
  
  ;; keywords
  [REQUIRE (:lit no-op 'syntax "require")]
  [MESSAGE (:lit no-op 'syntax "message")]
  [IS (:lit no-op 'syntax "is")]
  [OPEN-BRACKET (:lit no-op 'syntax "[")]
  [CLOSE-BRACKET (:lit no-op 'syntax "]")]

  ;; basics
  [END (:& (:/ (list NEWLINE :EOF)))]
  [NEWLINE (:lit no-op 'white-space "\n")]
  [STRING (:rx no-op 'constant #rx"\".*[^\\]\"")]
  [WHITESPACE (:rx no-op 'white-space #rx" +")]
  [?WHITESPACE (:? no-op WHITESPACE)]
  [KEYWORD (:seq (->stx (compose string->keyword symbol->string syntax->datum first))
                 'keyword
                 (list ID-LIKE ":"))]
  [ID (:seq (->stx first)
            'no-color
            (list ID-LIKE
                  (:! ":")))]
  [ID-LIKE (:rx (->stx string->symbol) #f #rx"[a-zA-Z]+")]
  [OPEN-PAREN (:lit no-op 'paren "(")]
  [CLOSE-PAREN (:lit no-op 'paren ")")]
  ;; silly
  [Todo (:! (:? no-op (:rx no-op #f #rx".")))]
  #:colors
  [syntax "red"]
  [constant "green"]
  [paren "blue"]
  [op "yellow"]
  [keyword "yellow"])

(module+ test
  (check-equal? (syntax->datum (parse "message test is [ a b: c ]"))
                '(module TODO pop-pl
                  (define test (make-message a #:b c)))))
