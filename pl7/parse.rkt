#lang racket
(provide parse lex color)
(require "packrat.rkt")
(module+ test (require rackunit))

(define TAB-WIDTH 4)

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
(define parse-init
  (match-lambda
   [(list* _ _ lines)
    `(initially ,@lines)]))
(define parse-handler
  (match-lambda
   [(list* _ name _ _ lines)
    `(define ,name (make-handler ,@(filter cons? lines)))]))
(define parse-line
  (match-lambda
   [(list indent expr _)
    '(line ,(depth indent) ,expr)]
   [(? string? v) null]))
(define (line-depth s)
  (for/fold ([l 0]) ([c (string->list s)])
    (case c
      [(#\space) (add1 l)]
      [(#\tab) (+ l TAB-WIDTH)]
      [else l])))
(define parse-whenever
  (match-lambda
   [(list "whenever") '(whenever)]
   [(list "whenever" expr) `(whenever ,expr)]
   [(list do "whenever" when) `(whenever ,do ,when)]))
(define parse-whenever-part
  (match-lambda
   [(list t _ _ _ e) `(part ,t ,e)]
   [(list _ _ e) `(part ,e)]))
(define parse-means
  (match-lambda
   [(list id _ _ _ expr) `(define ,id ,expr)]))

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
                 (list REQUIRE WHITESPACE ID WHITESPACE END))]
  
  [Initially (:seq (->stx parse-init)
                   #f
                   (list INITIALLY END (:+ raw Line)))]
  
  [Handler (:seq (->stx parse-handler)
                 #f
                 (list HANDLER ID IS END (:+ raw Line)))]
  
  [Line (:/ 
         (list
          (:seq (lambda (r p) (apply string-append (flatten r)))
                #f
                (list NEWLINE (:* no-op SPACING) END))
          (:seq (->stx parse-line)
                #f
                (list INDENTATION (:/ (list Expr Whenever Means WheneverPart)) END))))]
  [Whenever (:/
             (list (:seq (->stx parse-whenever) #f (list WHENEVER))
                   (:seq (->stx parse-whenever) #f (list WHENEVER WHITESPACE Expr))
                   (:seq (->stx parse-whenever) #f (list Expr WHITESPACE WHENEVER Expr))))]
  [Means (:seq (->stx parse-means) #f (list ID WHITESPACE MEANS WHITESPACE Expr))]
  [WheneverPart (:/ 
                 (list 
                  (:seq (->stx parse-whenever-part) #f (list Expr WHITESPACE PIPE WHITESPACE Expr))
                  (:seq (->stx parse-whenever-part) #f (list PIPE WHITESPACE Expr))))]

  [INDENTATION (:seq no-op
                     #f
                     (list NEWLINE ))]
  [SPACING (:* (lambda (r p) (apply string-append (flatten r)))
               WHITESPACE)]

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
  [ArgList (:* (->stx (compose (curry filter syntax?)
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
  [WHENEVER (:lit no-op 'syntax "whenever")]
  [HANDLER (:lit no-op 'syntax "handler")]
  [INITIALLY (:lit no-op 'syntax "initially")]
  [MEANS (:lit no-op 'syntax "means")]
  [PIPE (:lit no-op 'syntax "|")]

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
