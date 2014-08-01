#lang racket
(provide parse lex)
(require "../packrat.rkt")
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
   [(list _ _ lines)
    `(initially ,@(filter syntax? lines))]))
(define parse-handler
  (match-lambda
   [(list _ _ name _ _ _ _ lines)
    `(define ,name (make-handler ,@(filter syntax? lines)))]))
(module+ test
  (let ([s #'(line 0 2)])
    (check-equal? (parse-handler (list "handler" " " 'x "is" " " #t "\n" `(,s ,42)))
                  `(define x (make-handler ,s)))))

(define parse-line
  (match-lambda
   [(list indent expr _)
    `(line ,(line-depth indent) ,expr)]
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
(define parse-require
    (match-lambda
     [(list _ _ name _ _)
      `(add-handler ,name)]))

(define-parser/colorer (parse lex)
  [Top (:seq (->stx
              (compose
               (lambda (b) (list* 'module 'TODO 'pop-pl b))
               second))
             (list 
              (:? no-op LANG)
              (:+ no-op (:/ (list Require COMMENT Initially Handler Message)))
              :EOF))]
  [COMMENT (:rx (->stx (const '(void)))
                #rx"//.*?\n")]
  
  [Require (:seq (->stx parse-require)
                 (list REQUIRE WHITESPACE ID (:? no-op WHITESPACE) END))]
  
  [Initially (:seq (->stx parse-init)
                   (list INITIALLY END (:+ no-op Line)))]
  
  [Handler (:seq (->stx parse-handler)
                 (list HANDLER WHITESPACE ID WHITESPACE IS (:? no-op WHITESPACE) END (:+ no-op Line)))]
  
  [Line (:/ 
         (list
          (:seq (lambda (r p) (apply string-append (flatten r)))
                (list NEWLINE SPACING END))
          (:seq (->stx parse-line)
                (list INDENTATION (:/ (list Expr Whenever Means WheneverPart)) END))))]
  [Whenever (:/
             (list (:seq (->stx parse-whenever) (list WHENEVER))
                   (:seq (->stx parse-whenever) (list WHENEVER WHITESPACE Expr))
                   (:seq (->stx parse-whenever) (list Expr WHITESPACE WHENEVER Expr))))]
  [Means (:seq (->stx parse-means) (list ID WHITESPACE MEANS WHITESPACE Expr))]
  [WheneverPart (:/ 
                 (list 
                  (:seq (->stx parse-whenever-part) (list Expr WHITESPACE PIPE WHITESPACE Expr))
                  (:seq (->stx parse-whenever-part) (list PIPE WHITESPACE Expr))))]

  [INDENTATION (:seq (lambda (r p) (apply string-append r)) 
                     (list NEWLINE SPACING))]
  [SPACING (:+ (lambda (r p) (apply string-append (flatten r)))
               WHITESPACE)]

  ;; messages
  [Message (:/ (list (:seq (->stx message-parse)
                           (list MESSAGE WHITESPACE ID WHITESPACE IS WHITESPACE MessageForm END))
                     (:seq (->stx message-parse)
                           (list MESSAGE WHITESPACE ID WHITESPACE IS WHITESPACE ID END))
                     (:seq (->stx message-parse)
                           (list MESSAGE WHITESPACE ArgDef WHITESPACE IS WHITESPACE Expr END))))]
  [MessageForm (:seq  (->stx message-maker)
                      (list OPEN-BRACKET ArgList CLOSE-BRACKET))]
  
  ;; arguments
  [ArgDef (:seq (->stx second) 
                (list OPEN-PAREN ArgList CLOSE-PAREN))]
  [ArgList (:* (->stx (compose (curry filter syntax?)
                               flatten))
               (:/ (list WHITESPACE
                         (:seq no-op (list KEYWORD WHITESPACE ID))
                         ID)))]
  
  ;; expressions
  [Expr (:/
         (list
          Call
          Numberic
          ID))]
  
  [Call Todo]
  [Numberic Todo]
  [Number (:/
           (list Number+Unit
                 NUMBER-RAW))]
  [Number+Unit (:seq (->stx values) (list NUMBER-RAW Unit))]
  [NUMBER-RAW (:rx (->stx string->number) #rx"[0-9]+(\\.[0-9]+)?")]
  [Unit (:seq (->stx (compose string->symbol (curry apply string-append) flatten))
              (list UNIT-RAW (:? no-op (:* no-op (:seq no-op (list "/" UNIT-RAW))))))]
  [UNIT-RAW (:/ (list "unit"
                      "units"
                      "kg"
                      "hour"
                      "hours"
                      "day"
                      "days"
                      "week"
                      "weeks"
                      "minute"
                      "minutes"))]
  [OP (:/ (list "and" "<" ">"))]
  
  ;; keywords
  [REQUIRE "require"]
  [MESSAGE "message"]
  [IS "is"]
  [OPEN-BRACKET "["]
  [CLOSE-BRACKET "]"]
  [WHENEVER "whenever"]
  [HANDLER "handler"]
  [INITIALLY "initially"]
  [MEANS "means"]
  [PIPE "|"]
  [AFTER "after"]
  [FUNCTION "function"]

  ;; basics
  [END (:& (:/ (list NEWLINE :EOF)))]
  [NEWLINE "\n"]
  [STRING (:rx no-op #rx"\".*?[^\\]\"")]
  [WHITESPACE (:rx no-op #rx" +")]
  [?WHITESPACE (:? no-op WHITESPACE)]
  [KEYWORD (:seq (->stx (compose string->keyword symbol->string syntax->datum first))
                 (list ID-LIKE ":"))]
  [ID (:seq (->stx first)
            (list ID-LIKE
                  (:/ (list (:! ":") END))))]
  [ID-LIKE (:rx (->stx string->symbol)  #rx"[a-zA-Z]+")]
  [OPEN-PAREN "("]
  [CLOSE-PAREN ")"]
  [NEW "new"]
  ;; silly
  [Todo (:! (:? no-op (:rx no-op #rx".")))]
  [LANG (:seq no-op (list "#lang" (:rx no-op #rx".*\n")))]
  [INCOMPLETE-STRING (:rx no-op #rx"\"[^\"]*")]
  #:tokens 
  (comment LANG COMMENT) 
  (other REQUIRE MESSAGE IS OPEN-BRACKET CLOSE-BRACKET WHENEVER HANDLER INITIALLY MEANS PIPE AFTER
         OP FUNCTION NEW) 
  (white-space NEWLINE WHITESPACE) 
  (constant STRING INCOMPLETE-STRING Number Unit) 
  (keyword KEYWORD)
  (no-color ID)
  (parenthesis OPEN-PAREN CLOSE-PAREN))

(module+ test
  (define (module . e)
    `(module TODO pop-pl ,@e))
  (check-equal? (syntax->datum (parse "message test is [ a b: c ]"))
                (module '(define test (make-message a #:b c))))

  (check-equal? (syntax->datum (parse "handler x is\n  test"))
                (module '(define x (make-handler (line 2 test)))))

  (check-equal? (syntax->datum (parse "initially\n  test"))
                (module '(initially (line 2 test))))
  
  (check-equal? (syntax->datum (parse "require x"))
                (module '(add-handler x)))

  (let ([in (open-input-string "#lang test\nmessage test is [ a b: c ]")])
    (read-line in)
    (check-equal? (syntax->datum (parse in))
                  (module '(define test (make-message a #:b c))))))
