#lang racket
(provide parse lex)
(require (for-syntax syntax/parse))
(require "../packrat.rkt" syntax/parse)
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
    `(initially ,@(join-lines lines))]))
(define parse-handler
  (match-lambda
   [(list _ _ name _ _ _ _ lines)
    `(define ,name (make-handler ,@(join-lines lines)))]))
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
;;; joining things to the right depth
(define (join-lines lines)
  (define (join* lines depth [results null])
    (cond [(null? lines) 
           (values (reverse results) null)]
          [else
           (define line (first lines))
           (match line
             [(list 'line inner-depth e)
              (if (<= inner-depth depth)
                  (let ()
                    (define-values (res b) (join* (rest lines) depth))
                    (if (null? b)
                        (values results res)
                        (error 'internal-error "expected null, got ~s" b)))
                  (syntax-parse e
                    [(whenever e) 
                     (define-values (next rst) (join* (rest lines) inner-depth))
                     (values (cons #`(whenever e #,@next)
                                   (append rst results))
                             null)]
                    [_ 
                     (join* (rest lines) depth (cons e results))]))]
             [(? syntax? line)
              (join* (rest lines) depth (cons line results))]
             [_ (join* (rest lines) depth results)])]))
  ;; -- in --
  (define-values (result rst) (join* lines -1))
  (if (null? rst)
      result
      (error 'internal-error "expected null, got ~s" rst)))
(module+ test
  (check-equal? (syntax->datum
                 (datum->syntax
                  #f
                  (join-lines
                   `((line 0 ,#'x)
                     (line 0 ,#'(whenever y))
                     (line 1 ,#'x)
                     (line 2 ,#'x)))))
                '(x (whenever y x x)))
  (check-equal? (syntax->datum
                 (datum->syntax
                  #f
                  (join-lines
                   `((line 0 ,#'x)
                     (line 0 ,#'(whenever y))
                     (line 1 ,#'x)
                     (line 1 ,#'(whenever z))
                     (line 2 ,#'m)
                     (line 1 ,#'z)
                     (line 0 ,#'y)))))
                '(x (whenever y x (whenever z m) z y))))
;;; parsing whenevers
;; parse an whenever header
(define parse-whenever
  (match-lambda
   [(list "whenever" _ "new" _ id) `(whenever-new ,id)]
   [(list "whenever" _ expr) `(whenever ,expr)]
   [(list do _ "whenever" _ when) `(whenever ,do ,when)]))
;; parse a whenever that has many parts
(define parse-whenever+parts
  (match-lambda
   [(list "whenever" _ _ (list (list _ part _) ...))
    `(whenever  ,(flatten-parts part))]))
;; condense the line-by-line whenever into a series of clauses
(define (flatten-parts parts [results null])
  (cond [(null? parts) (reverse results)]
        [else
         (define-values (next rest) (get-next-clause parts))
         (flatten-parts rest (cons next results))]))
;; take a line-by-line parse of a multipart whenever and get the next full clause and whats after
(define (get-next-clause parts)
  (define start 
    (syntax-parse (first parts)
      [(t e) (first parts)]
      [_ (raise-syntax-error 'whenever "whenever must start with a condition" (first parts))]))
  (define-values (bits remainder)
    (splitf-at (rest parts) 
               (lambda (stx)
                 (syntax-parse stx
                   [(e) #f]
                   [(t e) #t]))))
  (values (syntax-parse (list start bits)
            [((t e1) e2 ...)
             (syntax/loc start (t e1 e2 ...))])
          remainder))
;; parse a litteral line from the part
(define parse-whenever-part
  (match-lambda
   [(list t _ _ _ e) `(,t ,e)]
   [(list _ _ e) `(,e)]))
;;
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
          (:seq (lambda (r p) (parse-line r))
                (list INDENTATION (:/ (list Expr Whenever Means)) END))))]
  [Whenever (:/
             (list (:seq (->stx parse-whenever+parts) (list WHENEVER ?WHITESPACE END (:+ no-op (list INDENTATION WheneverPart END))))
                   (:seq (->stx parse-whenever) (list WHENEVER WHITESPACE Expr))
                   (:seq (->stx parse-whenever) (list Expr WHITESPACE WHENEVER Expr))
                   (:seq (->stx parse-whenever) (list WHENEVER NEW ID))))]
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
  [END (:& (:seq no-op (list ?WHITESPACE (:/ (list NEWLINE :EOF)))))]
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
  (define-syntax (test-top-parse stx)
    (syntax-parse stx
      [(_ t:str (~optional (~seq #:debug d))
              e:expr ...)
       #`(check-equal? (syntax->datum (parse t #,@(if (attribute d) #'(#:debug d) #'())))
                       (module 'e ...))]))
  (test-top-parse "message test is [ a b: c ]"
                  (define test (make-message a #:b c)))

  (test-top-parse "handler x is\n  test"
                  (define x (make-handler test)))

  (test-top-parse "initially\n  test"
                  (initially test))
  
  (test-top-parse "require x"
                  (add-handler x))
  (test-top-parse "initially\n  whenever\n  x | x\n  | n"
                  ;#:debug #t
                  (initially (whenever (x x))))
  (test-top-parse "handler b is\n  whenever x\n    12\n  x\ninitially\n  x"
                  (define b (make-handler (whenever x 12) x))
                  (initially x))

  (let ([in (open-input-string "#lang test\nmessage test is [ a b: c ]")])
    (read-line in)
    (check-equal? (syntax->datum (parse in))
                  (module '(define test (make-message a #:b c))))))
