#lang racket
(provide parse lex)
(require (for-syntax syntax/parse))
(require "../packrat.rkt" syntax/parse syntax/readerr)
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
           (values results null)]
          [else
           (define line (first lines))
           (match line
             [(list 'line inner-depth e)
              (if (<= inner-depth depth)
                  (let ()
                    (define-values (res b) (join* lines (sub1 depth)))
                    (values results (append res b)))
                  (syntax-parse e
                    [(whenever e ...) 
                     (define-values (next rst) (join* (rest lines) inner-depth))
                     (values (append results (list #`(whenever e ... #,@next)) rst) null)]
                    [_ 
                     (join* (rest lines) depth (append results (list e)))]))]
             [(? syntax? line)
              (join* (rest lines) depth (append results (list line)))]
             [_ (join* (rest lines) depth results)])]))
  ; -- in --
  (define-values (result rst) (join* lines -1))
  (if (null? rst)
      result
      (error 'internal-error "expected null, got ~s result" rst)))

(module+ test
  (check-equal? (syntax->datum
                 (datum->syntax
                  #f
                  (join-lines
                   `((line 0 ,#'x)
                     (line 0 ,#'(whenever y #:derp 1))
                     (line 1 ,#'x)
                     (line 2 ,#'x)))))
                '(x (whenever y #:derp 1 x x)))
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
   [(list "whenever" _ expr (list extras ...)) `(whenever ,expr ,@(flatten extras))]
   [(list do _ "whenever" _ when) `(whenever ,do ,when)]))
;; parse a whenever that has many parts
(define parse-whenever+parts
  (match-lambda
   [(list "whenever" _ _ (list (list _ part _) ...))
    `(whenever  ,@(flatten-parts part))]))
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
                   [(e) #t]
                   [(t e) #f]))))
  (values (syntax-parse (list* start bits)
            [((t e1) (e2) ...)
             (define (stx->line stx)
               (syntax-parse stx
                 #:datum-literals (line)
                 [(line n e)
                  `(line ,(syntax->datum #'n) ,#'e)]))
             (quasisyntax/loc start (t #,@(join-lines (map stx->line (syntax->list #'(e1 e2 ...))))))])
          remainder))
(module+ test 
  (check-equal? (map syntax->datum (flatten-parts (list #'(t1 (line 0 1)) #'((line 5 2)) #'((line 4 3)) #'(t2 (line 12 4)) #'(t3 (line 6 5)))))
                '((t1 1 2 3) (t2 4) (t3 5))))
;; parse a literal line from the part
(define parse-whenever-part
  (match-lambda
   [(list t _ _ e) `(,t ,e)]
   [(list _ e) `(,e)]))
;;
(define parse-means
  (match-lambda
   [(list id _ _ _ expr) `(define ,id ,expr)]))
(define parse-require
    (match-lambda
     [(list _ _ name _ _)
      `(add-handler ,name)]))

(define (raise-parse-error p m)
  (raise-read-error m
                    (position-source p)
                    (position-line p)
                    (position-col p)
                    (position-start p)
                    (position-span p)))
(define-parser/colorer (parse lex)
  [Top (:seq (->stx
              (compose
               (lambda (b) (list* 'module 'TODO 'pop-pl b))
               second))
             (list 
              (:? no-op LANG)
              (:+ no-op (:seq (lambda (r p) (second r)) (list (:? no-op NEWLINE) (:/  (list Require COMMENT Initially Handler Message)))))
              :EOF))]
  [COMMENT (:rx (->stx (const '(void)))
                #rx"//.*?(\n|$)")]
  
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
                (list INDENTATION Line-Body END))
          ;; errors
          (:seq (lambda (r p) (raise-parse-error p "a line must start with indentation"))
                (list (:/ (list Whenever Means Expr)) END))))]
  [Whenever (:/
             (list (:seq (->stx parse-whenever+parts) (list WHENEVER ?WHITESPACE END (:+ no-op (:seq no-op (list INDENTATION WheneverPart END)))))
                   (:seq (->stx parse-whenever) (list WHENEVER WHITESPACE NEW WHITESPACE ID))
                   (:seq (->stx parse-whenever) (list WHENEVER WHITESPACE Expr (:* no-op WheneverExtras)))
                   (:seq (->stx parse-whenever) (list Expr WHITESPACE WHENEVER WHITESPACE Expr))))]
  [Means (:seq (->stx parse-means) (list ID WHITESPACE MEANS WHITESPACE Expr))]
  [WheneverExtras (:seq (lambda (r p) (fourth r))
                        (list ?WHITESPACE COMMA ?WHITESPACE WheneverExtrasStx))]
  [WheneverExtrasStx (:/ (list (:seq (lambda (r p) (list ((->stx values) '#:times p) (first r)))
                                     (list NUMBER-RAW WHITESPACE "times"))
                               (:seq (lambda (r p) (list ((->stx values) '#:apart p) (first r)))
                                     (list Number WHITESPACE "apart"))))]
  [WheneverPart (:/ 
                 (list 
                  (:seq (->stx parse-whenever-part) (list Expr WHITESPACE PIPE Line-Like))
                  (:seq (->stx parse-whenever-part) (list PIPE Line-Like))))]
  [Line-Like (:seq (lambda (r p) (parse-line r))
                   (list SPACING Line-Body END))]

  [Line-Body (:/ (list Whenever Means Expr))]

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
  
  ;;; expressions
  [Expr (:/
         (list
          Infix
          STRING
          Call
          ID))]
  
  ;; function calls
  [Call (:seq (->stx flatten)
              (list ID Args))]
  [Args (:/ (list (:seq (lambda (r p) (match (flatten r) [(list _ a ... _) a])) (list OPEN-PAREN (:? no-op ArgsListCall) CLOSE-PAREN))
                  ArgsListCall))]
  [ArgsListCall
   (:+ (lambda (r p) (filter (negate string?) r)) 
       (:/ (list
            (:seq (lambda (r p) (match r [(list _ k _ e) (list k e)])) (list WHITESPACE KEYWORD WHITESPACE Expr))
            (:seq (->stx second) (list WHITESPACE Expr)))))]
  
  ;; infix 

  [Infix (:/ (list Number Todo))]
  [Number (:/
           (list Number+Unit
                 NUMBER-RAW))]
  [Number+Unit (:seq (->stx (lambda (s) `(number ,(first s) ,(third s)))) (list NUMBER-RAW WHITESPACE Unit))]
  [NUMBER-RAW (:rx (->stx string->number) #rx"[0-9]+(\\.[0-9]+)?")]
  [Unit (:seq (->stx (compose string->symbol (curry apply string-append) flatten))
              (list UNIT-RAW (:? no-op (:* no-op (:seq no-op (list "/" UNIT-RAW))))))]
  [UNIT-RAW (:/ (sort
                 (list "units"
                       "unit"
                       "kgs"
                       "kg"
                       "cm"
                       "mgs"
                       "mg"
                       "micrograms"
                       "microgram"
                       "hours"
                       "hour"
                       "days"
                       "day"
                       "weeks"
                       "week"
                       "minutes"
                       "minute")
                 (lambda (l r) (>  (string-length l) (string-length r)))))]
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
  [STRING (:rx (->stx (lambda (s) (substring s 1 (sub1 (string-length s))))) #rx"\".*?[^\\]\"")]
  [WHITESPACE (:rx no-op #rx" +")]
  [?WHITESPACE (:? no-op WHITESPACE)]
  [KEYWORD (:seq (->stx (compose string->keyword symbol->string syntax->datum first))
                 (list ID-LIKE ":"))]
  [ID (:seq (->stx first)
            (list ID-LIKE
                  (:/ (list (:! ":") END))))]
  [ID-LIKE (:rx (->stx string->symbol)  #rx"[a-zA-Z][a-zA-Z0-9]*")]
  [OPEN-PAREN "("]
  [CLOSE-PAREN ")"]
  [NEW "new"]
  [COMMA ","]
  ;; silly
  [Todo (:! (:? no-op (:rx no-op #rx".")))]
  [LANG (:seq no-op (list "#lang" (:rx no-op #rx".*\n")))]
  [INCOMPLETE-STRING (:rx no-op #rx"\"[^\"]*")]
  #:tokens 
  (comment LANG COMMENT) 
  (other REQUIRE MESSAGE IS OPEN-BRACKET CLOSE-BRACKET WHENEVER HANDLER INITIALLY MEANS PIPE AFTER
         OP FUNCTION NEW COMMA) 
  (white-space NEWLINE WHITESPACE) 
  (constant STRING INCOMPLETE-STRING Number Unit) 
  (keyword KEYWORD)
  (no-color ID)
  (parenthesis OPEN-PAREN CLOSE-PAREN))

(module+ test
  (define (module . e)
    `(module TODO pop-pl ,@e))
  (define-syntax (test-parse stx)
    (syntax-parse stx
      [(_ t:str
          (~optional (~seq #:debug d))
          (~optional (~seq #:pattern p))
          e:expr ...)
       #`(let ([val (parse t 
                           #,@(if (attribute d) #'(#:debug d) #'())
                           #,@(if (attribute p) #'(#:pattern p) #'()))]
               [res #,@(if (attribute p) 
                           #'('e ...)
                           #'((module 'e ...)))]) 
           (define (convert v)
             (cond [(syntax? v)
                    (syntax->datum v)]
                   [(list? v)
                    (map convert v)]
                   [else v]))
           (if (not val)
               (fail (~a "parsing " t " returned false, expected " res))
               #,(quasisyntax/loc stx
                   (check-equal? (convert val)
                                 res 
                                 t))))]))
  (test-parse "message test is [ a b: c ]"
              (define test (make-message a #:b c)))

  (test-parse "handler x is\n  test"
              (define x (make-handler test)))
  
  (test-parse "initially\n whenever x, 3 times, 1 hour apart\n  x"
              (initially
               (whenever x #:times 3 #:apart (number 1 hour) x)))
  (test-parse "1 hour"
              #:pattern Number
              (number 1 hour))

  (test-parse "initially\n  test"
              (initially test))
  
  (test-parse "require x"
              (add-handler x))
  (test-parse "initially\n  whenever\n   x | x\n   | n"
              (initially (whenever (x x n))))
  (test-parse "initially\n  whenever x\n    x\n    n"
              (initially (whenever x x n)))
  (test-parse "handler b is\n  whenever x\n    12\n  x\ninitially\n  x"
              (define b (make-handler (whenever x 12) x))
              (initially x))
  (test-parse "require m"
              (add-handler m))
  (test-parse "initially\n whenever\n x | whenever x\n |  y"
              (initially
               (whenever
                [x (whenever x y)])))
  (test-parse 
   "handler b is
  QQ
  whenever t1
    e1
    e2
    whenever new x
      e3
      e4
    whenever
    g | m
      | x
    g2 | v
       | v2
    e5
initially
  whenever 12
    m"
 (define b (make-handler
              QQ
              (whenever t1
                        e1
                        e2 
                        (whenever-new x e3 e4)
                        (whenever (g m x) (g2 v v2))
                        e5)))
   (initially (whenever 12 m)))
  (test-parse "\n  QQ"
              #:pattern Line
              (line 2 QQ))
  (test-parse "\n whenever t1"
              #:pattern Line
              (line 1 (whenever t1)))
  (test-parse "\n    e1"
              #:pattern Line
              (line 4 e1))
  (test-parse "\n  whenever new x"
              #:pattern Line
              (line 2 (whenever-new x)))
 
  ;;; expressions
  (test-parse "x"
              #:pattern Expr
              x)
  (test-parse "x 1 2 y: 3 z"
              #:pattern Call
              (x 1 2 #:y 3 z))
  (test-parse "x 1 2 y: 3 z"
              #:pattern Expr
              (x 1 2 #:y 3 z))
  (test-parse "1+2*4"
              #:pattern Expr
              (+ 1 (* 2 4)))
  (test-parse "x 1 + 4 * 3 by: 3*z z"
              #:pattern Expr
              (x (+ 1 (* 4 3)) #:by (* 3 z) z))
  (test-parse "x \"m\" by: z"
              #:pattern Expr
              (x "m" #:by z))
  
  
  ;;; the big one
  #;
  (test-parse
"#lang pop-pl/current
require heparinPttChecking
require heparinInfusion
require ivInserted

initially 
   giveBolus 80 units/kg of: \"heparin\" by: \"iv\"
   start 18 units/kg/hour of: \"heparin\"

handler infusion is
  whenever new ptt
    whenever
    aPtt < 45        | giveBolus 80 units/kg of: \"heparin\" by: \"iv\"
                     | increase \"heparin\" by: 3 units/kg/hour

    45 < aPtt < 59   | giveBolus 40 units/kg of: "heparin" by: "iv"
                     | increase \"heparin\" by: 1 unit/kg/hour

    101 < aPtt < 123 | decrease \"heparin\" by: 1 unit/kg/hour

    aPtt > 123       | hold \"heparin\"
                     | after 1 hour
                     |     restart \"heparin\"
                     |     decrease \"heparin\" by: 3 units/kg/hour"
   (add-handler heparinPttChecking)
   (add-handler heparinInfusion)
   (add-handler ivInserted)
   (initially
    (giveBolus (number 80 units/kg) #:of "heparin" #:by "iv")
    (start (number 18 units/kg/hour) #:of "heparin"))
   (define infusion
     (make-handler
      (whenever-new
       ptt
       (whenever
        [(< aPtt 45) ]
        [])))))

  (let ([in (open-input-string "#lang test\nmessage test is [ a b: c ]")])
    (read-line in)
    (check-equal? (syntax->datum (parse in))
                  (module '(define test (make-message a #:b c))))))
