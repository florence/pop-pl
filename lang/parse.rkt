#lang racket/base
(provide parse lex raise-parse-error Expr)
(require (for-syntax syntax/parse racket/base)
         racket/function
         racket/format
         racket/list
         racket/match
         syntax/parse
         syntax/readerr
         "../packrat.rkt")

(module+ test (require rackunit))

(define TAB-WIDTH 4)

(define orig-prop (read-syntax 'x (open-input-bytes #"x")))
(define ((->stx f) e p)
  (datum->syntax #f
                 (f e)
                 (position->vector p)
                 orig-prop))
(define (->stx/filter f)
  (->stx (lambda (r) (f (filter syntax? (flatten r))))))
(define (no-op r p) r)
(define raw (->stx values))
(define message-parse
  (match-lambda [(list m _ id _ _ _ f _) `(,(datum->syntax #f 'define-message m) ,id ,f)]
           [(list m _ id args _ _ _ e _)
            `(,(datum->syntax #f 'define-message m) ,id ,args ,e)]))
(define message-maker
  (match-lambda
   [(list _ args _) args]))
(define parse-init
  (match-lambda
   [(list i _ lines)
    `(,i ,@(join-lines lines))]))
(define parse-handler
  (match-lambda
   [(list h _ name _ _ _ _ lines)
    `(,(datum->syntax #f 'define-handler h) ,name ,@(join-lines lines))]
   [(list kn _ _ lines)
    (define name (datum->syntax kn (string->symbol (keyword->string (syntax->datum kn)))))
    `(,(datum->syntax #f 'define-handler kn) ,name ,@(join-lines lines))]))
(module+ test
  #;
  (let ([h #'handler]
  [s #'(line 0 2)])
  (check-equal? (parse-handler (list h " " 'x "is" " " #t "\n" `(,s ,42)))
  `(,(datum->syntax h 'define-handler) x ,s))))
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
  (define-syntax-class scope-introducer
    #:datum-literals (after whenever whenever-new whenever-cond)
    (pattern (~or whenever whenever-new after)))
  (define (join* lines depth [results null])
    (cond [(null? lines)
           (values results null)]
          [else
           (define line (first lines))
           (match line
             [(list 'line inner-depth l)
              (if (<= inner-depth depth)
                  (let ()
                    (define-values (res b) (join* lines (sub1 depth)))
                    (values results (append res b)))
                  (syntax-parse l
                    [(i:scope-introducer e ...)
                     (define-values (next rst) (join* (rest lines) inner-depth))
                     (values (append results (list (quasisyntax/loc l (i e ... #,@next))) rst) null)]
                    [_
                     (join* (rest lines) depth (append results (list l)))]))]
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
;; parse whenever part lines
(define parse-iwpe
  (lambda (r p) (filter list? r)))
;; parse an whenever header
(define parse-whenever
  (match-lambda
   [(list w _ "new" _ id #t) `(,(datum->syntax #f 'whenever-new w) ,id)]
   [(list w _ "new" _ id (list _ _ _ e))
    `(,(datum->syntax #f 'whenever-new w) (,id ,e))]
   [(list w _ "latest" _ e)
    `(,w
      ,e
      ,(datum->syntax #f '#:latest w)
      ,(datum->syntax #f '#t w))]
   [(list w _ "latest" _ e (list* extras))
    `(,w
      ,e
      ,(datum->syntax #f '#:latest w)
      ,(datum->syntax #f '#t w)
      ,@(flatten extras))]
   [(list w _ expr (list* extras)) `(,w ,expr ,@(flatten extras))]
   [(list do _ w _ when (list* extras)) `(,w ,when ,@(flatten extras) ,do)]
   [(list do _ w _ "latest" _ when (list* extras)) `(,w ,when #:latest #t ,@(flatten extras) ,do)]))
;; parse a whenever that has many parts
(define (parse-whenever+parts l)
  (match l
    [(list w _ 'END (list (list _ part _) ...))
     `(,(datum->syntax #f 'whenever-cond w)  ,@(flatten-parts part))]
    [(list w ... _ 'END (list (list _ part _) ...))
     `(,@(parse-whenever w)
       (,(datum->syntax #f 'whenever-cond (first w))
        ,@(flatten-parts part)))]
    [w (parse-whenever w)]))
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
   [(list r _ name _ _)
    `(,(datum->syntax #f 'add-handler r) ,name)]))

;;; infix
(define (parse-bool-from-numb l)
  (define pats (flatten l))
  (define (go l)
    (match l
      [(list v) v]
      [(list v1 op v2) (list op v1 v2)]
      [(list* v1 op v2 rest)
       `(and ,(list op v1 v2)
         ,(go (cons v2 rest)))]))
  ;; -- in --
  (go pats))
(module+ test
  (check-equal? (parse-bool-from-numb '(1 (< (3 < 4))))
                '(and (< 1 3)
                  (< 3 4))))
(define (parse-in-range l)
  (match l
    [(list e _ _ _ o _ _ _ c)
     `(in-range ,e ,o ,c)]))
(define parse-outside
  (match-lambda
   [(list n _ _ _ _ _ _ _ lower _ _ _ upper)
    `(not (in-range ,n ,lower ,upper))]))
(define (raise-parse-error p m)
  (raise-read-error m
                    (position-source p)
                    (position-line p)
                    (position-col p)
                    (position-start p)
                    (position-span p)))
(define (empty-syntax id)
  (datum->syntax
   #f
   id
   #f))



(define (parse-test-top a)
  (match a
    [(list _begin results cases)
     `(here-be-tests
       (=> start ,@results)
       ,@cases)]))
(define-splicing-syntax-class ap
  (pattern (~or x:expr
                (~seq k:keyword x:expr))
           #:with nokw #'x))
(define (parse-test-case a)
  (match a
    [(list _nl _carrot _ws call _end results)
     (syntax-parse call
       [(a:ap ...)
        `(=> ,#'(a.nokw ...)
          ,@results)])]))
(define (parse-test-results a)
  (match a
    [(list _nl _ws _open call _close _end)
     (syntax-parse call
       [(a:ap ...)
        (syntax/loc call (a.nokw ...))])]))

(define-parser/colorer (parse lex)
  [Top (:seq (->stx
              (lambda (b)
                `(,(empty-syntax 'module)
                  ,(empty-syntax 'TODO)
                  ,(empty-syntax 'pop-pl/main)
                  ,@(filter syntax? (flatten b)))))
             (list
              (:? no-op LANG)
              (:* no-op
                  (:/
                   (list
                    (:seq no-op (list NEWLINE END))
                    (:seq (lambda (r p) (second r))
                          (list (:? no-op NEWLINE)
                                (:/  (list Require COMMENT Initially Handler Message Use)))))))
              (:* no-op NEWLINE)
              (:? no-op Tests)
              END*
              :EOF))]
  [COMMENT (:rx (->stx (const '(void)))
                #rx"//.*?(\n|$)")]

  [Use (:seq (->stx/filter values)
             (list USE WHITESPACE ID))]

  [Require (:seq (->stx parse-require)
                 (list REQUIRE WHITESPACE ID (:? no-op WHITESPACE) END))]


  [Initially (:seq (->stx parse-init)
                   (list INITIALLY END (:+ no-op Line)))]

  [Handler
   (:/
    (list
     (:seq (->stx parse-handler)
           (list HANDLER WHITESPACE ID WHITESPACE IS (:? no-op WHITESPACE) END (:+ no-op Line)))
     (:seq (->stx parse-handler)
           (list KEYWORD ?WHITESPACE END (:+ no-op Line)))))]

  [Line (:/
         (list
          (:seq (lambda (r p) (parse-line r))
                (list INDENTATION Line-Body END))
          EmptyLine
          ;; errors
          #;
          (:seq (lambda (r p) (raise-parse-error p "a line must start with indentation"))
          (list NEWLINE Line-Body END))))]
  [EmptyLine (:seq (lambda (r p) "")
                   (list NEWLINE END))]
  [Line-Body (:/ (list Whenever Means After Expr/CallId))]
  [After (:seq (->stx (match-lambda [(list a _ n) `(,a ,n)]))
               (list AFTER WHITESPACE Number))]

  [Whenever
   (:/
    (list
     (:seq
      (->stx (lambda (r) (parse-whenever+parts (cons (first r) (second r)))))
      (list WHENEVER
            (:~
             (:/
              (list
               (:seq no-op
                     (list ?WHITESPACE END ~
                           (:+ parse-iwpe
                               (:/ (list EmptyLine
                                         (:seq no-op (list INDENTATION WheneverPart END)))))))

               (:seq no-op
                     (list WHITESPACE NEW WHITESPACE ID
                           (:? no-op (:seq no-op (list WHITESPACE AND WHITESPACE Expr)))
                           ?WHITESPACE END
                           (:+ parse-iwpe
                               (:/ (list EmptyLine
                                         (:seq no-op (list INDENTATION WheneverPart END)))))))
               (:seq no-op
                     (list WHITESPACE NEW WHITESPACE ID
                           (:? no-op (:seq no-op (list WHITESPACE AND WHITESPACE Expr)))))
               (:seq no-op
                     (list WHITESPACE LATEST WHITESPACE Expr
                           ?WHITESPACE END
                           (:+ parse-iwpe
                               (:/ (list EmptyLine
                                         (:seq no-op (list INDENTATION WheneverPart END)))))))
               (:seq no-op
                     (list WHITESPACE LATEST WHITESPACE Expr (:* no-op WheneverExtras)
                           ?WHITESPACE END
                           (:+ parse-iwpe
                               (:/ (list EmptyLine
                                         (:seq no-op (list INDENTATION WheneverPart END)))))))
               (:seq no-op
                     (list WHITESPACE Expr (:* no-op WheneverExtras)
                           ?WHITESPACE END
                           (:+ parse-iwpe
                               (:/ (list EmptyLine
                                         (:seq no-op (list INDENTATION WheneverPart END)))))))
               (:seq no-op
                     (list WHITESPACE Expr (:* no-op WheneverExtras)))
               (:seq no-op
                     (list WHITESPACE LATEST WHITESPACE Expr (:* no-op WheneverExtras)))
               (:seq no-op
                     (list WHITESPACE LATEST WHITESPACE Expr)))))))

     (:seq (->stx parse-whenever+parts)
           (list Expr/CallId WHITESPACE WHENEVER WHITESPACE LATEST WHITESPACE Expr (:* no-op WheneverExtras)
                 ?WHITESPACE END
                 (:+ parse-iwpe
                     (:/ (list EmptyLine
                               (:seq no-op (list INDENTATION WheneverPart END)))))))
     (:seq (->stx parse-whenever+parts)
           (list Expr/CallId WHITESPACE WHENEVER WHITESPACE Expr (:* no-op WheneverExtras)
                 ?WHITESPACE END
                 (:+ parse-iwpe
                     (:/ (list EmptyLine
                               (:seq no-op (list INDENTATION WheneverPart END)))))))
     (:seq (->stx parse-whenever)
           (list Expr/CallId WHITESPACE WHENEVER WHITESPACE Expr (:* no-op WheneverExtras)))
     (:seq (->stx parse-whenever)
           (list Expr/CallId WHITESPACE WHENEVER WHITESPACE LATEST WHITESPACE Expr (:* no-op WheneverExtras)))))]

  [Means (:seq (->stx parse-means) (list ID WHITESPACE MEANS WHITESPACE Expr))]
  [WheneverExtras (:seq (lambda (r p) (fourth r))
                         (list ?WHITESPACE COMMA ~ ?WHITESPACE WheneverExtrasStx))]
  [WheneverExtrasStx (:/ (list (:seq (lambda (r p) (list ((->stx values) '#:times p) (third r)))
                                     (list X ~ ?WHITESPACE NUMBER-RAW))
                               (:seq (lambda (r p) (list ((->stx values) '#:apart p) (first r)))
                                     (list Number WHITESPACE APART))
                               (:seq (lambda (r p) (list ((->stx values) '#:since-last p) (last r)))
                                     (list SINCELAST ~ WHITESPACE CallId))))]

  [WheneverPart (:/
                 (list
                  (:seq (->stx parse-whenever-part) (list Expr ?WHITESPACE PIPE Line-Like))
                  (:seq (->stx parse-whenever-part) (list PIPE Line-Like))))]
  [Line-Like (:seq (lambda (r p) (parse-line r))
                   (list SPACING Line-Body END))]



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
                           (list MESSAGE WHITESPACE ID ArgDef WHITESPACE IS WHITESPACE Call END))))]
  [MessageForm (:seq  (->stx message-maker)
                      (list OPEN-BRACKET ArgList CLOSE-BRACKET))]

  ;; arguments
  [ArgDef (:seq (->stx second)
                (list OPEN-PAREN ArgList CLOSE-PAREN))]
  [ArgList (:* (->stx/filter values)
               (:/ (list WHITESPACE
                         (:seq no-op (list KEYWORD WHITESPACE ID))
                         ID)))]

  ;;; expressions
  [Expr (:/
         (list
          STRING
          Call
          Infix))]
  [Expr/CallParens
   (:/
    (list
     STRING
     Call+Parens
     Infix))]
  [Expr/CallId
   (:/
    (list
     STRING
     CallId
     Infix))]




  ;; function calls
  [Call+Parens (:seq (->stx flatten)
                     (list ID ArgsList+Parens))]

  [CallId (:seq (->stx
                 (lambda (r)
                   (match (flatten r)
                     [(list id #t) (list id)]
                     [v v])))
                (list ID (:? no-op Args)))]
  [Call (:seq (->stx flatten)
              (list ID Args))]
  [Args (:/ (list ArgsList+Parens ArgsListCall))]
  [ArgsList+Parens (:seq
                    (lambda (r p)
                      (match (flatten r)
                        [(list _ a ... _) a]))
                    (list OPEN-PAREN (:? no-op ArgsListCall) CLOSE-PAREN))]
  [ArgsListCall
   (:+ (lambda (r p) (filter (negate string?) r))
       (:/ (list
            (:seq (lambda (r p) (match r [(list _ k _ e) (list k e)]))
                  (list ?WHITESPACE KEYWORD WHITESPACE Expr/CallParens))
            (:seq (->stx second) (list ?WHITESPACE Expr/CallParens)))))]

  ;; infix

  [Infix (:/ (list Bool Numeric Todo))]
  [Bool Not]
  [Not (:/ (list
            (:seq
             (->stx (match-lambda [(list n _ e) (list n e)]))
             (list NOT WHITESPACE Not))
            AndOr))]
  [AndOr (:seq (->stx/filter
                (match-lambda
                 [(list b1 (? syntax? op) b2) (list op b1 b2)]
                 [(list b1) b1]))
               (list BoolFromNumTop (:? no-op (:seq no-op (list ?WHITESPACE ANDOR ?WHITESPACE Bool))) ))]
  [ANDOR (:seq (->stx (lambda (r) (string->symbol (first r)))) (list (:/ (list AND OR))))]





  [BoolFromNumTop (:seq (->stx/filter parse-bool-from-numb) (list BoolFromNum))]
  [BoolFromNum
   (:seq no-op (list Range (:? no-op (:seq no-op (list ?WHITESPACE LGT ?WHITESPACE BoolFromNum)))))]
  [LGT (:seq (->stx (lambda (r) (string->symbol (first r)))) (list (:/ (list IS "<=" ">=" ">" "<" "="))))]


  [Range (:/ (list
              (:seq (->stx parse-in-range)
                    (list Numeric WHITESPACE
                          Between WHITESPACE
                          Numeric WHITESPACE To WHITESPACE Numeric))
              (:seq (->stx parse-outside)
                    (list Numeric WHITESPACE
                          OUTSIDE WHITESPACE (:? no-op OF) ?WHITESPACE (:? no-op RANGE) ?WHITESPACE
                          Numeric WHITESPACE TO WHITESPACE Numeric))
              Numeric))]

  [To (:/ (list TO AND))]
  [Between (:/ (list (:seq no-op (list IN (:? no-op (:seq no-op (list WHITESPACE RANGE)))))
                     BETWEEN))]

  [Numeric Sum]



  [Sum (:seq (->stx/filter
              (match-lambda
               [(list r) r]
               [(list (? (lambda (l) (equal? (syntax-e l) '-))
                         s)
                      r)
                (list s r)]
               [(list* s r (? (negate null?) rest)) `(+ ,(list s r) ,@rest)]
               [(list* r rest) `(+ ,r ,@rest)]))
             (list
              (:? no-op MINUS)
              Product
              (:* no-op
                  (:seq (->stx/filter (lambda (r) (if (null? (rest r)) (first r) r)))
                        (list ?WHITESPACE PM ?WHITESPACE Product)))))]
  [PM (:/ (list PLUS MINUS))]
  [PLUS "+"]
  [MINUS (:seq (->stx (const '-)) (list "-"))]
  [Product (:seq (->stx/filter
                  (match-lambda
                   [(list r) r]
                   [(list r rest) `(* ,r ,rest)]))
                 (list NValue
                       (:* no-op
                           (:seq (->stx/filter (lambda (r) (if (null? (rest r)) (first r) r)))
                                 (list ?WHITESPACE TS ?WHITESPACE Product)))))]
  [TS (:/ (list TIMES DIVIDE))]
  [TIMES "*"]
  [DIVIDE (:seq (->stx (const '/)) (list "/"))]
  [NValue (:/ (list ID Number STRING (:seq (->stx second) (list OPEN-PAREN Numeric CLOSE-PAREN))))]
  [Number (:/
           (list Number+Unit
                 NUMBER-RAW))]
  [Number+Unit (:seq (->stx (lambda (s) `(-number ,(first s) ,(third s))))
                     (list NUMBER-RAW WHITESPACE Unit))]
  [NUMBER-RAW (:rx (->stx string->number) #rx"[0-9]+(\\.[0-9]+)?")]
  [NUMBER-RAW-TOK (:rx no-op #rx"[0-9]+(\\.[0-9]*)?")]
  [Unit (:seq (->stx (compose string->symbol (curry apply string-append) flatten))
              (list UNIT-RAW (:? no-op (:* no-op (:seq no-op (list "/" UNIT-RAW))))))]
  [UnitTok (:seq no-op
                 (list UNIT-RAW (:? no-op (:* no-op (:seq no-op (list "/" (:? no-op UNIT-RAW)))))))]
  [UNIT-RAW (:/ (sort
                 (list "units"
                       "unit"
                       "kgs"
                       "kg"
                       "cm"
                       "mgs"
                       "mg"
                       "dl"
                       "micrograms"
                       "microgram"
                       "gram"
                       "grams"
                       "hours"
                       "hour"
                       "days"
                       "day"
                       "weeks"
                       "week"
                       "minutes"
                       "minute"
                       "seconds"
                       "second")
                 (lambda (l r) (> (string-length l) (string-length r)))))]
  [OP (:/ (list "and" "or" "=" "<" ">" "+" "-" "*" "/"))]


  [Tests
   (:seq (->stx parse-test-top)
         (list TEST
               TestResults
               (:* no-op TestCase)))]
  [TestCase
   (:seq (->stx parse-test-case)
         (list
          (:+ no-op NEWLINE)
          CARROT
          ?WHITESPACE
          CallId
          END
          TestResults))]
  [TestResults
   (:* no-op
       (:seq (->stx parse-test-results)
             (list NEWLINE ?WHITESPACE OPEN-BRACKET CallId CLOSE-BRACKET END)))]


  ;; keywords
  [Keywords (:/ (list REQUIRE MESSAGE IS OPEN-BRACKET CLOSE-BRACKET
                      WHENEVER INITIALLY MEANS PIPE AFTER FUNCTION NOT
                      UNIT-RAW AND OR OP NEW COMMA OPEN-PAREN CLOSE-PAREN
                      SINCELAST APART
                      IN RANGE TO OF OUTSIDE BETWEEN LATEST
                      TEST CARROT))]
  [TEST (:seq no-op
              (list (:* no-op "-")
                    ?WHITESPACE
                    "Tests"
                    ?WHITESPACE
                    (:* no-op "-")
                    END
                    (:* no-op (:seq no-op (list NEWLINE END)))))]
  [CARROT ">"]
  [AND "and"]
  [OR "or"]
  [REQUIRE (:lit (->stx string->symbol) "require")]
  [MESSAGE (:lit (->stx string->symbol) "message")]
  [IS "is"]
  [OPEN-BRACKET "["]
  [CLOSE-BRACKET "]"]
  [WHENEVER (:lit (->stx string->symbol) "whenever")]
  [HANDLER (:lit (->stx string->symbol) "handler")]
  [INITIALLY (:lit (->stx string->symbol) "initially")]
  [MEANS "means"]
  [PIPE "|"]
  [AFTER (:lit (->stx string->symbol) "after")]
  [FUNCTION "function"]
  [NOT (:lit (->stx string->symbol) "not")]
  [X "x"]
  [APART "apart"]
  [SINCELAST "since last"]
  [SINCE "since"]
  [LAST "last"]
  [IN "in"]
  [RANGE "range"]
  [TO "to"]
  [OUTSIDE "outside"]
  [OF "of"]
  [USE (:seq (->stx (const 'use))
             (list (:/ (list (:seq no-op (list USED WHITESPACE BY))
                             (:seq no-op (list USED WHITESPACE AT))))))]
  [USED "used"]
  [BY "by"]
  [AT "at"]
  [BETWEEN "between"]
  [LATEST  "latest"]

  ;; basics
  [END (:/ (list (:seq (const 'END) (list ?WHITESPACE (:& (:/ (list NEWLINE &EOF)))))
                 (:seq (const 'END)
                       (list ?WHITESPACE (:& COMMENT)
                             (:rx no-op #px"//.*?(?=(\n|$))")
                             (:& (:/ (list NEWLINE &EOF)))))))]
  [END* (:* (const 'END)
            (:/ (list WHITESPACE NEWLINE)))]
  [&EOF (:& :EOF)]
  [NEWLINE "\n"]
  [STRING (:rx (->stx (lambda (s) (substring s 1 (sub1 (string-length s))))) #rx"\".*?[^\\]\"")]
  ;; handle non-breaking spaces for scribble
  [WHITESPACE (:rx no-op #px"( |\xA0)+")]
  [?WHITESPACE (:? no-op WHITESPACE)]
  [KEYWORD (:seq (->stx (compose string->keyword symbol->string syntax->datum first))
                 (list ID-LIKE ":"))]
  [ID (:seq (->stx second)
            (list
             (:& (:! (:seq no-op (list Keywords (:/ (list " " END))))))
             ID-LIKE
             (:/ (list (:! ":") END))))]
  [ID-LIKE (:rx (->stx string->symbol)  #rx"[a-zA-Z][a-zA-Z0-9]*")]
  [OPEN-PAREN "("]
  [CLOSE-PAREN ")"]
  [NEW "new"]
  [COMMA ","]
  ;; silly
  [Todo (:! (:? no-op (:rx no-op #rx".")))]
  [LANG (:seq no-op (list "#lang" (:rx no-op #rx".*?\n")))]
  [INCOMPLETE-STRING (:rx no-op #rx"\"[^\"]*")]
  [Other (:/ (list TEST CARROT REQUIRE MESSAGE IS OPEN-BRACKET CLOSE-BRACKET WHENEVER HANDLER
                   INITIALLY MEANS PIPE AFTER OP FUNCTION NEW COMMA NOT X SINCE LAST APART IN
                   RANGE TO OUTSIDE OF USED BY BETWEEN LATEST))]
  [Other+End
   (:/
    (list
     X
     OP
     OPEN-PAREN
     OPEN-BRACKET
     CLOSE-PAREN
     CLOSE-BRACKET
     (:seq
      no-op
      (list
       Other
       (:& (:/ (list WHITESPACE END)))))))]
  #:tokens
  (comment COMMENT)
  (hash-colon-keyword KEYWORD)
  (other LANG Other+End)
  (white-space NEWLINE WHITESPACE)
  (constant STRING INCOMPLETE-STRING NUMBER-RAW-TOK UnitTok)
  (symbol ID)
  (parenthesis OPEN-PAREN CLOSE-PAREN))

(module+ test
  (define (module . e)
    `(module TODO pop-pl/main ,@e))
  (define-syntax (test-parse stx)
    (syntax-parse stx
      [(_ t:str
          (~optional (~seq #:debug d))
          (~optional (~seq #:pattern p))
          e:expr ...)
       (quasisyntax/loc stx
         (let-values ([(val _) (parse t
                                      #,@(if (attribute d) #'(#:debug d) #'())
                                      #,@(if (not (attribute p))
                                             #'()
                                             #'(#:pattern (:seq (lambda (r p) (first r)) (list p :EOF)))))]
                      [(res) #,@(if (attribute p)
                                    #'('e ...) ;; there should only be one here
                                    #'((module 'e ...)))])
           (define (convert v)
             (cond [(syntax? v)
                    (syntax->datum v)]
                   [(list? v)
                    (map convert v)]
                   [else v]))
           (if (not val)
               #,(quasisyntax/loc stx (fail (~a "parsing \"" t "\" returned false, expected " res)))
               #,(quasisyntax/loc stx
                   (check-equal? (convert val)
                                 res
                                 t)))))]))
  (test-case
   "Begin"
   (test-parse "message test is [ a b: c ]"
               (define-message test (a #:b c)))
   (test-parse "message decrease(drug by: c) is change(drug by: -change)"
               (define-message decrease (drug #:by c) (change drug #:by (- change))))
   (test-parse "change(drug by: -change)"
               #:pattern Expr
               (change drug #:by (- change)))
   (test-parse "change(x)"
               #:pattern Expr
               (change x))
   (test-parse "-change"
               #:pattern Expr
               (- change))

   (test-parse "handler x is\n  test"
               (define-handler x (test)))
   (test-parse "hanDlEr X iS\n  tEst"
               (define-handler x (test)))
   (test-parse "initially\n whenever x, x3, 1 hour apart\n  x"
               (initially
                (whenever x #:times 3 #:apart (-number 1 hour) (x)))))
  (test-case
   "Second"
   (test-parse "1 hour"
               #:pattern Number
               (-number 1 hour))
   (test-parse "-1"
               #:pattern Expr
               (- 1))
   (test-parse "-1-1"
               #:pattern Expr
               (+ (- 1) (- 1)))
   (test-parse "initially\n notifyDoctor whenever painscore > 8, x3, since last notifyDoctor"
               (initially
                (whenever (> painscore 8) #:times 3 #:since-last (notifydoctor)
                          (notifydoctor))))
   (test-parse "initially\n  test"
               (initially (test)))

   (test-parse "require x"
               (add-handler x))
   (test-parse "initially\n  whenever\n   x | x\n   | n"
               (initially (whenever-cond (x (x) (n)))))
   (test-parse "initially\n  whenever x\n    x\n    n"
               (initially (whenever x (x) (n))))
   (test-parse "initially\n  whenever latest x\n    x\n    n"
               (initially (whenever x #:latest #t (x) (n))))
   (test-parse "handler b is\n  whenever x\n    12\n  x\ninitially\n  x"
               (define-handler b (whenever x 12) (x))
               (initially (x)))
   (test-parse "require m"
               (add-handler m))
   ;; uuggg.... do i really care?
   #;
   (test-parse "initially
   whenever
   x | whenever x
   |  y"
   (initially
   (whenever-cond
   [x (whenever x (y))]))))
  (test-parse
   "initially
  whenever new x
      y | y"
   (initially (whenever-new x (whenever-cond (y (y))))))
  (test-parse "initially\n after 1 hour\n  x\n  y"
              (initially (after (-number 1 hour) (x) (y))))
  (test-parse "x y z"
              #:pattern CallId
              (x y z))
  (test-case
   "collect"
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
    (define-handler b
      (qq)
      (whenever t1
                (e1)
                (e2)
                (whenever-new x (e3) (e4))
                (whenever-cond [g (m) (x)] [g2 (v) (v2)])
                (e5)))
    (initially (whenever 12 (m))))
   (test-parse "\n  QQ"
               #:pattern Line
               (line 2 (qq)))
   (test-parse "\n whenever t1"
               #:pattern Line
               (line 1 (whenever t1)))
   (test-parse "\n    e1"
               #:pattern Line
               (line 4 (e1)))
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
   (test-parse "x(1 2 y: 3 z)"
               #:pattern Expr
               (x 1 2 #:y 3 z))
   (test-parse "1+2*4"
               #:pattern Expr
               (+ 1 (* 2 4)))
   (test-parse "x 1+4*3 by: 3*z z"
               #:pattern Expr
               (x (+ 1 (* 4 3)) #:by (* 3 z) z))
   (test-parse "x \"m\" by: z"
               #:pattern Expr
               (x "m" #:by z)))

  ;; infix
  (test-case
   "infix"
   (test-parse "1<2"
               #:pattern Expr
               (< 1 2))
   (test-parse "x<2"
               #:pattern Expr
               (< x 2))
   (test-parse "1<2<3"
               #:pattern Expr
               (and (< 1 2) (< 2 3)))
   (test-parse "4>=4"
               #:pattern Expr
               (>= 4 4))
   (test-parse "1 < 2 < 3 and 4 >= 4"
               #:pattern Expr
               (and (and (< 1 2) (< 2 3))
                    (>= 4 4)))
   (test-parse "1<2<3and4>=4"
               #:pattern Expr
               (and (and (< 1 2) (< 2 3))
                    (>= 4 4)))
   (test-parse "1<2<3and0.4>=4.0"
               #:pattern Expr
               (and (and (< 1 2) (< 2 3))
                    (>= 0.4 4.0))))
  ;; ranges
  (test-case
   "range"
   (test-parse "x in 5 to z"
               #:pattern Expr
               (in-range x 5 z))
   (test-parse "x"
               #:pattern Sum
               x)
   (test-parse "x in 5 to z"
               #:pattern Range
               (in-range x 5 z))
   (test-parse "x in 5 to 5"
               #:pattern Range
               (in-range x 5 5))
   (test-parse "5 in range 5 to z"
               #:pattern Range
               (in-range 5 5 z))
   (test-parse "1 in range 5 to 12"
               #:pattern Expr
               (in-range 1 5 12))
   (test-parse "1 between 5 and 12"
               #:pattern Expr
               (in-range 1 5 12))
   (test-parse "1 outside of 5 to 12"
               #:pattern Expr
               (not (in-range 1 5 12)))
   (test-parse "1 outside 5 to 12"
               #:pattern Expr
               (not (in-range 1 5 12)))
   (test-parse "ptt outside of 59 to 101"
               #:pattern Expr
               (not (in-range ptt 59 101)))
   (test-parse "ptt outside of range 59 to 101"
               #:pattern Expr
               (not (in-range ptt 59 101)))
   (test-parse "ptt outside 59 to 101"
               #:pattern Expr
               (not (in-range ptt 59 101))))
  ;;; the big ones
  (test-case
   "big1"
   (test-parse
    "giveBolus 80 units/kg of: \"heparin\" by: \"iv\""
    #:pattern Expr
    (givebolus (-number 80 units/kg) #:of "heparin" #:by "iv"))
   (test-parse
    "start 18 units/kg/hour of: \"heparin\""
    #:pattern Expr
    (start (-number 18 units/kg/hour) #:of "heparin"))
   (test-parse
    "initially
   giveBolus 80 units/kg of: \"heparin\" by: \"iv\""
    (initially (givebolus (-number 80 units/kg) #:of "heparin" #:by "iv")))
   (test-parse
    "initially
   x"
    (initially (x)))
   (test-parse "\n  x"
               #:pattern Line
               (line 2 (x)))
   (test-parse "x"
               #:pattern Line-Body
               (x))
   (test-parse
    "initially
   giveBolus 80 units/kg of: \"heparin\" by: \"iv\"
   start 18 units/kg/hour of: \"heparin\""
    (initially
     (givebolus (-number 80 units/kg) #:of "heparin" #:by "iv")
     (start (-number 18 units/kg/hour) #:of "heparin")))
   (test-parse
    "handler infusion is
  whenever new ptt
    whenever x
     x"
    (define-handler infusion (whenever-new ptt (whenever x (x))))))
  (test-case
   "big2"
   (test-parse
    "handler infusion is
  whenever new ptt
    whenever
     x | x"
    (define-handler infusion (whenever-new ptt (whenever-cond (x (x))))))
   (test-parse
    "handler infusion is
  whenever new ptt
    whenever
     x < x| x"
    (define-handler infusion (whenever-new ptt (whenever-cond ((< x x) (x))))))
   (test-parse
    "handler infusion is
  whenever new ptt
    whenever
     x < x<x| x"
    (define-handler  infusion (whenever-new ptt (whenever-cond ((and (< x x) (< x x)) (x))))))
   (test-parse "x < x"
               #:pattern Expr
               (< x x))
   (test-parse
    "handler infusion is
  whenever new ptt
    whenever
     x < x<x | x

            | x

       y | z
         | q"
    (define-handler infusion
      (whenever-new ptt
                    (whenever-cond
                     [(and (< x x) (< x x)) (x) (x)]
                     [y (z) (q)])))))
  (test-case
   "big3"
   (test-parse
    "handler infusion is
  whenever new ptt
    whenever
     x < x<x | x

            | x

       y | after 1 hour
         |   x"
    (define-handler infusion
      (whenever-new ptt
                    (whenever-cond
                     [(and (< x x) (< x x)) (x) (x)]
                     [y (after (-number 1 hour) (x))]))))
   (test-parse
    "#lang pop-pl
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

    45 < aPtt < 59   | giveBolus 40 units/kg of: \"heparin\" by: \"iv\"
                     | increase \"heparin\" by: 1 unit/kg/hour

    101 < aPtt < 123 | decrease \"heparin\" by: 1 unit/kg/hour

    aPtt > 123       | hold \"heparin\"
                     | after 1 hour
                     |     restart \"heparin\"
                     |     decrease \"heparin\" by: 3 units/kg/hour"
    (add-handler heparinpttchecking)
    (add-handler heparininfusion)
    (add-handler ivinserted)
    (initially
     (givebolus (-number 80 units/kg) #:of "heparin" #:by "iv")
     (start (-number 18 units/kg/hour) #:of "heparin"))
    (define-handler infusion
      (whenever-new
       ptt
       (whenever-cond
        [(< aptt 45)
         (givebolus (-number 80 units/kg) #:of "heparin" #:by "iv")
         (increase "heparin" #:by (-number 3 units/kg/hour))]
        [(and (< 45 aptt)
              (< aptt 59))
         (givebolus (-number 40 units/kg) #:of "heparin" #:by "iv")
         (increase "heparin" #:by (-number 1 unit/kg/hour))]
        [(and (< 101 aptt)
              (< aptt 123))
         (decrease "heparin" #:by (-number 1 unit/kg/hour))]
        [(> aptt 123)
         (hold "heparin")
         (after (-number 1 hour)
                (restart "heparin")
                (decrease "heparin" #:by (-number 3 units/kg/hour)))]))))
   (test-parse
    "#lang pop-pl
require heparinPttChecking
require heparinInfusion
require ivInserted

initially
   giveBolus 80 units/kg of: \"heparin\" by: \"iv\"
   start 18 units/kg/hour of: \"heparin\"

infusion:
  whenever new ptt
    whenever
    aPtt < 45        | giveBolus 80 units/kg of: \"heparin\" by: \"iv\"
                     | increase \"heparin\" by: 3 units/kg/hour

    45 < aPtt < 59   | giveBolus 40 units/kg of: \"heparin\" by: \"iv\"
                     | increase \"heparin\" by: 1 unit/kg/hour

    101 < aPtt < 123 | decrease \"heparin\" by: 1 unit/kg/hour

    aPtt > 123       | hold \"heparin\"
                     | after 1 hour
                     |     restart \"heparin\"
                     |     decrease \"heparin\" by: 3 units/kg/hour"
    (add-handler heparinpttchecking)
    (add-handler heparininfusion)
    (add-handler ivinserted)
    (initially
     (givebolus (-number 80 units/kg) #:of "heparin" #:by "iv")
     (start (-number 18 units/kg/hour) #:of "heparin"))
    (define-handler infusion
      (whenever-new
       ptt
       (whenever-cond
        [(< aptt 45)
         (givebolus (-number 80 units/kg) #:of "heparin" #:by "iv")
         (increase "heparin" #:by (-number 3 units/kg/hour))]
        [(and (< 45 aptt)
              (< aptt 59))
         (givebolus (-number 40 units/kg) #:of "heparin" #:by "iv")
         (increase "heparin" #:by (-number 1 unit/kg/hour))]
        [(and (< 101 aptt)
              (< aptt 123))
         (decrease "heparin" #:by (-number 1 unit/kg/hour))]
        [(> aptt 123)
         (hold "heparin")
         (after (-number 1 hour)
                (restart "heparin")
                (decrease "heparin" #:by (-number 3 units/kg/hour)))]))))
   (test-parse
    "#lang pop-pl
require heparinPttChecking
require heparinInfusion
require ivInserted

initially
   giveBolus 80 units/kg of: \"heparin\" by: \"iv\"
   start 18 units/kg/hour of: \"heparin\"

infusion:
  whenever new ptt
    aPtt < 45        | giveBolus 80 units/kg of: \"heparin\" by: \"iv\"
                     | increase \"heparin\" by: 3 units/kg/hour

    45 < aPtt < 59   | giveBolus 40 units/kg of: \"heparin\" by: \"iv\"
                     | increase \"heparin\" by: 1 unit/kg/hour

    101 < aPtt < 123 | decrease \"heparin\" by: 1 unit/kg/hour

    aPtt > 123       | hold \"heparin\"
                     | after 1 hour
                     |     restart \"heparin\"
                     |     decrease \"heparin\" by: 3 units/kg/hour"
    (add-handler heparinpttchecking)
    (add-handler heparininfusion)
    (add-handler ivinserted)
    (initially
     (givebolus (-number 80 units/kg) #:of "heparin" #:by "iv")
     (start (-number 18 units/kg/hour) #:of "heparin"))
    (define-handler infusion
      (whenever-new
       ptt
       (whenever-cond
        [(< aptt 45)
         (givebolus (-number 80 units/kg) #:of "heparin" #:by "iv")
         (increase "heparin" #:by (-number 3 units/kg/hour))]
        [(and (< 45 aptt)
              (< aptt 59))
         (givebolus (-number 40 units/kg) #:of "heparin" #:by "iv")
         (increase "heparin" #:by (-number 1 unit/kg/hour))]
        [(and (< 101 aptt)
              (< aptt 123))
         (decrease "heparin" #:by (-number 1 unit/kg/hour))]
        [(> aptt 123)
         (hold "heparin")
         (after (-number 1 hour)
                (restart "heparin")
                (decrease "heparin" #:by (-number 3 units/kg/hour)))])))))
  (test-case
   "big4"
   (test-parse
    "handler heparinPttChecking is
  Q 6 hours checkPtt whenever not ptt in range 59 to 101, x2
  Q 24 hours checkPtt whenever 59 < ptt < 101, x2"
    (define-handler heparinpttchecking
      (whenever (not (in-range ptt 59 101)) #:times 2
                (q (-number 6 hours) checkptt))
      (whenever (and (< 59 ptt) (< ptt 101)) #:times 2
                (q (-number 24 hours) checkptt))))
   (test-parse
    "Q 6 hours checkPtt"
    #:pattern Expr
    (q (-number 6 hours) checkptt))
   (test-parse "not 59 < ptt < 101"
               #:pattern Expr
               (not (and (< 59 ptt) (< ptt 101))))
   (test-parse
    "\n Q 6 hours checkPtt whenever not 59 < ptt < 101, x2"
    #:pattern Line
    (line 1
          (whenever (not (and (< 59 ptt) (< ptt 101))) #:times 2
                    (q (-number 6 hours) checkptt))))
   (test-parse
    "handler x is
  whenever new q and qValue
    e"
    (define-handler x (whenever-new (q qvalue) (e))))

   (test-parse
    "x:
  whenever new m and left is right
    z"
    (define-handler x
      (whenever-new (m (is left right))
                    (z))))
   (test-parse
    "handler x is
  whenever new m and left is right
    z"
    (define-handler x
      (whenever-new (m (is left right))
                    (z)))))
  (test-case
   "big5"
   (test-parse
    "handler heparinPttChecking is
  Q 24 hours checkPtt
  // this is iffy may need nested whenevers (ew)
  whenever new change and drug is \"heparin\"
      after 6 hours
        checkPtt"
    (define-handler heparinpttchecking
      (q (-number 24 hours) checkptt)
      (whenever-new (change (is drug "heparin"))
                    (after (-number 6 hours)
                           (checkptt)))))
   (test-parse
    "handler heparinPttChecking is
  whenever new change and drug is \"heparin\"
      after 6 hours
        checkPtt"
    (define-handler heparinpttchecking
      (whenever-new (change (is drug "heparin"))
                    (after (-number 6 hours)
                           (checkptt)))))

   (test-parse
    "drug is \"heparin\""
    #:pattern Expr
    (is drug "heparin")))

  (test-parse
   "handler t is\n  notifyDoctor whenever painscore > 8, x3, since last notifyDoctor"
   (define-handler t
     (whenever (> painscore 8) #:times 3 #:since-last (notifydoctor)
               (notifydoctor))))

  (test-parse
   "handler t is\n  notifyDoctor whenever latest painscore > 8, x3, since last notifyDoctor"
   (define-handler t
     (whenever (> painscore 8) #:latest #t #:times 3 #:since-last (notifydoctor)
               (notifydoctor))))

  (let ([in (open-input-string "#lang test\nmessage test is [ a b: c ]")])
    (read-line in)
    (define-values (s _) (parse in))
    (check-equal? (syntax->datum s)
                  (module '(define-message test (a #:b c)))))
  (test-case
   "parse test forms"
   (test-parse
    "--- Tests ---"
    #:pattern Tests
    (here-be-tests (=> start)))
   (test-parse
    "--- Tests ---"
    (here-be-tests (=> start)))
   (test-parse
    "\n[x]"
    #:pattern TestResults
    ((x)))
   (test-parse
    "\n[x y: z]"
    #:pattern TestResults
    ((x z)))
   (test-parse
    "--- Tests ---\n [x y: z]"
    #:pattern Tests
    (here-be-tests (=> start (x z))))
   (test-parse
    "--- Tests ---\n [x y: z]"
    (here-be-tests (=> start (x z))))
   (test-parse
    "\n>x"
    #:pattern TestCase
    (=> (x)))
   (test-parse
    "\n>x\n [x]"
    #:pattern TestCase
    (=> (x) (x)))
   (test-parse
    "--- Tests ---\n [x y: z]\n  [x]\n> x y\n[a b: c d: e]"
    (here-be-tests
     (=> start
         (x z)
         (x))
     (=> (x y)
         (a c e))))))
