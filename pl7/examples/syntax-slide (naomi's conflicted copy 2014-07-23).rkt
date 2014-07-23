#lang at-exp slideshow
(require slideshow/code racket/draw)

(define (syntax t)
  (color "darkmagenta" t  'bold))
(define (kw t)
  (color "olive" t))
(define (delim t)
  (parameterize ([current-font-size 13])
    (color "royalblue" t)))
(define (const t)
  (color "seagreen" t))
(define (color c t . others)
  (define key (if (symbol? c) (symbol->string c) c))
  (text t 
        (cons (send the-color-database find-color key)
              (append others (current-main-font)))
        (current-font-size)))
(define (prepair-line l)
  (apply para (map (lambda (x) (if (string? x) (t x) x)) l)))
(define (make-item . l)
  (parameterize ([current-gap-size 1])
    (apply item (map prepair-line l))))

(current-font-size 15)
(current-line-sep 1)
(slide 
 (make-item
  @list{@syntax{require} heprin-ptt-checking}
  @list{@syntax{require} heparin-infusion}
  @list{@syntax{require} iv-inserted})
 (make-item
  @list{@syntax{handler} heprin-ptt-checking @syntax{is}}
  @list{    stable @syntax{means} most-recent @const{2} ptt @kw{that-are:} theripudic since-last@delim{(}change-heprin@delim{)}}
  @list{    Q6 check-ptt @syntax{whenever} not stable}
  @list{    Q24 check-ptt @syntax{whenever} stable})
 (make-item
  @list{@syntax{message} ptt @syntax{is} @syntax["["] update ptt@syntax{?}@syntax["]"]}
  @list{@syntax{message} change-heprin @syntax{is} @syntax["["]change @const{"heprin"} @const{"continous"} @syntax{?}@syntax["]"]}
  @list{@syntax{message} check-ptt @syntax{is} @syntax["["]check-ptt@syntax["]"]}))

