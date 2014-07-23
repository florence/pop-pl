#lang at-exp slideshow
(require slideshow/code racket/draw slideshow/slides-to-picts)

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
  @list{@syntax{initially}}
  @list{    start-heprin @const{18}units/kg/hour}
  @list{    give @const{80}units/kg/hour @kw{of:} @const{"heprin"} @kw{by:} @const{"iv"}})
 (make-item
  @list{@syntax{handler} infusion @syntax{is}}
  @list{    @syntax{whenever} event-is ptt}
  @list{         a-ptt @syntax{means} value-of event}
  @list{}
  @list{        @syntax{whenever} a-ptt@kw{<}45}
  @list{            change-heprin @kw{by:} @const{3}units/kg/hour}
  @list{            give @const{80}units/kg @kw{of:} @const{"heprin"} @kw{by:} @const{"iv"}}
  @list{}
  @list{        @syntax{whenever} 45@kw{<}a-ptt@kw{<}59}
  @list{            change-heprin @kw{by:} @const{1}unit/kg/hour}
  @list{            give @const{40}units/kg @kw{of:} @const{"heprin"} @kw{by:} @const{"iv"}}
  @list{}
  @list{        @syntax{whenever} 101@kw{<}a-ptt@kw{<}123}
  @list{            change-heprin @kw{by:} @const{-1}units/kg/hour}
  @list{}
  @list{        @syntax{whenever} a-ptt@kw{>}123}
  @list{            stop @const{"heparin"}}
  @list{           @syntax{after} @const{1}hour}
  @list{                restart @const{"heprin"}}
  @list{                change-heprin @const{-3}units/kg/hour})
 (make-item
  @list{@syntax{handler} heprin-ptt-checking @syntax{is}}
  @list{    stable @syntax{means} most-recent @const{2} ptt @kw{that-are:} theripudic since-last@delim{(}change-heprin@delim{)}}
  @list{    Q6 check-ptt @syntax{whenever} not stable}
  @list{    Q24 check-ptt @syntax{whenever} stable})
 (make-item
  @list{@syntax{message} ptt @syntax{is} @syntax{[}update ptt@syntax{?]}}
  @list{@syntax{message} change-heprin @syntax{is} @syntax{[}change @const{"heprin"} @const{"continous"} @kw{by:} @syntax{?]}}
  @list{@syntax{message} check-ptt @syntax{is} @syntax{[}check-ptt@syntax{]}}
  @list{@syntax{message} give @syntax{is} @syntax{[}give @syntax{?} @kw{of:} @syntax{?} @kw{by:} @syntax{?]}}
  @list{@syntax{message} stop @syntax{is} @syntax{[}stop @syntax{?]}}
  @list{@syntax{message} restart @syntax{is} @syntax{[}restart @syntax{?]}}))


(define (render)
  (map pict->bitmap (get-slides-as-picts "syntax-slide.rkt" 1024 768 #t)))
