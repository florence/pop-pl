#lang at-exp slideshow
(require slideshow/code racket/draw slideshow/slides-to-picts)

(define (syntax t)
  (color "darkmagenta" t  'bold))
(define (kw t)
  (color "olive" t))
(define op kw)
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

(current-font-size 12)
(current-line-sep 1)
(current-gap-size 12)
(current-main-font "monaco")
(slide 
 (make-item
  @list{@syntax{require} heparin-ptt-checking}
  @list{@syntax{require} heparin-infusion}
  @list{@syntax{require} iv-inserted})
 (make-item
  @list{@syntax{initially}}
  @list{    give-bolus @const{80}units/kg/hour @kw{of:} @const{"heparin"} @kw{by:} @const{"iv"}}
  @list{    start @const{18}units/kg @kw{of:} @const{"heparin"}})
 (make-item
  @list{@syntax{handler} infusion @syntax{is}}
  @list{    @syntax{whenever new} ptt}  
  @list{ }
  @list{        @syntax{whenever} }
  @list{         a-ptt@op{<}@const{45}      @syntax{|}increase @const{"heparin"} @kw{by:} @const{3}units/kg/hour}
  @list{                          @syntax{|}give-bolus @const{80}units/kg @kw{of:} @const{"heparin"} @kw{by:} @const{"iv"}}
  @list{ }
  @list{        @const{45}@op{<}a-ptt@op{<}@const{59} @syntax{|}increase @const{"heparin"} @kw{by:} @const{1}unit/kg/hour}
  @list{                          @syntax{|}give-bolus @const{40}units/kg @kw{of:} @const{"heparin"} @kw{by:} @const{"iv"}}
  @list{ }
  @list{        @const{101}@op{<}a-ptt@op{<}@const{123}@syntax{|}decrease @const{"heparin"} @kw{by:} @const{1}units/kg/hour}
  @list{ }
  @list{         a-ptt@op{>}@const{123}     @syntax{|}hold @const{"heparin"}}
  @list{                          @syntax{|}@syntax{after} @const{1}hour}
  @list{                          @syntax{|}       restart @const{"heparin"}}
  @list{                          @syntax{|}       decrease @const{"heparin"} @const{3}units/kg/hour})
 (make-item
  @list{@syntax{handler} heparin-ptt-checking @syntax{is}}
  @list{    stable @syntax{means} most-recent @const{2} ptt @kw{that-are:} therapeutic since-last@delim{(}change@delim{(}@const{"heparin"}@delim{)}@delim{)}}
  @list{    therapeutic @syntax{means a} ptt @syntax{where} @const{60} @op{<} a-ptt @op{<} @const{100}}
  @list{    Q6 check-ptt @syntax{whenever} not stable}
  @list{    Q24 check-ptt @syntax{whenever} stable}))
(slide
 (make-item
  @list{@syntax{message} ptt @syntax{is} @syntax{[} a-ptt @syntax{]}}
  @list{@syntax{message} change @syntax{is} @syntax{[} drug @kw{by:} amount @syntax{]}}
  @list{@syntax{message} increase @syntax{is} change}
  @list{@syntax{message} decrease@delim{(}d @kw{by:} a@delim{)}@syntax{is} change@delim{(}d by: -a @delim{)}}
  @list{@syntax{message} check-ptt @syntax{is} @syntax{[}@syntax{]}}
  @list{@syntax{message} give @syntax{is} @syntax{[}amount @kw{of:} drug @kw{by:} method @syntax{]}}
  @list{@syntax{message} give-bolus @syntax{is} give}
  @list{@syntax{message} start @syntax{is} @syntax{[} amount @kw{of:} drug @syntax{]}}
  @list{@syntax{message} hold @syntax{is} @syntax{[}drug@syntax{]}}
  @list{@syntax{message} restart @syntax{is} @syntax{[}drug@syntax{]}}))


(define (render)
  (map pict->bitmap (get-slides-as-picts "syntax-slide.rkt" 1024 768 #t)))
