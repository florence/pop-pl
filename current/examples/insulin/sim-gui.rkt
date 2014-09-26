#lang racket/base
(require racket/gui/base 
         racket/class
         pict)

(define (show-gui insul-infusion bg #:title [title ""])
  (define f (new frame% [width 800] [height 600] [label title]))
  (define c (new sim-canvas% 
                 [parent f]
                 [insul-infusion insul-infusion]
                 [bg bg]))
  (send f show #t))

(define infusion-color "red")
(define bg-color "blue")
(define bg-size 10)

(define sim-canvas%
  (class canvas%
    (init-field insul-infusion bg)
    (inherit get-dc get-virtual-size)
    (define/override (on-paint)
      (define dc (get-dc))
      (define-values (w h) (get-virtual-size))
      (define y-margin 10)
      
      (define-values (insul-min insul-max) (find-min/max (λ (x) (vector-ref x 1)) insul-infusion))
      (define (insul->y insul) (- h (scale-between insul 0 30 y-margin (- h y-margin))))

      (define-values (bg-min bg-max) (find-min/max (λ (x) (vector-ref x 1)) bg))
      (define (bg->y bg) (- h (scale-between bg 40 140 y-margin (- h y-margin))))
      
      ;; treat the range [0,30] with steps by 5 as reasonable insularin infusion rates
      (define right-axis 
        (colorize (build-axis 0 30 5 (- (insul->y 30) (insul->y 0)))
                  infusion-color))
      
      ;; treat the range [40,140] with steps by 10 as reasonable bg values
      (define left-axis
        (colorize (build-axis 40 140 10 (- (bg->y 140) (bg->y 40)))
                  bg-color))
      
      (define x-margin (max (pict-width left-axis)
                            (pict-width right-axis)))
      
      (define-values (min-time max-time) 
        (find-min/max (λ (x) (vector-ref x 0)) insul-infusion bg))
      (define (time->x time) (scale-between time min-time max-time x-margin (- w x-margin)))
      

      
      (send dc set-smoothing 'aligned)
      
      (draw-pict left-axis dc 0 (- (/ h 2) (/ (pict-height left-axis) 2)))
      (draw-pict right-axis dc 
                 (- w (pict-width right-axis))
                 (- (/ h 2) (/ (pict-height left-axis) 2)))
      
      (send dc set-pen infusion-color 1 'solid)
      (for ([before (in-list insul-infusion)]
            [after (in-list (cdr insul-infusion))])
        (send dc draw-line
              (time->x (vector-ref before 0))
              (insul->y (vector-ref before 1))
              (time->x (vector-ref after 0))
              (insul->y (vector-ref before 1)))
        (send dc draw-line
              (time->x (vector-ref after 0))
              (insul->y (vector-ref before 1))
              (time->x (vector-ref after 0))
              (insul->y (vector-ref after 1))))
      
      (send dc set-pen bg-color 1 'solid)
      (send dc set-brush bg-color 'solid)
      (for ([before (in-list bg)]
            [after (in-list (cdr bg))])
        (define x-center (time->x (vector-ref before 0)))
        (define y-center (bg->y (vector-ref before 1)))
        (send dc draw-ellipse
              (- x-center (/ bg-size 2))
              (- y-center (/ bg-size 2))
              bg-size bg-size)
        (send dc draw-line
              x-center 
              y-center 
              (time->x (vector-ref after 0))
              (bg->y (vector-ref after 1))))
      
      (send dc set-pen "black" 1 'transparent)
      (send dc set-brush (make-object color% 0 0 255 0.2) 'solid)
      (send dc draw-rectangle
            0
            (bg->y 180)
            w
            (- (bg->y 120) (bg->y 180)))
      
      (void))
    (super-new)))

(define (build-axis min-value max-value step height)
  (define picts
    (for/list ([x (in-range min-value (+ max-value step) step)])
      (vector (scale-between x min-value max-value 0 1) 
              (text (format "~a" x)))))
  (define widest-width
    (apply max (map (λ (x) (pict-width (vector-ref x 1))) picts)))
  (for/fold ([p (blank widest-width height)]) ([pr (in-list picts)])
    (define % (vector-ref pr 0))
    (define this-pict (vector-ref pr 1))
    (pin-over p
              0
              (- (* % height) (/ (pict-height this-pict) 2))
              this-pict)))
  
(define (scale-between x in-low in-high out-low out-high)
  (define ih-l (- in-high in-low))
  (if (zero? ih-l)
      0
      (+ (* (/ (- x in-low) (- in-high in-low))
            (- out-high out-low))
         out-low)))

(define (find-min/max sel . stuff)
  (define pr
    (let loop ([stuff stuff]
               [best #f])
      (cond
        [(pair? stuff)
         (loop (car stuff) (loop (cdr stuff) best))]
        [(vector? stuff)
         (if best
             (cons (min (sel stuff) (car best))
                   (max (sel stuff) (cdr best)))
             (cons (sel stuff) (sel stuff)))]
        [else best])))
  (values (car pr) (cdr pr)))


(module+ main
  (require "simulation.rkt")
  (define-values (ic bg) (simulate 5))
  (displayln ic)
  (displayln bg)
  (show-gui ic bg))
