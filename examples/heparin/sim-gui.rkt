#lang racket/base
(require racket/gui/base 
         racket/class
         pict)

(define (show-gui hep-infusion hep-bolus aptt #:title [title ""])
  (define f (new frame% [width 800] [height 600] [label title]))
  (define c (new sim-canvas% 
                 [parent f]
                 [hep-infusion hep-infusion]
                 [hep-bolus hep-bolus]
                 [aptt aptt]))
  (send f show #t))

(define bolus-color "forestgreen")
(define infusion-color "red")
(define aptt-color "blue")
(define bolus-radius-min 10)
(define bolus-radius-max 20)

(define sim-canvas%
  (class canvas%
    (init-field hep-infusion hep-bolus aptt)
    (inherit get-dc get-virtual-size)
    (define/override (on-paint)
      (define dc (get-dc))
      (define-values (w h) (get-virtual-size))
      (define y-margin 10)
      
      (define-values (hep-min hep-max) (find-min/max (λ (x) (vector-ref x 1)) hep-infusion))
      (define (hep->y hep) (- h (scale-between hep 0 30 y-margin (- h y-margin))))

      (define-values (aptt-min aptt-max) (find-min/max (λ (x) (vector-ref x 1)) aptt))
      (define (aptt->y aptt) (- h (scale-between aptt 40 140 y-margin (- h y-margin))))
      
      (define-values (bolus-min bolus-max) (find-min/max (λ (x) (vector-ref x 1)) hep-bolus))
      (define (bolus->size bolus) (scale-between bolus 40 80
                                                 bolus-radius-min bolus-radius-max))
      
      ;; treat the range [0,30] with steps by 5 as reasonable heparin infusion rates
      (define right-axis 
        (colorize (build-axis 0 30 5 (- (hep->y 30) (hep->y 0)))
                  infusion-color))
      
      ;; treat the range [40,140] with steps by 10 as reasonable aptt values
      (define left-axis
        (colorize (build-axis 40 140 10 (- (aptt->y 140) (aptt->y 40)))
                  aptt-color))
      
      (define x-margin (max (pict-width left-axis)
                            (pict-width right-axis)
                            bolus-radius-max))
      
      (define-values (min-time max-time) 
        (find-min/max (λ (x) (vector-ref x 0)) hep-infusion hep-bolus aptt))
      (define (time->x time) (scale-between time min-time max-time x-margin (- w x-margin)))
      

      
      (send dc set-smoothing 'aligned)
      
      (draw-pict left-axis dc 0 (- (/ h 2) (/ (pict-height left-axis) 2)))
      (draw-pict right-axis dc 
                 (- w (pict-width right-axis))
                 (- (/ h 2) (/ (pict-height left-axis) 2)))
      
      (send dc set-pen infusion-color 1 'solid)
      (for ([before (in-list hep-infusion)]
            [after (in-list (cdr hep-infusion))])
        (send dc draw-line
              (time->x (vector-ref before 0))
              (hep->y (vector-ref before 1))
              (time->x (vector-ref after 0))
              (hep->y (vector-ref before 1)))
        (send dc draw-line
              (time->x (vector-ref after 0))
              (hep->y (vector-ref before 1))
              (time->x (vector-ref after 0))
              (hep->y (vector-ref after 1))))
      
      (send dc set-pen aptt-color 1 'solid)
      (for ([before (in-list aptt)]
            [after (in-list (cdr aptt))])
        (send dc draw-line
              (time->x (vector-ref before 0))
              (aptt->y (vector-ref before 1))
              (time->x (vector-ref after 0))
              (aptt->y (vector-ref after 1))))
      
      (send dc set-pen "black" 1 'transparent)
      (send dc set-brush bolus-color 'solid)
      (for ([a-bolus (in-list hep-bolus)])
        (define x (time->x (vector-ref a-bolus 0)))
        (define size (bolus->size (vector-ref a-bolus 1)))
        (send dc draw-ellipse 
              (- x (/ size 2))
              (- (/ h 2) (/ size 2))
              size size))
      (send dc set-brush (make-object color% 0 0 255 0.2) 'solid)
      (send dc draw-rectangle
            0
            (aptt->y 101)
            w
            (- (aptt->y 59) (aptt->y 101)))
      
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
  (define-values (hc hb m) (simulate 5))
  (displayln hc)
  (displayln hb)
  (displayln m)
  (show-gui hc hb m))
