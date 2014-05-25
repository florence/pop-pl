#lang racket/base
(require racket/gui/base
         racket/class
         racket/match)

(struct task (description urgency importance))

(define task-radius 8)

(define task-gui%
  (class object%
    (init label)
    
    (define mx #f)
    (define my #f)
  
    (define active-task #f)
    
    (define/private (update-mx/my new-mx new-my)
      (set! mx new-mx)
      (set! my new-my)
      (update-active-task))
    
    (define/private (update-active-task)
      (define new-task 
        (cond
          [(and mx my)
           (for/or ([a-task (in-list all-tasks)])
             (define-values (x y w h) (task->xywh a-task))
             (cond
               [(and mx my
                     (<= x mx (+ x task-radius task-radius))
                     (<= y my (+ y task-radius task-radius)))
                a-task]
               [else #f]))]
          [else #f]))
      (unless (equal? new-task active-task)
        (set! active-task new-task)
        (send t erase)
        (send t insert (if active-task
                           (task-description active-task)
                           ""))
        (send c refresh)))
    
    (define f (new frame% [label label] [width 800] [height 400]))
    (define hp (new horizontal-panel% [parent f]))
    (define c (new (class canvas%
                     (define/override (on-event evt)
                       (cond
                         [(send evt leaving?)
                          (update-mx/my #f #f)]
                         [(send evt moving?)
                          (update-mx/my (send evt get-x) (send evt get-y))]))
                     (super-new))
                   [paint-callback (λ (c dc) (draw-tasks c dc))]
                   [parent hp]))
    (define t (new text%))
    (define ec (new editor-canvas% 
                    [parent hp]
                    [editor t]))
    (define all-tasks '())
    
    (define/public (add-task task)
      (set! all-tasks (cons task all-tasks))
      (update-active-task)
      (send c refresh))
    
    (define/private (task->xywh a-task)
      (define-values (cw ch) (send c get-client-size))
      (define-values (w h)
        (for/fold ([w 0][h 0]) ([a-task (in-list all-tasks)])
          (match-define (task description urgency importance) a-task)
          (values (max w urgency)
                  (max h importance))))
      (match-define (task description urgency importance) a-task)
      (define x (- (* (/ urgency w) (- cw task-radius)) task-radius))
      (define y (- (* (/ importance h) (- ch task-radius)) task-radius))
      (values x y (* task-radius 2) (* task-radius 2)))
      
    (define/private (draw-tasks c dc)
      (send dc set-smoothing 'smoothed)
      
      (for ([a-task (in-list all-tasks)])
        (define-values (x y w h) (task->xywh a-task))
        (cond
          [(equal? a-task active-task)
           (send dc draw-ellipse 
                 (- x task-radius) (- y task-radius) 
                 (+ w task-radius task-radius)
                 (+ h task-radius task-radius))]
          [else
           (send dc draw-ellipse x y w h)])))
    
    (define/public (show) (send f show #t))
  
    (super-new)))

(define tg (new task-gui% [label "Belknap"]))
(for ([x (in-range 10)])
  (send tg add-task (task (format "task ~a" x) (random 100) (random 100))))
(send tg show)
