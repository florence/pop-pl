#lang racket/unit

(require "system-sig.rkt" "private/shared.rkt"
         "prescription-sig.rkt")

(import prescription^)
(export system^)

(define TIME-ADVANCE 30)

(define (start!) (-start))

(define (send-message! msg)
  (-eval msg))

(define (advance! time)
  (define t (time->stamp time))
  (reverse
   (for/fold ([msg null])
             ([_ (in-range 0 t TIME-ADVANCE)])
     (append msg (send-message! (message '(time) (list TIME-ADVANCE) #f))))))
