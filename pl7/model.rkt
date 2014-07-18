#lang racket
(require redex)
(define-syntax quasiquote (make-rename-transformer #'term))

(define-language pop-pl
  (e (add-handler n e)
     (remove-handler n)
     (send e)
     (lambda (x ...) e)
     (e ...)
     x
     number
     (quote x)
     (cons e e)
     (seq e e)
     null
     (if0 e e e))
  ((x n) variable-not-otherwise-mentioned))


(define-extended-languge pop-pl pop-pl-eval

  ;; remaining handlers, current log, current-expression, new handlers, outgoing messages
  (m (H log e H (v ...)))
  (M (H log E H (v ...)))
  (H (h ...))
  ((P h) (n (lambda (x) e)))
  (v number
     (cons v v)
     null
     (lambda (x ...) e)
     (quote x))
  (log (cons v log) null)
  (E hole
     (add-handler n E)
     (begin E e)
     (send E)
     (v ... E e ...)
     (cons E e)
     (cons v E)
     (if0 E e e)))

(define R
  (reduction-relation
   pop-pl-eval
   #:domain m
   (--> (((n v_1) h ...) v_2 H log (v_3 ...))
        ;; --to--
        ((h ...) (v_1 log) H log (v_3 ...))
        switch)
   (--> (H log (in-hole E (add-handler n (lambda (x) e)) (h ...) (v ...)))
        ;; --to--
        (H log (in-hole E 42) ((n (lambda (x) e)) h ...) (v ...))
        (where ((n_2 _) ...) (h ...))
        (side-condition (not (member `n `(n_2 ...))))
        add)
   (--> (H
         log
         (in-hole E (remove-handler n))
         (h_1 ... (n v_h) h_2 ...)
         (v ...))
        ;; --to--
        (H log (in-hole E 42) (h_1 ... h_2 ...) (v ...))
        rem)
   (--> (H_1 log (in-hole E (send v))) H_2 (v_2 ...)
        ;; --to--
        (H_1 log (in-hole E 42) H_2 (v v_2 ...))
        sen)
   (--> (in-hole M ((lambda (x ...) e_b) e_a ...))
        ;; --to--
        (in-hole M (subst ((x e_a) ...) e_b))
        (side-condition (= (length `(x ...)) (length `(e_b ...))))
        β)
   (--> (in-hole M (seq v e))
        ;; --to--
        (in-hole M e)
        seq)
   (--> (in-hole M (if0 0 e_1 e_2))
        ;;--to--
        (in-hole M e_1)
        if0)
   (--> (in-hole M (if0 v e_1 e_2))
        ;; --to--
        (in-hole M e_2)
        (side-condition (not (equal? 0 `v)))
        if!0)))

(define-metafunction pop-pl-eval
  subst ((x v) ...) e -> e
  [(subst (_ ... (x v) _ ...) x) v]
  [(subst )])
  
  
   
