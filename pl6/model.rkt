#lang racket
(require redex)

(define-syntax quasiquote (make-rename-transformer #'term))

(define-language pop-pl
  (P (imports definitions ...))
  ;; definitions
  (import x ...)
  (definition 
    (define/handler (name x x x) 
      state-def+value ...
      e)
    (define/external name e ...)
    state-def)
  (state-def
   (define/state name e ...)
   state-def+value)
  (state-def+value
   (define/state name e ... -> e))
  ;; expressions
  (e x
     v
     (e e ...)
     (oⁿ e ...)
     (if0 e e e)
     (match e [e e] ...))
  (oⁿ + list)
  (v state
     (lambda (x ...) 
       state-def+value ...
       e)
     (quote x)
     (LIST v ...)
     message)
  (state (s (x v) ...)
         initial)
  ;; machine message
  (mm update new-handler remove-handler)
  (message
   (∇ x v ...)
   (∇ mm v ...))
  (n number)
  ((x name) variable-not-otherwise-mentioned))

(define-extended-language pop-pl-eval pop-pl
  ;; handler machine
  (HM (H e Σ state state message (message ...)))
  ;; message machine
  (MM (H state (message ...) (message ...)))
  (H ((name h) ...))
  (h (lambda (x x x) e))  (Σ ((x v) ...))
  (E hole
     (v ... E e ...)
     (oⁿ v ... E ...)
     (if0 E e e)
     (match E [e e] ...)))

(define-metafunction pop-pl-eval
  prepair : P -> (H Σ state)
  [(prepair P) (prepair* P (() () (st)))])
(define-metafunction pop-pl-eval
  prepair* : P (H Σ state) -> (H Σ state))

(define-metafunction pop-pl-eval
  eval : H Σ state state message -> (state (message ...)))

;; initial sigma: update new-handler remove-handler list
(define initial-Σ `((update
                     (lambda (m n) (∇ update m n)))
                    (new-handler 
                     (lambda (n f) (∇ new-handler n f)))
                    (remove-handler
                     (lambda (n) (∇ remove-handler n)))))

(define R_event
  (reduction-relation
   pop-pl-eval
   #:domain HM
   (--> (((name h) any_H ...) (LIST message_r ...) state_1 state_2 message (any ...))
        ;; -- to --
        ((any_H ...)
         (h state_1 state_2 message)
         state_1
         state_2 
         message
         (message_r (any ...))))
   (--> (H (in-hole E x) Σ any ...)
        ;; -- to --
        (H (in-hole E (lookup x Σ)) any ...)
        lookup)
   (--> (H (in-hole E ((lambda (x_l ...) e) v_l ...)) ((x_s v_s) ..) any ...)
        ;; -- to --
        (H (in-hole E e) ((x_l v_l) ... (x_s v_s) ...) any ...)
        β)
   (--> (in-hole HM (+ v ...))
        ;; -- to --
        (in-hole HM ,(apply + `(v ...)))
        δ_+)
   (--> (in-hole HM (list v ...))
        ;; -- to --
        (in-hole HM (LIST v ...))
        δ_list)
   (--> (in-hole HM (if0 0 e_1 e_2))
        ;; -- to --
        (in-hole HM e_1)
        if0)
   (--> (in-hole HM (if0 v e_1 e_2))
        ;; -- to --
        (in-hole HM e_1)
        (side-condition (not (equal? `v 0)))
        if!0)
   (--> (H (in-hole E (match v [e_p e_r] ...)) ((x_s v_s) ...) any ...)
        ;; -- to --
        (H (in-hole E e) ((x_r v_r) ... (x_s v_s) ...) any ...)
        (where (#f ...
                (((x_r v_r) ...) e)
                any ...)
               ,(raw-match* `v `(e_ps ...) `(e_es ...)))
        match)))

;; ... -> (Listof (U (list Σ e) #f))
(define (raw-match* v ps es)
  (map (curry raw-match) ps es))
;; ... -> (U (list Σ e) #f)
(define (raw-match v p e)
  ;; some magic will go here eventually
  (match (list v p)
    [(list x (? p symbol?))
     (list (list p x))]
    [`((state x ...) (state y ...))
     ]))

(define R_messages
  (reduction-relation
   pop-pl-eval
   #:domain MM
   (--> (H
         (st (x_1 v_1) ... (x v) (x_2 v_2) ...)
         ((∇ update (quote x) v_u) any_m ...)
         any_o)
        ;; -- to --
        (H
         (st (x_1 v_1) ... (x v_u) (x_2 v_2) ...)
         (any_m ...)
         any_o)
        (side-condition (not (member `x `(x_u ...))))
        (where (x_u ...) (updated (any_m ...)))
        updating-state)
   (--> (((name h_1) ...)
         state
         ((∇ new-handler (quote x) h_2) any_m ...)
         any_o)
        ;; -- to --
        (((x h_2) (name h_1 ...))
         state
         (any_m ...)
         any_o)
        (side-condition (not (member `x `(name ...))))
       add-handler)
   (--> (((name_1 h_1) ... (name h) (name_2 h_2) ...)
         state
         ((∇ remove-handler (quote name) any_m ...))
         any_o)
        ;; -- to --
        (((name_1 h_1) ... (name_2 h_2) ...)
         state
         (any_m ...)
         any_o)
        remove-handler)
   (--> (H state ((∇ x v ...) any_m ...) (any_o ...))
        ;; -- to --
        (H state (any_m ...) ((∇ x v ...) any_o ...))
        out)))

;; get the names of all variables being updated
(define-metafunction pop-pl-eval
  updated : (message ...) -> (x ...)
  [(updated ()) ()]
  [(updated ((∇ update (quote x) any) message ...))
   (x x_r ...)
   (where (x_r ...) (updated (message ...)))]
  [(updated (message_1 message ...))
   (updated (message ...))])
