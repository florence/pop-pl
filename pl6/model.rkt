#lang racket
(require redex)

(define-syntax quasiquote (make-rename-transformer #'term))

(define-language pop-pl
  (P (imports definitions ...))
  ;; definitions
  (import (imports x ...))
  ((definition def) 
   handler-def
   state-def)
  (handler-def
   (define/handler (x x x x) 
      state-def ...
      e))
  (state-def (define/state x e ... -> v))
  ;; expressions
  (e x
     v
     (seq e e ...)
     (e ...)
     (oⁿ e ...)
     (if0 e e e))
  (oⁿ state-get
      initial?
      update
      new-handler
      remove-handler
      send
      eq?
      drug
      message-tag=?
      message-value)
  (v state
     (lambda (x ...) 
       state-def ...
       e)
     (quote x)
     VOID
     message
     (drug-type x)
     (drug (drug-type x) v ...))
  (state (s (x v) ...)
         initial)
  ((message msg)
   (∇ (quote x) v))
  (n number)
  (x variable-not-otherwise-mentioned))

(define-extended-language pop-pl-eval pop-pl
  (HM (S msg E ΔS (msg ...)))
  (ΔS (H ((x v) ...)))
  (S (H state state))
  (H ((x h) ...))
  (h (lambda (x x x) e))
  (E hole
     (v ... E e ...)
     (oⁿ v ... E ...)
     (if0 E e e)
     (seq E e ...)))

(define-metafunction pop-pl-eval
  prepair : P -> S
  [(prepair (imports definition ...)) 
   (((x_h (subst h ((x_i v) ...))) ...)
    state_1
    state_2)
   (where ((x_i v) ...) (imports->bindings imports))
   (where (((x_h h) ...) state_1 state_2) (prepair* (definition ...) (() (st))))])
(define-metafunction pop-pl-eval
  prepair* : (definition ...) (H state) -> S
  [(prepair* () (H state)) (H initial state)]
  [(prepair* (handler-def def_r ...) (((x_h h) ...)) (st (x_s v) ...))
   (prepair* (state-def ... def_r ...)
            (((x_n h_l) (x_h h) ...)
             (st (x_r v_r) ... (x_s v) ...)))
   (where (define/state (x_n x_a ...) state-def ... e) handler-def)
   (where h_l (lambda (x_a ...) e))
   (where (st (x_r v_r) ...) (prepair-e h_l))]
  [(prepair* (state-def def ...) (H (st (x v) ...)))
   (prepair (def ...) (H (st (x_n v_n) (x v) ...)))
   (where (define/state x_n e ... -> v_n) state-def)])

(define-metafunction pop-pl-eval
  import->bindings : imports -> ((x v) ...)
  [(import->bindings (import x ...))
   ((x (lambda (v) (send (∇ x v)))) ...)])

(define-metafunction pop-pl-eval
  prepair-e : e -> state
  [(prepair-e (lambda (x ...) state-def ... e))
   (st (x_1 v_1) ... (x_2 v_2) ...)
   (where (st (x_1 v_1) ...) (prepair* (state-def ...) (() (st))))
   (where (st (x_2 v_2) ...) (prepair-e e))]
  [(prepair-e v) (st)]
  [(prepair-e x) (st)]
  [(prepair-e (seq e_1 e_2))
   (st (x_1 v_1) ... (x_2 v_2) ...)
   (where (st (x_1 v_1) ...) (prepair-e e_1))
   (where (st (x_2 v_2) ...) (prepair-e e_2))]
  [(prepair-e (e ...))
   (st (x v) ... ...)
   (where ((st (x v) ...) ...) 
          ((prepair-e e) ...))]
  [(prepair-e (oⁿ e ...))
   (st (x v) ... ...)
   (where ((st (x v) ...) ...) 
          ((prepair-e e) ...))]
  [(prepair-e (if0 e ...))
   (st (x v) ... ...)
   (where ((st (x v) ...) ...) 
          ((prepair-e e) ...))])

;; we need a semantics for handling state changes with and without a new message
(define-metafunction pop-pl-eval
  eval : S (message ...) -> S
  [(eval S ())
   (eval-msg S (∇ updated 0))]
  [(eval S (msg_1 msg ...))
   (eval S_2 (msg ...))
   (where (S_2 _) (eval-msg S msg_1))])

(define-metafunction pop-pl-eval
  eval-msg : S message -> (S (message ...))
  [(eval-msg S msg)
   ((apply-ΔS S ΔS) (msg_r ...))
   (where ((() _ _) _ v ΔS (msg_r ...))
          ,(apply-reduction-relation* R
                                      `(S msg VOID (H ()) ())))
   (where (H _ _) S)])

(define-metafunction pop-pl-eval
  apply-ΔS : S ΔS -> S
  [(apply-ΔS (_ _ state_1) (H_2 state_2))
   (H_2 state_1 (overwrite-state state_1 state_2))])
(define-metafunction pop-pl-eval
  overwrite-state : state ((x v) ...) -> state
  [(overwrite-state state ()) state]
  [(overwrite-state (st (x_1 v_1) ... (x _) (x_2 v_2) ...)
                    ((x v) (x_3 v_3) ...))
   (overwrite-state (st (x_1 v_1) ... (x v) (x_2 v_2) ...)
                    ((x_3 v_3) ...))])

(define R
  (reduction-relation
   pop-pl-eval
   #:domain HM
   (--> ((((x h) any_n ...) state_1 state_2) msg_e v ΔS (msg_r ...))
        ;; -- to --
        (((any_n ...) state_1 state_2)
         msg
         (h state_1 state_2 msg_e)
         ΔS
         (msg_r ...))
        next)       
   (--> (in-hole HM ((lambda (x ...) e) v ...))
        ;; -- to --
        (in-hole HM (subst e ((x v) ...)))
        β)
   (--> (in-hole HM
                 (state-get (quote x)
                            (st (x_1 v_1) ...
                                (x v)
                                (x_2 v_2) ...)))
        ;; -- to --
        (in-hole HM v)
        δ_state-get)
   (--> (in-hole HM (initial? initial))
        ;; -- to --
        (in-hole HM 0)
        δ_initial?-true)
   (--> (in-hole HM (initial? v))
        ;; -- to --
        (in-hole HM 42)
        (side-condition (not (equal? `v `initial)))
        δ_initial?-false)
   (--> (S
         msg_e
         (in-hole E (update (quote x) v))
         (H ((x_1 v_1) ...))
         (msg_r ...))
        ;; -- to --
        (S
         msg_e
         (in-hole E VOID)
         (H ((x v) (x_1 v_1) ...))
         (msg_r ...))
        (where (_ _ (st (x_s v_s) ...)) S)
        (side-condition (and
                         (member `x `(x_s ...))
                         (not (member `x `(x_1 ...)))))
        δ_update)
   (--> ((((x_1 h_1) ...) state_1 state_2) 
         msg_e
         (in-hole E (new-handler (quote x) h))
         (((x_2 h_2) ...) any)
         (msg_r ...))
        ;; -- to --
        ((((x_1 h_1) ...) state_1 state_2)
         msg_e
         (in-hole E VOID)
         (((x h) (x_2 h_2) ...) any)
         (msg_r ...))
        (side-condition (not (member `x `(x_1 ... x_2 ...))))
        δ_new-handler)
   (--> (((x_1 h_1) ... (x h_s) (x_2 h_2) ...)
         msg_e
         (in-hole E (remove-handler (quote x)))
         (((x_3 h_3) ... (x h_s) (x_4 h_4) ...) any)
         (msg_r ...))
        ;; -- to --
        (((x_1 h_1) ... (x h_s) (x_2 h_2) ...)
         msg_e
         (in-hole E VOID)
         (((x_3 h_3) ... (x_4 h_4) ...) any))
        δ_remove-handler)
   (--> (in-hole HM (message-tag=? (∇ x_1 v) (quote x_2)))
        ;; -- to --
        (in-hole HM ,(if (equal? `x_1 `x_2) 0 42))
        δ_message-tag=?) 
   (--> (in-hole HM (message-value  (∇ x v)))
        ;; -- to --
        (in-hole HM v) 
        δ_message-value)
   (--> (S
         msg_e
         (in-hole E (send msg))
         ΔS
         (msg_r ...))
        ;; -- to --
        (S msg_e (in-hole E VOID) ΔS (msg msg_r ...))
    δ_send)
   (--> (in-hole HM (eq? v_1 v_2))
        ;; -- to --
        (in-hole HM ,(if (and
                          (or (number? `v_1)
                              (symbol? `v_1))
                          (eqv? `v_1 `v_2))
                         0
                         42))
        δ_eq?)
   (--> (in-hole HM (if0 0 e_1 e_2))
        ;; -- to --
        (in-hole HM e_1)
        if0)
   (--> (in-hole HM (if0 v e_1 e_2))
        ;; -- to --
        (in-hole HM e_1)
        (side-condition (not (equal? `v 0)))
        if!0)))

(define-metafunction pop-pl
  get-possible-drugs : P -> (x ...)
  [(get-possible-drugs (imports definition ...))
   ((drug-type x) ... ...)
   (where ((x ...) ...) ((get-possible-drugs-def definition) ...))])

(define-metafunction pop-pl
  get-possible-drugs-def : definition -> ((drug-type x) ...)
  [(get-possible-drugs-def state-def) ()]
  [(get-possible-drugs-def
    (define/handler (x ...)
      state-def ...
      e))
   (get-possible-drugs-e e)])

(define-metafunction pop-pl
  get-possible-drugs-e : e -> (x ...)
  [(get-possible-drugs-e x) ()]
  [(get-possible-drugs-e (e ...))
   (x ... ...)
   (where ((x ...) ...) ((get-possible-drugs-e e) ...))]
  [(get-possible-drugs-e (seq e ...))
   (x ... ...)
   (where ((x ...) ...) ((get-possible-drugs-e e) ...))]
  [(get-possible-drugs-e (oⁿ e ...))
   (x ... ...)
   (where ((x ...) ...) ((get-possible-drugs-e e) ...))]
  [(get-possible-drugs-e (if0 e ...))
   (x ... ...)
   (where ((x ...) ...) ((get-possible-drugs-e e) ...))]
  [(get-possible-drugs-e (λ (x ...) state-def ... e))
   (x ...)
   (where (x ...) (get-possible-drugs-e e))]
  [(get-possible-drugs-e (drug-type x)) x])
