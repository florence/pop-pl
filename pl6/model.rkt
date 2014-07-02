#lang racket
(require redex)

(define-syntax quasiquote (make-rename-transformer #'term))

(define-language pop-pl
  (P (state-def ... handler-def ...))
  ;; definitions
  ((definition def) 
   handler-def
   state-def)
  (handler-def (define/handler (x x x x) e))
  (state-def (define/state x v))
  ;; expressions
  (e x
     (lambda (x ...) e)
     (quote x)
     (seq e e ...)
     (e ...)
     (oⁿ e ...)
     n
     (s (x e) ...)
     (update x e)
     (new-handler x e)
     (remove-handler x)
     (send e)
     (if0 e e e))
  (oⁿ state-get equal? +)
  (n number)
  (x variable-not-otherwise-mentioned))

(define-extended-language pop-pl-eval pop-pl
  ;; current state, current message, current-eval, current-changes, current responses
  (HM (S v E ΔS (v ...)))
  (ΔS (H ((x v) ...)))
  (S (H state/v state/v))
  (H ((x h) ...))
  (h (lambda (x x x) e))
  (v state/v
     n
     (lambda (x ...) e)
     (quote x)
     (drug-type x))
  (state/v (s (x v) ...))
  (E hole
     (s (x v) ... (x E) (x e) ...)
     (update x E)
     (new-handler x E)
     (send E)
     (v ... E e ...)
     (oⁿ v ... E ...)
     (if0 E e e)
     (seq E e ...)))

(define-metafunction pop-pl-eval
  prepair : P -> S
  [(prepair (state-def ... handler-def ...)) 
   (H (s) state/v)
   (where ((define/handler (x_h x_a ...) e) ...) (handler-def ...))
   (where H ((x_h (lambda (x_a ...) e)) ...))
   (where ((define/state x_s v) ...) (state-def ...))
   (where state/v (s (x_s v) ...))])

;; we need a semantics for handling state changes with and without a new message
(define-metafunction pop-pl-eval
  eval : S (message ...) -> S
  [(eval S (v_1 v ...))
   (eval S_2 (v ...))
   (where (S_2 _) (eval-msg S v_1))])

(define-metafunction pop-pl-eval
  eval-msg : S v -> (S (v...))
  [(eval-msg S v)
   ((apply-ΔS S ΔS) (v_r ...))
   (where ((() _ _) _ v ΔS (v_r ...))
          ,(apply-reduction-relation* R `(S v (s) (H ()) ())))
   (where (H _ _) S)])

(define-metafunction pop-pl-eval
  apply-ΔS : S ΔS -> S
  [(apply-ΔS (_ _ state/v_1) (H_2 state/v_2))
   (H_2 state/v_1 (overwrite-state state/v_1 state/v_2))])

(define-metafunction pop-pl-eval
  overwrite-state : state/v ((x v) ...) -> state/v
  [(overwrite-state state/v ()) state/v]
  [(overwrite-state (s (x_1 v_1) ... (x _) (x_2 v_2) ...)
                    ((x v) (x_3 v_3) ...))
   (overwrite-state (s (x_1 v_1) ... (x v) (x_2 v_2) ...)
                    ((x_3 v_3) ...))])

(define R
  (reduction-relation
   pop-pl-eval
   #:domain HM
   (--> ((((x h) any_n ...) state/v_1 state/v_2) v_e v ΔS (v_r ...))
        ;; -- to --
        (((any_n ...) state/v_1 state/v_2)
         v_e
         (h state/v_1 state/v_2 v_e)
         ΔS
         (v_r ...))
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
   (--> (in-hole HM (+ n_1 n_2))
        ;; -- to --
        (in-hole HM ,(+ `n_1 `n_2)))
   (--> (S
         v_e
         (in-hole E (update x v))
         (H ((x_1 v_1) ...))
         (v_r ...))
        ;; -- to --
        (S
         v_e
         (in-hole E (s))
         (H ((x v) (x_1 v_1) ...))
         (v_r ...))
        (where (_ _ (st (x_s v_s) ...)) S)
        (side-condition (and
                         (member `x `(x_s ...))
                         (not (member `x `(x_1 ...)))))
        update)
   (--> ((((x_1 h_1) ...) state/v_1 state/v_2) 
         v_e
         (in-hole E (new-handler x h))
         (((x_2 h_2) ...) any)
         (v_r ...))
        ;; -- to --
        ((((x_1 h_1) ...) state/v_1 state/v_2)
         v_e
         (in-hole E (s))
         (((x h) (x_2 h_2) ...) any)
         (v_r ...))
        (side-condition (not (member `x `(x_1 ... x_2 ...))))
        new-handler)
   (--> (((x_1 h_1) ... (x h_s) (x_2 h_2) ...)
         v_e
         (in-hole E (remove-handler x))
         (((x_3 h_3) ... (x h_s) (x_4 h_4) ...) any)
         (v_r ...))
        ;; -- to --
        (((x_1 h_1) ... (x h_s) (x_2 h_2) ...)
         v_e
         (in-hole E (s))
         (((x_3 h_3) ... (x_4 h_4) ...)
          any))
        remove-handler)
   (--> (S
         v_e
         (in-hole E (send v))
         ΔS
         (v_r ...))
        ;; -- to --
        (S v_e (in-hole E (s)) ΔS (v v_r ...))
    send)
   (--> (in-hole HM (equal? v_1 v_2))
        ;; -- to --
        (in-hole HM ,(if (and
                          (or (number? `v_1)
                              (symbol? `v_1))
                          (equal? `v_1 `v_2))
                         0
                         42))
        δ_equal?)
   (--> (in-hole HM (if0 0 e_1 e_2))
        ;; -- to --
        (in-hole HM e_1)
        if0)
   (--> (in-hole HM (if0 v e_1 e_2))
        ;; -- to --
        (in-hole HM e_2)
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
  [(get-possible-drugs-def (define/handler (x ...) e))
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
