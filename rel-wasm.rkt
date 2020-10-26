#lang racket
;;;; Copyright Johan Sjölén 2020
(require redex
         "simple-wasm.rkt"
         "sym-wasm.rkt")

(provide (all-defined-out))

(define reduction-error-log (make-logger))

(define-extended-language rel-sym-wasm sym-wasm
  (r.e ::=
       (pair e* e*)
       (pair tf e* e*))
  (r.v ::= (pair v* v*))
  (e ::= .... r.e)
  (v ::= .... r.v)
  ;; TODO: isn't this a bit too strict?
  (unary-s ::= (mem globs funcs))
  (s ::= unary-s
     (pair (mem globs funcs)
           (mem globs funcs))))

(define (not-Eif e*)
  (not (redex-match rel-sym-wasm
                    (in-hole E_random ((const t_rand c_rand)
                                       ((if tf_rand e*_rand else e*_rand2) e*_randrand)))
                    e*)))

;; Invert e*
(define-metafunction rel-sym-wasm
  inv :  e* -> e*
  [(inv e*)
   (seq ,@(reverse (e...ify (term e*))))])
(define (e...ify e*)
  (if (equal? e* 'ϵ)
      '()
      (cons (car e*)
            (e...ify (cadr e*)))))


;; append two Fs together
(define-metafunction rel-sym-wasm
  F-append : F F -> F
  [(F-append () F) F]
  [(F-append (v_1 ... v) (v_2 ...))
   (F-append (v_1 ...) (v v_2 ...))])

;; convert a nested v* to a (v ...), uses accumulator
(define-metafunction rel-sym-wasm
  v*->F : v* -> F
  [(v*->F v*) (v*->F-helper v* ())])

(define-metafunction rel-sym-wasm
  v*->F-helper : v* F -> F
  [(v*->F-helper ϵ F) F]
  [(v*->F-helper (v v*) (v_acc ...))
   (v*->F-helper v* (v_acc ... v))])

(define-metafunction rel-sym-wasm
  seq : e ... -> e*
  [(seq) ϵ]
  [(seq e_0 e_1 ...) (e_0 (seq e_1 ...))])
; (term (seq (const i32 5) (const i32 5) (add i32)))
(define-metafunction rel-sym-wasm
  seq* : e ... e* -> e*
  [(seq* e*) e*]
  [(seq* e_0 e_1 ... e*) (e_0 (seq* e_1 ... e*))])

(define-metafunction rel-sym-wasm
  e*-append : e* e* -> e*
  [(e*-append ϵ e*) e*]
  [(e*-append (e_0 e*_0) e*_1)
   (e_0 (e*-append e*_0 e*_1))])


(define-metafunction rel-sym-wasm
  do-sym-binop : binop c c sym-var -> pc
  ; Concrete cases
  ; Noop stubs for xor and rotl
  [(do-sym-binop xor c_1 c_2 sym-var)
   ((= sym-var sym-var) ϵ)]
  [(do-sym-binop rotl c_1 c_2 sym-var)
   ((= sym-var sym-var) ϵ)]
  ; Symbolic case
  ; BUG: Whoa this is wrong.
  ; Needs an ite here
  [(do-sym-binop eq c_1 c_2 sym-var)
   ((= sym-var c_1)
    ((= sym-var c_2)
     ϵ))]
  [(do-sym-binop binop c_1 c_2 sym-var)
   ((= sym-var (binop c_1 c_2)) ϵ)
   (side-condition (not (eq? (term binop) (term eq))))])

;; find the depth of label nestings in E
(define-metafunction rel-sym-wasm
  label-depth : E -> number
  [(label-depth hole)  0]
  [(label-depth (v E)) (label-depth E)]
  [(label-depth ((label n {e*_0} E) e*_1))
   ,(add1 (term (label-depth E)))])

;; split an eval context into two contexts:
;;   - an outer context surrounding the second
;;   - an inner context with nested values around a hole
;; precondition: E actually has l-nested values when called
(define-metafunction rel-sym-wasm
  v-split : E number -> (E E)
  [(v-split hole l)
   (hole hole)]
  [(v-split (in-hole E (v hole)) 0)
   ((in-hole E_outer (v hole)) hole)
   (where (E_outer hole) (v-split E 0))]
  [(v-split (in-hole E (v hole)) l)
   (E_outer (in-hole E_v (v hole)))
   (where (E_outer E_v) (v-split E ,(sub1 (term l))))]
  [(v-split (in-hole E ((label n {e*_0} hole) e*_2)) 0)
   ((in-hole E_outer ((label n {e*_0} hole) e*_2))
    hole)
   (where (E_outer hole) (v-split E 0))])

(define-metafunction rel-sym-wasm
  store-glob : globs i -> v
  [(store-glob  (glob v_0 ... v v_1 ...) i)
   v
   (side-condition (= (length (term (v_0 ...))) (term i)))])
(define-metafunction rel-sym-wasm
  store-glob= : globs i c -> globs
  [(store-glob=  (glob v_0 ... (const t c_old) v_1 ...) i c)
   (glob v_0 ... (const t c) v_1 ...)
   (side-condition (= (length (term (v_0 ...))) (term i)))])

(define-metafunction rel-sym-wasm
  store-func : funcs i -> cl
  [(store-func (func cl_0 ... cl cl_1 ...) i)
   cl
   (side-condition (= (length (term (cl_0 ...))) (term i)))])

(define-metafunction rel-sym-wasm
  cl-code : cl -> f
  [(cl-code {any (code f)}) f])
(define-metafunction sym-wasm
  cl-inst : cl -> i
  [(cl-inst {(inst i) any}) i])


(define-metafunction rel-sym-wasm
  conf...->conf* : (conf ...) -> conf*
  [(conf...->conf* ()) ϵ]
  [(conf...->conf* (conf conf_0 ...))
   (conf
    (conf...->conf* (conf_0 ...)))])

(define-metafunction rel-sym-wasm
  conf*->conf... : conf* -> (conf ...)
  [(conf*->conf... conf*) (conf*->conf...% conf* ())])
(define-metafunction rel-sym-wasm
  conf*->conf...% : conf* (conf ...) -> (conf ...)
  [(conf*->conf...% ϵ any_accum) any_accum]
  [(conf*->conf...% (conf conf*) (conf_accum ...))
   (conf*->conf...% conf* (conf conf_accum ...))])


(define-metafunction rel-sym-wasm
  conf*-append : conf* conf* -> conf*
  [(conf*-append ϵ conf*) conf*]
  [(conf*-append conf* ϵ) conf*]
  [(conf*-append (conf conf*_0) conf*_1)
   (conf (conf*-append conf*_0 conf*_1))])
(define-metafunction rel-sym-wasm
  pc-append : pc pc -> pc
  [(pc-append ϵ pc) pc]
  [(pc-append pc ϵ) pc]
  [(pc-append (constr pc_0) pc_1)
   (constr (pc-append pc_0 pc_1))])

(define-metafunction rel-sym-wasm
  conf*-append*% : conf* (conf* ...) -> conf*
  [(conf*-append*% conf* (ϵ))
   conf*]
  [(conf*-append*% conf* (ϵ conf*_rest ...))
   (conf*-append*% conf* (conf*_rest ...))]
  [(conf*-append*% conf* ((conf conf*_0) conf*_rest ...))
   (conf (conf*-append*% conf* (conf*_0 conf*_rest ...)))])
(define-metafunction rel-sym-wasm
  conf*-append* : conf* ... -> conf*
  [(conf*-append* conf*_0 conf*_1 ...)
   (conf*-append*% conf*_0 (conf*_1 ...))])

(define-metafunction rel-sym-wasm
  plug-local* : conf* any_e* any_E any_F0 -> conf*
  [(plug-local* ϵ any_e* any_E any_F0) ϵ]
  [(plug-local* ((s F_new e* pc) conf*) any_e*_hole any_E any_F0)
   ((s any_F0 (in-hole any_E ,(plug (plug (term any_e*_hole) (term F_new)) (term e*))) pc)
    (plug-local* conf* any_e*_hole any_E any_F0))])

(define-metafunction rel-sym-wasm
  unzip : ((string conf) ...) -> ((string ...) (conf ...))
  [(unzip any_stuff)
   (unzip% any_stuff (() ()))])

(define-metafunction rel-sym-wasm
  length* : conf* -> number
  ([length* (conf conf*)] ,(+ 1 (term (length* conf*))))
  ([length* ϵ] 0))

(define-metafunction rel-sym-wasm
  unzip% : ((string conf) ...) ((string ...) (conf ...)) -> ((string ...) (conf ...))
  [(unzip% () any_unzipped)
   any_unzipped]
  [(unzip% ((string_0 conf_0) (string_1 conf_1) ...)
          ((string ...) (conf ...)))
   (unzip% ((string_1 conf_1) ...)
    ((string_0 string ...) (conf_0 conf ...)))])


(define-metafunction rel-sym-wasm
  project-store : s number -> s  ; number ∈ (1,2)
  [(project-store (pair s_1 s_2) 1) s_1]
  [(project-store (pair s_1 s_2) 2) s_2]
  [(project-store s any) s])

(define-metafunction rel-sym-wasm
  first/empty-conf : (conf* ...) -> conf*
  [(first/empty-conf ())  (((ϵ (glob) (func)) () ϵ ϵ) ϵ)]
  [(first/empty-conf (conf* any ...)) conf*])

;; TODO: Expand merging of stores to lift concrete values.
(define-metafunction rel-sym-wasm
  merge-stores : s s -> s
  [(merge-stores (mem_1 globs_1 funcs_1) (mem_2 globs_2 funcs_2))
   (pair (mem_1 globs_1 funcs_1) (mem_2 globs_2 funcs_2))])

(define-metafunction rel-sym-wasm
  pick-rel-conf* : conf*_l conf*_r boolean_l-if? boolean_r-if? -> conf*
  [(pick-rel-conf* conf*_l conf*_r #f #f)
   (conf*-append conf*_l conf*_r)]
  [(pick-rel-conf* conf*_l conf*_r #t #f)
   conf*_r]
  [(pick-rel-conf* conf*_l conf*_r #f #t)
   conf*_l])

(define-metafunction rel-sym-wasm
  apply-rel-> : any -> (conf* ...)
  [(apply-rel-> any_arg) (conf* ...)
   (where (conf* ...) ,(apply-reduction-relation rel-> (term any_arg)))])
(define-metafunction rel-sym-wasm
  valid-reduction? : any -> boolean
  [(valid-reduction? any) #true
   (side-condition (> (length (term any)) 0))]
  [(valid-reduction? any) #false
   (side-condition (= (length (term any)) 0))])

(define-metafunction rel-sym-wasm
  plug-pair* : conf* any_e* any_E any_store -> conf*
  [(plug-pair* ϵ any_e* any_E any_store) ϵ]
  [(plug-pair* ((s F e* pc) conf*) any_e*_hole any_E any_store)
   (((merge-stores s any_store) F (in-hole any_E ,(plug (term any_e*_hole) (term e*))) pc)
    (plug-pair* conf* any_e*_hole any_E any_store))])

(define-metafunction rel-sym-wasm
  plug-pair : conf*_left conf*_right any_e*-hole any_E any_store pc -> conf*
  [(plug-pair ((s_l F_l e*_l pc_l) ϵ)
              ((s_r F_r e*_r pc_r) ϵ) any_e*-hole any_E)
   ((merge-stores s_l s_r) () ; TODO: F !!!
    (in-hole any_E 
             ,(plug (term e*_r) (plug (term e*_r) (term any_e*))))
    pc
    )])

(define-metafunction rel-sym-wasm
  final? : conf -> boolean
  [(final? ϵ) #t]
  [(final? (s F e* pc))
   ,(redex-match? rel-sym-wasm v* (term e*))])

(define (show x)
  (displayln x)
  x)

(define rel->
  (reduction-relation
   rel-sym-wasm
   #:domain conf*
   #:codomain conf*

   ;; =========================
   ;; Pair e*
   ;; e* | \eps -> e*' | \eps            -- step-left      D
   ;; \eps | e* -> \eps | e*'            -- step-right     D
   ;; e* | e* -> e*' | e* , e* > e*'     -- step-both      D
   ;; if | e* -> if | e*'                -- step-if-left   D
   ;; e* | if -> e*' | if                -- step-if-right  D
   ;; if | \eps -> tt | \eps, ff | \eps  -- step-if/ϵ      D
   ;; \eps | if -> \eps | tt, \eps | ff  -- step-ϵ/if      D
   ;; if | if -> tt|tt tt|ff ff|tt ff|ff -- step-if/if     D
   ;; =========================

   (--> (in-hole EE ((s F (in-hole E
                                   ((pair (in-hole E_l ((const t_l c_l)
                                                          ((if tf_l e*_thn-l else e*_els-l) e*_l)))
                                          (in-hole E_r ((const t_r c_r)
                                                        ((if tf_r e*_thn-r else e*_els-r) e*_r))))
                                    e*))
                        pc) conf*))
        (in-hole EE
                 (conf*-append* conf*_tt/tt conf*_tt/ff conf*_ff/tt conf*_ff/ff conf*))
        
        (where conf*_tt/tt
               ((s F (in-hole E ((pair (in-hole E_l ((block tf_l e*_thn-l) e*_l))
                                       (in-hole E_r ((block tf_r e*_thn-r) e*_r)))
                                 e*))
                   ((> c_r 0) ((> c_l 0) pc))) ϵ))
        (where conf*_tt/ff
               ((s F (in-hole E ((pair (in-hole E_l ((block tf_l e*_thn-l) e*_l))
                                       (in-hole E_r ((block tf_r e*_els-r) e*_r)))
                                 e*))
                   ((= c_r 0) ((> c_l 0) pc))) ϵ))
        (where conf*_ff/tt
               ((s F (in-hole E ((pair (in-hole E_l ((block tf_l e*_els-l) e*_l))
                                       (in-hole E_r ((block tf_r e*_thn-r) e*_r)))
                                 e*))
                   ((> c_r 0) ((= c_l 0) pc))) ϵ))
        (where conf*_ff/ff
               ((s F (in-hole E ((pair (in-hole E_l ((block tf_l e*_els-l) e*_l))
                                       (in-hole E_r ((block tf_r e*_els-r) e*_r)))
                                 e*))
                   ((= c_r 0) ((= c_l 0) pc))) ϵ))
        step-if/if)

   (--> (in-hole EE ((s F (in-hole E ((pair e*_l
                                            (in-hole E_r ((const t_r c_r)
                                                          ((if tf e*_thn else e*_els) e*_r))))
                                      e*))
                        pc) conf*))
        (in-hole EE (conf*-append conf*_tt conf*_ff))

        (where #true (final? ((project-store s 2) F e*_l  pc)))

        (where conf*_tt
               ((s F (in-hole E ((pair e*_l
                                       (in-hole E_r ((block tf e*_thn) e*_r)))
                                 e*))
                   ((> c_r 0) pc)) ϵ))
        (where conf*_ff
               ((s F (in-hole E ((pair e*_l
                                       (in-hole E_r ((block tf e*_els) e*_r)))
                                 e*))
                   ((= c_r 0) pc)) ϵ))
        step-ϵ/if)

   (--> (in-hole EE ((s F (in-hole E ((pair (in-hole E_l ((const t_l c_l)
                                                          ((if tf e*_thn else e*_els) e*_l)))
                                            e*_r) e*))
                        pc) conf*))
        (in-hole EE (conf*-append conf*_tt conf*_ff))

        (where #true (final? ((project-store s 2) F e*_r  pc)))

        (where conf*_tt
               ((s F (in-hole E ((pair (in-hole E_l ((block tf e*_thn) e*_l))
                                       e*_r)
                                 e*))
                   ((> c_l 0) pc)) ϵ))
        (where conf*_ff
               ((s F (in-hole E ((pair (in-hole E_l ((block tf e*_els) e*_l))
                                       e*_r)
                                 e*))
                   ((= c_l 0) pc)) ϵ))
        step-if/ϵ)
   


   (--> (in-hole EE ((s F (in-hole E ((pair e*_l
                                            (in-hole E_r ((const t_r c_r)
                                                          ((if tf e*_thn else e*_els) e*_r))))
                                      e*))
                        pc) conf*))
        (in-hole EE (conf*-append conf*_l-plugged conf*))

        (where conf_l ((project-store s 2) F e*_l  pc))

        (where #false (final? conf_l))
        (side-condition (not-Eif (term e*_l)))
        
        (where (conf*_all ...)
               (apply-rel-> (conf_l ϵ)))
        
        (where #true (valid-reduction? (conf*_all ...)))
        (where conf*_left (first/empty-conf (conf*_all ...)))

        (where conf*_l-plugged
               (plug-pair* conf*_left
                           ((pair hole
                                  (in-hole E_r ((const t_r c_r)
                                                ((if tf e*_thn else e*_els) e*_r)))) e*)
                           E
                           (project-store s 2)))
        step-if-right)


   (--> (in-hole EE ((s F (in-hole E ((pair (in-hole E_l ((const t_l c_l)
                                                          ((if tf e*_thn else e*_els) e*_l)))
                                            e*_r) e*))
                        pc) conf*))
        (in-hole EE (conf*-append conf*_r-plugged conf*)) ; conf*_r-plugged

        (where conf_r ((project-store s 2) F e*_r  pc))

        (where #false (final? conf_r))
        (side-condition (not-Eif (term e*_r)))
        
        (where (conf*_all ...)
               ,(apply-reduction-relation rel-> (term (conf_r ϵ))))
        
        (side-condition (> (length (term (conf*_all ...))) 0))
        (where conf*_right ,(first (term (conf*_all ...))))

        (where conf*_r-plugged
               (plug-pair* conf*_right
                           ((pair (in-hole E_l ((const t_l c_l) ((if tf e*_thn else e*_els) e*_l)))
                                  hole) e*)
                           E
                           (project-store s 1)))
        step-if-left)

   (--> (in-hole EE ((s F (in-hole E ((pair e*_l e*_r) e*)) pc) conf*))
        (in-hole EE (conf*-append* conf* conf*_reduced-once))

        (where #false (final? (s F e*_l pc)))
        (where #false (final? (s F e*_r pc)))
        (side-condition (not-Eif (term e*_l)))
        (side-condition (not-Eif (term e*_r)))
        
        (where (conf*_left ...) ; NOTE: Actually sym->, but pc might not be in sym-wasm
               (apply-rel->
                (((project-store s 1) F e*_l pc) ϵ)))
        (where #true (valid-reduction? (conf*_left ...)))
        (where (conf_l ϵ) (first/empty-conf (conf*_left ...))) ; Mustn't have branched
        (where (any_s_l any any_e*_ll any_pc_l) conf_l)

        (where (conf*_right ...) ; NOTE: Actually sym->, but pc might not be in sym-wasm
               (apply-rel->
                (((project-store s 2) 
                         F
                         e*_r 
                         any_pc_l)
                        ϵ)))
        (where #true (valid-reduction? (conf*_right ...)))
        (where ((any_s_r any any_e*_rr any_pc_r) ϵ) (first/empty-conf (conf*_right ...)))

        (where conf*_reduced-once
               (((pair
                  (project-store any_s_l 1)
                  (project-store any_s_r 2))
                 F
                 (in-hole E
                  ((pair any_e*_ll
                         any_e*_rr)
                   e*))
                 any_pc_r) ϵ))
        step-both)

   (--> (in-hole EE ((s F (in-hole E ((pair e*_l e*_r) e*)) pc)
                        conf*))
        (in-hole EE (conf*-append conf* conf*_r-plug))

        (where #true (final? (s F e*_l pc)))
        (side-condition (not-Eif (term e*_r)))

        (where (conf*_right ...)  ; NOTE: Actually sym->, but pc might not be in sym-wasm
               ,(apply-reduction-relation rel->
                                         (term (((project-store s 2) F e*_r pc) ϵ))))
        (side-condition (> (length (term (conf*_right ...))) 0))
        (where conf*_r ,(first (term (conf*_right ...))))
        
        (where conf*_r-plug
               (plug-pair* conf*_r
                            ((pair e*_l hole) e*)
                            E
                            (project-store s 1)))
        step-right)
   
   (--> (in-hole EE ((s F (in-hole E ((pair e*_l e*_r) e*)) pc)
                        conf*))
        (in-hole EE (conf*-append conf* conf*_l-plug))

        (where #true (final? (s F e*_r pc)))
        (side-condition (not-Eif (term e*_l)))
        
        (where (conf*_left ...)  ; NOTE: Actually sym->, but pc might not be in sym-wasm
               ,(apply-reduction-relation rel->
                                            (term (((project-store s 1) F e*_l pc) ϵ))))
        (side-condition (> (length (term (conf*_left ...))) 0))
        (where conf*_l ,(first (term (conf*_left ...))))
        
        (where conf*_l-plug
               (plug-pair* conf*_l
                            ((pair hole e*_r) e*)
                            E
                            (project-store s 2)))
        step-left)

   ;; =========================
   ;; sym->
   ;; =========================
   
   (--> (in-hole EE ((s F (in-hole E ((const t c_1) ((const t c_2) ((binop t) e*)))) pc) conf*))
        (in-hole EE ((s F (in-hole E ((const t sym-var-) e*)) 
                        (pc-append pc_0 pc))
                     conf*))
        (fresh sym-var-)
        (where pc_0 (do-sym-binop binop c_1 c_2 sym-var-))
        binop)

   (--> (in-hole EE ((s F (in-hole E (v (drop e*))) pc) conf*))
        (in-hole EE ((s F (in-hole E e*) pc) conf*))
        drop)
   (--> (in-hole EE ((s F (in-hole E (nop e*)) pc) conf*))
        (in-hole EE ((s F (in-hole E e*) pc) conf*))
        nop)
   (--> (in-hole EE ((s F (in-hole E (unreachable e*)) pc) conf*))
        (in-hole EE ((s F (in-hole E (trap ϵ)) pc) conf*))
        unreachable)
   (--> (in-hole EE ((s F (in-hole E (trap e*)) pc) conf*))
        (in-hole EE ((s F ϵ (false pc)) conf*))
        trap)
   (--> (in-hole EE (((mem globs funcs) F (in-hole E ((get-global i) e*)) pc) conf*))
        (in-hole EE (((mem globs funcs) F (in-hole E ((store-glob globs i) e*)) pc) conf*))
        global-load)

   (--> (in-hole EE (((mem globs funcs) F (in-hole E ((const t c) ((set-global i) e*))) pc) conf*))
        (in-hole EE (((mem (store-glob= globs i c) funcs) F (in-hole E e*) pc) conf*))
        global-store)

   (--> (in-hole EE ((s F (v ((tee-global j) e*)) pc) conf*))
        (in-hole EE ((s F (v (v ((set-global j) e*))) pc) conf*))
        tee-global)

   (--> (in-hole EE (((mem globs funcs) F (in-hole E ((const t c) ((load t a o) e*))) pc) conf*))
        (in-hole EE ((((constr_1 mem) globs funcs) F
                      (in-hole E ((const t sym-var-loaded) e*))
                      (constr_id (constr_nooverlap pc)))
                     conf*))
        (fresh sym-var-loaded)
        (where constr_1 (select (add o c) sym-var-loaded))
        (where constr_nooverlap (= (mod (add o c) 4) 0))
        ; Make sym-var-loaded appear in pc
        ; This is important for step-both
        (where constr_id (= sym-var-loaded sym-var-loaded))
        load)
   
   (--> (in-hole EE (((mem globs funcs) F (in-hole E ((const t k)
                                                      ((const t c) ((store t a o) e*)))) pc) conf*))
        (in-hole EE ((((constr mem) globs funcs) F (in-hole E e*) (constr_nooverlap pc)) conf*))
       (where constr (store (add o k) c))
       (where constr_nooverlap (= (mod (add o k) 4) 0))
       store)

   (--> (in-hole EE ((s F (in-hole E ((const t c) ((if tf e*_then else e*_else) e*))) pc) conf*))
        (in-hole EE ((s F  (in-hole E (seq* (block tf e*_else) e*)) ((= c 0) pc))
                     ((s F (in-hole E (seq* (block tf e*_then) e*)) ((> c 0) pc))
                      conf*)))
        if-then-else)

   (--> (in-hole EE ((s F (in-hole E ((block (-> (t_n ...) (t_m ...)) e*_body) e*_cont)) pc) conf*))
        (in-hole EE ((s F (in-hole E_outer (seq* (label l {ϵ}
                                                        (in-hole E_v e*_body)) e*_cont)) pc) conf*))
        (where l ,(length (term (t_m ...))))
        (where k ,(length (term (t_n ...))))
        (where (E_outer E_v) (v-split E k))
        block)
   
   (--> (in-hole EE ((s F (in-hole E ((name e_loop
                                            (loop (-> (t_1 ...) (t_2 ...)) e*_0)) e*_1)) pc) conf*))
        (in-hole EE ((s F (in-hole E_outer (seq* e_lbl e*_1)) pc) conf*))
        (where l ,(length (term (t_2 ...))))
        (where k ,(length (term (t_1 ...))))
        (where (E_outer E_v) (v-split E k))
        (where e_lbl (label k {(seq e_loop)} (in-hole E_v e*_0)))
        loop)
 
   (--> (in-hole EE ((s F (in-hole E ((label n {e*_cont} v*) e*_outer)) pc) conf*))
        (in-hole EE ((s F (in-hole E (e*-append v* e*_outer)) pc) conf*))
        label-value)
   (--> (in-hole EE ((s F (in-hole E ((label n {e*_cont} trap) e*_outer)) pc) conf*))
        (in-hole EE ((s F (in-hole E (trap e*_outer)) pc) conf*))
        label-trap)
   (--> (in-hole EE ((s F (in-hole E ((label n {e*_cont}
                                             (in-hole E_inner ((br j) e*_1))) e*_2)) pc) conf*))
        (in-hole EE ((s F (in-hole E (e*-append (in-hole E_v e*_cont) e*_2)) pc) conf*))
        (where j (label-depth E_inner))
        (where (E_outer E_v) (v-split E_inner n))
        label-br)

  (--> (in-hole EE (((mem globs funcs) F (in-hole E ((call j) e*)) pc) conf*))
       (in-hole EE (((mem globs funcs) F (in-hole E ((call (store-func funcs j)) e*)) pc) conf*))
    call-index)
  
  (--> (in-hole EE ((s F (in-hole E ((call cl) e*)) pc) conf*))
       (in-hole EE ((s F (in-hole E_outer ((local m {(cl-inst cl) F_new} e*_block) e*)) pc) conf*))
       (where (func (-> (t_n ...) (t_m ...)) local (t ...) e*_body) (cl-code cl))
       (where n ,(length (term (t_n ...))))
       (where m ,(length (term (t_m ...))))
       (where (E_outer E_v) (v-split E n))
       (where F_new (F-append (v*->F (in-hole E_v ϵ)) ((const t 0) ...)))
       (where e*_block (seq (block (-> () (t_m ...)) e*_body)))
       call-closure)
  
  (--> (in-hole EE ((s (name F (v_1 ... v v_2 ...)) (in-hole E ((get-local j) e*)) pc) conf*))
       (in-hole EE ((s F (in-hole E (v e*)) pc) conf*))
       (side-condition (= (length (term (v_1 ...))) (term j)))
       get-local)

   (--> (in-hole EE ((s (v_1 ... v v_2 ...) (in-hole E (v_new ((set-local j) e*))) pc) conf*))
        (in-hole EE ((s (v_1 ... v_new v_2 ...) (in-hole E e*) pc) conf*))
        (side-condition (= (length (term (v_1 ...))) (term j)))
        set-local)

   (--> (in-hole EE ((s F (v ((tee-local j) e*)) pc) conf*))
        (in-hole EE ((s F (v (v ((set-local j) e*))) pc) conf*))
        tee-local)
   
   (-->  (in-hole EE ((s F (in-hole E ((local n {i F} v*) e*)) pc) conf*))
         (in-hole EE ((s F (in-hole E (e*-append v* e*)) pc) conf*))
       local-value)
   (--> (in-hole EE ((s F (in-hole E ((local n {i F} (trap e*_0)) e*)) pc) conf*))
        (in-hole EE ((s F (in-hole E ϵ) pc) conf*))
       local-trap)
   (--> (in-hole EE ((s F (in-hole E ((local n {i F_1}
                                        (in-hole E_inner (return e*_0))) e*_1)) pc) conf*))
        (in-hole EE ((s F (in-hole E (in-hole E_v e*_1)) pc) conf*))
        (where (E_outer E_v) (v-split E_inner n))
        local-return)
   
   (--> (in-hole EE ((s_0 F_0 (in-hole E ((local n {i F_1} e*_0) e*_2)) pc) conf*))
        (in-hole EE (conf*-append conf*_plugged conf*))
        (where (conf*_next ...)
               ,(apply-reduction-relation rel-> (term ((s_0 F_1 e*_0 pc) ϵ))))
        (side-condition (> (length (term (conf*_next ...))) 0)) ; Needs to reduce to something
        ;; `deterministic' since we collect all configurations
        (where (name C conf*_C) ,(first (term (conf*_next ...))))
        (where conf*_plugged  (plug-local* C
                                           ((local n {i hole} hole) e*_2)
                                           E
                                           F_0))
        reduce-local)
   ))

; (rule-pict-style 'horizontal-left-align)
; (render-reduction-relation-rules (list 'step-both))

(define test
  (term (( ; funcs HAS to be the same
          (pair (ϵ (glob) (func {(inst 0) (code (func (-> () (i32)) local () (seq (const i32 2))))}))
                (ϵ (glob) (func {(inst 0) (code (func (-> () (i32)) local () (seq (const i32 2))))})))
          ()
          (seq (call 0))
          ϵ)
         ϵ)))

(define test2
  (term (( ; funcs HAS to be the same
          (pair (ϵ (glob 0) (func {(inst 0)
                                   (code (func (-> () (i32)) local () (seq (const i32 2))))}))
                (ϵ (glob 1) (func {(inst 0)
                                   (code (func (-> () (i32)) local () (seq (const i32 2))))})))
          ()
          (seq (get-global 0))
          ϵ)
         ϵ)))

(define test3
  (term (((pair (ϵ (glob (const i32 0)) (func))
                (ϵ (glob (const i32 1)) (func)))
          ()
          (seq (pair (seq (const i32 1))
                     (seq (const i32 2)))
               (const i32 1)
               (const i32 2)
               (add i32)
               (get-global 0))
          ϵ)
         ϵ)))
