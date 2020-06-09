#lang racket
;;;; Copyright Johan Sjölén 2020
(require redex
         "simple-wasm.rkt"
         pict)

(provide (all-defined-out))
#|
TODOs:
- Implement invariants for function calls and loops
|#

;; Not seen in definition:
;; Only supports non-overlapping loads/stores
(define-extended-language sym-wasm simple-wasm-runtime-lang
  (sym-var ::= (variable-prefix sym-var-))
  (c ::= .... sym-var (binop c c) (relop c c) (mod c c)) ;; mod = modulo, for asserts

  ((i j l k m n a o) c)
  (constr ::=
          (= c c)
          (<= c c) ; c lte c
          (> c c) ; c gt c
          (select c c) ; The.Arr.
          (store c c) ; The.Arr.
          false ; equiv. to trap in practice
          (-> constr constr)
          (and constr constr)
          (not constr)
          (global c))
  (pc ::= ϵ (constr pc)) ; Alias
  
  (conf ::= (s F e* pc))
  (conf* ::= ϵ
        (conf conf*))
  (EE ::= hole
      ((s F v* pc) EE)))

;; append two Fs together
(define-metafunction sym-wasm
  F-append : F F -> F
  [(F-append () F) F]
  [(F-append (v_1 ... v) (v_2 ...))
   (F-append (v_1 ...) (v v_2 ...))])

;; convert a nested v* to a (v ...), uses accumulator
(define-metafunction sym-wasm
  v*->F : v* -> F
  [(v*->F v*) (v*->F-helper v* ())])

(define-metafunction sym-wasm
  v*->F-helper : v* F -> F
  [(v*->F-helper ϵ F) F]
  [(v*->F-helper (v v*) (v_acc ...))
   (v*->F-helper v* (v_acc ... v))])

(define-metafunction sym-wasm
  seq : any ... -> any
  [(seq) ϵ]
  [(seq any_0 any_1 ...) (any_0 (seq any_1 ...))])
; (term (seq (const i32 5) (const i32 5) (add i32)))
(define-metafunction sym-wasm
  seq* : e ... e* -> e*
  [(seq* e*) e*]
  [(seq* e_0 e_1 ... e*) (e_0 (seq* e_1 ... e*))])

(define-metafunction sym-wasm
  e*-append : e* e* -> e*
  [(e*-append ϵ e*) e*]
  [(e*-append (e_0 e*_0) e*_1)
   (e_0 (e*-append e*_0 e*_1))])

(define-metafunction sym-wasm
  do-sym-binop : binop c c -> c
  ; Concrete cases
#|  [(do-sym-binop add c_1 c_2)
   ,(+ (term c_1)  (term c_2))
   (side-condition (number? (term c_1)))
   (side-condition (number? (term c_2)))]
  [(do-sym-binop sub c_1 c_2)
   ,(- (term c_1)  (term c_2))
   (side-condition (number? (term c_1)))
   (side-condition (number? (term c_2)))]
  [(do-sym-binop mul c_1 c_2)
   ,(* (term c_1)  (term c_2))
   (side-condition (number? (term c_1)))
   (side-condition (number? (term c_2)))]
  [(do-sym-binop div c_1 c_2)
   ,(/ (term c_1)  (term c_2))
   (side-condition (number? (term c_1)))
   (side-condition (number? (term c_2)))]|#
  ; Symbolic case
  [(do-sym-binop binop c_1 c_2)  (binop c_1 c_2)])

;; find the depth of label nestings in E
(define-metafunction sym-wasm
  label-depth : E -> number
  [(label-depth hole)  0]
  [(label-depth (v E)) (label-depth E)]
  [(label-depth ((label n {e*_0} E) e*_1))
   ,(add1 (term (label-depth E)))])

;; split an eval context into two contexts:
;;   - an outer context surrounding the second
;;   - an inner context with nested values around a hole
;; precondition: E actually has l-nested values when called
(define-metafunction sym-wasm
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

(define-metafunction sym-wasm
  store-glob : globs i -> v
  [(store-glob  (glob v_0 ... v v_1 ...) i)
   v
   (side-condition (= (length (term (v_0 ...))) (term i)))])
(define-metafunction sym-wasm
  store-glob= : globs i c -> globs
  [(store-glob=  (glob v_0 ... (const t c_old) v_1 ...) i c)
   (glob v_0 ... (const t c) v_1 ...)
   (side-condition (= (length (term (v_0 ...))) (term i)))])

(define-metafunction sym-wasm
  store-func : funcs i -> cl
  [(store-func (func cl_0 ... cl cl_1 ...) i)
   cl
   (side-condition (= (length (term (cl_0 ...))) (term i)))])

(define-metafunction sym-wasm
  cl-code : cl -> f
  [(cl-code {any (code f)}) f])
(define-metafunction sym-wasm
  cl-inst : cl -> i
  [(cl-inst {(inst i) any}) i])


(define-metafunction sym-wasm
  conf...->conf* : (conf ...) -> conf*
  [(conf...->conf* ()) ϵ]
  [(conf...->conf* (conf conf_0 ...))
   (conf
    (conf...->conf* (conf_0 ...)))])

(define-metafunction sym-wasm
  conf*->conf... : conf* -> (conf ...)
  [(conf*->conf... conf*) (conf*->conf...% conf* ())])
(define-metafunction sym-wasm
  conf*->conf...% : conf* (conf ...) -> (conf ...)
  [(conf*->conf...% ϵ any_accum) any_accum]
  [(conf*->conf...% (conf conf*) (conf_accum ...))
   (conf*->conf...% conf* (conf conf_accum ...))])


(define-metafunction sym-wasm
  conf*-append : conf* conf* -> conf*
  [(conf*-append ϵ conf*) conf*]
  [(conf*-append conf* ϵ) conf*]
  [(conf*-append (conf conf*_0) conf*_1)
   (conf (conf*-append conf*_0 conf*_1))])

(define-metafunction sym-wasm
  plug-local* : conf* any_e* any_E any_F0 -> conf*
  [(plug-local* ϵ any_e* any_E any_F0) ϵ]
  [(plug-local* ((s F_new e* pc) conf*) any_e*_hole any_E any_F0)
   ((s any_F0 (in-hole any_E ,(plug (plug (term any_e*_hole) (term F_new)) (term e*))) pc)
    (plug-local* conf* any_e*_hole any_E any_F0))])

(define-metafunction sym-wasm
  unzip : ((string conf) ...) -> ((string ...) (conf ...))
  [(unzip any_stuff)
   (unzip% any_stuff (() ()))])

(define-metafunction sym-wasm
  length* : conf* -> number
  ([length* (conf conf*)] ,(+ 1 (term (length* conf*))))
  ([length* ϵ] 0))

(define-metafunction sym-wasm
  unzip% : ((string conf) ...) ((string ...) (conf ...)) -> ((string ...) (conf ...))
  [(unzip% () any_unzipped)
   any_unzipped]
  [(unzip% ((string_0 conf_0) (string_1 conf_1) ...)
          ((string ...) (conf ...)))
   (unzip% ((string_1 conf_1) ...)
    ((string_0 string ...) (conf_0 conf ...)))])


(define sym->
  (reduction-relation
   sym-wasm
   #:domain conf*
   #:codomain conf*

   (--> (in-hole EE ((s F (in-hole E ((const t c_1) ((const t c_2) ((binop t) e*)))) pc) conf*))
        (in-hole EE ((s F (in-hole E ((const t c_3) e*)) pc) conf*))
        (where c_3 (do-sym-binop binop c_1 c_2))
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

   (--> (in-hole EE (((mem globs funcs) F (in-hole E ((const t c) ((load t a o) e*))) pc) conf*))
        (in-hole EE ((((constr_1 mem) globs funcs) F
                      (in-hole E ((const t sym-var-loaded) e*))
                      (constr_nooverlap pc))
                     conf*))
        (fresh sym-var-loaded)
        (where constr_1 (select (add o c) sym-var-loaded))
        (where constr_nooverlap (= (mod (add o c) 4) 0))
        load)
   
   (--> (in-hole EE (((mem globs funcs) F (in-hole E ((const t k) ((const t c) ((store t a o) e*)))) pc) conf*))
        (in-hole EE ((((constr mem) globs funcs) F (in-hole E e*) (constr_nooverlap pc)) conf*))
       (where constr (store (add o k) c))
       (where constr_nooverlap (= (mod (add o k) 4) 0))
       store)

   (--> (in-hole EE ((s F (in-hole E ((const t c) ((if tf e*_then else e*_else) e*))) pc) conf*))
        (in-hole EE ((s F  (in-hole E (seq* (block tf e*_else) e*)) ((<= c 0) pc))
                     ((s F (in-hole E (seq* (block tf e*_then) e*)) ((> c 0) pc))
                      conf*)))
        if-then-else)

   (--> (in-hole EE ((s F (in-hole E ((block (-> (t_n ...) (t_m ...)) e*_body) e*_cont)) pc) conf*))
        (in-hole EE ((s F (in-hole E_outer (seq* (label l {ϵ} (in-hole E_v e*_body)) e*_cont)) pc) conf*))
        (where l ,(length (term (t_m ...))))
        (where k ,(length (term (t_n ...))))
        (where (E_outer E_v) (v-split E k))
        block)
   
   (--> (in-hole EE ((s F (in-hole E ((name e_loop (loop (-> (t_1 ...) (t_2 ...)) e*_0)) e*_1)) pc) conf*))
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
   (--> (in-hole EE ((s F (in-hole E ((label n {e*_cont} (in-hole E_inner ((br j) e*_1))) e*_2)) pc) conf*))
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
   (--> (in-hole EE ((s F (in-hole E ((local n {i F_1}(in-hole E_inner (return e*_0))) e*_1)) pc) conf*))
        (in-hole EE ((s F (in-hole E (in-hole E_v e*_1)) pc) conf*))
        (where (E_outer E_v) (v-split E_inner n))
        local-return)
   
   (--> (in-hole EE ((s_0 F_0 (in-hole E ((local n {i F_1} e*_0) e*_2)) pc) conf*))
        (in-hole EE (conf*-append conf*_plugged conf*))
        (where (conf*_next ...)
               ,(apply-reduction-relation sym-> (term ((s_0 F_1 e*_0 pc) ϵ))))
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
; (render-reduction-relation-rules (list 'if-then-else))
; (lt-find (render-reduction-relation sym->) (arrow->pict '-->))
; (pin-over (render-reduction-relation sym->) 378.0 -3  (text "sym"))

#|
> (require racket/draw)
> (define (make-ps/pdf-dc filename)
  (let ([ps-setup (make-object ps-setup%)])
    (send ps-setup copy-from (current-ps-setup))
    (send ps-setup set-file filename)
    (send ps-setup set-mode 'file)
    (define is-pdf? 
      (cond
        [(path? filename) (regexp-match #rx#"[.]pdf$" (path->bytes filename))]
        [else (regexp-match #rx"[.]pdf$" filename)]))
    (define % (if is-pdf? pdf-dc% post-script-dc%))
    (parameterize ([current-ps-setup ps-setup])
      (make-object % #f #f))))
> (define (save-as-ps/pdf mk-pict filename) 
  (let ([ps/pdf-dc (make-ps/pdf-dc filename)])
    (parameterize ([dc-for-text-size ps/pdf-dc])
      (send ps/pdf-dc start-doc "x")
      (send ps/pdf-dc start-page)
      (draw-pict (mk-pict) ps/pdf-dc 0 0)
      (send ps/pdf-dc end-page)
      (send ps/pdf-dc end-doc))))
|#
;(render-reduction-relation sym->)
