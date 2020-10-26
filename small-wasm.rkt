#lang racket
;;;; Copyright Johan Sjölén 2020
;; Small fragment of WebAssembly

(require redex)

(provide (all-defined-out))
(define (/= a b)
  (not (= a b)))

(define-language simple-wasm-lang


  ;; Takes lhs values off of stack and leaves rhs values on the stack after execution
  (t ::= i32)
  (tf  ::= ([t ...] → [t ...]))
  ;; instructions
  (e-no-v ::=
          (block tf e*) (loop tf e*) (if tf then e* else e*) (br i)
          (i32.load offset) (i32.store offset)
          op
          (label n {e*} e*))
  (v       ::= (i32.const c))
  (e    ::= e-no-v v)
  (mem ::= Vector<u8>)
  ;; primitive operations
  (op ::= binop-i relop-i)
  (relop-i  ::= i32.eq)
  (binop-i  ::= i32.add i32.sub i32.mul i32.and i32.or)
  ;; sequences of expressions
  ;; (we use this to avoid splicing)
  (e*   ::= ϵ
        (e e*))
  (v*      ::= ϵ
           (v v*))
  (c    ::= integer)
  (offset ::= integer)
  (Config ::= {mem ; e*})
  (FinalConfig ::= {mem ; v*})
  (E       ::= hole
           (v E)
           ((label n {e*} E) e*)))

(define-metafunction simple-wasm-lang
  do-op : op integer integer -> integer
  [(do-op a b) a])
(define-metafunction simple-wasm-lang
  load-i32 : mem integer -> integer
  [(load-i32 a b) a])
(define-metafunction simple-wasm-lang
  store-i32 : mem integer -> integer
  [(store-i32 a b) a])


;; split an eval context into two contexts:
;;   - an outer context surrounding the second
;;   - an inner context with nested values around a hole
;; precondition: E actually has l-nested values when called
(define-metafunction simple-wasm-lang
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

(define-metafunction simple-wasm-lang
  label-depth : E -> number
  [(label-depth hole)  0]
  [(label-depth (v E)) (label-depth E)]
  [(label-depth ((label n {e*_0} E) e*_1))
   ,(add1 (term (label-depth E)))])

(define ->
  (reduction-relation
   simple-wasm-lang
   #:domain Config
   #:codomain Config
   (--> {mem ; (in-hole E ((i32.const c_0) (i32.const c_1) op e*))}
        {mem ; (in-hole E ((i32.const c_2) e*))}
        (where c_2 (do-op op c_0 c_1))
        "Operation")
   
   (--> {mem ; (in-hole E ((i32.const c) (i32.load offset) e*))}
        {mem ; (in-hole E ((i32.const c_1) e*))}
        (where c_1 (load-i32 mem (+ c offset)))
        "Load")
   (--> {mem_0 ; (in-hole E   ((i32.const c_0) (i32.const c_1) (i32.store offset) e*))}
        {mem_1 ; (in-hole E e*)}
        (where mem_1 (store-i32 mem_0 ,(+ (term c_0) (term offset)) c_1))
        "Store")

   (--> {mem ; (in-hole E ((block ([t_n ...] → [t_m ...]) e*_body) e*_cont))}
        {mem ; (in-hole E_outer ((label l {ϵ} (in-hole E_v e*_body)) e*_cont))}
        (where l  ,(length (term [t_n ...])))
        (where k  ,(length (term [t_m ...])))
        (where (E_outer E_v) (v-split E k))
        "Block")

   (--> {mem ; (in-hole E ((label n {e*_cont} v*) e*_outer))}
        {mem ; (in-hole E (v* e*_outer))}
        "Label-value")
   (--> {mem ; (in-hole E ((label n {e*_cont} (in-hole E_inner ((br j) e*_1))) e*_2))}
        {mem ; (in-hole E ((in-hole E_v e*_cont) e*_2))}
        (where j (label-depth E_inner))
        (where (E_outer E_v) (v-split E_inner n))
        "Label-br")
   (--> {mem ; (in-hole E ((i32.const 0) (if tf then e*_thn else e*_els) e*))}
        {mem ; (in-hole E ((block tf e*_els) e*))}
        "If-false")
   (--> {mem ; (in-hole E ((i32.const k) (if tf then e*_thn else e*_els) e*))}
        {mem ; (in-hole E ((block tf e*_thn) e*))}
        (side-condition (/= (term k) (term 0)))
        "If-true")

   (--> {mem ; (in-hole E ((name e_loop (loop ([t_n ...] → [t_m ...]) e*_0)) e*_1))}
        {mem ; (in-hole E_outer (e_lbl e*_1))}

        (where l             ,(length (term [t_m ...])))
        (where k             ,(length (term [t_n ...])))
        (where (E_outer E_v) (v-split E k))
        (where e_lbl         (label l {e_loop} (in-hole E_v e*_0)))
        "Loop")))

(define render
  (lambda ()
    (with-compound-rewriters
        (['>=
          (lambda (lws)
            (define lhs (list-ref lws 2))
            (define rhs (list-ref lws 3))
            (list "" lhs " ≥ " rhs "") )]
         ['/=
          (lambda (lws)
            (define lhs (list-ref lws 2))
            (define rhs (list-ref lws 3))
            (list "" lhs " ≠ " rhs "") )]
         ['+
          (lambda (lws)
            (define lhs (list-ref lws 2))
            (define rhs (list-ref lws 3))
            (list "" lhs " + " rhs "") )]
         ['length
          (lambda (lws)
            (define lhs (list-ref lws 2))
            (list "‖" lhs "‖") )])
      (render-reduction-relation -> "../report/assets/branch-rules.ps"))))

(render-reduction-relation-rules '("If-false" "If-true" "Loop" "Block"))
