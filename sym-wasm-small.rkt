#lang racket
;;;; Copyright Johan Sjölén 2020
(require redex
         "small-wasm.rkt"
         pict)

(provide (all-defined-out))

(define-extended-language sym-wasm simple-wasm-lang
  (sym-var ::= (variable-prefix sym-var))
  (c ::= .... sym-var constr-expr)
  (constr-expr ::=
             ArithmeticExpr)
  (constr ::=
          false
          (= constr-expr constr-expr) (< constr-expr constr-expr) (> constr-expr constr-expr) 
          (select constr-expr constr-expr) (store constr-expr constr-expr))
  (pc ::= ϵ (constr pc)) ; Alias
  
  (meminst ::= (select c c) (store c c))
  (mem ::= ϵ
       (meminst mem))
  
  (Config ::= (mem ; e* ; pc))
  (Config* ::= ϵ (Config Config*))
  (EE ::= hole
      ((mem ; v* ; pc) EE)))

(define-metafunction sym-wasm
  translate-op : op -> op)

(define ->
  (reduction-relation
   sym-wasm
   #:domain Config*
   #:codomain Config*
   (--> (in-hole EE ({mem ; (in-hole E ((i32.const c_0) (i32.const c_1) op_0 e*)) ; pc} Config*))
        (in-hole EE ({mem ; (in-hole E ((i32.const sym-var) e*)) 
                          ((= sym-var (op c_0 c_1)) pc)} Config*))
        (fresh sym-var)
        (where op (translate-op op_0))
        "Operation")
   
   (--> (in-hole EE ({mem ; (in-hole E ((i32.const c) (i32.load offset) e*)) ; pc} Config*))
        (in-hole EE ({(meminst mem) ; (in-hole E ((i32.const sym-var-loaded) e*)) ; (constr_nooverlap pc)} Config*))
        (fresh sym-var-loaded)
        (where meminst (select (+ offset c) sym-var-loaded))
        (where constr_nooverlap (= (mod (+ o c) 4) 0))
        "Load")
   
   (--> (in-hole EE ({mem ; (in-hole E   ((i32.const c_0) (i32.const c_1) (i32.store offset) e*)) ; pc} Config*))
        (in-hole EE ({(meminst mem) ; (in-hole E e*) ; (constr_nooverlap pc)} Config*))
        (where meminst (store (+ offset c_0) c_1))
        (where constr_nooverlap (= (mod (+ offset c_0) 4) 0))
        "Store")
   (-->
    (in-hole EE ({mem ; (in-hole E ((const t c) (if then tf e*_thn else e*_els) e*)) ;  pc} Config*))
    (in-hole EE ({mem ;  (in-hole E ((block tf e*_els) e*)) ; (,(= (term c) (term 0)) pc)}
                 {mem ;  (in-hole E ((block tf e*_thn) e*)) ; (,(/= (term c) (term 0)) pc)}
                 Config*))
    "If")))

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
         ['=
          (lambda (lws)
            (define lhs (list-ref lws 2))
            (define rhs (list-ref lws 3))
            (list "" lhs " = " rhs "") )]
         ['>
          (lambda (lws)
            (define lhs (list-ref lws 2))
            (define rhs (list-ref lws 3))
            (list "" lhs " > " rhs "") )]
         ['+
          (lambda (lws)
            (define lhs (list-ref lws 2))
            (define rhs (list-ref lws 3))
            (list "" lhs " + " rhs "") )]
         ['mod
          (lambda (lws)
            (define lhs (list-ref lws 2))
            (define rhs (list-ref lws 3))
            (list "(" lhs ")(mod " rhs ")") )]
         ['length
          (lambda (lws)
            (define lhs (list-ref lws 2))
            (list "‖" lhs "‖") )]
         ['op
          (lambda (lws)
            (define lhs (list-ref lws 2))
            (define rhs (list-ref lws 3))
            (list "" lhs " op " rhs "") )])
      (render-reduction-relation -> "../report/assets/small-sym-sem.ps"))))
