#lang racket
;;;; Copyright Asumu Takikawa 2019, Johan Sjölén 2020
;; Small fragment of WebAssembly

(require redex)

(provide (all-defined-out))

;; wasm defines memory for module instances in 64Ki increments, but this is
;; unwieldly in redex so we define the increment in bytes here
(define *page-size* 20)

;; some non-terminals (like for modules) differ from the paper due to redex
;; constraints on non-terminals appearing as keywords, and some forms have
;; extra keywords for ease of identification
(define-language simple-wasm-lang
  ;; types
  (t   ::= t-i)
  (t-i ::= i32)
  
  ;; Takes lhs values off of stack and leaves rhs values on the stack after execution
  (tf  ::= (-> (t ...) (t ...)))

  (f ::=
     (func tf local (t ...) e*))
  (cl ::= {(inst i) ; Unused, which module it comes from.
           (code f)})

  ;; instructions
  (e-no-v ::= unreachable
              nop
              drop
              select
              return
              (block tf e*)
              (loop tf e*)
              (if tf e* else e*)
              (br i)
              (get-global i)
              (set-global i)
              (tee-global i)
              (get-local i)
              (set-local i)
              (tee-local i)
              (load t a o)
              (store t a o)
              current-memory
              grow-memory
              (binop-i t-i)
              (relop-i t-i))
  (e    ::= e-no-v
            (const t c))
  ;; primitive operations
  [binop    ::= binop-i]
  (binop-i  ::= add sub mul and or xor eq rotl)
  (relop    ::= relop-i)
  (relop-i  ::= eq ne lt-s gt-s le-s ge-s)

  ;; sequences of expressions
  ;; (we use this to avoid splicing)
  (e*   ::= ϵ
            (e e*))

  (c    ::= integer)

  ;; constant numerals
  ((i j l k m n a o) integer)

  ;; bytes
  (b    ::= integer)

  (m-mem  ::= (memory n)))

(define-extended-language simple-wasm-runtime-lang simple-wasm-lang
  ;; administrative expressions
  (e-no-v  ::= ....
               trap
               (label n {e*} e*)
               (local n {i (v ...)} e*) ; i unused, current module.
               (call j)
               (call cl))

  ;; runtime
  (ex     ::= (export string))

  (meminst ::= (select c c) (store c c))
  (mem ::= ϵ
       (meminst mem))
  (globs ::= (glob v ...))
  (funcs ::= (func cl ...))
  
  (F ::= (v ...))
  
  (s ::= (mem globs funcs))

  (v       ::= (const t c))
  (v*      ::= ϵ
               (v v*))

  ;; evaluation contexts
  ;; using an inductive definition instead of using sequences because
  ;; we would need splicing holes otherwise
  (E       ::= hole
               (v E)
               ((label n (e*) E) e*)))
