#lang racket
;;;; Copyright Johan Sjölén 2020
(require 
  "simple-wasm.rkt"
  "smt-discharge.rkt"
  "sym-wasm.rkt"
  "wasm-pict.rkt"
  redex)

(define add1
   (term
    ((ϵ
      (glob)
      (func))
     ()
     (seq
      (const i32 0)
      (const i32 5)
      (const i32 6)
      (add i32)
      (store i32 0 0)
      (const i32 0)
      (load i32 0 0)
      (const i32 1)
      (const i32 2)
      (store i32 0 0)
      (const i32 0)
      (load i32 0 0)
      (if (-> () ()) (unreachable ϵ) else (unreachable ϵ)))
     ϵ)))

(define unreachable
   (term
    ((ϵ
      (glob)
      (func))
     ()
     (seq
      (const i32 1)
      (if (-> () ())
          (unreachable ϵ)
          else
          ((const i32 2)
           ((add i32) ϵ))))
     ϵ)))

(define reachable
  (term
   ((ϵ
     (glob)
     (func))
    ()
    (seq
     (const i32 0)
     (const i32 0)
     (if (-> (i32) ())
         (unreachable
          ((const i32 2) ϵ))
         else
         ((const i32 2)
           ((const i32 2)
            ((add i32)
             ((store i32 0 0) ϵ))))))
    ϵ)))
(define f-re-post ; Gotta add the not yourself.
  '(assert (not (= (select final-heap 0) 4))))

(define break
  (term
   ((ϵ
     (generate-term sym-wasm-lang globs #:i-th 100)
     (func))
    ()
    (seq
     (block
      (-> () (i32))
      (seq
       (const i32 0)
       (const i32 0)
       (add i32)
       (br 0))))
    ϵ)))

(define loop
  (term
   ((ϵ
     (glob)
     (func))
    ()
    (seq
     (loop (-> () ())
      (nop
       ((br 0)
        ϵ))))
    ϵ)))

(define loop?
  (term
   ((ϵ
     (glob)
     (func))
    ()
    (seq
     (loop (-> () ())
       (seq
         (const i32 0)
          (if (-> () ())
           ((br 1) ϵ)
           else
           (nop ϵ)))))
    ϵ)))

(define globals
  (term
   ((ϵ
     (glob (const i32 0))
     (func))
    ()
   (seq
    (const i32 2)
    (set-global 0)
    (get-global 0)
    drop)
   ϵ)))

(define local-step
  (term
   ((ϵ
     (glob)
     (func))
    ()
    (seq
     (local 0 {1 ((const i32 1) (const i32 2) (const i32 3))}
                        ((get-local 1)
                         (drop
                          (return ϵ)))))
    ϵ)))
;(traces symV-> (list local-step 'ϵ))

(define funcs
  (term
   ((ϵ
     (glob)
     (func ((inst 0)
            (code
             (func (-> () ())
                   local ()
                   (seq
                    (const i32 0)
                    (const i32 0)
                    (add i32)
                    drop
                    return))))))
    ()
    (seq
     (call 0))
    ϵ)))

(define factorial-iter
  (term
   ((ϵ
     (glob)
     (func ((inst 0)
            (code
             (func (-> (i32) (i32))
                   local (i32) ; accum
                   (seq
                    (const i32 1)
                    (set-local 1)
                    (loop
                     (-> () ())
                     (seq
                      (get-local 0)
                      (if (-> () ())
                          (seq
                           (get-local 0)
                           (get-local 1)
                           (mul i32)
                           (set-local 1)
                           (get-local 0)
                           (const i32 1)
                           (sub i32)
                           (set-local 0)
                           (br 1))
                          else
                          (seq
                           (get-local 1)
                           return))))
                    (get-local 1)))))))
    ()
    (seq
     (const i32 6)
     (call 0)
     drop)
    ϵ)))

(define factorial-recur
  (term
   ((ϵ
     (glob)
     (func ((inst 0)
            (code
             (func (-> (i32 i32) (i32)) ; [n accum] -> [n!]
                   local ()
                   (seq
                    (get-local 0)
                    (if (-> () ())
                        (seq
                         (get-local 0)
                         (get-local 1)
                         (mul i32)
                         (set-local 1)
                         (get-local 0)
                         (const i32 1)
                         (sub i32)
                         (set-local 0)
                         ; Set up recur. call
                         (get-local 0)
                         (get-local 1)
                         (call 0)
                         return)
                        else
                        (seq
                         (get-local 1)
                         return))))))))
    ()
    (seq
     (const i32 6)
     (const i32 1)
     (call 0)
     drop)
    ϵ)))

(define rec-loop
  (term
   ((ϵ
     (glob)
     (func ((inst 0)
            (code
             (func (-> () ())
                   local ()
                   (seq
                    (call 0)))))))
    ()
    (seq
     (call 0))
    ϵ)))


;(traces symV-> (list factorial-iter 'ϵ) #:pp ppV/pict)

(term (((ϵ
         (glob)
         (func
          ((inst 0)
           (code
            (func
             (-> () ())
             local
             ()
             ((call 0) ϵ))))))
        ()
        ((local
           0
           (0 ())
           ((block
             (-> () ())
             ((call 0) ϵ))
            ϵ))
         ϵ)
        ϵ)
       ϵ))

(term
 ((
   (ϵ (glob) (func ((inst 0) (code (func (-> () ()) local () ((call 0) ϵ))))))
   ()
   ((label 0 (ϵ) ((call 0) ϵ)) ϵ)
   ϵ)
  ϵ))
