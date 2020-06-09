#lang racket
;;;; Copyright Johan Sjölén 2020
(require redex
         "rel-wasm.rkt"
         "smt-discharge.rkt")

(define const-cond
  (term
   ((ϵ (glob) (func))
      ()
      ((pair
        (seq
         (const i32 0)
         (if (-> () (i32))
             ((const i32 1) ϵ)
             else
             ((const i32 2) ϵ)))
        (seq
         (const i32 2)))
       ϵ)
      ϵ)))

(define program-equivalent?
  '((assert (not (= final-left-heap final-right-heap)))
    (assert (not (= left-stack-0 right-stack-0)))))

(define program-equivalent?2
  '((assert (not (= final-left-heap final-right-heap)))
    (assert (not (= left-glob-var-1 right-glob-var-1)))))

(define loop-equiv
  (term
   ((ϵ (glob (const i32 0) (const i32 0)) (func))
    ()
    ((pair
      (seq
       (const i32 3)
       (set-global 0)
       (block (-> () ())
              (seq
               (loop (-> () ())
                     (seq
                      (const i32 3)
                      (get-global 0)
                      (sub i32)
                      (if (-> () ())
                          (seq
                           (const i32 1)
                           (get-global 1)
                           (add i32)
                           (set-global 1))
                          else
                          (seq
                           (br 2)))
                      (br 0))))))
      (seq
       (const i32 3)
       (get-global 1)
       (add i32)
       (set-global 1))
      )
     ϵ)
    ϵ)))
