#lang racket
;;;; Copyright Johan Sjölén 2020
(require redex
         (except-in "ct-wasm.rkt"
                    L))

(define-metafunction ct-wasm
  L : conf -> observation
  [(L (s (in-hole E ((const i32 c) (if then e*_thn else e*_els) e*)) pc))
   (if (const i32 c))]
  [(L (s (in-hole E ((const i32 k) (load i32 o) e*))))
   (load (+ o k))])
