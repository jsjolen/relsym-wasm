#lang racket
;;;; Copyright Johan Sjölén 2020
(require redex
         "sym-wasm-small.rkt"
         pict)

(provide (all-defined-out))

(define-extended-language rel-sym-wasm sym-wasm
  (rel.e* ::= (pair e* e*))
  (rel.mem ::=
     (pair mem
           mem))
  (Config ::= (rel.mem ; rel.e* ; pc)))
