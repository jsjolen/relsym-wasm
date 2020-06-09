#lang racket
;;;; Copyright Johan Sjölén 2020
(require redex
         "rel-wasm.rkt"
         "smt-discharge.rkt")

;; ================= NON-INTERFERENCE =================
#|
Non-interference:
BeckertUlbrich
{ ((s_1, s_1'),(s_2, s_2')) | if s_1(l) = s_2(l) then s_1'(l) = s_2'(l)}

|#

(define pwd-ni-fun
  (term
   (((((select 0 sym-var-pwd) ϵ) (glob)
                                  (func ((inst 0)
                                         (code
                                          (func (-> (i32) (i32))
                                                local ()
                                                (seq
                                                 (const i32 0) ; heap[0]
                                                 (load i32 0 0)
                                                 (get-local 0)
                                                 (eq i32) ; heap[0] == input-pwd
                                                 return
                                                 ))))))
      ()
      (seq
       (const i32 1)
       (pair (-> (i32) (i32))
               (seq
                (call 0))
               (seq
                (call 0))))
      ϵ)
     ϵ)))
(define pwd-ni
  (term
   (((((select 0 sym-var-pwd) ϵ) (glob)
                                 (func))
     ()
     (seq
      (const i32 1)
      (pair (-> (i32) (i32))
            (seq
             (const i32 0) ; heap[0]
             (load i32 0 0)
             (eq i32) ; heap[0] == input-pwd
             )
            (seq
             (const i32 0) ; heap[0]
             (load i32 0 0)
             (eq i32) ; heap[0] == input-pwd
             )))
     ϵ)
    ϵ)))

; { ((s_1, s_1'),(s_2, s_2')) | if s_1(l) = s_2(l) then s_1'(l) = s_2'(l)}



(define post-condition
  '((assert
     (forall ((x Int))
             (=> (and (< x 65) (> x 3))
                 (and
                  (= (select left-heap x) 0)
                  (= (select right-heap x) 0)))))
    (assert
  (or (not (= (select final-left-heap 4) (select final-right-heap 4)))
      (not (= (select final-left-heap 5) (select final-right-heap 5)))
      (not (= (select final-left-heap 6) (select final-right-heap 6)))
      (not (= (select final-left-heap 7) (select final-right-heap 7)))
      (not (= (select final-left-heap 8) (select final-right-heap 8)))
      (not (= (select final-left-heap 9) (select final-right-heap 9)))
      (not (= (select final-left-heap 10) (select final-right-heap 10)))
      (not (= (select final-left-heap 11) (select final-right-heap 11)))
      (not (= (select final-left-heap 12) (select final-right-heap 12)))
      (not (= (select final-left-heap 13) (select final-right-heap 13)))
      (not (= (select final-left-heap 14) (select final-right-heap 14)))
      (not (= (select final-left-heap 15) (select final-right-heap 15)))
      (not (= (select final-left-heap 16) (select final-right-heap 16)))
      (not (= (select final-left-heap 17) (select final-right-heap 17)))
      (not (= (select final-left-heap 18) (select final-right-heap 18)))
      (not (= (select final-left-heap 19) (select final-right-heap 19)))
      (not (= (select final-left-heap 20) (select final-right-heap 20)))
      (not (= (select final-left-heap 21) (select final-right-heap 21)))
      (not (= (select final-left-heap 22) (select final-right-heap 22)))
      (not (= (select final-left-heap 23) (select final-right-heap 23)))
      (not (= (select final-left-heap 24) (select final-right-heap 24)))
      (not (= (select final-left-heap 25) (select final-right-heap 25)))
      (not (= (select final-left-heap 26) (select final-right-heap 26)))
      (not (= (select final-left-heap 27) (select final-right-heap 27)))
      (not (= (select final-left-heap 28) (select final-right-heap 28)))
      (not (= (select final-left-heap 29) (select final-right-heap 29)))
      (not (= (select final-left-heap 30) (select final-right-heap 30)))
      (not (= (select final-left-heap 31) (select final-right-heap 31)))
      (not (= (select final-left-heap 32) (select final-right-heap 32)))
      (not (= (select final-left-heap 33) (select final-right-heap 33)))
      (not (= (select final-left-heap 34) (select final-right-heap 34)))
      (not (= (select final-left-heap 35) (select final-right-heap 35)))
      (not (= (select final-left-heap 36) (select final-right-heap 36)))
      (not (= (select final-left-heap 37) (select final-right-heap 37)))
      (not (= (select final-left-heap 38) (select final-right-heap 38)))
      (not (= (select final-left-heap 39) (select final-right-heap 39)))
      (not (= (select final-left-heap 40) (select final-right-heap 40)))
      (not (= (select final-left-heap 41) (select final-right-heap 41)))
      (not (= (select final-left-heap 42) (select final-right-heap 42)))
      (not (= (select final-left-heap 43) (select final-right-heap 43)))
      (not (= (select final-left-heap 44) (select final-right-heap 44)))
      (not (= (select final-left-heap 45) (select final-right-heap 45)))
      (not (= (select final-left-heap 46) (select final-right-heap 46)))
      (not (= (select final-left-heap 47) (select final-right-heap 47)))
      (not (= (select final-left-heap 48) (select final-right-heap 48)))
      (not (= (select final-left-heap 49) (select final-right-heap 49)))
      (not (= (select final-left-heap 50) (select final-right-heap 50)))
      (not (= (select final-left-heap 51) (select final-right-heap 51)))
      (not (= (select final-left-heap 52) (select final-right-heap 52)))
      (not (= (select final-left-heap 53) (select final-right-heap 53)))
      (not (= (select final-left-heap 54) (select final-right-heap 54)))
      (not (= (select final-left-heap 55) (select final-right-heap 55)))
      (not (= (select final-left-heap 56) (select final-right-heap 56)))
      (not (= (select final-left-heap 57) (select final-right-heap 57)))
      (not (= (select final-left-heap 58) (select final-right-heap 58)))
      (not (= (select final-left-heap 59) (select final-right-heap 59)))
      (not (= (select final-left-heap 60) (select final-right-heap 60)))
      (not (= (select final-left-heap 61) (select final-right-heap 61)))
      (not (= (select final-left-heap 62) (select final-right-heap 62)))
      (not (= (select final-left-heap 63) (select final-right-heap 63)))
      (not (= (select final-left-heap 64) (select final-right-heap 64)))))))


;;; Non-interference 1
(define n-inter1-term
  (term
   (seq
    (const i32 8)
    (const i32 5)
    (const i32 0)
    (load i32 0 0)
    (add i32)
    (store i32 0 0))))

(define n-inter1-conf
  (term ( (ϵ (glob) (func))
          ()
          ((pair (-> () ())
                 ,n-inter1-term
                 ,n-inter1-term)
           ϵ)
          ϵ)))

(define n-inter2-term
  (term
   (seq
    (const i32 4)
    (const i32 5)
    (const i32 0)
    (load i32 0 0)
    (add i32)
    (const i32 sym-var-unknown)
    (if (-> (i32 i32) ())
        ((add i32) ϵ)
        else
        ((store i32 0 0) ϵ))
)))
(define n-inter2-conf
  (term ( (ϵ (glob) (func))
          ()
          ((pair (-> () ())
                 ,n-inter2-term
                 ,n-inter2-term)
           ϵ)
          ϵ)))

(define n-inter4-term
  (term
   (seq
    (const i32 0)
    (load i32 0 0)
    (const i32 0)
    (eq i32)
    (if (-> () ())
        (seq
         (const i32 4)
         (const i32 0)
         (load i32 0 0)
         (const i32 4)
         (load i32 0 0)
         (add i32)
         (store i32 0 0))
        else ϵ))))
(define n-inter4-conf
  (term ((ϵ (glob) (func))
         ()
         ((pair (-> () ())
                ,n-inter4-term
                ,n-inter4-term)
          ϵ)
         ϵ)))


#|
Express some complicated control flow
Have assignments be tainted and see the flow of colored assignments.
x := H[0]
for i from 0 to 10:
  for j from 0 to 10:
    y := i*H[0]
    if j == 5:
        br 2 ; for i... Inner break!

(define n-inter3-term
  (term
   (seq
    (const i32 0)
    (load i32 0 0)
    (loop (-> (i32) ())
      (loop (-> (i32) ()))
      )
    )))
|#  
