#lang racket
;;;; Copyright Johan Sjölén 2020
(require redex
         "rel-wasm.rkt"
         "smt-discharge.rkt"
         "wasm-pict.rkt")

(reduction-steps-cutoff 5)
(define add1
   (term
    ((ϵ
      (glob)
      (func))
     ()
     (seq
      (const i32 5)
      (const i32 6)
      (add i32))
     ϵ)))


(define add2
  (list
   (term
    ((ϵ
      (glob)
      (func))
     ()
     (seq
      (pair
       ((const i32 5)
        ((const i32 6) ϵ))
       ((const i32 5)
        ((const i32 6) ϵ)))
      (const i32 6)
      (add i32))
     ϵ))
   'ϵ))


(define block-ret
  (term (((ϵ (glob) (func)) () ((block (-> () (i32)) ((pair ((const i32 2) ϵ)
                                                            ((const i32 1) ϵ)) ϵ)) ϵ) ϵ) ϵ)))

; Expects < 6+6 | 5+6>
(define add1-pair
  (term
   (((ϵ
      (glob)
      (func))
     ()
     (seq
      (pair  (seq (const i32 5)
                  (const i32 6))
             (seq (const i32 6)
                  (const i32 5)))
      (const i32 6)
      (add i32))
     ϵ)
    ϵ)))

; Expects 5+6
(define add2-pair
  (term
   (((ϵ
      (glob)
      (func))
     ()
     (seq
      (pair  (seq
              (const i32 1)
              (const i32 5)
              (const i32 6))
             (seq
              (const i32 1)
              (const i32 6)
              (const i32 5)))
      (add i32))
     ϵ)
    ϵ)))

(define add-or-sub
  (list
   (term
    ((ϵ (glob)
        (func))
     ()
     (seq
      (const i32 1)
      (const i32 1)
      (pair (-> (i32 i32) (i32))
            ((add i32) ϵ) 
            ((sub i32) ϵ)))
     ϵ))
   'ϵ))


; Final boss lol :-)
(define factorial-pair
  (list
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
                     (get-local 1)))))
            ((inst 0)
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
      (const i32 5)
      (pair ((call 0) ϵ)
            ((call 1) ϵ)))
     ϵ))
   'ϵ))

(define pair-globals
  (list
   (term
    ((pair
      (ϵ
       (glob (const i32 0))
       (func))
      (ϵ
       (glob (const i32 2))
       (func)))
     ()
     (seq
      (const i32 2)
      (get-global 0)
      (add i32)
      (set-global 0)
      (get-global 0)
      drop)
     ϵ))
   'ϵ))

; Test suite
(define-syntax-rule (define-test name start --> result)
  (define (name)
    (test--> rel->
             (term (start ϵ))
             (term (result ϵ)))))
(define-syntax-rule (define-test* name start --> result)
  (define (name)
    (test--> rel->
             (term start)
             (term result))))

(define-test pair-s-global-load
 ((pair (ϵ (glob (const i32 1)) (func))
        (ϵ (glob (const i32 2)) (func)))
     ()
     ((const i32 0) ((get-global 0) ϵ))
     ϵ)
  -->
  ((pair
     (ϵ
      (glob (const i32 1))
      (func))
     (ϵ
      (glob (const i32 2))
      (func)))
    ()
    ((const i32 0)
     ((pair
       ((const i32 1) ϵ)
       ((const i32 2) ϵ))
      ϵ))
    ϵ))

(define-test pair-s-global-store
  ((pair (ϵ (glob (const i32 0)) (func))
           (ϵ (glob (const i32 0)) (func)))
     ()
     ((const i32 3) 
      ((set-global 0) ϵ)) 
     ϵ)
  -->
  ((pair
   (ϵ
    (glob (const i32 3))
    (func))
   (ϵ
    (glob (const i32 3))
    (func)))
  ()
  ϵ
  ϵ))

(define-test pair-v-global-store
  ((ϵ (glob (const i32 2)) (func))
   ()
   (seq
    (pair ((const i32 0) ϵ)
          ((const i32 1) ϵ))
    (set-global 0)
    )
   ϵ)
  -->
  ((pair
      (ϵ
       (glob (const i32 0))
       (func))
      (ϵ
       (glob (const i32 1))
       (func)))
     ()
     ((pair ϵ ϵ) ϵ)
     ϵ))

(define-test pair-v-global-store2
  ((pair (ϵ (glob (const i32 2)) (func))
         (ϵ (glob (const i32 2)) (func)))
   ()
   (seq
    (pair ((const i32 0) ϵ)
          ((const i32 1) ϵ))
    (set-global 0)
    )
   ϵ)
  -->
  ((pair
      (ϵ
       (glob (const i32 0))
       (func))
      (ϵ
       (glob (const i32 1))
       (func)))
     ()
     ((pair ϵ ϵ) ϵ)
     ϵ))

(define-test pair-s-load
  ((pair (ϵ (glob) (func))
         (ϵ (glob) (func)))
   ()
   ((const i32 0) ((load i32 3 0) ϵ)) ϵ)
  -->
  ((pair
   (((select
      (add 0 0)
      sym-var-loaded)
     ϵ)
    (glob)
    (func))
   (((select
      (add 0 0)
      sym-var-loaded)
     ϵ)
    (glob)
    (func)))
  ()
  ((pair
    ((const
      i32
      sym-var-loaded)
     ϵ)
    ((const
      i32
      sym-var-loaded)
     ϵ))
   ϵ)
  ((= (mod (add 0 0) 4) 0) ϵ)))

(define-test pair-s-store
  ((pair (ϵ (glob) (func))
         (ϵ (glob) (func)))
   ()
   ((const i32 0) ((const i32 0) ((store i32 0 0) ϵ)))
   ϵ)
  -->
  ((pair
      (((store (add 0 0) 0) ϵ)
       (glob)
       (func))
      (((store (add 0 0) 0) ϵ)
       (glob)
       (func)))
     ()
     ϵ
     ((= (mod (add 0 0) 4) 0) ϵ)))

(define-test pair-v-load
  ((ϵ (glob) (func))
   ()
   ((pair
     ((const i32 4) ϵ)
     ((const i32 3) ϵ))
    ((load i32 0 0) ϵ))
   ϵ)
  -->
 ((pair
   (((select
      (add 0 4)
      sym-var-loaded)
     ϵ)
    (glob)
    (func))
   (((select
      (add 0 3)
      sym-var-loaded)
     ϵ)
    (glob)
    (func)))
  ()
  ((pair
    ((const
      i32
      sym-var-loaded)
     ϵ)
    ((const
      i32
      sym-var-loaded)
     ϵ))
   ϵ)
  ((= (mod (add 0 4) 4) 0)
   ((= (mod (add 0 3) 4) 0)
    ϵ))))

(define-test k-pair-v-store
  ((pair (ϵ (glob) (func))
         (ϵ (glob) (func)))
   ()
   ((pair ((const i32 0) ((const i32 1) ϵ))
          ((const i32 0) ((const i32 2) ϵ)))
    ((const i32 4)
     ((store i32 1 0) ϵ)))
   ϵ)
  -->
((pair
   (((store (add 0 1) 4) ϵ)
    (glob)
    (func))
   (((store (add 0 2) 4) ϵ)
    (glob)
    (func)))
  ()
  ((pair
    ((const i32 0) ϵ)
    ((const i32 0) ϵ))
   ϵ)
  ((= (mod (add 0 1) 4) 0)
   ((= (mod (add 0 2) 4) 0)
    ϵ))))

(define-test c-pair-v-store
  ((pair
   (ϵ (glob) (func))
   (ϵ (glob) (func)))
  ()
  ((const i32 1)
   ((pair
     ((const i32 0)
      ((const i32 1) ϵ))
     ((const i32 0)
      ((const i32 2) ϵ)))
    ((store i32 1 0) ϵ)))
  ϵ)
  --> 
((pair
   (((store (add 0 1) 1) ϵ)
    (glob)
    (func))
   (((store (add 0 1) 2) ϵ)
    (glob)
    (func)))
  ()
  ((pair
    ((const i32 0) ϵ)
    ((const i32 0) ϵ))
   ϵ)
  ((= (mod (add 0 1) 4) 0) ϵ)))

(define-test ck-pair-v-store
((pair
   (ϵ (glob) (func))
   (ϵ (glob) (func)))
  ()
  ((pair
    ((const i32 5)
     ((const i32 1) ϵ))
    ((const i32 0)
     ((const i32 0) ϵ)))
   ((store i32 1 1) ϵ))
  ϵ)
  -->
  ((pair
   (((store (add 1 1) 5) ϵ)
    (glob)
    (func))
   (((store (add 1 0) 0) ϵ)
    (glob)
    (func)))
  ()
  ((pair ϵ ϵ) ϵ)
  ((= (mod (add 1 1) 4) 0)
   ((= (mod (add 1 0) 4) 0)
    ϵ))))

(define-test* pair-v-if
  (((pair
   (ϵ (glob) (func))
   (ϵ (glob) (func)))
  ()
  ((pair
    ((const i32 0) ϵ)
    ((const i32 2) ϵ))
   ((if (-> () ()) ϵ else ϵ)
    ϵ))
  ϵ)
 ϵ)
  -->
(((pair
   (ϵ (glob) (func))
   (ϵ (glob) (func)))
  ()
  ((pair
    ((block (-> () ()) ϵ) ϵ)
    ((block (-> () ()) ϵ) ϵ))
   ϵ)
  ((> 0 0) ((<= 2 0) ϵ)))
 (((pair
    (ϵ (glob) (func))
    (ϵ (glob) (func)))
   ()
   ((pair ϵ ϵ)
    ((block (-> () ()) ϵ) ϵ))
   ((> 0 0) ((> 2 0) ϵ)))
  (((pair
     (ϵ (glob) (func))
     (ϵ (glob) (func)))
    ()
    ((pair
      ((block (-> () ()) ϵ) ϵ)
      ((block (-> () ()) ϵ)
       ϵ))
     ϵ)
    ((> 0 0) ((<= 2 0) ϵ)))
   (((pair
      (ϵ (glob) (func))
      (ϵ (glob) (func)))
     ()
     ((pair
       ((block (-> () ()) ϵ)
        ϵ)
       ((block (-> () ()) ϵ)
        ϵ))
      ϵ)
     ((<= 0 0) ((> 2 0) ϵ)))
    ϵ)))))

(define-test pair-e-if-right
  ((pair (ϵ (glob) (func))
           (ϵ (glob) (func)))
     ()
     ((pair ((const i32 2) ((if (-> () ()) ϵ else ϵ) ϵ))
            ((const i32 5) ((const i32 6) ((add i32) ϵ)))) ϵ) ϵ)
  -->
((pair
   (ϵ (glob) (func))
   (ϵ (glob) (func)))
  ()
  ((pair
    ((const i32 2)
     ((if (-> () ()) ϵ else ϵ)
      ϵ))
    ((const i32 11) ϵ))
   ϵ)
  ϵ))

(define-test pair-e-if-left
  (((pair (ϵ (glob) (func))
          (ϵ (glob) (func)))
    ()
    ((pair ((const i32 5) ((const i32 6) ((add i32) ϵ)))
           ((const i32 2) ((if (-> () ()) ϵ else ϵ) ϵ))) ϵ)
    ϵ)
   ϵ)
  -->
  ((pair
   (ϵ (glob) (func))
   (ϵ (glob) (func)))
  ()
  ((pair
    ((const i32 11) ϵ)
    ((const i32 2)
     ((if (-> () ()) ϵ else ϵ)
      ϵ)))
   ϵ)
  ϵ))

; TODO Fix this test!
(define-test pair-if
  ((ϵ (glob) (func))
   ()
   ((pair ((const i32 0)
           ((if (-> () (i32)) ((const i32 2) ϵ) else ((const i32 1) ϵ))
            ((const i32 1) ((add i32) ϵ))))
          ((const i32 0) ((if (-> () (i32)) ((const i32 2) ϵ) else ((const i32 1) ϵ))
                          ((const i32 1) ((add i32) ϵ)))))
    ϵ)
   ϵ)
  -->
  ())


(define add-2
  (term
   (
    ((ϵ (glob) (func))
     ()
     ((const i32 sym-var-sum)
      ((pair (-> (i32) (i32))
             (seq
              (const i32 1)
              (add i32)
              (const i32 1)
              (add i32))
             (seq
              (const i32 2)
              (add i32)))
       ϵ))
     ϵ)
    ϵ)))

(define conf
  (term ((pair
          (ϵ (glob) (func))
          (ϵ (glob) (func)))
         ()
         ((pair
           ((const i32 sym-var-2)
            ϵ)
           ((const i32 sym-var-)
            ϵ))
          ϵ)
         ((=
           sym-var-2
           (add sym-var-1 1))
          ((=
            sym-var-1
            (add sym-var-sum 1))
           ((=
             sym-var-
             (add sym-var-sum 2))
            ϵ))))))
; (sat? conf '() '((assert (not (= left-stack-0 right-stack-0)))))


;; ================= NON-INTERFERENCE =================
#|
Non-interference:
BeckertUlbrich
{ ((s_1, s_1'),(s_2, s_2')) | if s_1(l) = s_2(l) then s_1'(l) = s_2'(l)}

|#

(define (sat-it pre conf final)
  (let ([conf* (first (apply-reduction-relation* rel-> conf))])
    (reduce-ϵ (lambda (conf b)
                ; we expect final to *not* hold
                (displayln conf)
                (displayln (sat? conf pre final))
                (and b (not (sat? conf pre final))))
              conf* #t)))

(define non-interference-1-byte
  (list
   ; Pre-condition
   '((assert (= (select left-heap 0)
                (select right-heap 0))))
   ; Post-condition
   ; can it find a conf where this holds? Then we messed up!
   '((assert (not (= (select final-left-heap 0)
                     (select final-right-heap 0)))))
   ))
; This interferes!
(define n-i-1-b-1
  (term
   (((ϵ (glob) (func))
     ()
     ((pair
       (seq
        (const i32 0)
        (const i32 0)
        (store i32 0 0))
       (seq
        (const i32 0)
        (const i32 1)
        (store i32 0 0)))
      ϵ)
     ϵ)
    ϵ)))
; This doesn't interfere!
(define n-i-1-b-2
  (term
   (((ϵ (glob) (func))
     ()
     ((pair
       (seq
        (const i32 0)
        (const i32 0)
        (store i32 0 0))
       (seq
        (const i32 0)
        (const i32 0)
        (store i32 0 0)))
      ϵ)
     ϵ)
    ϵ)))


; Technically breaks non-interference
#|
heap[0] = sym-var-pwd
fun check-pwd(input-pwd : uint8) : uint8 {
    if(heap[0] == input-pwd) return 1
    else return 0
}
check-pwd(1);
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

(define test-sat-step
  (term
   (( (ϵ (glob) (func))
      ()
      (seq
       (const i32 0)
       (if (-> () (i32))
           ((const i32 2) ϵ)
           else
           ((const i32 3) ϵ)))
      ϵ
      )
    ϵ)))
