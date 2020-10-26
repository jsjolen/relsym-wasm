#lang racket
;;;; Copyright Johan Sjölén 2020
(require redex
         "simple-wasm.rkt"
         "rel-wasm.rkt"
         "smt-discharge.rkt"
         racket/match racket/system uuid)
(provide (all-defined-out))

(define-extended-language ct-wasm rel-sym-wasm
  (observation ::= any)
  (observation* ::= ;any
                (observation observation*)
                ϵ
                )
  (pair-observation* ::= 
                     (observation* observation*)
                     ;any
                     )
  (obs-conf* ::=
             ((pair-observation* conf) obs-conf*)
             ϵ
             ;any
             )
  (EEE ::= hole
      ((pair-observation* conf*) EEE)))

(define-metafunction ct-wasm
  L-helper : conf integer -> observation*
  
  [(L-helper (s F ; load|ϵ
                (in-hole E ((pair (in-hole E_l
                                           ((const t c) ((load t a o) e*_l)))
                                  e*_r)
                            e*))
                pc) 1)
   ((load (+ o c)) ϵ)]
  [(L-helper (s F ; ϵ|load
                (in-hole E ((pair e*_l
                                  (in-hole E_r
                                           ((const t c) ((load t a o) e*_r))))
                            e*))
                pc) 2)
   ((load (+ o c)) ϵ)]
  [(L-helper (s F ; store|ϵ
                (in-hole E ((pair (in-hole E_l
                                           ((const t k) ((const t c) ((store t a o) e*_l))))
                                  e*_r)
                            e*))
                pc) 1)
   ((store (+ o k)) ϵ)]
  [(L-helper (s F ; ϵ|store
                (in-hole E ((pair e*_l
                                  (in-hole E_r
                                           ((const t k) ((const t c) ((store t a o) e*_r)))))
                            e*))
                pc) 2)
   ((store (+ o k)) ϵ)]

  [(L-helper (s F (in-hole E ((pair 
                             (in-hole E_l
                                      ((const t k) ((const t c) ((div t) e*_l))))
                             e*_r)
                            e*))
                pc) 1)
   (div ϵ)]

  [(L-helper (s F (in-hole E ((pair e*_l (in-hole E_r  
                                                  ((const t k) ((const t c) ((div t) e*_r)))))
                              e*))
                pc) 2)
   (div ϵ)]
  
  
  [(L-helper any_l any_r) ϵ])

(define-metafunction ct-wasm
  L : conf -> pair-observation*
  [(L (s F (in-hole E ((pair (in-hole E_l (trap e*_l))
                             (in-hole E_r (trap e*_r))) e*))
         pc)
      )
   ((trap ϵ) (trap ϵ))]
  ;; branch
  [(L (s F  (in-hole E ; if|if
                    ((pair (in-hole E_l ((const t_l c_l)
                                         ((if tf_l e*_thn-l else e*_els-l) e*_l)))
                           (in-hole E_r ((const t_r c_r)
                                         ((if tf_r e*_thn-r else e*_els-r) e*_r))))
                     e*))
         pc))
      (((const t_l c_l) ϵ) ((const t_r c_r) ϵ))]

  [(L (s F  (in-hole E; if|e*
                    ((pair (in-hole E_l ((const t_l c_l)
                                           ((if tf e*_thn else e*_els) e*_l)))
                             v*_r) e*))
         pc))
   (((const t_l c_l) ϵ) ϵ)]
  [(L (s F  (in-hole E ((pair v*_l  ; e*|if
                             (in-hole E_r ((const t_r c_r)
                                           ((if tf e*_thn else e*_els) e*_r))))
                       e*))
         pc))
   (ϵ ((const t_r c_r) ϵ))]
  ; e*|e*
  [(L (s F (in-hole E ((pair e*_l e*_r) e*)) pc))
   (observation*_l observation*_r)
   (where observation*_l (L-helper (s F (in-hole E ((pair e*_l e*_r) e*)) pc) 1))
   (where observation*_r (L-helper (s F (in-hole E ((pair e*_l e*_r) e*)) pc) 2))]
  ; any
  [(L any) (ϵ ϵ)]
  )


(define-metafunction ct-wasm
  append-observation* : observation* observation* -> observation*
  [(append-observation* ϵ observation*) observation*]
  [(append-observation* (observation_0 observation*_0) observation*_1)
   (observation_0 (append-observation* observation*_0 observation*_1))])

(define-metafunction ct-wasm
  obs-conf*-append : obs-conf* obs-conf* -> obs-conf*
  [(obs-conf*-append ϵ obs-conf*) obs-conf*]
  [(obs-conf*-append (any obs-conf*_l) obs-conf*_r)
   (any (obs-conf*-append obs-conf*_l obs-conf*_r))])

(define-metafunction ct-wasm
  apply-L : conf* pair-observation* -> obs-conf*
  [(apply-L ϵ pair-observation*) ϵ]
  [(apply-L (conf conf*) pair-observation*)
   (((merge-pair-observation* pair-observation* (L conf)) conf)
    (apply-L conf* pair-observation*))])

(define-metafunction ct-wasm
  conf...->conf* : (conf ...) -> conf*
  [(conf...->conf* ()) ϵ]
  [(conf...->conf* (conf_0 conf_1 ...))
   (conf_0 (conf...->conf* (conf_1 ...)))])

(define-metafunction ct-wasm
  apply-pair-observation* : pair-observation* conf* -> obs-conf*
  [(apply-pair-observation* pair-observation* ϵ) ϵ]
  [(apply-pair-observation* pair-observation* (conf conf*))
   ((pair-observation* conf) (apply-pair-observation* pair-observation* conf*))])

(define-metafunction ct-wasm
  merge-pair-observation* : pair-observation* pair-observation* -> pair-observation*
  [(merge-pair-observation* (observation*_l0 observation*_r0)
                            (observation*_l1 observation*_r1))
   ((append-observation* observation*_l0 observation*_l1)
    (append-observation* observation*_r0 observation*_r1))])


; For trace c_0c_1c_2...c_n collects observations
; c_0c_1c_2...c_n-1
; NOTE: L[c_n] is not collect, not an issue since a final config has no observations!
(define ct->
  (reduction-relation
   ct-wasm
   #:domain (obs-conf* obs-conf*) ; (confs finals)
   #:codomain (obs-conf* obs-conf*) ; (confs finals)
   (--> 
    ((in-hole EEE ((pair-observation* conf) obs-conf*)) obs-conf*_f)
    ((in-hole EEE (obs-conf*-append obs-conf*_1 obs-conf*))
     (obs-conf*-append obs-conf*_f obs-conf*_f1))
    (where ((conf_new-final ...) conf*_1) ,(sat-step1 (term (conf ϵ))))
    (where ((pair-observation*_1 any) ϵ)
           (apply-L (conf ϵ)
                    pair-observation*))
    ;(where #t ,(begin0 #t (displayln (term pair-observation*_1))))
    (where obs-conf*_1
          (apply-pair-observation* pair-observation*_1 conf*_1))
    (where obs-conf*_f1
           (apply-pair-observation* pair-observation*_1 (conf...->conf* (conf_new-final ...))))
    "ct-step")))

(define (apply-ct->2 conf)
  (let iter ([obs-conf* (list (list (term (ϵ ϵ)) conf) 'ϵ)]
             [final-conf* 'ϵ])
    (let*
        ([obs-conf (first obs-conf*)]
         [current-pair-obs* (first obs-conf)]
         [current-conf (second obs-conf)]
         [current-conf* (list current-conf 'ϵ)])
      (let* ([satted (sat-step1 current-conf*)]
             [final-confs (first satted)]
             [next-conf* (second satted)]
             [observe (term (apply-L ,current-conf* ,current-pair-obs*))] ; obs-conf*
             [new-obs-conf (first observe)]
             [new-pair-obs* (first new-obs-conf)]
             [next-final-obs-conf*
              (term (apply-pair-observation* ,new-pair-obs* (conf...->conf* ,final-confs)))])
        (if (and (eq? next-conf* 'ϵ) (eq? (second obs-conf*) 'ϵ))
            (term (obs-conf*-append ,next-final-obs-conf* ,final-conf*))
            (let* ([next-obs-conf* (term (apply-pair-observation* ,new-pair-obs* ,next-conf*))])
              (iter (term (obs-conf*-append ,next-obs-conf* ,(second obs-conf*)))
                    (term (obs-conf*-append ,next-final-obs-conf* ,final-conf*)))))))))

(define (apply-ct-> conf #:traces? (traces? #t))
  (let ([init (term ((((ϵ ϵ) ,conf) ϵ)  ϵ))])
    (if traces?
        (traces ct-> init)
        (first (apply-reduction-relation* ct-> init)))))

(define (obs-equiv? obs-l obs-r conf)
  (if (= (length obs-l) (length obs-r))
      (let ([stmts? (compile-obs obs-l obs-r)])
        (match stmts?
          [#f #f]
          ['() #t]
          [_
           ;(displayln (format "Smts: ~a" stmts?))
           ; Can we *not* find a counter-example? Then good!
           (let-values ([(satted? _ __) (sat? conf stmts?)])
             (displayln _)
             (not satted?))]))
      #f))

; Assumes |obs-l| = |obs-r|
(define (compile-obs obs-l obs-r) ; -> #f | Listof SMT-stmts
  (let loop
    ([obs-l obs-l]
     [obs-r obs-r]
     [stmts '()])
    (match (list obs-l obs-r)
      [(list 'ϵ 'ϵ)
       `((assert (not (and ,@stmts))))]
      [(list (list ol ol-rest) (list or or-rest))
       (match (list ol or)
         [(list (list 'const t-l v-l) (list 'const t-r v-r))
          (loop ol-rest or-rest
                ;stmts)]
                (append stmts `((= ,v-l ,v-r))))]
         [(list (list 'store x) (list 'store y))
          (loop ol-rest or-rest
                (append stmts `((= ,x ,y))))
                ;stmts)
          ]
         [(list (list 'load x) (list 'load y))
          (loop ol-rest or-rest
                (append stmts `((= ,x ,y))))
          ]
         [(list x x)
          (loop ol-rest or-rest stmts)]
         [_ #false])])))

(define (ct? conf)
  (reduce-ϵ
   (λ (obs-conf good?)
     (match obs-conf
       [(list (list obs-left obs-right) conf)
        (displayln (format "Left: ~a, Right: ~a" obs-left obs-right))
        (and good?
             (obs-equiv? obs-left obs-right conf))]))
   ;; Grab only final states
   (apply-ct->2 conf)
   #t))
