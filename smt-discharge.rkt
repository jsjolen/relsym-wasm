#lang racket
;;;; Copyright Johan Sjölén 2020
#|
TODO:
Gotta write separate left/right globals and locals!!!
|#

(require racket/match racket/system uuid
         "rel-wasm.rkt"
         redex
         (only-in srfi/13
                  string-contains))
(provide (all-defined-out))
(define i 0)

(define current-smt-call-count (make-parameter 0))
(define current-smt-time-elapsed (make-parameter 0))

(define (smt-discharge smt-code [path #f])
  (let* ([tmp-dir (find-system-path 'temp-dir)]
        [fname (uuid-string)]
        [final-path (or path (build-path tmp-dir fname))])
    (with-output-to-file
       final-path 
      (lambda ()
        (map displayln smt-code)
        (displayln "(check-sat)")
        (displayln "(get-model)")))
    final-path))

(define (smt-discharge-string smt-code [path #f])
  (let* ([tmp-dir (find-system-path 'temp-dir)]
        [fname (uuid-string)]
        [final-path (or path (build-path tmp-dir fname))])
    (with-output-to-string
      (lambda ()
        (map displayln smt-code)
        (displayln "(check-sat)")
        (displayln "(get-model)")))))

(define (make-fresh prefix)
  (let ([id -1])
    (lambda ()
      (set! id (+ id 1))
      (string->symbol (format "~a-~a" prefix id)))))

(define (make-fresh-global-var (prefix ""))
  (make-fresh (format "~a-glob-var" prefix)))
(define (make-fresh-local-var)
  (make-fresh "local-var"))

(define (smt-compile smt-asserts symbolic-variables store locals v* pc)
  (let ([fresh-left-global-var (make-fresh-global-var "left")]
        [fresh-right-global-var (make-fresh-global-var "right")]
        [fresh-local-var (make-fresh-local-var)]
        [globals-left (second (term (project-store ,store 1)))]
        [globals-right (second (term (project-store ,store 2)))])
    (let-values ([(selects left-final-heap right-final-heap) (compile-heap store)])
      (append
       '((declare-const left-heap (Array Int Int))
         (declare-const right-heap (Array Int Int)))
       (map (λ (x)
              `(declare-const ,x Int))
            symbolic-variables)
       (foldl append '()
              (map (λ (v) (compile-v v (fresh-left-global-var)))
                   (rest globals-left))) ; (glob v_0 ... v)
       (foldl append '()
              (map (λ (v) (compile-v v (fresh-right-global-var)))
                   (rest globals-right)))
       (foldl append '()
              (map (λ (v) (compile-v v (fresh-local-var)))
                   locals)) ; (v_0 ... v)
       selects
       (list '(declare-const final-left-heap (Array Int Int))
             `(assert (= final-left-heap ,left-final-heap))
             '(declare-const final-right-heap (Array Int Int))
             `(assert (= final-right-heap ,right-final-heap)))
       (compile-v* v* 0 "left-stack")
       (compile-pc pc)
       smt-asserts))))


(define (v*-height v*)
  (match v*
    ['ϵ 0]
    [(list v v*)
     (+ 1 (v*-height v*))]))

(define (compile-v* v* [stack-offset 0] [prefix "stack"])
  (match v*
    ['ϵ '()]
    [(list (list 'pair left right) v*)
     (let ([height (v*-height left)])
       (append
        (compile-v* left stack-offset "left-stack")
        (compile-v* right stack-offset "right-stack")
        (compile-v* v* (+ stack-offset height) prefix)))]
    [(list (list 'const any c) v*)
     (let ([v (list 'const any c)])
       (append
        (compile-v v (string->symbol (format "~a-~a" prefix stack-offset)))
        (compile-v* v* (+ stack-offset 1) prefix)))]
    [_ '()]))

(define (compile-v v var)
  (match v
    [(list 'const any c)
     (list
      `(declare-const ,var Int)
      `(assert (= ,var ,(compile-c c))))]
    [_ (list)]))

; (c ::= .... sym-var (binop c c) (relop c c) (mod c c))
;  (binop-i  ::= add sub mul and or xor)
;  (testop-i ::= eqz)
;  (relop-i  ::= eq ne lt-s gt-s le-s ge-s)
(define (compile-c c)
  (match c
    [(list 'add c1 c2)
     (list '+ (compile-c c1) (compile-c c2))]
    [(list 'sub c1 c2)
     (list '- (compile-c c1) (compile-c c2))]
    [(list 'mul c1 c2)
     (list '* (compile-c c1) (compile-c c2))]
    ; and or xor missing
    [(list 'eq c1 c2)
     (list '= (compile-c c1) (compile-c c2))]
    [(list 'ne c1 c2)
     (list 'not (list '= (compile-c c1) (compile-c c2)))]
    [(list 'lt-s c1 c2)
     (list '< (compile-c c1) (compile-c c2))]
    [(list 'gt-s c1 c2)
     (list '> (compile-c c1) (compile-c c2))]
    [(list 'le-s c1 c2)
     (list '<= (compile-c c1) (compile-c c2))]
    [(list 'ge-s c1 c2)
     (list '>= (compile-c c1) (compile-c c2))]
    [(list op a b)
     (list op (compile-c a) (compile-c b))]
    [any any]))

(define (reduce-ϵ f ϵ initial-value)
  (if (equal? 'ϵ ϵ)
      initial-value ; now reduced value
      (reduce-ϵ f (second ϵ) (f (first ϵ) initial-value))))
; Need to invert heap assertions since they're inserted in temporal reverse-order

(define (invert-heap heap)
  (match heap
    ['ϵ heap]
    [(list fst ϵ)
     (list fst ϵ)]
    [(list fst snd)
     (list (invert-heap snd) fst)]))

(define (invert-heap% heap)
  (match heap
    ['ϵ heap]
    [(list _ 'ϵ) (first heap)]
    [(list fst snd)
     (list (invert-heap% snd) fst)]))

(define (compile-pc pc)
  (reduce-ϵ (lambda (c reduction)
              (cons `(assert ,(compile-c c))
                    reduction))
            pc
            '()))

(define (compile-heap store)
  (match store
    [(list 'pair (list left-heap _ _) (list right-heap _ _))
     (match-let
         ([(list left-selects left-heap) (compile-heap% (invert-heap left-heap) 'left-heap)]
          [(list right-selects right-heap) (compile-heap% (invert-heap right-heap) 'right-heap)])
       (values (append left-selects right-selects) left-heap right-heap))]
    [(list heap _ _)
     (match-let ([(list s h)
                   (compile-heap% (invert-heap heap))])
       (values s h h))]))

(define (compile-heap% ordered-operations [heap-symbol 'left-heap])
  (reduce-ϵ
   (lambda (term reduction)
     (match term
       [(list 'store from to)
        (list (first reduction) (list 'store (second reduction) (compile-c from) (compile-c to)))]
       [(list 'select from to)
        (list
         (cons `(assert (= (select ,(second reduction) ,(compile-c from))
                           ,(compile-c to)))
               (first reduction))
         (second reduction))]
       [_ reduction]))
   ordered-operations
   `(() ,heap-symbol))) ; Select x Current-Store


(define (symbolic-variables configuration)
  (remove-duplicates
   (filter (lambda (x) (redex-match rel-sym-wasm sym-var x))
           (flatten configuration))))

(define (c-store c) (first c))
(define (c-heap c) (first (c-store c)))
(define (c-globs c) (second (c-store c)))
(define (c-locals c) (second c))
(define (c-pc c) (last c))
(define (c-e* c) (third c))

(define (sat? conf smt-asserts)
  (let* ([smt-code
          (smt-compile smt-asserts
                       (symbolic-variables conf)
                       (c-store conf) ; Store
                       (c-locals conf) ; Local frame
                       (c-e* conf)
                       (c-pc conf))] ; PC
         [smt-code (smt-discharge-string
                                  smt-code)]
         [output-string
          (let-values ([(v  cpu user gc)
                        (time-apply 
                         (lambda ()
                           (with-output-to-string
                             (lambda ()
                               (with-input-from-string smt-code
                                 (lambda ()
                                   (system*
                                    "/usr/bin/z3"
                                    "-smt2"
                                    "-in")))))) null)])
            (current-smt-time-elapsed (+ (current-smt-time-elapsed) user))
            (apply values v))])
    (current-smt-call-count (+ 1 (current-smt-call-count)))
    (if (string-prefix? output-string "(error")
        (begin
          ;(displayln smt-code)
          ;(displayln output-string)
          (raise  (format "Error in SMT")))
        '())
    (values (not (string-prefix? output-string "unsat"))
            (if (string-prefix? output-string "sat")
                (read 
                   (open-input-string
                    (substring output-string (string-contains output-string "\n"))))
                output-string)
            smt-code)))

(define (ensure-first lst)
  (if (null? lst)
      'ϵ
      (first lst)))

(define call-count 0)

(define ctime (current-inexact-milliseconds))
(define last-step 0)
(define iter 0)

(define (length+ conf*)
  (reduce-ϵ
   (lambda (c accum)
     (+ 1 accum))
   conf*
   0))
(define (sat-step1 conf*)
  (set! call-count (+ call-count 1))
  (display call-count) (display " ")
  ;(display (reduce-ϵ (lambda (x red) (+ 1 red)) conf* 0))
  (let* ([ret (ensure-first (apply-reduction-relation/tag-with-names rel-> conf*))]
        [rule (first ret)]
        [conf*% (second ret)]
        [in-if? (or #t (> (length+ conf*%) (length+ conf*)))])
    (reduce-ϵ  
     (lambda (conf accum)
       (match-let ([(list final-confs acc-conf*) accum])
         (let ([final? (term (final? ,conf))])
           (if in-if?
               (begin
                 ;(displayln (format "~a IF(~a/sec)" iter (/ (- call-count last-step)
                 ;                                   (/ (- (current-inexact-milliseconds)
                 ;                                        ctime) 1000))))
                 ;(set! last-step call-count)
                 ;(set! iter (+ iter 1))
                 ;(set! ctime (current-inexact-milliseconds))
                 (let-values ([(b _ __)  (sat? conf '())])
                   (match (list final? b)
                     [(list #t #f)
                      ; (cons conf final-confs)
                      (list final-confs acc-conf*)]
                     [(list #t #t)
                      (list (cons conf final-confs) acc-conf*)]
                     [(list #f #t)
                      (list final-confs (list conf acc-conf*))]
                     [(list #f #f)
                      (list final-confs acc-conf*)])))
               (if final?
                   (list (cons conf final-confs) acc-conf*)
                   (list final-confs (list conf acc-conf*)))))))
     conf*% (list '() 'ϵ))))

(define (sat-fixpoint conf*)
  (let loop ([final-confs '()]
             [conf* conf*])
    (match  (sat-step1 conf*)
      [(list new-final-confs 'ϵ)
       (append final-confs new-final-confs)]
      [(list new-final-confs next-conf*)
       (loop (append final-confs new-final-confs)
             next-conf*)])))

(define (sat-map confs smt-code)
  (map
     (lambda (conf)
       (call-with-values
        (lambda ()
          (sat? conf smt-code))
        (lambda (satisfied? y z)
          (list satisfied? y))))
     confs))

(define (test-conf pre conf post #:test-count [test-count 50])
  (parameterize ([current-smt-call-count 0]
                 [current-smt-time-elapsed 0])
    (let ([test-program
           (lambda ()
             (let-values ([(satisfied? _ __)  (sat? conf pre)])
               (displayln satisfied?)
               (if satisfied?
                   (let ([final-confs (sat-fixpoint (list conf 'ϵ))])
                     (values (sat-map final-confs
                                      post)
                             (current-smt-call-count)
                             (current-smt-time-elapsed)))
                   (values '()
                           (current-smt-call-count)
                           (current-smt-time-elapsed)))))])
      (time
       (let loop [(cnt 0)] ; where's dotimes at yooo?
         (if (< (+ 1 cnt) test-count)
             (begin
               (test-program)
               (displayln (current-smt-call-count))
               (loop (+ 1 cnt)))
             (test-program)
             ))))))
