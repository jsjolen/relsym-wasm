#lang racket

(require "salsa20.rkt")

;;;; Copyright Johan Sjölén 2020

(define (linearize c)
  (match c
    ; (s32.store offset=40 (get_local $o) (s32.add (get_local $x10) (get_local $j10)))
    [(list op (? symbol? a) (? list? b) (? list? c))
     (append
      (linearize b)
      (linearize c)
      (list (list op a)))]
    [(list op (? list? a) (? list? b))
     (append
      (linearize a)
      (linearize b)
      (list (list op)))]
    [(list op (? list? a) (? symbol? b))
     (append
      (linearize a)
      (list op b))]
    [(list op (? symbol? a) (? list? b))
     (append
      (linearize b)
      (list (list op a)))]
    [(list op (? list? a))
     (append
      (linearize a)
      (list (list op)))]
    [v (list v)]))

#|
(define (linearize c)
  (if (list? (last c))
      (append
       (linearize (last c))
       (list (drop-right c 1)))
      (list c)))
|#

(define (trans-op sym)
  (let ([str (string-replace (symbol->string sym) "_" "-")])
    (if (regexp-match #rx"(s|i)32.[a-z\\_]+" str)
        (reverse
         (map string->symbol (string-split str ".")))
        (list (string->symbol str)))))
(define (trans-instr instr)
  (append
   (trans-op (first instr))
   (foldl append '()
          (map (lambda (s)
                 (if (and (symbol? s) (regexp-match #rx"offset=[0-9]*" (symbol->string s)))
                     (list 0 (string->number (last (string-split (symbol->string s) "="))))
                     (list s)))
               (rest instr)))))

(define vars
  `($o $p $k $c
       ,@(map (lambda (x) (string->symbol (format "$x~a" x)))
              (stream->list (in-range 0 16)))
       ,@(map (lambda (x) (string->symbol (format "$j~a" x)))
              (stream->list (in-range 0 16)))
       $i))
(define var->i
  (let ([ht (make-hash
              (let ([i 0])
                (map
                 (lambda (x)
                   (begin0 (cons x i)
                     (set! i (+ i 1))))
                 vars)))])
    (lambda (x)
      (hash-ref ht x))))

; :-(
(define (subst-if new pred tree)
  (map
   (lambda (x)
     (if (pred x)
         (new x)
         (if (list? x)
             (subst-if new pred x)
             x)))
   tree))

(define
  salsa20
  (subst-if
   var->i
   (lambda (x)
     (and (symbol? x) (member x vars)))
   (subst-if
    (lambda (x) 'i32)
    (lambda (x)
      (and (symbol? x) (eq? x 's32)))
    `(func (-> (i32 i32 i32 i32) ())
           ; $o  $p  $k  $c [0,3]
           local (i32 i32 i32 i32
                      ; $x0 ... x15  [4,19]
                      i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32
                      ; $j0 ... $j15 [20, 35]
                      i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32
                      ; $i [36]
                      i32
                      )
           (seq
            ; Stack: [$o $p $k $c]
            ; Load in $o $p $k $c
            (set-local 3)
            (set-local 2)
            (set-local 1)
            (set-local 0)
            ; Stack: []
            ,@(map trans-instr
                   (foldl append '()
                          (map linearize set-local-block)))

            (block (-> () ())
                   (seq
                    (loop (-> () ())
                          (seq
                           (const i32 20)
                           (get-local $i)
                           (eq i32)
                           (if (-> () ())
                               (seq (br 1))
                               else
                               ϵ)
                           ,@(map trans-instr 
                                  (foldl append '()
                                         (map linearize loop-body)))

                           (const i32 2)
                           (get-local $i)
                           (add i32)
                           (set-local $i)
                           (br 0)
                           ))))
            ,@(map trans-instr
                   (foldl append '()
                          (map linearize post-loop)))
            )))))
