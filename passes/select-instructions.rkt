#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v7
  rackunit)

(provide select-instructions)

;; imp-cmf-lang-v7 -> asm-pred-lang-v7
;; compiles p to Asm-pred-lang v7 by selecting appropriate sequences of abstract
;; assembly instructions to implement the operations of the source language
(define/contract (select-instructions p)
  (-> imp-cmf-lang-v7? asm-pred-lang-v7?)

  ;; func-info is `(define ,label ,info ,tail)
  ;; interp. a function definition that has metadata

  ; imp-cmf-lang-v7.value -> (list (List-of asm-pred-lang-v7.effect) aloc)
  ; Assigns the value v to a fresh temporary, returning two values: the list of
  ; statements the implement the assignment in Loc-lang, and the aloc that the
  ; value is stored in
  (define (assign-tmp v)
    (match v
      [`(,binop ,op1 ,op2)
       (match-let ([`(,stmts1 ,loc1) (select-triv op1)]
                   [`(,stmts2 ,loc2) (select-triv op2)])
         (list (append stmts1 stmts2 (list `(set! ,loc1 (,binop ,loc1 ,loc2)))) loc1))]
      [triv (select-triv triv)]))

  ;; func-info -> func-info
  (define (select-func f)
    (match f
      [`(define ,label ,info ,tail)
       `(define ,label ,info ,(select-tail tail))]))

  ;; imp-cmf-lang-v7.tail -> asm-pred-lang-v7.tail
  (define (select-tail t)
    (match t
      [`(jump ,trg ,locs ...) `(jump ,trg ,@locs)]
      [`(if ,pred ,tail1 ,tail2)
       `(if ,(select-pred pred)
            ,(select-tail tail1)
            ,(select-tail tail2))]
      [`(begin ,fx ... ,tail)
       (define compiled-fx (for/foldr ([instructions empty])
                             ([e fx])
                             (append (select-effect e) instructions)))
       (define tail-compiled (select-tail tail))
       (cond
         [(empty? compiled-fx) tail-compiled]
         [else (match tail-compiled
                 [`(begin ,inner-effects^ ... ,inner-tail^)
                  `(begin ,@compiled-fx ,@inner-effects^ ,inner-tail^)]
                 [_ `(begin ,@compiled-fx ,tail-compiled)])])]))

  ;; aloc imp-cmf-lang-v7.effect -> (List-of asm-pred-lang-v7.effect)
  (define (select-value x v)
    (match v
      [`(,binop ,op1 ,op2)
       (cond
         [(or (int64? op1) (not (eq? x op1))) (list `(set! ,x ,op1) `(set! ,x (,binop ,x ,op2)))]
         [else (list `(set! ,x (,binop ,x ,op2)))])]
      [triv (list `(set! ,x ,triv))]))

  ;; imp-cmf-lang-v7.effect -> (List-of asm-pred-lang-v7.effect)
  (define (select-effect e)
    (match e
      [`(set! ,x ,v) (select-value x v)]
      [`(begin ,fx ... ,e)
       (define compiled-fx (for/foldr ([fx-acc empty])
                             ([e fx])
                             (append (select-effect e) fx-acc)))
       `(,@compiled-fx ,@(select-effect e))]
      [`(if ,pred ,e1 ,e2)
       (define e1^ (match e1
                     [`(begin ,e ...) `((begin ,@(select-effect e1)))]
                     [_ (select-effect e1)]))
       (define e2^ (match e2
                     [`(begin ,e ...) `((begin ,@(select-effect e2)))]
                     [_ (select-effect e2)]))
       (list `(if ,(select-pred pred)
                  ,@e1^
                  ,@e2^))]
      [`(return-point ,label ,tail)
       (list `(return-point ,label ,(select-tail tail)))]))

  ;; imp-cmf-lang-v7.pred -> asm-pred-lang-v7.pred
  (define (select-pred p)
    (match p
      [`(not ,pred)
       `(not ,(select-pred pred))]
      [`(begin ,fx ... ,pred)
       (define compiled-fx (for/foldr ([fx-acc empty])
                             ([e fx])
                             (append (select-effect e) fx-acc)))
       `(begin ,@compiled-fx ,(select-pred pred))]
      [`(if ,pred1 ,pred2 ,pred3)
       `(if ,(select-pred pred1) ,(select-pred pred2) ,(select-pred pred3))]
      [`(,relop ,triv1 ,triv2)
       (match-let ([`(,stmts ,loc) (select-triv triv1)])
         (if (empty? stmts)
             `(,relop ,loc ,triv2)
             `(begin ,@stmts (,relop ,loc ,triv2))))]
      ['(true) p]
      ['(false) p]))

  ;; imp-cmf-lang-v7.triv -> (list (List-of asm-pred-lang-v7.effect) asm-pred-lang-v7.trg)
  (define (select-triv t)
    (match t
      [label #:when (label? label) (list empty label)]
      [opand (select-opand opand)]))

  ;; imp-cmf-lang-v7.opand -> (list (List-of asm-pred-lang-v7.effect) asm-pred-lang-v7.trg)
  (define (select-opand op)
    (match op
      [int64
       #:when (int64? int64)
       (define tmp (fresh 'tmp))
       (list (list `(set! ,tmp ,int64)) tmp)]
      [loc (select-loc loc)]))

  ;; imp-cmf-lang-v7.loc -> (list (List-of asm-pred-lang-v7.effect) asm-pred-lang-v7.trg)
  (define (select-loc loc)
    (match loc
      [aloc #:when (aloc? aloc) (list empty aloc)]
      [rloc #:when (rloc? rloc) (list empty rloc)]))

  ;; imp-cmf-lang-v7.trg -> (list (List-of asm-pred-lang-v7.effect) asm-pred-lang-v7.trg)
  (define (select-trg trg)
    (match trg
      [label #:when (label? label) (list empty label)]
      [loc (select-loc loc)]))

  (match p
    [`(module ,info ,funcs ... ,tail)
     `(module ,info ,@(map select-func funcs) ,(select-tail tail))]))

(module+ test
  (check-equal? (select-instructions '(module
                                          ((new-frames ((nfv.36) (nfv.35))))
                                        (define L.f.1
                                          ((new-frames ()))
                                          (begin
                                            (set! tmp-ra.24 r15)
                                            (begin
                                              (set! x.1 fv0)
                                              (set! y.1 fv1)
                                              (begin (set! rax (+ x.1 y.1)) (jump tmp-ra.24 rbp rax)))))
                                        (define L.g.1
                                          ((new-frames
                                            ((nfv.32 nfv.33) (nfv.30 nfv.31) (nfv.28 nfv.29) (nfv.26 nfv.27))))
                                          (begin
                                            (set! tmp-ra.25 r15)
                                            (begin
                                              (set! x.1 fv0)
                                              (begin
                                                (begin
                                                  (return-point L.rp.6
                                                                (begin
                                                                  (set! nfv.26 x.1)
                                                                  (set! nfv.27 1)
                                                                  (set! r15 L.rp.6)
                                                                  (jump L.f.1 rbp r15 nfv.26 nfv.27)))
                                                  (set! y.1 rax))
                                                (begin
                                                  (return-point L.rp.7
                                                                (begin
                                                                  (set! nfv.28 x.1)
                                                                  (set! nfv.29 2)
                                                                  (set! r15 L.rp.7)
                                                                  (jump L.f.1 rbp r15 nfv.28 nfv.29)))
                                                  (set! z.1 rax))
                                                (if (true)
                                                    (begin
                                                      (begin
                                                        (return-point L.rp.8
                                                                      (begin
                                                                        (set! nfv.30 y.1)
                                                                        (set! nfv.31 z.1)
                                                                        (set! r15 L.rp.8)
                                                                        (jump L.f.1 rbp r15 nfv.30 nfv.31)))
                                                        (set! a.1 rax))
                                                      (begin (set! rax (* a.1 x.1)) (jump tmp-ra.25 rbp rax)))
                                                    (begin
                                                      (begin
                                                        (return-point L.rp.9
                                                                      (begin
                                                                        (set! nfv.32 y.1)
                                                                        (set! nfv.33 x.1)
                                                                        (set! r15 L.rp.9)
                                                                        (jump L.f.1 rbp r15 nfv.32 nfv.33)))
                                                        (set! b.1 rax))
                                                      (begin (set! rax (- b.1 z.1)) (jump tmp-ra.25 rbp rax))))))))
                                        (begin
                                          (set! tmp-ra.34 r15)
                                          (begin
                                            (begin
                                              (return-point L.rp.10
                                                            (begin
                                                              (set! nfv.35 1)
                                                              (set! r15 L.rp.10)
                                                              (jump L.g.1 rbp r15 nfv.35)))
                                              (set! x.1 rax))
                                            (begin
                                              (return-point L.rp.11
                                                            (begin
                                                              (set! nfv.36 2)
                                                              (set! r15 L.rp.11)
                                                              (jump L.g.1 rbp r15 nfv.36)))
                                              (set! x.2 rax))
                                            (begin (set! rax (* x.1 x.2)) (jump tmp-ra.34 rbp rax))))))
                '(module
                     ((new-frames ((nfv.36) (nfv.35))))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.24 r15)
                       (set! x.1 fv0)
                       (set! y.1 fv1)
                       (set! rax x.1)
                       (set! rax (+ rax y.1))
                       (jump tmp-ra.24 rbp rax)))
                   (define L.g.1
                     ((new-frames
                       ((nfv.32 nfv.33) (nfv.30 nfv.31) (nfv.28 nfv.29) (nfv.26 nfv.27))))
                     (begin
                       (set! tmp-ra.25 r15)
                       (set! x.1 fv0)
                       (return-point L.rp.6
                                     (begin
                                       (set! nfv.26 x.1)
                                       (set! nfv.27 1)
                                       (set! r15 L.rp.6)
                                       (jump L.f.1 rbp r15 nfv.26 nfv.27)))
                       (set! y.1 rax)
                       (return-point L.rp.7
                                     (begin
                                       (set! nfv.28 x.1)
                                       (set! nfv.29 2)
                                       (set! r15 L.rp.7)
                                       (jump L.f.1 rbp r15 nfv.28 nfv.29)))
                       (set! z.1 rax)
                       (if (true)
                           (begin
                             (return-point L.rp.8
                                           (begin
                                             (set! nfv.30 y.1)
                                             (set! nfv.31 z.1)
                                             (set! r15 L.rp.8)
                                             (jump L.f.1 rbp r15 nfv.30 nfv.31)))
                             (set! a.1 rax)
                             (set! rax a.1)
                             (set! rax (* rax x.1))
                             (jump tmp-ra.25 rbp rax))
                           (begin
                             (return-point L.rp.9
                                           (begin
                                             (set! nfv.32 y.1)
                                             (set! nfv.33 x.1)
                                             (set! r15 L.rp.9)
                                             (jump L.f.1 rbp r15 nfv.32 nfv.33)))
                             (set! b.1 rax)
                             (set! rax b.1)
                             (set! rax (- rax z.1))
                             (jump tmp-ra.25 rbp rax)))))
                   (begin
                     (set! tmp-ra.34 r15)
                     (return-point L.rp.10
                                   (begin (set! nfv.35 1) (set! r15 L.rp.10) (jump L.g.1 rbp r15 nfv.35)))
                     (set! x.1 rax)
                     (return-point L.rp.11
                                   (begin (set! nfv.36 2) (set! r15 L.rp.11) (jump L.g.1 rbp r15 nfv.36)))
                     (set! x.2 rax)
                     (set! rax x.1)
                     (set! rax (* rax x.2))
                     (jump tmp-ra.34 rbp rax))))
  (check-equal? (select-instructions '(module
                                          ((new-frames (())))
                                        (define L.f.1
                                          ((new-frames ()))
                                          (begin
                                            (set! tmp-ra.6 r15)
                                            (begin (set! x.1 rdi) (begin (set! rax x.1) (jump tmp-ra.6 rbp rax)))))
                                        (begin
                                          (set! tmp-ra.7 r15)
                                          (begin
                                            (set! x.1 1)
                                            (set! x.2 10)
                                            (set! x.3 (+ x.1 x.2))
                                            (begin
                                              (return-point L.rp.3
                                                            (begin (set! rdi x.3) (set! r15 L.rp.3) (jump L.f.4 rbp r15 rdi)))
                                              (set! x.4 rax))
                                            (begin (set! rax (* x.4 x.3)) (jump tmp-ra.7 rbp rax))))))
                '(module
                     ((new-frames (())))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.6 r15)
                       (set! x.1 rdi)
                       (set! rax x.1)
                       (jump tmp-ra.6 rbp rax)))
                   (begin
                     (set! tmp-ra.7 r15)
                     (set! x.1 1)
                     (set! x.2 10)
                     (set! x.3 x.1)
                     (set! x.3 (+ x.3 x.2))
                     (return-point L.rp.3
                                   (begin (set! rdi x.3) (set! r15 L.rp.3) (jump L.f.4 rbp r15 rdi)))
                     (set! x.4 rax)
                     (set! rax x.4)
                     (set! rax (* rax x.3))
                     (jump tmp-ra.7 rbp rax))))
  (check-equal? (select-instructions '(module ((new-frames ())) (begin (set! tmp-ra.1 r15) (set! foo.1 1) (set! rax (+ foo.1 foo.1)) (jump tmp-ra.1 rbp rax))))
                '(module
                     ((new-frames ()))
                   (begin
                     (set! tmp-ra.1 r15)
                     (set! foo.1 1)
                     (set! rax foo.1)
                     (set! rax (+ rax foo.1))
                     (jump tmp-ra.1 rbp rax))))
  (check-equal? (select-instructions '(module ((new-frames ())) (begin (set! tmp-ra.1 r15) (set! foo.12 1) (begin (set! bar.13 2) (set! rax (+ foo.12 bar.13))) (jump tmp-ra.1 rbp rax))))
                '(module
                     ((new-frames ()))
                   (begin
                     (set! tmp-ra.1 r15)
                     (set! foo.12 1)
                     (set! bar.13 2)
                     (set! rax foo.12)
                     (set! rax (+ rax bar.13))
                     (jump tmp-ra.1 rbp rax))))
  (check-equal? (select-instructions '(module ((new-frames ())) (begin (set! tmp-ra.1 r15) (set! foo.3 1) (begin (begin (set! x.5 1) (set! bar.4 (+ x.5 5))) (set! rax (+ foo.3 bar.4)) (jump tmp-ra.1 rbp rax)))))
                '(module
                     ((new-frames ()))
                   (begin
                     (set! tmp-ra.1 r15)
                     (set! foo.3 1)
                     (set! x.5 1)
                     (set! bar.4 x.5)
                     (set! bar.4 (+ bar.4 5))
                     (set! rax foo.3)
                     (set! rax (+ rax bar.4))
                     (jump tmp-ra.1 rbp rax))))
  (check-equal? (select-instructions '(module ((new-frames ())) (begin (set! tmp-ra.1 r15) (set! rax (+ 2 2)) (jump tmp-ra.1 rbp rax))))
                '(module
                     ((new-frames ()))
                   (begin
                     (set! tmp-ra.1 r15)
                     (set! rax 2)
                     (set! rax (+ rax 2))
                     (jump tmp-ra.1 rbp rax))))
  (check-equal? (select-instructions '(module ((new-frames ())) (define L.start.1 ((new-frames ())) (begin
                                                                                                      (set! tmp-ra.1 r15)
                                                                                                      (set! rax 1)
                                                                                                      (jump tmp-ra.1 rbp rax)))
                                        (begin (set! tmp-ra.2 r15) (set! x.1 5) (set! rax x.1) (jump tmp-ra.2 rbp rax))))
                '(module
                     ((new-frames ()))
                   (define L.start.1
                     ((new-frames ()))
                     (begin (set! tmp-ra.1 r15) (set! rax 1) (jump tmp-ra.1 rbp rax)))
                   (begin
                     (set! tmp-ra.2 r15)
                     (set! x.1 5)
                     (set! rax x.1)
                     (jump tmp-ra.2 rbp rax))))
  (check-equal? (select-instructions '(module ((new-frames ())) (begin (set! tmp-ra.1 r15) (begin (begin (set! rax 5) (jump tmp-ra.1 rbp rax))))))
                '(module
                     ((new-frames ()))
                   (begin (set! tmp-ra.1 r15) (set! rax 5) (jump tmp-ra.1 rbp rax))))
  (check-equal? (select-instructions
                 '(module
                      ((new-frames ()))
                    (define L.start.1 ((new-frames ()))
                      (begin
                        (set! tmp-ra.1 r15)
                        (set! x.1 0)
                        (if
                         (if (> rax rbx)
                             (< 1 fv1)
                             (> x.1 5))
                         (set! x.1 1)
                         (set! x.1 2))
                        (set! rax x.1)
                        (jump tmp-ra.1 rbp rax)))
                    (begin
                      (set! tmp-ra.2 r15)
                      (begin (set! rdi x.1) (set! r15 tmp-ra.2) (jump L.start.1 rbp r15 rdi)))))
                '(module
                     ((new-frames ()))
                   (define L.start.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.1 r15)
                       (set! x.1 0)
                       (if (if (> rax rbx) (begin (set! tmp.1 1) (< tmp.1 fv1)) (> x.1 5))
                           (set! x.1 1)
                           (set! x.1 2))
                       (set! rax x.1)
                       (jump tmp-ra.1 rbp rax)))
                   (begin
                     (set! tmp-ra.2 r15)
                     (set! rdi x.1)
                     (set! r15 tmp-ra.2)
                     (jump L.start.1 rbp r15 rdi))))
  (check-equal? (select-instructions '(module ((new-frames ())) (begin (set! tmp-ra.1 r15) (set! x.1 (+ 2 2)) (set! rax x.1) (jump tmp-ra.1 rbp rax))))
                '(module
                     ((new-frames ()))
                   (begin
                     (set! tmp-ra.1 r15)
                     (set! x.1 2)
                     (set! x.1 (+ x.1 2))
                     (set! rax x.1)
                     (jump tmp-ra.1 rbp rax))))
  (check-equal? (select-instructions '(module ((new-frames ())) (begin (set! tmp-ra.1 r15) (begin (set! x.1 1)) (set! rax x.1) (jump tmp-ra.1 rbp rax))))
                '(module
                     ((new-frames ()))
                   (begin
                     (set! tmp-ra.1 r15)
                     (set! x.1 1)
                     (set! rax x.1)
                     (jump tmp-ra.1 rbp rax))))
  (check-equal? (select-instructions '(module ((new-frames ())) (begin (set! tmp-ra.1 r15) (set! x.1 2) (set! x.1 (+ x.1 x.1)) (set! rax x.1) (jump tmp-ra.1 rbp rax))))
                '(module
                     ((new-frames ()))
                   (begin
                     (set! tmp-ra.1 r15)
                     (set! x.1 2)
                     (set! x.1 (+ x.1 x.1))
                     (set! rax x.1)
                     (jump tmp-ra.1 rbp rax))))
  (check-equal? (select-instructions '(module ((new-frames ())) (begin (set! tmp-ra.1 r15) (set! x.1 2) (begin (set! x.1 (+ x.1 2))) (set! rax x.1) (jump tmp-ra.1 rbp rax))))
                '(module
                     ((new-frames ()))
                   (begin
                     (set! tmp-ra.1 r15)
                     (set! x.1 2)
                     (set! x.1 (+ x.1 2))
                     (set! rax x.1)
                     (jump tmp-ra.1 rbp rax))))
  (check-equal? (select-instructions '(module ((new-frames ()))
                                        (begin
                                          (set! tmp-ra.1 r15)
                                          (begin
                                            (set! x.1 1)
                                            (set! x.2 2)
                                            (if (begin
                                                  (set! x.2 (+ x.2 1))
                                                  (= x.2 3))
                                                (set! x.1 (+ x.1 x.2))
                                                (set! x.1 (* x.1 x.2)))
                                            (if (if (not (> x.1 1)) (false) (true))
                                                (begin
                                                  (set! rax x.1)
                                                  (jump tmp-ra.1 rbp rax))
                                                (begin
                                                  (set! rax x.2)
                                                  (jump tmp-ra.1 rbp rax)))))))
                '(module
                     ((new-frames ()))
                   (begin
                     (set! tmp-ra.1 r15)
                     (set! x.1 1)
                     (set! x.2 2)
                     (if (begin (set! x.2 (+ x.2 1)) (= x.2 3))
                         (set! x.1 (+ x.1 x.2))
                         (set! x.1 (* x.1 x.2)))
                     (if (if (not (> x.1 1)) (false) (true))
                         (begin (set! rax x.1) (jump tmp-ra.1 rbp rax))
                         (begin (set! rax x.2) (jump tmp-ra.1 rbp rax))))))
  (check-equal? (select-instructions '(module ((new-frames ()))
                                        (begin
                                          (set! tmp-ra.1 r15)
                                          (set! foo.3 1)
                                          (begin
                                            (begin
                                              (set! x.5 1)
                                              (set! bar.4 (+ x.5 5)))
                                            (set! rax (+ foo.3 bar.4))
                                            (jump tmp-ra.1 rbp rax)))))
                '(module
                     ((new-frames ()))
                   (begin
                     (set! tmp-ra.1 r15)
                     (set! foo.3 1)
                     (set! x.5 1)
                     (set! bar.4 x.5)
                     (set! bar.4 (+ bar.4 5))
                     (set! rax foo.3)
                     (set! rax (+ rax bar.4))
                     (jump tmp-ra.1 rbp rax))))
  (check-equal? (select-instructions '(module ((new-frames ()))
                                        (begin (set! tmp-ra.1 r15) (set! x.1 1) (set! y.1 1) (set! z.1 (+ x.1 y.1)) (set! rax z.1) (jump tmp-ra.1 rbp rax))))
                '(module
                     ((new-frames ()))
                   (begin
                     (set! tmp-ra.1 r15)
                     (set! x.1 1)
                     (set! y.1 1)
                     (set! z.1 x.1)
                     (set! z.1 (+ z.1 y.1))
                     (set! rax z.1)
                     (jump tmp-ra.1 rbp rax))))
  (check-equal? (select-instructions '(module ((new-frames ()))
                                        (begin (set! tmp-ra.1 r15) (set! x.6 (+ 2 3)) (set! x.7 (+ x.6 x.6)) (begin (set! y.2 5) (set! rax x.6) (jump tmp-ra.1 rbp rax)))))
                '(module
                     ((new-frames ()))
                   (begin
                     (set! tmp-ra.1 r15)
                     (set! x.6 2)
                     (set! x.6 (+ x.6 3))
                     (set! x.7 x.6)
                     (set! x.7 (+ x.7 x.6))
                     (set! y.2 5)
                     (set! rax x.6)
                     (jump tmp-ra.1 rbp rax))))

  (check-equal? (select-instructions '(module ((new-frames ()))
                                        (begin
                                          (set! tmp-ra.157 r15)
                                          (begin
                                            (set! x.1 0)
                                            (if (true)
                                                (begin (set! y.2 (+ x.1 17)) (set! x.5 12))
                                                (begin (set! x.5 15)))
                                            (begin
                                              (set! rax x.5) (jump tmp-ra.157 rbp rax))))))
                '(module
                     ((new-frames ()))
                   (begin
                     (set! tmp-ra.157 r15)
                     (set! x.1 0)
                     (if (true)
                         (begin (set! y.2 x.1) (set! y.2 (+ y.2 17)) (set! x.5 12))
                         (begin (set! x.5 15)))
                     (set! rax x.5)
                     (jump tmp-ra.157 rbp rax))))
  (check-equal? (select-instructions '(module
                                          ((new-frames ()))
                                        (define L.f.1
                                          ((new-frames ()))
                                          (begin
                                            (set! tmp-ra.1 r15)
                                            (begin
                                              (set! x.1 rdi)
                                              (begin
                                                (set! y.1 1)
                                                (set! z.1 2)
                                                (begin
                                                  (set! a.1 (bitwise-and y.1 x.1))
                                                  (set! b.1 (bitwise-ior z.1 x.1))
                                                  (begin
                                                    (set! a.1 (bitwise-xor a.1 b.1))
                                                    (begin
                                                      (set! rax (arithmetic-shift-right a.1 3))
                                                      (jump tmp-ra.1 rbp rax))))))))
                                        (begin
                                          (set! tmp-ra.2 r15)
                                          (begin
                                            (set! x.2 10)
                                            (if (begin (set! x.3 100) (not (!= x.2 x.3)))
                                                (begin (set! rdi x.2) (set! r15 tmp-ra.2) (jump L.f.1 rbp r15 rdi))
                                                (begin
                                                  (set! rdi 1000)
                                                  (set! r15 tmp-ra.2)
                                                  (jump L.f.2 rbp r15 rdi)))))))
                '(module
                     ((new-frames ()))
                   (define L.f.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.1 r15)
                       (set! x.1 rdi)
                       (set! y.1 1)
                       (set! z.1 2)
                       (set! a.1 y.1)
                       (set! a.1 (bitwise-and a.1 x.1))
                       (set! b.1 z.1)
                       (set! b.1 (bitwise-ior b.1 x.1))
                       (set! a.1 (bitwise-xor a.1 b.1))
                       (set! rax a.1)
                       (set! rax (arithmetic-shift-right rax 3))
                       (jump tmp-ra.1 rbp rax)))
                   (begin
                     (set! tmp-ra.2 r15)
                     (set! x.2 10)
                     (if (begin (set! x.3 100) (not (!= x.2 x.3)))
                         (begin (set! rdi x.2) (set! r15 tmp-ra.2) (jump L.f.1 rbp r15 rdi))
                         (begin (set! rdi 1000) (set! r15 tmp-ra.2) (jump L.f.2 rbp r15 rdi))))))
  (check-equal? (select-instructions '(module
                                          ((new-frames ()))
                                        (define L.*.2
                                          ((new-frames ()))
                                          (begin
                                            (set! tmp-ra.39 r15)
                                            (begin
                                              (set! tmp.1 rdi)
                                              (set! tmp.2 rsi)
                                              (if (begin
                                                    (if (begin (set! tmp.24 (bitwise-and tmp.2 7)) (= tmp.24 0))
                                                        (set! tmp.23 14)
                                                        (set! tmp.23 6))
                                                    (!= tmp.23 6))
                                                  (if (begin
                                                        (if (begin (set! tmp.26 (bitwise-and tmp.1 7)) (= tmp.26 0))
                                                            (set! tmp.25 14)
                                                            (set! tmp.25 6))
                                                        (!= tmp.25 6))
                                                      (begin
                                                        (set! tmp.27 (arithmetic-shift-right tmp.2 3))
                                                        (begin (set! rax (* tmp.1 tmp.27)) (jump tmp-ra.39 rbp rax)))
                                                      (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))
                                                  (begin (set! rax 318) (jump tmp-ra.39 rbp rax))))))
                                        (define L.+.1
                                          ((new-frames ()))
                                          (begin
                                            (set! tmp-ra.40 r15)
                                            (begin
                                              (set! tmp.3 rdi)
                                              (set! tmp.4 rsi)
                                              (if (begin
                                                    (if (begin (set! tmp.29 (bitwise-and tmp.4 7)) (= tmp.29 0))
                                                        (set! tmp.28 14)
                                                        (set! tmp.28 6))
                                                    (!= tmp.28 6))
                                                  (if (begin
                                                        (if (begin (set! tmp.31 (bitwise-and tmp.3 7)) (= tmp.31 0))
                                                            (set! tmp.30 14)
                                                            (set! tmp.30 6))
                                                        (!= tmp.30 6))
                                                      (begin (set! rax (+ tmp.3 tmp.4)) (jump tmp-ra.40 rbp rax))
                                                      (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))
                                                  (begin (set! rax 574) (jump tmp-ra.40 rbp rax))))))
                                        (define L.add.10
                                          ((new-frames (() () () () () ())))
                                          (begin
                                            (set! tmp-ra.41 r15)
                                            (begin
                                              (set! a.61 rdi)
                                              (set! b.62 rsi)
                                              (set! c.63 rdx)
                                              (set! d.64 rcx)
                                              (set! e.65 r8)
                                              (set! f.66 r9)
                                              (set! g.67 fv0)
                                              (set! h.68 fv1)
                                              (begin
                                                (begin
                                                  (begin
                                                    (begin
                                                      (begin
                                                        (begin
                                                          (begin
                                                            (return-point L.rp.12
                                                                          (begin
                                                                            (set! rdi g.67)
                                                                            (set! rsi h.68)
                                                                            (set! r15 L.rp.12)
                                                                            (jump L.+.1 rbp r15 rdi rsi)))
                                                            (set! tmp.37 rax))
                                                          (begin
                                                            (return-point L.rp.13
                                                                          (begin
                                                                            (set! rdi f.66)
                                                                            (set! rsi tmp.37)
                                                                            (set! r15 L.rp.13)
                                                                            (jump L.+.1 rbp r15 rdi rsi)))
                                                            (set! tmp.36 rax)))
                                                        (begin
                                                          (return-point L.rp.14
                                                                        (begin
                                                                          (set! rdi e.65)
                                                                          (set! rsi tmp.36)
                                                                          (set! r15 L.rp.14)
                                                                          (jump L.+.1 rbp r15 rdi rsi)))
                                                          (set! tmp.35 rax)))
                                                      (begin
                                                        (return-point L.rp.15
                                                                      (begin
                                                                        (set! rdi d.64)
                                                                        (set! rsi tmp.35)
                                                                        (set! r15 L.rp.15)
                                                                        (jump L.+.1 rbp r15 rdi rsi)))
                                                        (set! tmp.34 rax)))
                                                    (begin
                                                      (return-point L.rp.16
                                                                    (begin
                                                                      (set! rdi c.63)
                                                                      (set! rsi tmp.34)
                                                                      (set! r15 L.rp.16)
                                                                      (jump L.+.1 rbp r15 rdi rsi)))
                                                      (set! tmp.33 rax)))
                                                  (begin
                                                    (return-point L.rp.17
                                                                  (begin
                                                                    (set! rdi b.62)
                                                                    (set! rsi tmp.33)
                                                                    (set! r15 L.rp.17)
                                                                    (jump L.+.1 rbp r15 rdi rsi)))
                                                    (set! tmp.32 rax)))
                                                (begin
                                                  (set! rdi a.61)
                                                  (set! rsi tmp.32)
                                                  (set! r15 tmp-ra.41)
                                                  (jump L.+.1 rbp r15 rdi rsi))))))
                                        (define L.add-and-multiply.11
                                          ((new-frames ((nfv.43 nfv.44))))
                                          (begin
                                            (set! tmp-ra.42 r15)
                                            (begin
                                              (set! a.69 rdi)
                                              (set! b.70 rsi)
                                              (set! c.71 rdx)
                                              (set! d.72 rcx)
                                              (set! e.73 r8)
                                              (set! f.74 r9)
                                              (set! g.75 fv0)
                                              (set! h.76 fv1)
                                              (set! i.77 fv2)
                                              (begin
                                                (begin
                                                  (return-point L.rp.18
                                                                (begin
                                                                  (set! rdi a.69)
                                                                  (set! rsi b.70)
                                                                  (set! rdx c.71)
                                                                  (set! rcx d.72)
                                                                  (set! r8 e.73)
                                                                  (set! r9 f.74)
                                                                  (set! nfv.43 g.75)
                                                                  (set! nfv.44 h.76)
                                                                  (set! r15 L.rp.18)
                                                                  (jump L.add.10 rbp r15 rdi rsi rdx rcx r8 r9 nfv.43 nfv.44)))
                                                  (set! sum.78 rax))
                                                (begin
                                                  (set! rdi sum.78)
                                                  (set! rsi i.77)
                                                  (set! r15 tmp-ra.42)
                                                  (jump L.*.2 rbp r15 rdi rsi))))))
                                        (begin
                                          (set! tmp-ra.45 r15)
                                          (begin
                                            (set! rdi 8)
                                            (set! rsi 16)
                                            (set! rdx 24)
                                            (set! rcx 32)
                                            (set! r8 40)
                                            (set! r9 48)
                                            (set! fv0 56)
                                            (set! fv1 64)
                                            (set! fv2 16)
                                            (set! r15 tmp-ra.45)
                                            (jump
                                             L.add-and-multiply.11
                                             rbp
                                             r15
                                             rdi
                                             rsi
                                             rdx
                                             rcx
                                             r8
                                             r9
                                             fv0
                                             fv1
                                             fv2)))))
                '(module
                     ((new-frames ()))
                   (define L.*.2
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.39 r15)
                       (set! tmp.1 rdi)
                       (set! tmp.2 rsi)
                       (if (begin
                             (if (begin
                                   (begin
                                     (set! tmp.24 tmp.2)
                                     (set! tmp.24 (bitwise-and tmp.24 7)))
                                   (= tmp.24 0))
                                 (set! tmp.23 14)
                                 (set! tmp.23 6))
                             (!= tmp.23 6))
                           (if (begin
                                 (if (begin
                                       (begin
                                         (set! tmp.26 tmp.1)
                                         (set! tmp.26 (bitwise-and tmp.26 7)))
                                       (= tmp.26 0))
                                     (set! tmp.25 14)
                                     (set! tmp.25 6))
                                 (!= tmp.25 6))
                               (begin
                                 (set! tmp.27 tmp.2)
                                 (set! tmp.27 (arithmetic-shift-right tmp.27 3))
                                 (set! rax tmp.1)
                                 (set! rax (* rax tmp.27))
                                 (jump tmp-ra.39 rbp rax))
                               (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))
                           (begin (set! rax 318) (jump tmp-ra.39 rbp rax)))))
                   (define L.+.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.40 r15)
                       (set! tmp.3 rdi)
                       (set! tmp.4 rsi)
                       (if (begin
                             (if (begin
                                   (begin
                                     (set! tmp.29 tmp.4)
                                     (set! tmp.29 (bitwise-and tmp.29 7)))
                                   (= tmp.29 0))
                                 (set! tmp.28 14)
                                 (set! tmp.28 6))
                             (!= tmp.28 6))
                           (if (begin
                                 (if (begin
                                       (begin
                                         (set! tmp.31 tmp.3)
                                         (set! tmp.31 (bitwise-and tmp.31 7)))
                                       (= tmp.31 0))
                                     (set! tmp.30 14)
                                     (set! tmp.30 6))
                                 (!= tmp.30 6))
                               (begin
                                 (set! rax tmp.3)
                                 (set! rax (+ rax tmp.4))
                                 (jump tmp-ra.40 rbp rax))
                               (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))
                           (begin (set! rax 574) (jump tmp-ra.40 rbp rax)))))
                   (define L.add.10
                     ((new-frames (() () () () () ())))
                     (begin
                       (set! tmp-ra.41 r15)
                       (set! a.61 rdi)
                       (set! b.62 rsi)
                       (set! c.63 rdx)
                       (set! d.64 rcx)
                       (set! e.65 r8)
                       (set! f.66 r9)
                       (set! g.67 fv0)
                       (set! h.68 fv1)
                       (return-point L.rp.12
                                     (begin
                                       (set! rdi g.67)
                                       (set! rsi h.68)
                                       (set! r15 L.rp.12)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                       (set! tmp.37 rax)
                       (return-point L.rp.13
                                     (begin
                                       (set! rdi f.66)
                                       (set! rsi tmp.37)
                                       (set! r15 L.rp.13)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                       (set! tmp.36 rax)
                       (return-point L.rp.14
                                     (begin
                                       (set! rdi e.65)
                                       (set! rsi tmp.36)
                                       (set! r15 L.rp.14)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                       (set! tmp.35 rax)
                       (return-point L.rp.15
                                     (begin
                                       (set! rdi d.64)
                                       (set! rsi tmp.35)
                                       (set! r15 L.rp.15)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                       (set! tmp.34 rax)
                       (return-point L.rp.16
                                     (begin
                                       (set! rdi c.63)
                                       (set! rsi tmp.34)
                                       (set! r15 L.rp.16)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                       (set! tmp.33 rax)
                       (return-point L.rp.17
                                     (begin
                                       (set! rdi b.62)
                                       (set! rsi tmp.33)
                                       (set! r15 L.rp.17)
                                       (jump L.+.1 rbp r15 rdi rsi)))
                       (set! tmp.32 rax)
                       (set! rdi a.61)
                       (set! rsi tmp.32)
                       (set! r15 tmp-ra.41)
                       (jump L.+.1 rbp r15 rdi rsi)))
                   (define L.add-and-multiply.11
                     ((new-frames ((nfv.43 nfv.44))))
                     (begin
                       (set! tmp-ra.42 r15)
                       (set! a.69 rdi)
                       (set! b.70 rsi)
                       (set! c.71 rdx)
                       (set! d.72 rcx)
                       (set! e.73 r8)
                       (set! f.74 r9)
                       (set! g.75 fv0)
                       (set! h.76 fv1)
                       (set! i.77 fv2)
                       (return-point L.rp.18
                                     (begin
                                       (set! rdi a.69)
                                       (set! rsi b.70)
                                       (set! rdx c.71)
                                       (set! rcx d.72)
                                       (set! r8 e.73)
                                       (set! r9 f.74)
                                       (set! nfv.43 g.75)
                                       (set! nfv.44 h.76)
                                       (set! r15 L.rp.18)
                                       (jump L.add.10 rbp r15 rdi rsi rdx rcx r8 r9 nfv.43 nfv.44)))
                       (set! sum.78 rax)
                       (set! rdi sum.78)
                       (set! rsi i.77)
                       (set! r15 tmp-ra.42)
                       (jump L.*.2 rbp r15 rdi rsi)))
                   (begin
                     (set! tmp-ra.45 r15)
                     (set! rdi 8)
                     (set! rsi 16)
                     (set! rdx 24)
                     (set! rcx 32)
                     (set! r8 40)
                     (set! r9 48)
                     (set! fv0 56)
                     (set! fv1 64)
                     (set! fv2 16)
                     (set! r15 tmp-ra.45)
                     (jump L.add-and-multiply.11 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2)))))
