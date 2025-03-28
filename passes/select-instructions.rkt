#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8
  rackunit)

(provide select-instructions)

;; imp-cmf-lang-v8 -> asm-alloc-lang-v8
;; compiles p to Asm-alloc-lang v8 by selecting appropriate sequences of abstract
;; assembly instructions to implement the operations of the source language
(define (select-instructions p)
  (-> imp-cmf-lang-v8? asm-alloc-lang-v8?)

  ;; func-info is `(define ,label ,info ,tail)
  ;; interp. a function definition that has metadata

  ;; func-info -> func-info
  (define (select-func f)
    (match f
      [`(define ,label ,info ,tail)
       `(define ,label ,info ,(select-tail tail))]))

  ;; imp-cmf-lang-v8.tail -> asm-alloc-lang-v8.tail
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

  ;; imp-cmf-lang-v8.value loc -> (List-of asm-alloc-lang-v8.effect)
  (define (select-value value loc)
    (match value
      [`(mref ,l ,op)
       `((set! ,loc (mref ,l ,op)))]
      [`(alloc ,op)
       `((set! ,loc (alloc ,op)))]
      [`(,binop ,op1 ,op2)
       (define-values (stmts1 loc1) (select-opand op1 loc))
       (append stmts1
               `((set! ,loc (,binop ,loc ,op2))))]
      [triv
       `((set! ,loc ,triv))]))

  ;; imp-cmf-lang-v8.effect -> (List-of asm-alloc-lang-v8.effect)
  (define (select-effect e)
    (match e
      [`(set! ,loc ,value)
       (select-value value loc)]
      [`(mset! ,loc ,opand ,triv)
       `((mset! ,loc ,opand ,triv))]
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

  ;; imp-cmf-lang-v8.pred -> asm-alloc-lang-v8.pred
  (define (select-pred p)
    (match p
      ['(true) p]
      ['(false) p]
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
       (define-values (stmts loc) (select-triv triv1))
       (if (empty? stmts)
           `(,relop ,loc ,triv2)
           `(begin ,@stmts (,relop ,loc ,triv2)))]))

  ;; imp-cmf-lang-v8.triv -> (List-of asm-alloc-lang-v8.effect) asm-alloc-lang-v8.triv
  (define (select-triv t)
    (match t
      [label #:when (label? label) (values empty label)]
      [int64 #:when (int64? int64)
             (define tmp (fresh 'tmp))
             (values (list `(set! ,tmp ,int64)) tmp)]
      [loc (values empty loc)]))

  ;; imp-cmf-lang-v8.opand loc -> (List-of asm-alloc-lang-v8.effect) asm-alloc-lang-v8.opand
  (define (select-opand op loc)
    (match op
      [int64 #:when (int64? int64)
             (values (list `(set! ,loc ,int64)) loc)]
      [l
       (if (equal? l loc)
           (values empty l)
           (values (list `(set! ,loc ,l)) loc))]))

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
  (check-equal? (select-instructions '(module ((new-frames ())) (define L.cons.31 ((new-frames ())) (begin (set! tmp-ra.250 r15) (begin (set! tmp.68 rdi) (set! tmp.69 rsi) (begin (begin (set! tmp.122 (alloc 16)) (set! tmp.86 (+ tmp.122 1))) (begin (mset! tmp.86 -1 tmp.68) (mset! tmp.86 7 tmp.69) (begin (set! rax tmp.86) (jump tmp-ra.250 rbp rax))))))) (begin (set! tmp-ra.251 r15) (begin (set! rdi 56) (set! rsi 22) (set! r15 tmp-ra.251) (jump L.cons.31 rbp r15 rdi rsi)))))
                '(module
                     ((new-frames ()))
                   (define L.cons.31
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.250 r15)
                       (set! tmp.68 rdi)
                       (set! tmp.69 rsi)
                       (set! tmp.122 (alloc 16))
                       (set! tmp.86 tmp.122)
                       (set! tmp.86 (+ tmp.86 1))
                       (mset! tmp.86 -1 tmp.68)
                       (mset! tmp.86 7 tmp.69)
                       (set! rax tmp.86)
                       (jump tmp-ra.250 rbp rax)))
                   (begin
                     (set! tmp-ra.251 r15)
                     (set! rdi 56)
                     (set! rsi 22)
                     (set! r15 tmp-ra.251)
                     (jump L.cons.31 rbp r15 rdi rsi))))
  (check-equal? (select-instructions '(module ((new-frames (() () ()))) (define L.*.27 ((new-frames ())) (begin (set! tmp-ra.230 r15) (begin (set! tmp.60 rdi) (set! tmp.61 rsi) (if (begin (if (begin (set! tmp.92 (bitwise-and tmp.60 7)) (= tmp.92 0)) (set! tmp.91 14) (set! tmp.91 6)) (!= tmp.91 6)) (if (begin (if (begin (set! tmp.94 (bitwise-and tmp.61 7)) (= tmp.94 0)) (set! tmp.93 14) (set! tmp.93 6)) (!= tmp.93 6)) (begin (set! tmp.95 (arithmetic-shift-right tmp.61 3)) (begin (set! rax (* tmp.60 tmp.95)) (jump tmp-ra.230 rbp rax))) (begin (set! rax 318) (jump tmp-ra.230 rbp rax))) (begin (set! rax 318) (jump tmp-ra.230 rbp rax)))))) (define L.eq?.26 ((new-frames ())) (begin (set! tmp-ra.231 r15) (begin (set! tmp.58 rdi) (set! tmp.59 rsi) (if (= tmp.58 tmp.59) (begin (set! rax 14) (jump tmp-ra.231 rbp rax)) (begin (set! rax 6) (jump tmp-ra.231 rbp rax)))))) (define L.cons.25 ((new-frames ())) (begin (set! tmp-ra.232 r15) (begin (set! tmp.56 rdi) (set! tmp.57 rsi) (begin (begin (set! tmp.96 (alloc 16)) (set! tmp.90 (+ tmp.96 1))) (begin (mset! tmp.90 -1 tmp.56) (mset! tmp.90 7 tmp.57) (begin (set! rax tmp.90) (jump tmp-ra.232 rbp rax))))))) (begin (set! tmp-ra.233 r15) (begin (if (begin (begin (return-point L.rp.53 (begin (set! rdi 56) (set! rsi 64) (set! r15 L.rp.53) (jump L.eq?.26 rbp r15 rdi rsi))) (set! tmp.98 rax)) (!= tmp.98 6)) (begin (return-point L.rp.54 (begin (set! rdi 56) (set! rsi 64) (set! r15 L.rp.54) (jump L.*.27 rbp r15 rdi rsi))) (set! tmp.97 rax)) (begin (return-point L.rp.55 (begin (set! rdi 64) (set! rsi 56) (set! r15 L.rp.55) (jump L.*.27 rbp r15 rdi rsi))) (set! tmp.97 rax))) (begin (set! rdi tmp.97) (set! rsi 22) (set! r15 tmp-ra.233) (jump L.cons.25 rbp r15 rdi rsi))))))
                '(module
                     ((new-frames (() () ())))
                   (define L.*.27
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.230 r15)
                       (set! tmp.60 rdi)
                       (set! tmp.61 rsi)
                       (if (begin
                             (if (begin
                                   (set! tmp.92 tmp.60)
                                   (set! tmp.92 (bitwise-and tmp.92 7))
                                   (= tmp.92 0))
                                 (set! tmp.91 14)
                                 (set! tmp.91 6))
                             (!= tmp.91 6))
                           (if (begin
                                 (if (begin
                                       (set! tmp.94 tmp.61)
                                       (set! tmp.94 (bitwise-and tmp.94 7))
                                       (= tmp.94 0))
                                     (set! tmp.93 14)
                                     (set! tmp.93 6))
                                 (!= tmp.93 6))
                               (begin
                                 (set! tmp.95 tmp.61)
                                 (set! tmp.95 (arithmetic-shift-right tmp.95 3))
                                 (set! rax tmp.60)
                                 (set! rax (* rax tmp.95))
                                 (jump tmp-ra.230 rbp rax))
                               (begin (set! rax 318) (jump tmp-ra.230 rbp rax)))
                           (begin (set! rax 318) (jump tmp-ra.230 rbp rax)))))
                   (define L.eq?.26
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.231 r15)
                       (set! tmp.58 rdi)
                       (set! tmp.59 rsi)
                       (if (= tmp.58 tmp.59)
                           (begin (set! rax 14) (jump tmp-ra.231 rbp rax))
                           (begin (set! rax 6) (jump tmp-ra.231 rbp rax)))))
                   (define L.cons.25
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.232 r15)
                       (set! tmp.56 rdi)
                       (set! tmp.57 rsi)
                       (set! tmp.96 (alloc 16))
                       (set! tmp.90 tmp.96)
                       (set! tmp.90 (+ tmp.90 1))
                       (mset! tmp.90 -1 tmp.56)
                       (mset! tmp.90 7 tmp.57)
                       (set! rax tmp.90)
                       (jump tmp-ra.232 rbp rax)))
                   (begin
                     (set! tmp-ra.233 r15)
                     (if (begin
                           (return-point L.rp.53
                                         (begin
                                           (set! rdi 56)
                                           (set! rsi 64)
                                           (set! r15 L.rp.53)
                                           (jump L.eq?.26 rbp r15 rdi rsi)))
                           (set! tmp.98 rax)
                           (!= tmp.98 6))
                         (begin
                           (return-point L.rp.54
                                         (begin
                                           (set! rdi 56)
                                           (set! rsi 64)
                                           (set! r15 L.rp.54)
                                           (jump L.*.27 rbp r15 rdi rsi)))
                           (set! tmp.97 rax))
                         (begin
                           (return-point L.rp.55
                                         (begin
                                           (set! rdi 64)
                                           (set! rsi 56)
                                           (set! r15 L.rp.55)
                                           (jump L.*.27 rbp r15 rdi rsi)))
                           (set! tmp.97 rax)))
                     (set! rdi tmp.97)
                     (set! rsi 22)
                     (set! r15 tmp-ra.233)
                     (jump L.cons.25 rbp r15 rdi rsi))))
  (check-equal? (select-instructions '(module ((new-frames ())) (define L.vector-init-loop.20 ((new-frames ())) (begin (set! tmp-ra.236 r15) (begin (set! len.45 rdi) (set! i.46 rsi) (set! vec.47 rdx) (if (begin (if (= len.45 i.46) (set! tmp.185 14) (set! tmp.185 6)) (!= tmp.185 6)) (begin (set! rax vec.47) (jump tmp-ra.236 rbp rax)) (begin (begin (begin (begin (set! tmp.188 (arithmetic-shift-right i.46 3)) (set! tmp.187 (* tmp.188 8))) (set! tmp.186 (+ tmp.187 5))) (mset! vec.47 tmp.186 0)) (begin (set! tmp.189 (+ i.46 8)) (begin (set! rdi len.45) (set! rsi tmp.189) (set! rdx vec.47) (set! r15 tmp-ra.236) (jump L.vector-init-loop.20 rbp r15 rdi rsi rdx)))))))) (define L.make-vector.18 ((new-frames ())) (begin (set! tmp-ra.237 r15) (begin (set! tmp.42 rdi) (if (begin (if (begin (set! tmp.191 (bitwise-and tmp.42 7)) (= tmp.191 0)) (set! tmp.190 14) (set! tmp.190 6)) (!= tmp.190 6)) (begin (set! rdi tmp.42) (set! r15 tmp-ra.237) (jump L.make-init-vector.19 rbp r15 rdi)) (begin (set! rax 2110) (jump tmp-ra.237 rbp rax)))))) (define L.make-init-vector.19 ((new-frames ())) (begin (set! tmp-ra.238 r15) (begin (set! tmp.43 rdi) (if (begin (if (>= tmp.43 0) (set! tmp.192 14) (set! tmp.192 6)) (!= tmp.192 6)) (begin (begin (begin (begin (begin (begin (set! tmp.196 (arithmetic-shift-right tmp.43 3)) (set! tmp.195 (+ 1 tmp.196))) (set! tmp.194 (* tmp.195 8))) (set! tmp.193 (alloc tmp.194))) (set! tmp.87 (+ tmp.193 3))) (begin (mset! tmp.87 -3 tmp.43) (set! tmp.44 tmp.87))) (begin (set! rdi tmp.43) (set! rsi 0) (set! rdx tmp.44) (set! r15 tmp-ra.238) (jump L.vector-init-loop.20 rbp r15 rdi rsi rdx))) (begin (set! rax 3134) (jump tmp-ra.238 rbp rax)))))) (begin (set! tmp-ra.239 r15) (begin (set! rdi 0) (set! r15 tmp-ra.239) (jump L.make-vector.18 rbp r15 rdi)))))
                '(module
                     ((new-frames ()))
                   (define L.vector-init-loop.20
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.236 r15)
                       (set! len.45 rdi)
                       (set! i.46 rsi)
                       (set! vec.47 rdx)
                       (if (begin
                             (if (= len.45 i.46) (set! tmp.185 14) (set! tmp.185 6))
                             (!= tmp.185 6))
                           (begin (set! rax vec.47) (jump tmp-ra.236 rbp rax))
                           (begin
                             (set! tmp.188 i.46)
                             (set! tmp.188 (arithmetic-shift-right tmp.188 3))
                             (set! tmp.187 tmp.188)
                             (set! tmp.187 (* tmp.187 8))
                             (set! tmp.186 tmp.187)
                             (set! tmp.186 (+ tmp.186 5))
                             (mset! vec.47 tmp.186 0)
                             (set! tmp.189 i.46)
                             (set! tmp.189 (+ tmp.189 8))
                             (set! rdi len.45)
                             (set! rsi tmp.189)
                             (set! rdx vec.47)
                             (set! r15 tmp-ra.236)
                             (jump L.vector-init-loop.20 rbp r15 rdi rsi rdx)))))
                   (define L.make-vector.18
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.237 r15)
                       (set! tmp.42 rdi)
                       (if (begin
                             (if (begin
                                   (set! tmp.191 tmp.42)
                                   (set! tmp.191 (bitwise-and tmp.191 7))
                                   (= tmp.191 0))
                                 (set! tmp.190 14)
                                 (set! tmp.190 6))
                             (!= tmp.190 6))
                           (begin
                             (set! rdi tmp.42)
                             (set! r15 tmp-ra.237)
                             (jump L.make-init-vector.19 rbp r15 rdi))
                           (begin (set! rax 2110) (jump tmp-ra.237 rbp rax)))))
                   (define L.make-init-vector.19
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.238 r15)
                       (set! tmp.43 rdi)
                       (if (begin
                             (if (>= tmp.43 0) (set! tmp.192 14) (set! tmp.192 6))
                             (!= tmp.192 6))
                           (begin
                             (set! tmp.196 tmp.43)
                             (set! tmp.196 (arithmetic-shift-right tmp.196 3))
                             (set! tmp.195 1)
                             (set! tmp.195 (+ tmp.195 tmp.196))
                             (set! tmp.194 tmp.195)
                             (set! tmp.194 (* tmp.194 8))
                             (set! tmp.193 (alloc tmp.194))
                             (set! tmp.87 tmp.193)
                             (set! tmp.87 (+ tmp.87 3))
                             (mset! tmp.87 -3 tmp.43)
                             (set! tmp.44 tmp.87)
                             (set! rdi tmp.43)
                             (set! rsi 0)
                             (set! rdx tmp.44)
                             (set! r15 tmp-ra.238)
                             (jump L.vector-init-loop.20 rbp r15 rdi rsi rdx))
                           (begin (set! rax 3134) (jump tmp-ra.238 rbp rax)))))
                   (begin
                     (set! tmp-ra.239 r15)
                     (set! rdi 0)
                     (set! r15 tmp-ra.239)
                     (jump L.make-vector.18 rbp r15 rdi))))
  (check-equal? (select-instructions '(module ((new-frames ())) (define L.make-vector.28 ((new-frames ())) (begin (set! tmp-ra.256 r15) (begin (set! tmp.62 rdi) (if (begin (if (begin (set! tmp.135 (bitwise-and tmp.62 7)) (= tmp.135 0)) (set! tmp.134 14) (set! tmp.134 6)) (!= tmp.134 6)) (begin (set! rdi tmp.62) (set! r15 tmp-ra.256) (jump L.make-init-vector.29 rbp r15 rdi)) (begin (set! rax 2110) (jump tmp-ra.256 rbp rax)))))) (define L.make-init-vector.29 ((new-frames ())) (begin (set! tmp-ra.257 r15) (begin (set! tmp.63 rdi) (if (begin (if (>= tmp.63 0) (set! tmp.136 14) (set! tmp.136 6)) (!= tmp.136 6)) (begin (begin (begin (begin (begin (begin (set! tmp.140 (arithmetic-shift-right tmp.63 3)) (set! tmp.139 (+ 1 tmp.140))) (set! tmp.138 (* tmp.139 8))) (set! tmp.137 (alloc tmp.138))) (set! tmp.88 (+ tmp.137 3))) (begin (mset! tmp.88 -3 tmp.63) (set! tmp.64 tmp.88))) (begin (set! rdi tmp.63) (set! rsi 0) (set! rdx tmp.64) (set! r15 tmp-ra.257) (jump L.vector-init-loop.30 rbp r15 rdi rsi rdx))) (begin (set! rax 3134) (jump tmp-ra.257 rbp rax)))))) (define L.vector-init-loop.30 ((new-frames ())) (begin (set! tmp-ra.258 r15) (begin (set! len.65 rdi) (set! i.66 rsi) (set! vec.67 rdx) (if (begin (if (= len.65 i.66) (set! tmp.141 14) (set! tmp.141 6)) (!= tmp.141 6)) (begin (set! rax vec.67) (jump tmp-ra.258 rbp rax)) (begin (begin (begin (begin (set! tmp.144 (arithmetic-shift-right i.66 3)) (set! tmp.143 (* tmp.144 8))) (set! tmp.142 (+ tmp.143 5))) (mset! vec.67 tmp.142 0)) (begin (set! tmp.145 (+ i.66 8)) (begin (set! rdi len.65) (set! rsi tmp.145) (set! rdx vec.67) (set! r15 tmp-ra.258) (jump L.vector-init-loop.30 rbp r15 rdi rsi rdx)))))))) (begin (set! tmp-ra.259 r15) (begin (set! rdi 16) (set! r15 tmp-ra.259) (jump L.make-vector.28 rbp r15 rdi)))))
                '(module
                     ((new-frames ()))
                   (define L.make-vector.28
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.256 r15)
                       (set! tmp.62 rdi)
                       (if (begin
                             (if (begin
                                   (set! tmp.135 tmp.62)
                                   (set! tmp.135 (bitwise-and tmp.135 7))
                                   (= tmp.135 0))
                                 (set! tmp.134 14)
                                 (set! tmp.134 6))
                             (!= tmp.134 6))
                           (begin
                             (set! rdi tmp.62)
                             (set! r15 tmp-ra.256)
                             (jump L.make-init-vector.29 rbp r15 rdi))
                           (begin (set! rax 2110) (jump tmp-ra.256 rbp rax)))))
                   (define L.make-init-vector.29
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.257 r15)
                       (set! tmp.63 rdi)
                       (if (begin
                             (if (>= tmp.63 0) (set! tmp.136 14) (set! tmp.136 6))
                             (!= tmp.136 6))
                           (begin
                             (set! tmp.140 tmp.63)
                             (set! tmp.140 (arithmetic-shift-right tmp.140 3))
                             (set! tmp.139 1)
                             (set! tmp.139 (+ tmp.139 tmp.140))
                             (set! tmp.138 tmp.139)
                             (set! tmp.138 (* tmp.138 8))
                             (set! tmp.137 (alloc tmp.138))
                             (set! tmp.88 tmp.137)
                             (set! tmp.88 (+ tmp.88 3))
                             (mset! tmp.88 -3 tmp.63)
                             (set! tmp.64 tmp.88)
                             (set! rdi tmp.63)
                             (set! rsi 0)
                             (set! rdx tmp.64)
                             (set! r15 tmp-ra.257)
                             (jump L.vector-init-loop.30 rbp r15 rdi rsi rdx))
                           (begin (set! rax 3134) (jump tmp-ra.257 rbp rax)))))
                   (define L.vector-init-loop.30
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.258 r15)
                       (set! len.65 rdi)
                       (set! i.66 rsi)
                       (set! vec.67 rdx)
                       (if (begin
                             (if (= len.65 i.66) (set! tmp.141 14) (set! tmp.141 6))
                             (!= tmp.141 6))
                           (begin (set! rax vec.67) (jump tmp-ra.258 rbp rax))
                           (begin
                             (set! tmp.144 i.66)
                             (set! tmp.144 (arithmetic-shift-right tmp.144 3))
                             (set! tmp.143 tmp.144)
                             (set! tmp.143 (* tmp.143 8))
                             (set! tmp.142 tmp.143)
                             (set! tmp.142 (+ tmp.142 5))
                             (mset! vec.67 tmp.142 0)
                             (set! tmp.145 i.66)
                             (set! tmp.145 (+ tmp.145 8))
                             (set! rdi len.65)
                             (set! rsi tmp.145)
                             (set! rdx vec.67)
                             (set! r15 tmp-ra.258)
                             (jump L.vector-init-loop.30 rbp r15 rdi rsi rdx)))))
                   (begin
                     (set! tmp-ra.259 r15)
                     (set! rdi 16)
                     (set! r15 tmp-ra.259)
                     (jump L.make-vector.28 rbp r15 rdi))))
  (check-equal? (select-instructions '(module ((new-frames (()))) (define L.cdr.37 ((new-frames ())) (begin (set! tmp-ra.260 r15) (begin (set! tmp.80 rdi) (if (begin (if (begin (set! tmp.151 (bitwise-and tmp.80 7)) (= tmp.151 1)) (set! tmp.150 14) (set! tmp.150 6)) (!= tmp.150 6)) (begin (set! rax (mref tmp.80 7)) (jump tmp-ra.260 rbp rax)) (begin (set! rax 3390) (jump tmp-ra.260 rbp rax)))))) (define L.cons.38 ((new-frames ())) (begin (set! tmp-ra.261 r15) (begin (set! tmp.81 rdi) (set! tmp.82 rsi) (begin (begin (set! tmp.152 (alloc 16)) (set! tmp.89 (+ tmp.152 1))) (begin (mset! tmp.89 -1 tmp.81) (mset! tmp.89 7 tmp.82) (begin (set! rax tmp.89) (jump tmp-ra.261 rbp rax))))))) (begin (set! tmp-ra.262 r15) (begin (begin (return-point L.rp.58 (begin (set! rdi 56) (set! rsi 22) (set! r15 L.rp.58) (jump L.cons.38 rbp r15 rdi rsi))) (set! tmp.153 rax)) (begin (set! rdi tmp.153) (set! r15 tmp-ra.262) (jump L.cdr.37 rbp r15 rdi))))))
                '(module
                     ((new-frames (())))
                   (define L.cdr.37
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.260 r15)
                       (set! tmp.80 rdi)
                       (if (begin
                             (if (begin
                                   (set! tmp.151 tmp.80)
                                   (set! tmp.151 (bitwise-and tmp.151 7))
                                   (= tmp.151 1))
                                 (set! tmp.150 14)
                                 (set! tmp.150 6))
                             (!= tmp.150 6))
                           (begin (set! rax (mref tmp.80 7)) (jump tmp-ra.260 rbp rax))
                           (begin (set! rax 3390) (jump tmp-ra.260 rbp rax)))))
                   (define L.cons.38
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.261 r15)
                       (set! tmp.81 rdi)
                       (set! tmp.82 rsi)
                       (set! tmp.152 (alloc 16))
                       (set! tmp.89 tmp.152)
                       (set! tmp.89 (+ tmp.89 1))
                       (mset! tmp.89 -1 tmp.81)
                       (mset! tmp.89 7 tmp.82)
                       (set! rax tmp.89)
                       (jump tmp-ra.261 rbp rax)))
                   (begin
                     (set! tmp-ra.262 r15)
                     (return-point L.rp.58
                                   (begin
                                     (set! rdi 56)
                                     (set! rsi 22)
                                     (set! r15 L.rp.58)
                                     (jump L.cons.38 rbp r15 rdi rsi)))
                     (set! tmp.153 rax)
                     (set! rdi tmp.153)
                     (set! r15 tmp-ra.262)
                     (jump L.cdr.37 rbp r15 rdi))))
  (check-equal? (select-instructions '(module ((new-frames ())) (begin (set! tmp-ra.277 r15) (if (begin (set! x.1 (alloc 8)) (set! y.1 (alloc 16)) (set! z.1 0) (begin (begin (begin (set! t.1 32) (begin (set! tmp.165 (+ t.1 8)) (set! tmp.164 (+ t.1 tmp.165)))) (begin (set! tmp.215 (alloc tmp.164)) (mset! x.1 0 tmp.215))) (mset! y.1 z.1 18) (begin (set! tmp.166 (+ z.1 8)) (mset! y.1 tmp.166 40)) (begin (set! tmp.167 (mref y.1 z.1)) (begin (begin (set! tmp.169 (+ z.1 8)) (set! tmp.168 (mref y.1 tmp.169))) (= tmp.167 tmp.168))))) (begin (set! rax 8) (jump tmp-ra.277 rbp rax)) (begin (set! rax 16) (jump tmp-ra.277 rbp rax))))))
                '(module
                     ((new-frames ()))
                   (begin
                     (set! tmp-ra.277 r15)
                     (if (begin
                           (set! x.1 (alloc 8))
                           (set! y.1 (alloc 16))
                           (set! z.1 0)
                           (begin
                             (set! t.1 32)
                             (set! tmp.165 t.1)
                             (set! tmp.165 (+ tmp.165 8))
                             (set! tmp.164 t.1)
                             (set! tmp.164 (+ tmp.164 tmp.165))
                             (set! tmp.215 (alloc tmp.164))
                             (mset! x.1 0 tmp.215)
                             (mset! y.1 z.1 18)
                             (set! tmp.166 z.1)
                             (set! tmp.166 (+ tmp.166 8))
                             (mset! y.1 tmp.166 40)
                             (begin
                               (set! tmp.167 (mref y.1 z.1))
                               (begin
                                 (set! tmp.169 z.1)
                                 (set! tmp.169 (+ tmp.169 8))
                                 (set! tmp.168 (mref y.1 tmp.169))
                                 (= tmp.167 tmp.168)))))
                         (begin (set! rax 8) (jump tmp-ra.277 rbp rax))
                         (begin (set! rax 16) (jump tmp-ra.277 rbp rax))))))
  (check-equal? (select-instructions '(module ((new-frames (()))) (define L.vector-ref.32 ((new-frames ())) (begin (set! tmp-ra.271 r15) (begin (set! tmp.70 rdi) (set! tmp.71 rsi) (if (begin (if (begin (set! tmp.100 (bitwise-and tmp.71 7)) (= tmp.100 0)) (set! tmp.99 14) (set! tmp.99 6)) (!= tmp.99 6)) (if (begin (if (begin (set! tmp.102 (bitwise-and tmp.70 7)) (= tmp.102 3)) (set! tmp.101 14) (set! tmp.101 6)) (!= tmp.101 6)) (begin (set! rdi tmp.70) (set! rsi tmp.71) (set! r15 tmp-ra.271) (jump L.unsafe-vector-ref.33 rbp r15 rdi rsi)) (begin (set! rax 2878) (jump tmp-ra.271 rbp rax))) (begin (set! rax 2878) (jump tmp-ra.271 rbp rax)))))) (define L.make-vector.34 ((new-frames ())) (begin (set! tmp-ra.272 r15) (begin (set! tmp.74 rdi) (if (begin (if (begin (set! tmp.104 (bitwise-and tmp.74 7)) (= tmp.104 0)) (set! tmp.103 14) (set! tmp.103 6)) (!= tmp.103 6)) (begin (set! rdi tmp.74) (set! r15 tmp-ra.272) (jump L.make-init-vector.35 rbp r15 rdi)) (begin (set! rax 2110) (jump tmp-ra.272 rbp rax)))))) (define L.vector-init-loop.36 ((new-frames ())) (begin (set! tmp-ra.273 r15) (begin (set! len.77 rdi) (set! i.78 rsi) (set! vec.79 rdx) (if (begin (if (= len.77 i.78) (set! tmp.105 14) (set! tmp.105 6)) (!= tmp.105 6)) (begin (set! rax vec.79) (jump tmp-ra.273 rbp rax)) (begin (begin (begin (begin (set! tmp.108 (arithmetic-shift-right i.78 3)) (set! tmp.107 (* tmp.108 8))) (set! tmp.106 (+ tmp.107 5))) (mset! vec.79 tmp.106 0)) (begin (set! tmp.109 (+ i.78 8)) (begin (set! rdi len.77) (set! rsi tmp.109) (set! rdx vec.79) (set! r15 tmp-ra.273) (jump L.vector-init-loop.36 rbp r15 rdi rsi rdx)))))))) (define L.unsafe-vector-ref.33 ((new-frames ())) (begin (set! tmp-ra.274 r15) (begin (set! tmp.72 rdi) (set! tmp.73 rsi) (if (begin (if (begin (set! tmp.111 (mref tmp.72 -3)) (< tmp.73 tmp.111)) (set! tmp.110 14) (set! tmp.110 6)) (!= tmp.110 6)) (if (begin (if (>= tmp.73 0) (set! tmp.112 14) (set! tmp.112 6)) (!= tmp.112 6)) (begin (begin (begin (set! tmp.115 (arithmetic-shift-right tmp.73 3)) (set! tmp.114 (* tmp.115 8))) (set! tmp.113 (+ tmp.114 5))) (begin (set! rax (mref tmp.72 tmp.113)) (jump tmp-ra.274 rbp rax))) (begin (set! rax 2878) (jump tmp-ra.274 rbp rax))) (begin (set! rax 2878) (jump tmp-ra.274 rbp rax)))))) (define L.make-init-vector.35 ((new-frames ())) (begin (set! tmp-ra.275 r15) (begin (set! tmp.75 rdi) (if (begin (if (>= tmp.75 0) (set! tmp.116 14) (set! tmp.116 6)) (!= tmp.116 6)) (begin (begin (begin (begin (begin (begin (set! tmp.120 (arithmetic-shift-right tmp.75 3)) (set! tmp.119 (+ 1 tmp.120))) (set! tmp.118 (* tmp.119 8))) (set! tmp.117 (alloc tmp.118))) (set! tmp.83 (+ tmp.117 3))) (begin (mset! tmp.83 -3 tmp.75) (set! tmp.76 tmp.83))) (begin (set! rdi tmp.75) (set! rsi 0) (set! rdx tmp.76) (set! r15 tmp-ra.275) (jump L.vector-init-loop.36 rbp r15 rdi rsi rdx))) (begin (set! rax 3134) (jump tmp-ra.275 rbp rax)))))) (begin (set! tmp-ra.276 r15) (begin (begin (return-point L.rp.61 (begin (set! rdi 16) (set! r15 L.rp.61) (jump L.make-vector.34 rbp r15 rdi))) (set! tmp.121 rax)) (begin (set! rdi tmp.121) (set! rsi 0) (set! r15 tmp-ra.276) (jump L.vector-ref.32 rbp r15 rdi rsi))))))
                '(module
                     ((new-frames (())))
                   (define L.vector-ref.32
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.271 r15)
                       (set! tmp.70 rdi)
                       (set! tmp.71 rsi)
                       (if (begin
                             (if (begin
                                   (set! tmp.100 tmp.71)
                                   (set! tmp.100 (bitwise-and tmp.100 7))
                                   (= tmp.100 0))
                                 (set! tmp.99 14)
                                 (set! tmp.99 6))
                             (!= tmp.99 6))
                           (if (begin
                                 (if (begin
                                       (set! tmp.102 tmp.70)
                                       (set! tmp.102 (bitwise-and tmp.102 7))
                                       (= tmp.102 3))
                                     (set! tmp.101 14)
                                     (set! tmp.101 6))
                                 (!= tmp.101 6))
                               (begin
                                 (set! rdi tmp.70)
                                 (set! rsi tmp.71)
                                 (set! r15 tmp-ra.271)
                                 (jump L.unsafe-vector-ref.33 rbp r15 rdi rsi))
                               (begin (set! rax 2878) (jump tmp-ra.271 rbp rax)))
                           (begin (set! rax 2878) (jump tmp-ra.271 rbp rax)))))
                   (define L.make-vector.34
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.272 r15)
                       (set! tmp.74 rdi)
                       (if (begin
                             (if (begin
                                   (set! tmp.104 tmp.74)
                                   (set! tmp.104 (bitwise-and tmp.104 7))
                                   (= tmp.104 0))
                                 (set! tmp.103 14)
                                 (set! tmp.103 6))
                             (!= tmp.103 6))
                           (begin
                             (set! rdi tmp.74)
                             (set! r15 tmp-ra.272)
                             (jump L.make-init-vector.35 rbp r15 rdi))
                           (begin (set! rax 2110) (jump tmp-ra.272 rbp rax)))))
                   (define L.vector-init-loop.36
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.273 r15)
                       (set! len.77 rdi)
                       (set! i.78 rsi)
                       (set! vec.79 rdx)
                       (if (begin
                             (if (= len.77 i.78) (set! tmp.105 14) (set! tmp.105 6))
                             (!= tmp.105 6))
                           (begin (set! rax vec.79) (jump tmp-ra.273 rbp rax))
                           (begin
                             (set! tmp.108 i.78)
                             (set! tmp.108 (arithmetic-shift-right tmp.108 3))
                             (set! tmp.107 tmp.108)
                             (set! tmp.107 (* tmp.107 8))
                             (set! tmp.106 tmp.107)
                             (set! tmp.106 (+ tmp.106 5))
                             (mset! vec.79 tmp.106 0)
                             (set! tmp.109 i.78)
                             (set! tmp.109 (+ tmp.109 8))
                             (set! rdi len.77)
                             (set! rsi tmp.109)
                             (set! rdx vec.79)
                             (set! r15 tmp-ra.273)
                             (jump L.vector-init-loop.36 rbp r15 rdi rsi rdx)))))
                   (define L.unsafe-vector-ref.33
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.274 r15)
                       (set! tmp.72 rdi)
                       (set! tmp.73 rsi)
                       (if (begin
                             (if (begin (set! tmp.111 (mref tmp.72 -3)) (< tmp.73 tmp.111))
                                 (set! tmp.110 14)
                                 (set! tmp.110 6))
                             (!= tmp.110 6))
                           (if (begin
                                 (if (>= tmp.73 0) (set! tmp.112 14) (set! tmp.112 6))
                                 (!= tmp.112 6))
                               (begin
                                 (set! tmp.115 tmp.73)
                                 (set! tmp.115 (arithmetic-shift-right tmp.115 3))
                                 (set! tmp.114 tmp.115)
                                 (set! tmp.114 (* tmp.114 8))
                                 (set! tmp.113 tmp.114)
                                 (set! tmp.113 (+ tmp.113 5))
                                 (set! rax (mref tmp.72 tmp.113))
                                 (jump tmp-ra.274 rbp rax))
                               (begin (set! rax 2878) (jump tmp-ra.274 rbp rax)))
                           (begin (set! rax 2878) (jump tmp-ra.274 rbp rax)))))
                   (define L.make-init-vector.35
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.275 r15)
                       (set! tmp.75 rdi)
                       (if (begin
                             (if (>= tmp.75 0) (set! tmp.116 14) (set! tmp.116 6))
                             (!= tmp.116 6))
                           (begin
                             (set! tmp.120 tmp.75)
                             (set! tmp.120 (arithmetic-shift-right tmp.120 3))
                             (set! tmp.119 1)
                             (set! tmp.119 (+ tmp.119 tmp.120))
                             (set! tmp.118 tmp.119)
                             (set! tmp.118 (* tmp.118 8))
                             (set! tmp.117 (alloc tmp.118))
                             (set! tmp.83 tmp.117)
                             (set! tmp.83 (+ tmp.83 3))
                             (mset! tmp.83 -3 tmp.75)
                             (set! tmp.76 tmp.83)
                             (set! rdi tmp.75)
                             (set! rsi 0)
                             (set! rdx tmp.76)
                             (set! r15 tmp-ra.275)
                             (jump L.vector-init-loop.36 rbp r15 rdi rsi rdx))
                           (begin (set! rax 3134) (jump tmp-ra.275 rbp rax)))))
                   (begin
                     (set! tmp-ra.276 r15)
                     (return-point L.rp.61
                                   (begin
                                     (set! rdi 16)
                                     (set! r15 L.rp.61)
                                     (jump L.make-vector.34 rbp r15 rdi)))
                     (set! tmp.121 rax)
                     (set! rdi tmp.121)
                     (set! rsi 0)
                     (set! r15 tmp-ra.276)
                     (jump L.vector-ref.32 rbp r15 rdi rsi))))
  (check-equal? (select-instructions '(module ((new-frames ())) (define L.addup.1 ((new-frames ())) (begin (set! tmp-ra.240 r15) (begin (begin (set! y.1 (alloc 16)) (begin (set! x.2 8) (set! x.3 16) (begin (set! tmp.214 (+ x.2 x.3)) (mset! y.1 8 tmp.214))) (begin (set! rax (mref y.1 8)) (jump tmp-ra.240 rbp rax)))))) (begin (set! tmp-ra.241 r15) (begin (set! r15 tmp-ra.241) (jump L.addup.1 rbp r15)))))
                '(module
                     ((new-frames ()))
                   (define L.addup.1
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.240 r15)
                       (set! y.1 (alloc 16))
                       (set! x.2 8)
                       (set! x.3 16)
                       (set! tmp.214 x.2)
                       (set! tmp.214 (+ tmp.214 x.3))
                       (mset! y.1 8 tmp.214)
                       (set! rax (mref y.1 8))
                       (jump tmp-ra.240 rbp rax)))
                   (begin (set! tmp-ra.241 r15) (set! r15 tmp-ra.241) (jump L.addup.1 rbp r15))))
  (check-equal? (select-instructions '(module ((new-frames (()))) (define L.car.9 ((new-frames ())) (begin (set! tmp-ra.252 r15) (begin (set! tmp.25 rdi) (if (begin (if (begin (set! tmp.147 (bitwise-and tmp.25 7)) (= tmp.147 1)) (set! tmp.146 14) (set! tmp.146 6)) (!= tmp.146 6)) (begin (set! rax (mref tmp.25 -1)) (jump tmp-ra.252 rbp rax)) (begin (set! rax 3134) (jump tmp-ra.252 rbp rax)))))) (define L.cons.10 ((new-frames ())) (begin (set! tmp-ra.253 r15) (begin (set! tmp.26 rdi) (set! tmp.27 rsi) (begin (begin (set! tmp.148 (alloc 16)) (set! tmp.85 (+ tmp.148 1))) (begin (mset! tmp.85 -1 tmp.26) (mset! tmp.85 7 tmp.27) (begin (set! rax tmp.85) (jump tmp-ra.253 rbp rax))))))) (begin (set! tmp-ra.254 r15) (begin (begin (return-point L.rp.57 (begin (set! rdi 56) (set! rsi 22) (set! r15 L.rp.57) (jump L.cons.10 rbp r15 rdi rsi))) (set! tmp.149 rax)) (begin (set! rdi tmp.149) (set! r15 tmp-ra.254) (jump L.car.9 rbp r15 rdi))))))
                '(module
                     ((new-frames (())))
                   (define L.car.9
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.252 r15)
                       (set! tmp.25 rdi)
                       (if (begin
                             (if (begin
                                   (set! tmp.147 tmp.25)
                                   (set! tmp.147 (bitwise-and tmp.147 7))
                                   (= tmp.147 1))
                                 (set! tmp.146 14)
                                 (set! tmp.146 6))
                             (!= tmp.146 6))
                           (begin (set! rax (mref tmp.25 -1)) (jump tmp-ra.252 rbp rax))
                           (begin (set! rax 3134) (jump tmp-ra.252 rbp rax)))))
                   (define L.cons.10
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.253 r15)
                       (set! tmp.26 rdi)
                       (set! tmp.27 rsi)
                       (set! tmp.148 (alloc 16))
                       (set! tmp.85 tmp.148)
                       (set! tmp.85 (+ tmp.85 1))
                       (mset! tmp.85 -1 tmp.26)
                       (mset! tmp.85 7 tmp.27)
                       (set! rax tmp.85)
                       (jump tmp-ra.253 rbp rax)))
                   (begin
                     (set! tmp-ra.254 r15)
                     (return-point L.rp.57
                                   (begin
                                     (set! rdi 56)
                                     (set! rsi 22)
                                     (set! r15 L.rp.57)
                                     (jump L.cons.10 rbp r15 rdi rsi)))
                     (set! tmp.149 rax)
                     (set! rdi tmp.149)
                     (set! r15 tmp-ra.254)
                     (jump L.car.9 rbp r15 rdi))))
  (check-equal? (select-instructions '(module ((new-frames ())) (begin (set! sz.0 16) (set! x.8 (alloc sz.0)) (mset! x.8 0 2147483648) (mset! x.8 8 8) (set! y.1 (mref x.8 0)) (set! i.0 8) (set! y.2 (mref x.8 i.0)) (set! rax (+ y.1 y.2)) (jump r15))))
                '(module
                     ((new-frames ()))
                   (begin
                     (set! sz.0 16)
                     (set! x.8 (alloc sz.0))
                     (mset! x.8 0 2147483648)
                     (mset! x.8 8 8)
                     (set! y.1 (mref x.8 0))
                     (set! i.0 8)
                     (set! y.2 (mref x.8 i.0))
                     (set! rax y.1)
                     (set! rax (+ rax y.2))
                     (jump r15))))
  (check-equal? (select-instructions '(module ((new-frames (()))) (define L.*.24 ((new-frames ())) (begin (set! tmp-ra.245 r15) (begin (set! tmp.54 rdi) (set! tmp.55 rsi) (if (begin (if (begin (set! tmp.198 (bitwise-and tmp.54 7)) (= tmp.198 0)) (set! tmp.197 14) (set! tmp.197 6)) (!= tmp.197 6)) (if (begin (if (begin (set! tmp.200 (bitwise-and tmp.55 7)) (= tmp.200 0)) (set! tmp.199 14) (set! tmp.199 6)) (!= tmp.199 6)) (begin (set! tmp.201 (arithmetic-shift-right tmp.55 3)) (begin (set! rax (* tmp.54 tmp.201)) (jump tmp-ra.245 rbp rax))) (begin (set! rax 318) (jump tmp-ra.245 rbp rax))) (begin (set! rax 318) (jump tmp-ra.245 rbp rax)))))) (define L.cons.23 ((new-frames ())) (begin (set! tmp-ra.246 r15) (begin (set! tmp.52 rdi) (set! tmp.53 rsi) (begin (begin (set! tmp.202 (alloc 16)) (set! tmp.84 (+ tmp.202 1))) (begin (mset! tmp.84 -1 tmp.52) (mset! tmp.84 7 tmp.53) (begin (set! rax tmp.84) (jump tmp-ra.246 rbp rax))))))) (begin (set! tmp-ra.247 r15) (begin (begin (return-point L.rp.56 (begin (set! rdi 56) (set! rsi 64) (set! r15 L.rp.56) (jump L.*.24 rbp r15 rdi rsi))) (set! tmp.203 rax)) (begin (set! rdi tmp.203) (set! rsi 22) (set! r15 tmp-ra.247) (jump L.cons.23 rbp r15 rdi rsi))))))
                '(module
                     ((new-frames (())))
                   (define L.*.24
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.245 r15)
                       (set! tmp.54 rdi)
                       (set! tmp.55 rsi)
                       (if (begin
                             (if (begin
                                   (set! tmp.198 tmp.54)
                                   (set! tmp.198 (bitwise-and tmp.198 7))
                                   (= tmp.198 0))
                                 (set! tmp.197 14)
                                 (set! tmp.197 6))
                             (!= tmp.197 6))
                           (if (begin
                                 (if (begin
                                       (set! tmp.200 tmp.55)
                                       (set! tmp.200 (bitwise-and tmp.200 7))
                                       (= tmp.200 0))
                                     (set! tmp.199 14)
                                     (set! tmp.199 6))
                                 (!= tmp.199 6))
                               (begin
                                 (set! tmp.201 tmp.55)
                                 (set! tmp.201 (arithmetic-shift-right tmp.201 3))
                                 (set! rax tmp.54)
                                 (set! rax (* rax tmp.201))
                                 (jump tmp-ra.245 rbp rax))
                               (begin (set! rax 318) (jump tmp-ra.245 rbp rax)))
                           (begin (set! rax 318) (jump tmp-ra.245 rbp rax)))))
                   (define L.cons.23
                     ((new-frames ()))
                     (begin
                       (set! tmp-ra.246 r15)
                       (set! tmp.52 rdi)
                       (set! tmp.53 rsi)
                       (set! tmp.202 (alloc 16))
                       (set! tmp.84 tmp.202)
                       (set! tmp.84 (+ tmp.84 1))
                       (mset! tmp.84 -1 tmp.52)
                       (mset! tmp.84 7 tmp.53)
                       (set! rax tmp.84)
                       (jump tmp-ra.246 rbp rax)))
                   (begin
                     (set! tmp-ra.247 r15)
                     (return-point L.rp.56
                                   (begin
                                     (set! rdi 56)
                                     (set! rsi 64)
                                     (set! r15 L.rp.56)
                                     (jump L.*.24 rbp r15 rdi rsi)))
                     (set! tmp.203 rax)
                     (set! rdi tmp.203)
                     (set! rsi 22)
                     (set! r15 tmp-ra.247)
                     (jump L.cons.23 rbp r15 rdi rsi))))
  #;(check-equal? (select-instructions '(module
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
                       (jump L.add-and-multiply.11 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2))))
  (check-equal? (select-instructions '(module
                                          ((new-frames ()))
                                        (define L.f.1
                                          ((new-frames (())))
                                          (begin
                                            (set! tmp-ra.50 r15)
                                            (begin
                                              (set! x.1 rdi)
                                              (set! x.2 rsi)
                                              (begin
                                                (begin
                                                  (begin (set! tmp.39 (+ 10 6)) (set! tmp.38 (alloc tmp.39)))
                                                  (begin
                                                    (begin
                                                      (return-point L.rp.21
                                                                    (begin (set! r15 L.rp.21) (jump L.g.1 rbp r15)))
                                                      (set! tmp.40 rax))
                                                    (if (true)
                                                        (mset! tmp.38 tmp.40 x.1)
                                                        (mset! tmp.38 tmp.40 x.2))))
                                                (begin
                                                  (begin (set! tmp.42 (+ 10 6)) (set! tmp.41 (alloc tmp.42)))
                                                  (begin
                                                    (set! tmp.43 (bitwise-and 8 8))
                                                    (begin
                                                      (set! rax (mref tmp.41 tmp.43))
                                                      (jump tmp-ra.50 rbp rax))))))))
                                        (define L.g.1
                                          ((new-frames ()))
                                          (begin
                                            (set! tmp-ra.51 r15)
                                            (begin (begin (set! rax 8) (jump tmp-ra.51 rbp rax)))))
                                        (begin
                                          (set! tmp-ra.52 r15)
                                          (begin
                                            (set! rdi 1)
                                            (set! rsi 2)
                                            (set! r15 tmp-ra.52)
                                            (jump L.f.1 rbp r15 rdi rsi)))))
                '(module
                     ((new-frames ()))
                   (define L.f.1
                     ((new-frames (())))
                     (begin
                       (set! tmp-ra.50 r15)
                       (set! x.1 rdi)
                       (set! x.2 rsi)
                       (set! tmp.39 10)
                       (set! tmp.39 (+ tmp.39 6))
                       (set! tmp.38 (alloc tmp.39))
                       (return-point L.rp.21 (begin (set! r15 L.rp.21) (jump L.g.1 rbp r15)))
                       (set! tmp.40 rax)
                       (if (true) (mset! tmp.38 tmp.40 x.1) (mset! tmp.38 tmp.40 x.2))
                       (set! tmp.42 10)
                       (set! tmp.42 (+ tmp.42 6))
                       (set! tmp.41 (alloc tmp.42))
                       (set! tmp.43 8)
                       (set! tmp.43 (bitwise-and tmp.43 8))
                       (set! rax (mref tmp.41 tmp.43))
                       (jump tmp-ra.50 rbp rax)))
                   (define L.g.1
                     ((new-frames ()))
                     (begin (set! tmp-ra.51 r15) (set! rax 8) (jump tmp-ra.51 rbp rax)))
                   (begin
                     (set! tmp-ra.52 r15)
                     (set! rdi 1)
                     (set! rsi 2)
                     (set! r15 tmp-ra.52)
                     (jump L.f.1 rbp r15 rdi rsi))))

  (check-equal? (interp-asm-pred-lang-v8 (select-instructions
                                          '(module
                                               ((new-frames (() () () ())))
                                             (define L.+.12
                                               ((new-frames ()))
                                               (begin
                                                 (set! tmp-ra.95 r15)
                                                 (begin
                                                   (set! tmp.19 rdi)
                                                   (set! tmp.20 rsi)
                                                   (if (begin
                                                         (if (begin (set! tmp.55 (bitwise-and tmp.20 7)) (= tmp.55 0))
                                                             (set! tmp.54 14)
                                                             (set! tmp.54 6))
                                                         (!= tmp.54 6))
                                                       (if (begin
                                                             (if (begin (set! tmp.57 (bitwise-and tmp.19 7)) (= tmp.57 0))
                                                                 (set! tmp.56 14)
                                                                 (set! tmp.56 6))
                                                             (!= tmp.56 6))
                                                           (begin (set! rax (+ tmp.19 tmp.20)) (jump tmp-ra.95 rbp rax))
                                                           (begin (set! rax 574) (jump tmp-ra.95 rbp rax)))
                                                       (begin (set! rax 574) (jump tmp-ra.95 rbp rax))))))
                                             (define L.void?.11
                                               ((new-frames ()))
                                               (begin
                                                 (set! tmp-ra.96 r15)
                                                 (begin
                                                   (set! tmp.43 rdi)
                                                   (if (begin (set! tmp.58 (bitwise-and tmp.43 255)) (= tmp.58 30))
                                                       (begin (set! rax 14) (jump tmp-ra.96 rbp rax))
                                                       (begin (set! rax 6) (jump tmp-ra.96 rbp rax))))))
                                             (define L.unsafe-vector-ref.3
                                               ((new-frames ()))
                                               (begin
                                                 (set! tmp-ra.97 r15)
                                                 (begin
                                                   (set! tmp.14 rdi)
                                                   (set! tmp.15 rsi)
                                                   (if (begin
                                                         (if (begin (set! tmp.60 (mref tmp.14 -3)) (< tmp.15 tmp.60))
                                                             (set! tmp.59 14)
                                                             (set! tmp.59 6))
                                                         (!= tmp.59 6))
                                                       (if (begin
                                                             (if (>= tmp.15 0) (set! tmp.61 14) (set! tmp.61 6))
                                                             (!= tmp.61 6))
                                                           (begin
                                                             (begin
                                                               (begin
                                                                 (set! tmp.64 (arithmetic-shift-right tmp.15 3))
                                                                 (set! tmp.63 (* tmp.64 8)))
                                                               (set! tmp.62 (+ tmp.63 5)))
                                                             (begin (set! rax (mref tmp.14 tmp.62)) (jump tmp-ra.97 rbp rax)))
                                                           (begin (set! rax 2878) (jump tmp-ra.97 rbp rax)))
                                                       (begin (set! rax 2878) (jump tmp-ra.97 rbp rax))))))
                                             (define L.vector-ref.10
                                               ((new-frames ()))
                                               (begin
                                                 (set! tmp-ra.98 r15)
                                                 (begin
                                                   (set! tmp.36 rdi)
                                                   (set! tmp.37 rsi)
                                                   (if (begin
                                                         (if (begin (set! tmp.66 (bitwise-and tmp.37 7)) (= tmp.66 0))
                                                             (set! tmp.65 14)
                                                             (set! tmp.65 6))
                                                         (!= tmp.65 6))
                                                       (if (begin
                                                             (if (begin (set! tmp.68 (bitwise-and tmp.36 7)) (= tmp.68 3))
                                                                 (set! tmp.67 14)
                                                                 (set! tmp.67 6))
                                                             (!= tmp.67 6))
                                                           (begin
                                                             (set! rsi tmp.37)
                                                             (set! rdi tmp.36)
                                                             (set! r15 tmp-ra.98)
                                                             (jump L.unsafe-vector-ref.3 rbp r15 rdi rsi))
                                                           (begin (set! rax 2878) (jump tmp-ra.98 rbp rax)))
                                                       (begin (set! rax 2878) (jump tmp-ra.98 rbp rax))))))
                                             (define L.unsafe-vector-set!.2
                                               ((new-frames ()))
                                               (begin
                                                 (set! tmp-ra.99 r15)
                                                 (begin
                                                   (set! tmp.9 rdi)
                                                   (set! tmp.10 rsi)
                                                   (set! tmp.11 rdx)
                                                   (if (begin
                                                         (if (begin (set! tmp.70 (mref tmp.9 -3)) (< tmp.10 tmp.70))
                                                             (set! tmp.69 14)
                                                             (set! tmp.69 6))
                                                         (!= tmp.69 6))
                                                       (if (begin
                                                             (if (>= tmp.10 0) (set! tmp.71 14) (set! tmp.71 6))
                                                             (!= tmp.71 6))
                                                           (begin
                                                             (begin
                                                               (begin
                                                                 (begin
                                                                   (set! tmp.74 (arithmetic-shift-right tmp.10 3))
                                                                   (set! tmp.73 (* tmp.74 8)))
                                                                 (set! tmp.72 (+ tmp.73 5)))
                                                               (mset! tmp.9 tmp.72 tmp.11))
                                                             (begin (set! rax 30) (jump tmp-ra.99 rbp rax)))
                                                           (begin (set! rax 2622) (jump tmp-ra.99 rbp rax)))
                                                       (begin (set! rax 2622) (jump tmp-ra.99 rbp rax))))))
                                             (define L.vector-set!.9
                                               ((new-frames ()))
                                               (begin
                                                 (set! tmp-ra.100 r15)
                                                 (begin
                                                   (set! tmp.33 rdi)
                                                   (set! tmp.34 rsi)
                                                   (set! tmp.35 rdx)
                                                   (if (begin
                                                         (if (begin (set! tmp.76 (bitwise-and tmp.34 7)) (= tmp.76 0))
                                                             (set! tmp.75 14)
                                                             (set! tmp.75 6))
                                                         (!= tmp.75 6))
                                                       (if (begin
                                                             (if (begin (set! tmp.78 (bitwise-and tmp.33 7)) (= tmp.78 3))
                                                                 (set! tmp.77 14)
                                                                 (set! tmp.77 6))
                                                             (!= tmp.77 6))
                                                           (begin
                                                             (set! rdx tmp.35)
                                                             (set! rsi tmp.34)
                                                             (set! rdi tmp.33)
                                                             (set! r15 tmp-ra.100)
                                                             (jump L.unsafe-vector-set!.2 rbp r15 rdi rsi rdx))
                                                           (begin (set! rax 2622) (jump tmp-ra.100 rbp rax)))
                                                       (begin (set! rax 2622) (jump tmp-ra.100 rbp rax))))))
                                             (define L.vector-init-loop.7
                                               ((new-frames ()))
                                               (begin
                                                 (set! tmp-ra.101 r15)
                                                 (begin
                                                   (set! len.6 rdi)
                                                   (set! i.8 rsi)
                                                   (set! vec.7 rdx)
                                                   (if (begin
                                                         (if (= len.6 i.8) (set! tmp.79 14) (set! tmp.79 6))
                                                         (!= tmp.79 6))
                                                       (begin (set! rax vec.7) (jump tmp-ra.101 rbp rax))
                                                       (begin
                                                         (begin
                                                           (begin
                                                             (begin
                                                               (set! tmp.82 (arithmetic-shift-right i.8 3))
                                                               (set! tmp.81 (* tmp.82 8)))
                                                             (set! tmp.80 (+ tmp.81 5)))
                                                           (mset! vec.7 tmp.80 0))
                                                         (begin
                                                           (set! tmp.83 (+ i.8 8))
                                                           (begin
                                                             (set! rdx vec.7)
                                                             (set! rsi tmp.83)
                                                             (set! rdi len.6)
                                                             (set! r15 tmp-ra.101)
                                                             (jump L.vector-init-loop.7 rbp r15 rdi rsi rdx))))))))
                                             (define L.make-init-vector.1
                                               ((new-frames ()))
                                               (begin
                                                 (set! tmp-ra.102 r15)
                                                 (begin
                                                   (set! tmp.4 rdi)
                                                   (if (begin
                                                         (if (>= tmp.4 0) (set! tmp.84 14) (set! tmp.84 6))
                                                         (!= tmp.84 6))
                                                       (begin
                                                         (begin
                                                           (begin
                                                             (begin
                                                               (begin
                                                                 (begin
                                                                   (set! tmp.88 (arithmetic-shift-right tmp.4 3))
                                                                   (set! tmp.87 (+ 1 tmp.88)))
                                                                 (set! tmp.86 (* tmp.87 8)))
                                                               (set! tmp.85 (alloc tmp.86)))
                                                             (set! tmp.53 (+ tmp.85 3)))
                                                           (begin (mset! tmp.53 -3 tmp.4) (set! tmp.5 tmp.53)))
                                                         (begin
                                                           (set! rdx tmp.5)
                                                           (set! rsi 0)
                                                           (set! rdi tmp.4)
                                                           (set! r15 tmp-ra.102)
                                                           (jump L.vector-init-loop.7 rbp r15 rdi rsi rdx)))
                                                       (begin (set! rax 3134) (jump tmp-ra.102 rbp rax))))))
                                             (define L.make-vector.8
                                               ((new-frames ()))
                                               (begin
                                                 (set! tmp-ra.103 r15)
                                                 (begin
                                                   (set! tmp.31 rdi)
                                                   (if (begin
                                                         (if (begin (set! tmp.90 (bitwise-and tmp.31 7)) (= tmp.90 0))
                                                             (set! tmp.89 14)
                                                             (set! tmp.89 6))
                                                         (!= tmp.89 6))
                                                       (begin
                                                         (set! rdi tmp.31)
                                                         (set! r15 tmp-ra.103)
                                                         (jump L.make-init-vector.1 rbp r15 rdi))
                                                       (begin (set! rax 2110) (jump tmp-ra.103 rbp rax))))))
                                             (define L.v.4
                                               ((new-frames ()))
                                               (begin
                                                 (set! tmp-ra.104 r15)
                                                 (begin
                                                   (begin
                                                     (set! rdi 24)
                                                     (set! r15 tmp-ra.104)
                                                     (jump L.make-vector.8 rbp r15 rdi)))))
                                             (define L.set-first.5
                                               ((new-frames ()))
                                               (begin
                                                 (set! tmp-ra.105 r15)
                                                 (begin
                                                   (set! vec.1 rdi)
                                                   (begin
                                                     (set! rdx 336)
                                                     (set! rsi 0)
                                                     (set! rdi vec.1)
                                                     (set! r15 tmp-ra.105)
                                                     (jump L.vector-set!.9 rbp r15 rdi rsi rdx)))))
                                             (define L.get-first.6
                                               ((new-frames ()))
                                               (begin
                                                 (set! tmp-ra.106 r15)
                                                 (begin
                                                   (set! vec.2 rdi)
                                                   (begin
                                                     (set! rsi 0)
                                                     (set! rdi vec.2)
                                                     (set! r15 tmp-ra.106)
                                                     (jump L.vector-ref.10 rbp r15 rdi rsi)))))
                                             (begin
                                               (set! tmp-ra.107 r15)
                                               (begin
                                                 (begin
                                                   (return-point L.rp.13 (begin (set! r15 L.rp.13) (jump L.v.4 rbp r15)))
                                                   (set! vec.3 rax))
                                                 (begin
                                                   (if (begin
                                                         (begin
                                                           (begin
                                                             (return-point L.rp.14
                                                                           (begin
                                                                             (set! rdi vec.3)
                                                                             (set! r15 L.rp.14)
                                                                             (jump L.set-first.5 rbp r15 rdi)))
                                                             (set! tmp.93 rax))
                                                           (begin
                                                             (return-point L.rp.15
                                                                           (begin
                                                                             (set! rdi tmp.93)
                                                                             (set! r15 L.rp.15)
                                                                             (jump L.void?.11 rbp r15 rdi)))
                                                             (set! tmp.92 rax)))
                                                         (!= tmp.92 6))
                                                       (set! tmp.91 0)
                                                       (set! tmp.91 318))
                                                   (begin
                                                     (begin
                                                       (return-point L.rp.16
                                                                     (begin
                                                                       (set! rdi vec.3)
                                                                       (set! r15 L.rp.16)
                                                                       (jump L.get-first.6 rbp r15 rdi)))
                                                       (set! tmp.94 rax))
                                                     (begin
                                                       (set! rsi tmp.94)
                                                       (set! rdi tmp.91)
                                                       (set! r15 tmp-ra.107)
                                                       (jump L.+.12 rbp r15 rdi rsi)))))))))
                (interp-asm-pred-lang-v8 '(module
                                              ((new-frames (() () () ())))
                                            (define L.+.12
                                              ((new-frames ()))
                                              (begin
                                                (set! tmp-ra.95 r15)
                                                (set! tmp.19 rdi)
                                                (set! tmp.20 rsi)
                                                (if (begin
                                                      (if (begin
                                                            (begin
                                                              (set! tmp.55 tmp.20)
                                                              (set! tmp.55 (bitwise-and tmp.55 7)))
                                                            (= tmp.55 0))
                                                          (set! tmp.54 14)
                                                          (set! tmp.54 6))
                                                      (!= tmp.54 6))
                                                    (if (begin
                                                          (if (begin
                                                                (begin
                                                                  (set! tmp.57 tmp.19)
                                                                  (set! tmp.57 (bitwise-and tmp.57 7)))
                                                                (= tmp.57 0))
                                                              (set! tmp.56 14)
                                                              (set! tmp.56 6))
                                                          (!= tmp.56 6))
                                                        (begin
                                                          (set! rax tmp.19)
                                                          (set! rax (+ rax tmp.20))
                                                          (jump tmp-ra.95 rbp rax))
                                                        (begin (set! rax 574) (jump tmp-ra.95 rbp rax)))
                                                    (begin (set! rax 574) (jump tmp-ra.95 rbp rax)))))
                                            (define L.void?.11
                                              ((new-frames ()))
                                              (begin
                                                (set! tmp-ra.96 r15)
                                                (set! tmp.43 rdi)
                                                (if (begin
                                                      (begin (set! tmp.58 tmp.43) (set! tmp.58 (bitwise-and tmp.58 255)))
                                                      (= tmp.58 30))
                                                    (begin (set! rax 14) (jump tmp-ra.96 rbp rax))
                                                    (begin (set! rax 6) (jump tmp-ra.96 rbp rax)))))
                                            (define L.unsafe-vector-ref.3
                                              ((new-frames ()))
                                              (begin
                                                (set! tmp-ra.97 r15)
                                                (set! tmp.14 rdi)
                                                (set! tmp.15 rsi)
                                                (if (begin
                                                      (if (begin (set! tmp.60 (mref tmp.14 -3)) (< tmp.15 tmp.60))
                                                          (set! tmp.59 14)
                                                          (set! tmp.59 6))
                                                      (!= tmp.59 6))
                                                    (if (begin
                                                          (if (>= tmp.15 0) (set! tmp.61 14) (set! tmp.61 6))
                                                          (!= tmp.61 6))
                                                        (begin
                                                          (set! tmp.64 tmp.15)
                                                          (set! tmp.64 (arithmetic-shift-right tmp.64 3))
                                                          (set! tmp.63 tmp.64)
                                                          (set! tmp.63 (* tmp.63 8))
                                                          (set! tmp.62 tmp.63)
                                                          (set! tmp.62 (+ tmp.62 5))
                                                          (set! rax (mref tmp.14 tmp.62))
                                                          (jump tmp-ra.97 rbp rax))
                                                        (begin (set! rax 2878) (jump tmp-ra.97 rbp rax)))
                                                    (begin (set! rax 2878) (jump tmp-ra.97 rbp rax)))))
                                            (define L.vector-ref.10
                                              ((new-frames ()))
                                              (begin
                                                (set! tmp-ra.98 r15)
                                                (set! tmp.36 rdi)
                                                (set! tmp.37 rsi)
                                                (if (begin
                                                      (if (begin
                                                            (begin
                                                              (set! tmp.66 tmp.37)
                                                              (set! tmp.66 (bitwise-and tmp.66 7)))
                                                            (= tmp.66 0))
                                                          (set! tmp.65 14)
                                                          (set! tmp.65 6))
                                                      (!= tmp.65 6))
                                                    (if (begin
                                                          (if (begin
                                                                (begin
                                                                  (set! tmp.68 tmp.36)
                                                                  (set! tmp.68 (bitwise-and tmp.68 7)))
                                                                (= tmp.68 3))
                                                              (set! tmp.67 14)
                                                              (set! tmp.67 6))
                                                          (!= tmp.67 6))
                                                        (begin
                                                          (set! rsi tmp.37)
                                                          (set! rdi tmp.36)
                                                          (set! r15 tmp-ra.98)
                                                          (jump L.unsafe-vector-ref.3 rbp r15 rdi rsi))
                                                        (begin (set! rax 2878) (jump tmp-ra.98 rbp rax)))
                                                    (begin (set! rax 2878) (jump tmp-ra.98 rbp rax)))))
                                            (define L.unsafe-vector-set!.2
                                              ((new-frames ()))
                                              (begin
                                                (set! tmp-ra.99 r15)
                                                (set! tmp.9 rdi)
                                                (set! tmp.10 rsi)
                                                (set! tmp.11 rdx)
                                                (if (begin
                                                      (if (begin (set! tmp.70 (mref tmp.9 -3)) (< tmp.10 tmp.70))
                                                          (set! tmp.69 14)
                                                          (set! tmp.69 6))
                                                      (!= tmp.69 6))
                                                    (if (begin
                                                          (if (>= tmp.10 0) (set! tmp.71 14) (set! tmp.71 6))
                                                          (!= tmp.71 6))
                                                        (begin
                                                          (set! tmp.74 tmp.10)
                                                          (set! tmp.74 (arithmetic-shift-right tmp.74 3))
                                                          (set! tmp.73 tmp.74)
                                                          (set! tmp.73 (* tmp.73 8))
                                                          (set! tmp.72 tmp.73)
                                                          (set! tmp.72 (+ tmp.72 5))
                                                          (mset! tmp.9 tmp.72 tmp.11)
                                                          (set! rax 30)
                                                          (jump tmp-ra.99 rbp rax))
                                                        (begin (set! rax 2622) (jump tmp-ra.99 rbp rax)))
                                                    (begin (set! rax 2622) (jump tmp-ra.99 rbp rax)))))
                                            (define L.vector-set!.9
                                              ((new-frames ()))
                                              (begin
                                                (set! tmp-ra.100 r15)
                                                (set! tmp.33 rdi)
                                                (set! tmp.34 rsi)
                                                (set! tmp.35 rdx)
                                                (if (begin
                                                      (if (begin
                                                            (begin
                                                              (set! tmp.76 tmp.34)
                                                              (set! tmp.76 (bitwise-and tmp.76 7)))
                                                            (= tmp.76 0))
                                                          (set! tmp.75 14)
                                                          (set! tmp.75 6))
                                                      (!= tmp.75 6))
                                                    (if (begin
                                                          (if (begin
                                                                (begin
                                                                  (set! tmp.78 tmp.33)
                                                                  (set! tmp.78 (bitwise-and tmp.78 7)))
                                                                (= tmp.78 3))
                                                              (set! tmp.77 14)
                                                              (set! tmp.77 6))
                                                          (!= tmp.77 6))
                                                        (begin
                                                          (set! rdx tmp.35)
                                                          (set! rsi tmp.34)
                                                          (set! rdi tmp.33)
                                                          (set! r15 tmp-ra.100)
                                                          (jump L.unsafe-vector-set!.2 rbp r15 rdi rsi rdx))
                                                        (begin (set! rax 2622) (jump tmp-ra.100 rbp rax)))
                                                    (begin (set! rax 2622) (jump tmp-ra.100 rbp rax)))))
                                            (define L.vector-init-loop.7
                                              ((new-frames ()))
                                              (begin
                                                (set! tmp-ra.101 r15)
                                                (set! len.6 rdi)
                                                (set! i.8 rsi)
                                                (set! vec.7 rdx)
                                                (if (begin
                                                      (if (= len.6 i.8) (set! tmp.79 14) (set! tmp.79 6))
                                                      (!= tmp.79 6))
                                                    (begin (set! rax vec.7) (jump tmp-ra.101 rbp rax))
                                                    (begin
                                                      (set! tmp.82 i.8)
                                                      (set! tmp.82 (arithmetic-shift-right tmp.82 3))
                                                      (set! tmp.81 tmp.82)
                                                      (set! tmp.81 (* tmp.81 8))
                                                      (set! tmp.80 tmp.81)
                                                      (set! tmp.80 (+ tmp.80 5))
                                                      (mset! vec.7 tmp.80 0)
                                                      (set! tmp.83 i.8)
                                                      (set! tmp.83 (+ tmp.83 8))
                                                      (set! rdx vec.7)
                                                      (set! rsi tmp.83)
                                                      (set! rdi len.6)
                                                      (set! r15 tmp-ra.101)
                                                      (jump L.vector-init-loop.7 rbp r15 rdi rsi rdx)))))
                                            (define L.make-init-vector.1
                                              ((new-frames ()))
                                              (begin
                                                (set! tmp-ra.102 r15)
                                                (set! tmp.4 rdi)
                                                (if (begin
                                                      (if (>= tmp.4 0) (set! tmp.84 14) (set! tmp.84 6))
                                                      (!= tmp.84 6))
                                                    (begin
                                                      (set! tmp.88 tmp.4)
                                                      (set! tmp.88 (arithmetic-shift-right tmp.88 3))
                                                      (set! tmp.87 1)
                                                      (set! tmp.87 (+ tmp.87 tmp.88))
                                                      (set! tmp.86 tmp.87)
                                                      (set! tmp.86 (* tmp.86 8))
                                                      (set! tmp.85 (alloc tmp.86))
                                                      (set! tmp.53 tmp.85)
                                                      (set! tmp.53 (+ tmp.53 3))
                                                      (mset! tmp.53 -3 tmp.4)
                                                      (set! tmp.5 tmp.53)
                                                      (set! rdx tmp.5)
                                                      (set! rsi 0)
                                                      (set! rdi tmp.4)
                                                      (set! r15 tmp-ra.102)
                                                      (jump L.vector-init-loop.7 rbp r15 rdi rsi rdx))
                                                    (begin (set! rax 3134) (jump tmp-ra.102 rbp rax)))))
                                            (define L.make-vector.8
                                              ((new-frames ()))
                                              (begin
                                                (set! tmp-ra.103 r15)
                                                (set! tmp.31 rdi)
                                                (if (begin
                                                      (if (begin
                                                            (begin
                                                              (set! tmp.90 tmp.31)
                                                              (set! tmp.90 (bitwise-and tmp.90 7)))
                                                            (= tmp.90 0))
                                                          (set! tmp.89 14)
                                                          (set! tmp.89 6))
                                                      (!= tmp.89 6))
                                                    (begin
                                                      (set! rdi tmp.31)
                                                      (set! r15 tmp-ra.103)
                                                      (jump L.make-init-vector.1 rbp r15 rdi))
                                                    (begin (set! rax 2110) (jump tmp-ra.103 rbp rax)))))
                                            (define L.v.4
                                              ((new-frames ()))
                                              (begin
                                                (set! tmp-ra.104 r15)
                                                (set! rdi 24)
                                                (set! r15 tmp-ra.104)
                                                (jump L.make-vector.8 rbp r15 rdi)))
                                            (define L.set-first.5
                                              ((new-frames ()))
                                              (begin
                                                (set! tmp-ra.105 r15)
                                                (set! vec.1 rdi)
                                                (set! rdx 336)
                                                (set! rsi 0)
                                                (set! rdi vec.1)
                                                (set! r15 tmp-ra.105)
                                                (jump L.vector-set!.9 rbp r15 rdi rsi rdx)))
                                            (define L.get-first.6
                                              ((new-frames ()))
                                              (begin
                                                (set! tmp-ra.106 r15)
                                                (set! vec.2 rdi)
                                                (set! rsi 0)
                                                (set! rdi vec.2)
                                                (set! r15 tmp-ra.106)
                                                (jump L.vector-ref.10 rbp r15 rdi rsi)))
                                            (begin
                                              (set! tmp-ra.107 r15)
                                              (return-point L.rp.13 (begin (set! r15 L.rp.13) (jump L.v.4 rbp r15)))
                                              (set! vec.3 rax)
                                              (if (begin
                                                    (begin
                                                      (return-point L.rp.14
                                                                    (begin
                                                                      (set! rdi vec.3)
                                                                      (set! r15 L.rp.14)
                                                                      (jump L.set-first.5 rbp r15 rdi)))
                                                      (set! tmp.93 rax)
                                                      (return-point L.rp.15
                                                                    (begin
                                                                      (set! rdi tmp.93)
                                                                      (set! r15 L.rp.15)
                                                                      (jump L.void?.11 rbp r15 rdi)))
                                                      (set! tmp.92 rax))
                                                    (!= tmp.92 6))
                                                  (set! tmp.91 0)
                                                  (set! tmp.91 318))
                                              (return-point L.rp.16
                                                            (begin
                                                              (set! rdi vec.3)
                                                              (set! r15 L.rp.16)
                                                              (jump L.get-first.6 rbp r15 rdi)))
                                              (set! tmp.94 rax)
                                              (set! rsi tmp.94)
                                              (set! rdi tmp.91)
                                              (set! r15 tmp-ra.107)
                                              (jump L.+.12 rbp r15 rdi rsi))))))
