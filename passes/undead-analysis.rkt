#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v5
  rackunit)

(provide undead-analysis)

;; asm-pred-lang-v5/locals -> asm-pred-lang-v5/undead
;; compiles p to Asm-pred-lang v5/undead by performing undeadness analysis,
;; decorating the program with undead-set tree
(define/contract (undead-analysis p)
  (-> asm-pred-lang-v5/locals? asm-pred-lang-v5/undead?)

  (define (analyze-func f)
    (match f
      [`(define ,label ,info ,tail)
       (define-values (undead-tree _) (analyze-tail tail))
       (define updated-info (info-set info 'undead-out undead-tree))
       `(define ,label ,updated-info ,tail)]))

  ;; asm-pred-lang-v5/locals.tail -> (values undead-set-tree undead-set)
  (define (analyze-tail t)
    (match t
      [`(begin ,effects ... ,tail)
       (define-values (t-ust undead-out)
         (analyze-tail tail))
       (define-values (ust undead-in)
         (for/foldr ([rev-ust (list t-ust)]
                     [undead-out undead-out])
           ([effect effects])
           (define-values (ust undead-in)
             (analyze-effects effect undead-out))
           (values (cons ust rev-ust) undead-in)))
       (values ust undead-in)]
      [`(if ,pred ,t1 ,t2)
       (define-values (t1-ust t1-undead-out) (analyze-tail t1))
       (define-values (t2-ust t2-undead-out) (analyze-tail t2))
       (define tail-undead-in (set-union t1-undead-out t2-undead-out))
       (define ust^ (list t1-ust t2-ust))
       (define-values (p-ust p-undead-out) (analyze-pred pred tail-undead-in))
       (values (cons p-ust ust^) p-undead-out)]
      [`(jump ,trg ,locs ...)
       (define undead-trg (analyze-trg trg))
       (define undead-set (if (empty? undead-trg)
                              '()
                              (set-add '() (first undead-trg))))
       (define undead-out (set-union undead-set locs))
       (values undead-out undead-out)]
      [`(halt ,op)
       (define undead-in (analyze-opand op))
       (values '() undead-in)]))

  ;; asm-pred-lang-v5/locals.effect undead-set -> (values undead-set-tree undead-set)
  (define (analyze-effects e undead-out)
    (match e
      [`(begin ,effects ...)
       (define-values (rev-ust undead-in)
         (for/foldr ([rev-ust '()]
                     [undead-out undead-out])
           ([effect effects])
           (define-values (ust undead-in)
             (analyze-effects effect undead-out))
           (values
            (cons ust rev-ust)
            undead-in)))
       (values rev-ust undead-in)]
      [`(set! ,loc (,binop ,loc ,triv))
       (define undead-loc (analyze-loc loc))
       (define undead-set (if (empty? undead-loc)
                              undead-out
                              (set-add (set-remove undead-out (first undead-loc)) (first undead-loc))))
       (define undead-in (set-union undead-set (analyze-triv triv)))
       (values undead-out undead-in)]
      [`(set! ,loc ,triv)
       (define undead-loc (analyze-loc loc))
       (define undead-set (if (empty? undead-loc)
                              undead-out
                              (set-remove undead-out (first undead-loc))))
       (define undead-in (set-union undead-set (analyze-triv triv)))
       (values undead-out undead-in)]
      [`(if ,pred ,e1 ,e2)
       (define-values (e1-ust e1-undead-out) (analyze-effects e1 undead-out))
       (define-values (e2-ust e2-undead-out) (analyze-effects e2 undead-out))
       (define effect-undead-in (set-union e1-undead-out e2-undead-out))
       (define ust (list e1-ust e2-ust))
       (define-values (p-ust p-undead-out) (analyze-pred pred effect-undead-in))
       (values (cons p-ust ust) p-undead-out)]))

  ;; asm-pred-lang-v5/locals.pred undead-set -> (values undead-set-tree undead-set)
  (define (analyze-pred p undead-out)
    (match p
      [`(not ,pred)
       (define-values (ust p-undead-out) (analyze-pred pred undead-out))
       (values ust p-undead-out)]
      [`(begin ,effects ... ,pred)
       (define-values (p-out p-undead-out)
         (analyze-pred pred undead-out))
       (define-values (e-ust e-undead-out)
         (for/foldr ([ust^ (list p-out)]
                     [undead-in^ p-undead-out])
           ([effect effects])
           (define-values (ust undead-in)
             (analyze-effects effect undead-in^))
           (values (cons ust ust^) undead-in)))
       (values e-ust e-undead-out)]
      [`(if ,p1 ,p2 ,p3)
       (define-values (p3-ust p3-undead-in) (analyze-pred p3 undead-out))
       (define-values (p2-ust p2-undead-in) (analyze-pred p2 undead-out))
       (define pred-undead-in (set-union p3-undead-in p2-undead-in))
       (define ust (list p3-ust p2-ust))
       (define-values (p1-ust p1-undead-out) (analyze-pred p1 pred-undead-in))
       (values (cons p1-ust ust) p1-undead-out)]
      [`(,relop ,loc ,op)
       (define undead-loc (analyze-loc loc))
       (define undead-set (if (empty? undead-loc)
                              undead-out
                              (set-add undead-out (first undead-loc))))
       (define undead-in (set-union undead-set (analyze-opand op)))
       (values undead-out undead-in)]
      ['(true) (values undead-out undead-out)]
      ['(false) (values undead-out undead-out)]))

  ;; asm-pred-lang-v5/locals.triv -> (List-of loc)
  (define (analyze-triv triv)
    (match triv
      [label #:when (label? label) '()]
      [opand (analyze-opand opand)]))

  ;; asm-pred-lang-v5/locals.loc -> (List-of loc)
  (define (analyze-loc loc)
    (match loc
      [rloc #:when (rloc? rloc) (list rloc)]
      [aloc #:when (aloc? aloc) (list aloc)]))

  ;; asm-pred-lang-v5/locals.trg -> (List-of loc)
  (define (analyze-trg trg)
    (match trg
      [label #:when (label? label) '()]
      [loc (analyze-loc loc)]))

  ;; asm-pred-lang-v5/locals.opand -> (List-of loc)
  (define (analyze-opand op)
    (match op
      [int64 #:when (int64? int64) '()]
      [loc (analyze-loc loc)]))

  (match p
    [`(module ,info ,funcs ... ,tail)
     (define-values (undead-tree _) (analyze-tail tail))
     (define updated-info (info-set info 'undead-out undead-tree))
     `(module ,updated-info ,@(map analyze-func funcs) ,tail)]))

(module+ test
  (check-equal? (undead-analysis '(module ((locals ()))
                                    (define L.f.1 ((locals (x.1))) (begin (set! x.1 rdi) (halt x.1)))
                                    (begin (set! rdi 1) (jump L.f.1 rbp rdi))))
                '(module ((locals ()) (undead-out ((rdi rbp) (rdi rbp))))
                   (define L.f.1
                     ((locals (x.1)) (undead-out ((x.1) ())))
                     (begin (set! x.1 rdi) (halt x.1)))
                   (begin (set! rdi 1) (jump L.f.1 rbp rdi))))
  (check-equal? (undead-analysis '(module ((locals (a.1)))
                                    (define L.f.1 ((locals (x.1))) (begin (set! x.1 rdi) (halt x.1)))
                                    (begin (set! a.1 L.f.1) (set! rdi 1) (jump a.1 rbp rdi))))
                '(module
                     ((locals (a.1)) (undead-out ((rbp a.1) (rdi rbp a.1) (rdi rbp a.1))))
                   (define L.f.1
                     ((locals (x.1)) (undead-out ((x.1) ())))
                     (begin (set! x.1 rdi) (halt x.1)))
                   (begin (set! a.1 L.f.1) (set! rdi 1) (jump a.1 rbp rdi))))
  (check-equal? (undead-analysis '(module ((locals ()))
                                    (define L.f.1 ((locals (x.1))) (begin (set! x.1 rdi) (halt x.1)))
                                    (begin (set! r13 L.f.1) (set! rdi 1) (jump r13 rbp rdi))))
                '(module
                     ((locals ()) (undead-out ((rbp r13) (rdi rbp r13) (rdi rbp r13))))
                   (define L.f.1
                     ((locals (x.1)) (undead-out ((x.1) ())))
                     (begin (set! x.1 rdi) (halt x.1)))
                   (begin (set! r13 L.f.1) (set! rdi 1) (jump r13 rbp rdi))))
  (check-equal? (undead-analysis '(module
                                      ((locals ()))
                                    (define L.f.1 ((locals (x.1))) (begin (set! x.1 rdi) (halt x.1)))
                                    (define L.g.1
                                      ((locals (y.1 x.1 z.1)))
                                      (begin
                                        (set! x.1 rdi)
                                        (set! y.1 rsi)
                                        (set! z.1 rdx)
                                        (set! rdi x.1)
                                        (jump L.f.1 rbp rdi)))
                                    (if (true)
                                        (begin
                                          (set! rdx 3)
                                          (set! rsi 2)
                                          (set! rdi 1)
                                          (jump L.g.1 rbp rdi rsi rdx))
                                        (begin (set! rdi 1) (jump L.f.1 rbp rdi)))))
                '(module
                     ((locals ())
                      (undead-out
                       ((rbp)
                        ((rdx rbp) (rdx rsi rbp) (rdx rsi rdi rbp) (rdx rsi rdi rbp))
                        ((rdi rbp) (rdi rbp)))))
                   (define L.f.1
                     ((locals (x.1)) (undead-out ((x.1) ())))
                     (begin (set! x.1 rdi) (halt x.1)))
                   (define L.g.1
                     ((locals (y.1 x.1 z.1))
                      (undead-out
                       ((rsi rdx x.1 rbp) (rdx x.1 rbp) (x.1 rbp) (rdi rbp) (rdi rbp))))
                     (begin
                       (set! x.1 rdi)
                       (set! y.1 rsi)
                       (set! z.1 rdx)
                       (set! rdi x.1)
                       (jump L.f.1 rbp rdi)))
                   (if (true)
                       (begin (set! rdx 3) (set! rsi 2) (set! rdi 1) (jump L.g.1 rbp rdi rsi rdx))
                       (begin (set! rdi 1) (jump L.f.1 rbp rdi)))))
  (check-equal? (undead-analysis
                 '(module ((locals ()))
                    (halt x.1)))
                '(module
                     ((locals ()) (undead-out ()))
                   (halt x.1)))
  (check-equal? (undead-analysis
                 '(module ((locals ()))
                    (begin
                      (set! x.1 1)
                      (halt x.1))))
                '(module
                     ((locals ()) (undead-out ((x.1) ())))
                   (begin
                     (set! x.1 1)
                     (halt x.1))))
  (check-equal? (undead-analysis
                 '(module ((locals (x.1)))
                    (begin
                      (set! x.1 42)
                      (halt x.1))))
                '(module
                     ((locals (x.1)) (undead-out ((x.1) ())))
                   (begin (set! x.1 42) (halt x.1)))
                "Testing basic small program")
  (check-equal? (undead-analysis '(module
                                      ((locals (x.1 y.2)))
                                    (begin
                                      (set! y.2 1)
                                      (set! x.1 2)
                                      (set! y.2 (* y.2 x.1))
                                      (begin
                                        (set! x.1 (+ x.1 -1))
                                        (set! y.2 (* y.2 x.1))
                                        (begin
                                          (set! x.1 (+ x.1 -1))
                                          (set! y.2 (* y.2 x.1))))
                                      (halt y.2))))
                '(module
                     ((locals (x.1 y.2))
                      (undead-out ((y.2)
                                   (y.2 x.1)
                                   (x.1 y.2)
                                   ((y.2 x.1)
                                    (x.1 y.2)
                                    ((x.1 y.2)
                                     (y.2)))
                                   ())))
                   (begin
                     (set! y.2 1)
                     (set! x.1 2)
                     (set! y.2 (* y.2 x.1))
                     (begin
                       (set! x.1 (+ x.1 -1))
                       (set! y.2 (* y.2 x.1))
                       (begin
                         (set! x.1 (+ x.1 -1))
                         (set! y.2 (* y.2 x.1))))
                     (halt y.2))))
  (check-equal? (undead-analysis
                 '(module ((locals ()))
                    (halt 1)))
                '(module
                     ((locals ()) (undead-out ()))
                   (halt 1)))
  (check-equal? (undead-analysis
                 '(module ((locals (x.1 y.2 z.3)))
                    (begin
                      (set! x.1 42)
                      (set! x.1 x.1)
                      (set! z.3 x.1)
                      (set! z.3 z.3)
                      (set! z.3 (+ z.3 z.3))
                      (halt z.3))))
                '(module
                     ((locals (x.1 y.2 z.3))
                      (undead-out ((x.1) (x.1) (z.3) (z.3) (z.3) ())))
                   (begin
                     (set! x.1 42)
                     (set! x.1 x.1)
                     (set! z.3 x.1)
                     (set! z.3 z.3)
                     (set! z.3 (+ z.3 z.3))
                     (halt z.3))))
  (check-equal? (undead-analysis
                 '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1)))
                    (begin
                      (set! v.1 1)
                      (set! w.2 46)
                      (set! x.3 v.1)
                      (set! p.1 7)
                      (set! x.3 (+ x.3 p.1))
                      (set! y.4 x.3)
                      (set! p.1 4)
                      (set! y.4 (+ y.4 p.1))
                      (set! z.5 x.3)
                      (set! z.5 (+ z.5 w.2))
                      (set! t.6 y.4)
                      (set! p.1 -1)
                      (set! t.6 (* t.6 p.1))
                      (set! z.5 (+ z.5 t.6))
                      (halt z.5))))
                '(module
                     ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                      (undead-out
                       ((v.1)
                        (v.1 w.2)
                        (x.3 w.2)
                        (p.1 x.3 w.2)
                        (x.3 w.2)
                        (y.4 x.3 w.2)
                        (p.1 y.4 x.3 w.2)
                        (x.3 w.2 y.4)
                        (w.2 z.5 y.4)
                        (y.4 z.5)
                        (t.6 z.5)
                        (p.1 t.6 z.5)
                        (t.6 z.5)
                        (z.5)
                        ())))
                   (begin
                     (set! v.1 1)
                     (set! w.2 46)
                     (set! x.3 v.1)
                     (set! p.1 7)
                     (set! x.3 (+ x.3 p.1))
                     (set! y.4 x.3)
                     (set! p.1 4)
                     (set! y.4 (+ y.4 p.1))
                     (set! z.5 x.3)
                     (set! z.5 (+ z.5 w.2))
                     (set! t.6 y.4)
                     (set! p.1 -1)
                     (set! t.6 (* t.6 p.1))
                     (set! z.5 (+ z.5 t.6))
                     (halt z.5))))
  (check-equal? (undead-analysis '(module
                                      ((locals (x.1 y.2 b.3 c.4)))
                                    (begin
                                      (set! x.1 5)
                                      (set! y.2 x.1)
                                      (begin
                                        (set! b.3 x.1)
                                        (set! b.3 (+ b.3 y.2))
                                        (set! c.4 b.3)
                                        (if (= c.4 b.3)
                                            (halt c.4)
                                            (begin
                                              (set! x.1 c.4)
                                              (halt c.4)))))))
                '(module
                     ((locals (x.1 y.2 b.3 c.4))
                      (undead-out
                       ((x.1) (x.1 y.2) ((y.2 b.3) (b.3) (b.3 c.4) ((c.4) () ((c.4) ()))))))
                   (begin
                     (set! x.1 5)
                     (set! y.2 x.1)
                     (begin
                       (set! b.3 x.1)
                       (set! b.3 (+ b.3 y.2))
                       (set! c.4 b.3)
                       (if (= c.4 b.3) (halt c.4) (begin (set! x.1 c.4) (halt c.4)))))))
  (check-equal? (undead-analysis '(module ((locals (x.1 x.2 x.3)))
                                    (if (if (begin (set! x.1 1)
                                                   (set! x.2 x.1)
                                                   (set! x.3 x.1)
                                                   (set! x.3 (+ x.3 x.2))
                                                   (true))
                                            (true)
                                            (false))
                                        (halt x.3)
                                        (halt x.2))))
                '(module ((locals (x.1 x.2 x.3))
                          (undead-out ((((x.1)
                                         (x.1 x.2)
                                         (x.3 x.2)
                                         (x.2 x.3)
                                         (x.2 x.3))
                                        (x.2 x.3)
                                        (x.2 x.3))
                                       ()
                                       ())))
                   (if (if (begin (set! x.1 1)
                                  (set! x.2 x.1)
                                  (set! x.3 x.1)
                                  (set! x.3 (+ x.3 x.2))
                                  (true))
                           (true)
                           (false))
                       (halt x.3)
                       (halt x.2))))
  (check-equal? (undead-analysis '(module ((locals (x.1 x.2 x.3)))
                                    (begin (set! x.1 1)
                                           (set! x.2 x.1)
                                           (set! x.2 (+ x.2 x.1))
                                           (if (> x.2 2)
                                               (set! x.3 3)
                                               (set! x.3 4))
                                           (set! x.2 (+ x.2 x.3))
                                           (halt x.2))))
                '(module ((locals (x.1 x.2 x.3))
                          (undead-out ((x.1)
                                       (x.1 x.2)
                                       (x.2)
                                       ((x.2)
                                        (x.3 x.2)
                                        (x.3 x.2))
                                       (x.2)
                                       ())))
                   (begin (set! x.1 1)
                          (set! x.2 x.1)
                          (set! x.2 (+ x.2 x.1))
                          (if (> x.2 2)
                              (set! x.3 3)
                              (set! x.3 4))
                          (set! x.2 (+ x.2 x.3))
                          (halt x.2)))))
