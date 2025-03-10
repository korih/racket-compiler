#lang racket

(require "common.rkt"
         "undead-analysis.rkt"
         "uncover-locals.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v6
  rackunit)

(provide conflict-analysis)

;; asm-pred-lang-v6/undead -> asm-pred-lang-v6/conflicts
;; compiles p to Asm-pred-lang v5/conflicts by performing conflict analysis and
;; decorating programs with their conflict graph
(define/contract (conflict-analysis p)
  (-> asm-pred-lang-v6/undead? asm-pred-lang-v6/conflicts?)

  ;; func-info is `(define ,label ,info ,tail)
  ;; interp. a function definition that has metadata

  ;; acc is Graph
  ;; the conflict graphs of abstract locations
  (define conflict-graph (void))

  ;; undead-set-tree aloc (List-of aloc) ->
  ;; interp. track the conflict resulting from the move instruction to the dest aloc in the conflict graph
  (define (analyze-move-instruction udt dest src)
    (set! conflict-graph (add-edges conflict-graph dest (set-subtract udt (list dest) src))))

  ;; func-info -> func-info
  (define (conflict-analysis-func func)
    (match func
      [`(define ,label ,info ,tail)
       (set! conflict-graph (new-graph (info-ref info 'locals)))
       (define udt (info-ref info 'undead-out))
       (define analyzed-tail (conflict-analysis-tail udt tail))
       (define updated-info (info-set info
                                      'conflicts
                                      conflict-graph))
       `(define ,label ,updated-info ,tail)]))

  ;; undead-set-tree asm-pred-lang/undead.tail -> asm-pred-lang/conflicts.tail
  (define (conflict-analysis-tail udt tail)
    (match (cons udt tail)
      [(cons `(,undead-set-trees ... ,undead-set-tree-tail) `(begin ,fx ... ,inner-tail))
       (define compiled-effects (for/list ([e fx]
                                           [udt-e undead-set-trees])
                                  (conflict-analysis-effect udt-e e)))
       `(begin ,@compiled-effects ,(conflict-analysis-tail undead-set-tree-tail inner-tail))]
      [(cons `(,undead-out ...) `(jump ,trg ,locs ...))
       (define analyzed-trg (conflict-analysis-trg trg))
       (when (not (empty? analyzed-trg))
         (set! conflict-graph (add-edges conflict-graph (first analyzed-trg) (remove (first analyzed-trg) undead-out))))
       (for ([loc locs])
         (set! conflict-graph (add-edges conflict-graph loc (remove loc undead-out))))
       `(jump ,trg ,@locs)]
      [(cons `(,undead-set-tree-p ,undead-set-tree-t ,undead-set-tree-f) `(if ,pred ,tail-t ,tail-f))
       `(if ,(conflict-analysis-pred undead-set-tree-p pred)
            ,(conflict-analysis-tail undead-set-tree-t tail-t)
            ,(conflict-analysis-tail undead-set-tree-f tail-f))]))

  ;; undead-set-tree asm-pred-lang/undead.effect -> asm-pred-lang/conflicts.effect
  (define (conflict-analysis-effect udt e)
    (match (cons udt e)
      [(cons `(,undead-set-trees ... ,last-undead-set-tree) `(begin ,fx ... ,effect))
       (define analyzed-fx (for/list ([e fx]
                                      [ust undead-set-trees])
                             (conflict-analysis-effect ust e)))
       `(begin ,@analyzed-fx ,(conflict-analysis-effect last-undead-set-tree effect))]
      [(cons `(,undead-out ...) `(set! ,loc (,binop ,loc ,op)))
       (analyze-move-instruction undead-out loc empty)
       `(set! ,loc (,binop ,loc ,op))]
      [(cons `(,undead-out ...) `(set! ,loc ,triv))
       (analyze-move-instruction undead-out loc (conflict-analysis-triv triv))
       `(set! ,loc ,triv)]
      [(cons `(,udt-p ,udt-t ,udt-f) `(if ,pred ,t-e ,f-e))
       `(if ,(conflict-analysis-pred udt-p pred)
            ,(conflict-analysis-effect udt-t t-e)
            ,(conflict-analysis-effect udt-f f-e))]
      [(cons `(,udt-inner ,udt-outer) `(return-point ,label ,tail))
       `(return-point ,label ,(conflict-analysis-tail udt-outer tail))]))

  ;; undead-set-tree asm-pred-lang/undead.pred -> asm-pred-lang/conflicts.pred
  (define (conflict-analysis-pred udt pred)
    (match (cons udt pred)
      [(cons undead-set-tree '(true)) '(true)]
      [(cons undead-set-tree '(false)) '(false)]
      [(cons undead-set-tree `(not ,pred))
       `(not ,(conflict-analysis-pred undead-set-tree pred))]
      [(cons `(,udts ... ,udt-p) `(begin ,fx ... ,pred))
       `(begin ,@(for/list ([e fx]
                            [udt-e udts])
                   (conflict-analysis-effect udt-e e))
               ,(conflict-analysis-pred udt-p pred))]
      [(cons `(,udt-p ,udt-t ,udt-f) `(if ,pred ,t-pred ,f-pred))
       `(if ,(conflict-analysis-pred udt-p pred)
            ,(conflict-analysis-pred udt-t t-pred)
            ,(conflict-analysis-pred udt-f f-pred))]
      [(cons undead-set-tree `(,relop ,aloc ,triv))
       `(,relop ,aloc ,triv)]))

  ;; asm-pred-lang/undead.triv -> (List-of loc)
  (define (conflict-analysis-triv triv)
    (match triv
      [label #:when (label? label) '()]
      [opand (conflict-analysis-opand opand)]))

  ;; asm-pred-lang/undead.loc -> (List-of loc)
  (define (conflict-analysis-loc loc)
    (match loc
      [rloc #:when (rloc? rloc) (list rloc)]
      [aloc #:when (aloc? aloc) (list aloc)]))

  ;; asm-pred-lang/undead.trg -> (List-of loc)
  (define (conflict-analysis-trg trg)
    (match trg
      [label #:when (label? label) '()]
      [loc (conflict-analysis-loc loc)]))

  ;; asm-pred-lang/undead.opand -> (List-of loc)
  (define (conflict-analysis-opand op)
    (match op
      [int64 #:when (int64? int64) '()]
      [loc (conflict-analysis-loc loc)]))

  (match p
    [`(module ,info ,funcs ... ,tail)
     (define updated-funcs (map conflict-analysis-func funcs))
     (set! conflict-graph (new-graph (info-ref info 'locals)))
     (define udt (info-ref info 'undead-out))
     (define analyzed-tail (conflict-analysis-tail udt tail))
     (define updated-info (info-set info
                                    'conflicts
                                    conflict-graph))
     `(module ,updated-info ,@updated-funcs ,tail)]))

(check-equal? (conflict-analysis '(module
                                      ((new-frames ())
                                       (locals (ra.12))
                                       (call-undead ())
                                       (undead-out ((ra.12 rbp) (ra.12 fv0 rbp) (fv0 r15 rbp) (fv0 r15 rbp))))
                                    (define L.fact.4
                                      ((new-frames ((nfv.16)))
                                       (locals (ra.13 x.9 tmp.14 tmp.15 new-n.10 nfv.16 factn-1.11 tmp.17))
                                       (undead-out
                                        ((r15 x.9 rbp)
                                         (x.9 ra.13 rbp)
                                         ((x.9 ra.13 rbp)
                                          ((ra.13 rax rbp) (rax rbp))
                                          ((tmp.14 x.9 ra.13 rbp)
                                           (tmp.14 tmp.15 x.9 ra.13 rbp)
                                           (tmp.15 x.9 ra.13 rbp)
                                           (new-n.10 x.9 ra.13 rbp)
                                           ((rax x.9 ra.13 rbp) ((nfv.16 rbp) (nfv.16 r15 rbp) (nfv.16 r15 rbp)))
                                           (x.9 factn-1.11 ra.13 rbp)
                                           (factn-1.11 tmp.17 ra.13 rbp)
                                           (tmp.17 ra.13 rbp)
                                           (ra.13 rax rbp)
                                           (rax rbp)))))
                                       (call-undead (x.9 ra.13)))
                                      (begin
                                        (set! x.9 fv0)
                                        (set! ra.13 r15)
                                        (if (= x.9 0)
                                            (begin (set! rax 1) (jump ra.13 rbp rax))
                                            (begin
                                              (set! tmp.14 -1)
                                              (set! tmp.15 x.9)
                                              (set! tmp.15 (+ tmp.15 tmp.14))
                                              (set! new-n.10 tmp.15)
                                              (return-point L.rp.6
                                                            (begin
                                                              (set! nfv.16 new-n.10)
                                                              (set! r15 L.rp.6)
                                                              (jump L.fact.4 rbp r15 nfv.16)))
                                              (set! factn-1.11 rax)
                                              (set! tmp.17 x.9)
                                              (set! tmp.17 (* tmp.17 factn-1.11))
                                              (set! rax tmp.17)
                                              (jump ra.13 rbp rax)))))
                                    (begin
                                      (set! ra.12 r15)
                                      (set! fv0 5)
                                      (set! r15 ra.12)
                                      (jump L.fact.4 rbp r15 fv0))))
              '(module
                   ((new-frames ())
                    (locals (ra.12))
                    (call-undead ())
                    (undead-out ((ra.12 rbp) (ra.12 fv0 rbp) (fv0 r15 rbp) (fv0 r15 rbp)))
                    (conflicts
                     ((ra.12 (fv0 rbp))
                      (rbp (r15 fv0 ra.12))
                      (fv0 (r15 ra.12 rbp))
                      (r15 (fv0 rbp)))))
                 (define L.fact.4
                   ((new-frames ((nfv.16)))
                    (locals (ra.13 x.9 tmp.14 tmp.15 new-n.10 nfv.16 factn-1.11 tmp.17))
                    (undead-out
                     ((r15 x.9 rbp)
                      (x.9 ra.13 rbp)
                      ((x.9 ra.13 rbp)
                       ((ra.13 rax rbp) (rax rbp))
                       ((tmp.14 x.9 ra.13 rbp)
                        (tmp.14 tmp.15 x.9 ra.13 rbp)
                        (tmp.15 x.9 ra.13 rbp)
                        (new-n.10 x.9 ra.13 rbp)
                        ((rax x.9 ra.13 rbp)
                         ((nfv.16 rbp) (nfv.16 r15 rbp) (nfv.16 r15 rbp)))
                        (x.9 factn-1.11 ra.13 rbp)
                        (factn-1.11 tmp.17 ra.13 rbp)
                        (tmp.17 ra.13 rbp)
                        (ra.13 rax rbp)
                        (rax rbp)))))
                    (call-undead (x.9 ra.13))
                    (conflicts
                     ((tmp.17 (factn-1.11 ra.13 rbp))
                      (factn-1.11 (tmp.17 x.9 ra.13 rbp))
                      (nfv.16 (r15 rbp))
                      (new-n.10 (x.9 ra.13 rbp))
                      (tmp.15 (x.9 tmp.14 ra.13 rbp))
                      (tmp.14 (tmp.15 x.9 ra.13 rbp))
                      (x.9 (factn-1.11 new-n.10 tmp.15 tmp.14 ra.13 r15 rbp))
                      (ra.13 (tmp.17 factn-1.11 new-n.10 tmp.15 tmp.14 rax x.9 rbp))
                      (rbp
                       (tmp.17 factn-1.11 r15 nfv.16 new-n.10 tmp.15 tmp.14 rax ra.13 x.9))
                      (r15 (nfv.16 rbp x.9))
                      (rax (ra.13 rbp)))))
                   (begin
                     (set! x.9 fv0)
                     (set! ra.13 r15)
                     (if (= x.9 0)
                         (begin (set! rax 1) (jump ra.13 rbp rax))
                         (begin
                           (set! tmp.14 -1)
                           (set! tmp.15 x.9)
                           (set! tmp.15 (+ tmp.15 tmp.14))
                           (set! new-n.10 tmp.15)
                           (return-point L.rp.6
                                         (begin
                                           (set! nfv.16 new-n.10)
                                           (set! r15 L.rp.6)
                                           (jump L.fact.4 rbp r15 nfv.16)))
                           (set! factn-1.11 rax)
                           (set! tmp.17 x.9)
                           (set! tmp.17 (* tmp.17 factn-1.11))
                           (set! rax tmp.17)
                           (jump ra.13 rbp rax)))))
                 (begin
                   (set! ra.12 r15)
                   (set! fv0 5)
                   (set! r15 ra.12)
                   (jump L.fact.4 rbp r15 fv0)))
              )
#;
(module+ test
  (check-equal? (conflict-analysis '(module
                                        ((new-frame ())(locals ()) (call-undead ()) (undead-out (rbp)))
                                      (define L.f.1
                                        ((locals (tmp.1)) (undead-out ((tmp.1) (tmp.1) ())))
                                        (begin (set! tmp.1 1) (set! tmp.1 (* tmp.1 2)) (halt tmp.1)))
                                      (jump L.f.1 rbp)))
                '(module
                     ((locals ()) (conflicts ()))
                   (define L.f.1
                     ((locals (tmp.1))
                      (conflicts ((tmp.1 ()))))
                     (begin (set! tmp.1 1) (set! tmp.1 (* tmp.1 2)) (halt tmp.1)))
                   (jump L.f.1 rbp)))
  (check-equal? (conflict-analysis '(module
                                        ((locals ())
                                         (undead-out
                                          ((r9 rbp)
                                           (r9 r8 rbp)
                                           (r9 r8 rcx rbp)
                                           (r9 r8 rcx rdx rbp)
                                           (r9 r8 rcx rdx rsi rbp)
                                           (r9 r8 rcx rdx rsi rdi rbp)
                                           (r9 r8 rcx rdx rsi rdi rbp))))
                                      (define L.f.1
                                        ((locals (f.1 e.1 d.1 c.1 b.1 a.1))
                                         (undead-out
                                          ((rsi rdx rcx r8 r9 a.1)
                                           (rdx rcx r8 r9 b.1 a.1)
                                           (rcx r8 r9 b.1 a.1 c.1)
                                           (r8 r9 b.1 a.1 c.1 d.1)
                                           (r9 b.1 a.1 c.1 d.1 e.1)
                                           (b.1 a.1 c.1 d.1 e.1 f.1)
                                           (c.1 a.1 d.1 e.1 f.1)
                                           (d.1 a.1 e.1 f.1)
                                           (e.1 a.1 f.1)
                                           (f.1 a.1)
                                           (a.1)
                                           ())))
                                        (begin
                                          (set! a.1 rdi)
                                          (set! b.1 rsi)
                                          (set! c.1 rdx)
                                          (set! d.1 rcx)
                                          (set! e.1 r8)
                                          (set! f.1 r9)
                                          (set! a.1 (+ a.1 b.1))
                                          (set! a.1 (+ a.1 c.1))
                                          (set! a.1 (+ a.1 d.1))
                                          (set! a.1 (+ a.1 e.1))
                                          (set! a.1 (+ a.1 f.1))
                                          (halt a.1)))
                                      (begin
                                        (set! r9 6)
                                        (set! r8 5)
                                        (set! rcx 4)
                                        (set! rdx 3)
                                        (set! rsi 2)
                                        (set! rdi 1)
                                        (jump L.f.1 rbp rdi rsi rdx rcx r8 r9))))
                '(module
                     ((locals ())
                      (conflicts
                       ((r9 (rdi rsi rdx rcx r8 rbp))
                        (rbp (rdi rsi rdx rcx r8 r9))
                        (r8 (rdi rsi rdx rcx r9 rbp))
                        (rcx (rdi rsi rdx r9 r8 rbp))
                        (rdx (rdi rsi r9 r8 rcx rbp))
                        (rsi (rdi r9 r8 rcx rdx rbp))
                        (rdi (r9 r8 rcx rdx rsi rbp)))))
                   (define L.f.1
                     ((locals (f.1 e.1 d.1 c.1 b.1 a.1))
                      (conflicts
                       ((a.1 (f.1 e.1 d.1 c.1 b.1 rsi rdx rcx r8 r9))
                        (b.1 (f.1 e.1 d.1 c.1 rdx rcx r8 r9 a.1))
                        (c.1 (f.1 e.1 d.1 rcx r8 r9 b.1 a.1))
                        (d.1 (f.1 e.1 r8 r9 b.1 a.1 c.1))
                        (e.1 (f.1 r9 b.1 a.1 c.1 d.1))
                        (f.1 (b.1 a.1 c.1 d.1 e.1))
                        (r9 (e.1 d.1 c.1 b.1 a.1))
                        (r8 (d.1 c.1 b.1 a.1))
                        (rcx (c.1 b.1 a.1))
                        (rdx (b.1 a.1))
                        (rsi (a.1)))))
                     (begin
                       (set! a.1 rdi)
                       (set! b.1 rsi)
                       (set! c.1 rdx)
                       (set! d.1 rcx)
                       (set! e.1 r8)
                       (set! f.1 r9)
                       (set! a.1 (+ a.1 b.1))
                       (set! a.1 (+ a.1 c.1))
                       (set! a.1 (+ a.1 d.1))
                       (set! a.1 (+ a.1 e.1))
                       (set! a.1 (+ a.1 f.1))
                       (halt a.1)))
                   (begin
                     (set! r9 6)
                     (set! r8 5)
                     (set! rcx 4)
                     (set! rdx 3)
                     (set! rsi 2)
                     (set! rdi 1)
                     (jump L.f.1 rbp rdi rsi rdx rcx r8 r9))))
  (check-equal? (conflict-analysis '(module
                                        ((locals (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                                         (undead-out
                                          ((a.1 rbp)
                                           (b.1 a.1 rbp)
                                           (c.1 b.1 a.1 rbp)
                                           (d.1 c.1 b.1 a.1 rbp)
                                           (e.1 d.1 c.1 b.1 a.1 rbp)
                                           (f.1 e.1 d.1 c.1 b.1 a.1 rbp)
                                           (g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp)
                                           (h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp)
                                           (i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp)
                                           (j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp)
                                           (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp)
                                           (j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 fv4 rbp)
                                           (i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 fv4 fv3 rbp)
                                           (h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 rbp)
                                           (g.1 f.1 e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 rbp)
                                           (f.1 e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 rbp)
                                           (e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 rbp)
                                           (d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rbp)
                                           (c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rbp)
                                           (b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rbp)
                                           (a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rsi rbp)
                                           (fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
                                           (fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp))))
                                      (define L.f.1
                                        ((locals (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                                         (undead-out
                                          ((rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4)
                                           (rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4)
                                           (rcx r8 r9 fv0 fv1 fv2 fv3 fv4)
                                           (r8 r9 fv0 fv1 fv2 fv3 fv4)
                                           (r9 fv0 fv1 fv2 fv3 fv4)
                                           (fv0 fv1 fv2 fv3 fv4)
                                           (fv1 fv2 fv3 fv4)
                                           (fv2 fv3 fv4)
                                           (fv3 fv4)
                                           (fv4)
                                           ()
                                           ())))
                                        (begin
                                          (set! a.1 rdi)
                                          (set! b.1 rsi)
                                          (set! c.1 rdx)
                                          (set! d.1 rcx)
                                          (set! e.1 r8)
                                          (set! f.1 r9)
                                          (set! g.1 fv0)
                                          (set! h.1 fv1)
                                          (set! i.1 fv2)
                                          (set! j.1 fv3)
                                          (set! k.1 fv4)
                                          (halt 10)))
                                      (begin
                                        (set! a.1 1)
                                        (set! b.1 2)
                                        (set! c.1 3)
                                        (set! d.1 4)
                                        (set! e.1 5)
                                        (set! f.1 6)
                                        (set! g.1 7)
                                        (set! h.1 8)
                                        (set! i.1 9)
                                        (set! j.1 10)
                                        (set! k.1 11)
                                        (set! fv4 k.1)
                                        (set! fv3 j.1)
                                        (set! fv2 i.1)
                                        (set! fv1 h.1)
                                        (set! fv0 g.1)
                                        (set! r9 f.1)
                                        (set! r8 e.1)
                                        (set! rcx d.1)
                                        (set! rdx c.1)
                                        (set! rsi b.1)
                                        (set! rdi a.1)
                                        (jump L.f.1 rbp rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4))))
                '(module
                     ((locals (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                      (conflicts
                       ((a.1
                         (rsi
                          rdx
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          fv2
                          fv3
                          fv4
                          k.1
                          j.1
                          i.1
                          h.1
                          g.1
                          f.1
                          e.1
                          d.1
                          c.1
                          b.1
                          rbp))
                        (b.1
                         (rdx
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          fv2
                          fv3
                          fv4
                          k.1
                          j.1
                          i.1
                          h.1
                          g.1
                          f.1
                          e.1
                          d.1
                          c.1
                          a.1
                          rbp))
                        (c.1
                         (rcx
                          r8
                          r9
                          fv0
                          fv1
                          fv2
                          fv3
                          fv4
                          k.1
                          j.1
                          i.1
                          h.1
                          g.1
                          f.1
                          e.1
                          d.1
                          b.1
                          a.1
                          rbp))
                        (d.1
                         (r8 r9 fv0 fv1 fv2 fv3 fv4 k.1 j.1 i.1 h.1 g.1 f.1 e.1 c.1 b.1 a.1 rbp))
                        (e.1
                         (r9 fv0 fv1 fv2 fv3 fv4 k.1 j.1 i.1 h.1 g.1 f.1 d.1 c.1 b.1 a.1 rbp))
                        (f.1 (fv0 fv1 fv2 fv3 fv4 k.1 j.1 i.1 h.1 g.1 e.1 d.1 c.1 b.1 a.1 rbp))
                        (g.1 (fv1 fv2 fv3 fv4 k.1 j.1 i.1 h.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp))
                        (h.1 (fv2 fv3 fv4 k.1 j.1 i.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp))
                        (i.1 (fv3 fv4 k.1 j.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp))
                        (j.1 (fv4 k.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp))
                        (k.1 (j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1 rbp))
                        (rbp
                         (rdi
                          rsi
                          rdx
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          fv2
                          fv3
                          fv4
                          k.1
                          j.1
                          i.1
                          h.1
                          g.1
                          f.1
                          e.1
                          d.1
                          c.1
                          b.1
                          a.1))
                        (fv4
                         (rdi
                          rsi
                          rdx
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          fv2
                          fv3
                          j.1
                          i.1
                          h.1
                          g.1
                          f.1
                          e.1
                          d.1
                          c.1
                          b.1
                          a.1
                          rbp))
                        (fv3
                         (rdi
                          rsi
                          rdx
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          fv2
                          i.1
                          h.1
                          g.1
                          f.1
                          e.1
                          d.1
                          c.1
                          b.1
                          a.1
                          fv4
                          rbp))
                        (fv2
                         (rdi
                          rsi
                          rdx
                          rcx
                          r8
                          r9
                          fv0
                          fv1
                          h.1
                          g.1
                          f.1
                          e.1
                          d.1
                          c.1
                          b.1
                          a.1
                          fv4
                          fv3
                          rbp))
                        (fv1
                         (rdi rsi rdx rcx r8 r9 fv0 g.1 f.1 e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 rbp))
                        (fv0 (rdi rsi rdx rcx r8 r9 f.1 e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 rbp))
                        (r9 (rdi rsi rdx rcx r8 e.1 d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 rbp))
                        (r8 (rdi rsi rdx rcx d.1 c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 rbp))
                        (rcx (rdi rsi rdx c.1 b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rbp))
                        (rdx (rdi rsi b.1 a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rbp))
                        (rsi (rdi a.1 fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rbp))
                        (rdi (fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rsi rbp)))))
                   (define L.f.1
                     ((locals (k.1 j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                      (conflicts
                       ((a.1 (rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4))
                        (b.1 (rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4))
                        (c.1 (rcx r8 r9 fv0 fv1 fv2 fv3 fv4))
                        (d.1 (r8 r9 fv0 fv1 fv2 fv3 fv4))
                        (e.1 (r9 fv0 fv1 fv2 fv3 fv4))
                        (f.1 (fv0 fv1 fv2 fv3 fv4))
                        (g.1 (fv1 fv2 fv3 fv4))
                        (h.1 (fv2 fv3 fv4))
                        (i.1 (fv3 fv4))
                        (j.1 (fv4))
                        (k.1 ())
                        (fv4 (j.1 i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                        (fv3 (i.1 h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                        (fv2 (h.1 g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                        (fv1 (g.1 f.1 e.1 d.1 c.1 b.1 a.1))
                        (fv0 (f.1 e.1 d.1 c.1 b.1 a.1))
                        (r9 (e.1 d.1 c.1 b.1 a.1))
                        (r8 (d.1 c.1 b.1 a.1))
                        (rcx (c.1 b.1 a.1))
                        (rdx (b.1 a.1))
                        (rsi (a.1)))))
                     (begin
                       (set! a.1 rdi)
                       (set! b.1 rsi)
                       (set! c.1 rdx)
                       (set! d.1 rcx)
                       (set! e.1 r8)
                       (set! f.1 r9)
                       (set! g.1 fv0)
                       (set! h.1 fv1)
                       (set! i.1 fv2)
                       (set! j.1 fv3)
                       (set! k.1 fv4)
                       (halt 10)))
                   (begin
                     (set! a.1 1)
                     (set! b.1 2)
                     (set! c.1 3)
                     (set! d.1 4)
                     (set! e.1 5)
                     (set! f.1 6)
                     (set! g.1 7)
                     (set! h.1 8)
                     (set! i.1 9)
                     (set! j.1 10)
                     (set! k.1 11)
                     (set! fv4 k.1)
                     (set! fv3 j.1)
                     (set! fv2 i.1)
                     (set! fv1 h.1)
                     (set! fv0 g.1)
                     (set! r9 f.1)
                     (set! r8 e.1)
                     (set! rcx d.1)
                     (set! rdx c.1)
                     (set! rsi b.1)
                     (set! rdi a.1)
                     (jump L.f.1 rbp rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 fv3 fv4))))
  (check-equal? (conflict-analysis '(module ((locals ()) (undead-out ((rdi rbp) (rdi rbp))))
                                      (define L.f.1
                                        ((locals (x.1)) (undead-out ((x.1) ())))
                                        (begin (set! x.1 rdi) (halt x.1)))
                                      (begin (set! rdi 1) (jump L.f.1 rbp rdi))))
                '(module
                     ((locals ())
                      (conflicts ((rdi (rbp)) (rbp (rdi)))))
                   (define L.f.1
                     ((locals (x.1)) (conflicts ((x.1 ()))))
                     (begin (set! x.1 rdi) (halt x.1)))
                   (begin (set! rdi 1) (jump L.f.1 rbp rdi))))
  (check-equal? (conflict-analysis '(module
                                        ((locals (a.1)) (undead-out ((rbp a.1) (rdi rbp a.1) (rdi rbp a.1))))
                                      (define L.f.1
                                        ((locals (x.1)) (undead-out ((x.1) ())))
                                        (begin (set! x.1 rdi) (halt x.1)))
                                      (begin (set! a.1 L.f.1) (set! rdi 1) (jump a.1 rbp rdi))))
                '(module
                     ((locals (a.1))
                      (conflicts ((a.1 (rdi rbp)) (rbp (rdi a.1)) (rdi (rbp a.1)))))
                   (define L.f.1
                     ((locals (x.1)) (conflicts ((x.1 ()))))
                     (begin (set! x.1 rdi) (halt x.1)))
                   (begin (set! a.1 L.f.1) (set! rdi 1) (jump a.1 rbp rdi))))
  (check-equal? (conflict-analysis '(module
                                        ((locals ()) (undead-out ((rbp r13) (rdi rbp r13) (rdi rbp r13))))
                                      (define L.f.1
                                        ((locals (x.1)) (undead-out ((x.1) ())))
                                        (begin (set! x.1 rdi) (halt x.1)))
                                      (begin (set! r13 L.f.1) (set! rdi 1) (jump r13 rbp rdi))))
                '(module
                     ((locals ()) (conflicts ((r13 (rdi rbp)) (rbp (rdi r13)) (rdi (rbp r13)))))
                   (define L.f.1
                     ((locals (x.1)) (conflicts ((x.1 ()))))
                     (begin (set! x.1 rdi) (halt x.1)))
                   (begin (set! r13 L.f.1) (set! rdi 1) (jump r13 rbp rdi))))
  (check-equal? (conflict-analysis '(module
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
                '(module
                     ((locals ())
                      (conflicts
                       ((rdx (rdi rsi rbp))
                        (rbp (rdi rsi rdx))
                        (rsi (rdi rdx rbp))
                        (rdi (rdx rsi rbp)))))
                   (define L.f.1
                     ((locals (x.1)) (conflicts ((x.1 ()))))
                     (begin (set! x.1 rdi) (halt x.1)))
                   (define L.g.1
                     ((locals (y.1 x.1 z.1))
                      (conflicts
                       ((z.1 (x.1 rbp))
                        (x.1 (z.1 y.1 rsi rdx rbp))
                        (y.1 (rdx x.1 rbp))
                        (rbp (rdi z.1 y.1 x.1))
                        (rdx (y.1 x.1))
                        (rsi (x.1))
                        (rdi (rbp)))))
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

  (check-equal? (conflict-analysis '(module ((locals (x.1)) (undead-out ((x.1) ()))) (begin (set! x.1 42) (halt x.1))))
                '(module ((locals (x.1)) (conflicts ((x.1 ())))) (begin (set! x.1 42) (halt x.1))))
  (match (conflict-analysis '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                                      (undead-out ((v.1)
                                                   (v.1 w.2)
                                                   (w.2 x.3)
                                                   (p.1 w.2 x.3)
                                                   (w.2 x.3)
                                                   (y.4 w.2 x.3)
                                                   (p.1 y.4 w.2 x.3)
                                                   (y.4 w.2 x.3)
                                                   (z.5 y.4 w.2)
                                                   (z.5 y.4)
                                                   (t.6 z.5)
                                                   (t.6 z.5 p.1)
                                                   (t.6 z.5)
                                                   (z.5)
                                                   ())))
                               (begin (set! v.1 1)
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
    [`(module ((locals ,ls) (conflicts ,conflicts)) ,tail)
     (check-true (set=? (get-neighbors conflicts 'v.1) (list 'w.2)))
     (check-true (set=? (get-neighbors conflicts 'w.2) (list 'z.5 'y.4 'p.1 'x.3 'v.1)))
     (check-true (set=? (get-neighbors conflicts 'x.3) (list 'y.4 'p.1 'w.2)))
     (check-true (set=? (get-neighbors conflicts 'y.4) (list 'z.5 'x.3 'p.1 'w.2)))
     (check-true (set=? (get-neighbors conflicts 'z.5) (list 'p.1 't.6 'w.2 'y.4)))
     (check-true (set=? (get-neighbors conflicts 't.6) (list 'p.1 'z.5)))
     (check-true (set=? (get-neighbors conflicts 'p.1) (list 'z.5 't.6 'y.4 'x.3 'w.2)))])

  (match (conflict-analysis (undead-analysis (uncover-locals '(module ()
                                                                (begin (set! x.6 2)
                                                                       (set! x.6 (+ x.6 3))
                                                                       (set! x.7 x.6)
                                                                       (set! x.7 (+ x.7 x.6))
                                                                       (begin (set! y.2 5)
                                                                              (halt x.6)))))))
    [`(module ((locals ,ls) (conflicts ,conflicts)) ,tail)
     (check-true (set=? (get-neighbors conflicts 'y.2) (list 'x.6)))
     (check-true (set=? (get-neighbors conflicts 'x.6) (list 'y.2 'x.7))
                 (format "unexpected conflict graph: ~a" (get-neighbors conflicts 'x.6)))
     (check-true (set=? (get-neighbors conflicts 'x.7) (list 'x.6)))])

  (match (conflict-analysis (undead-analysis (uncover-locals '(module ()
                                                                (begin (set! x.1 1)
                                                                       (set! x.2 x.1)
                                                                       (set! x.1 (+ x.1 x.1))
                                                                       (halt x.2))))))
    [`(module ((locals ,ls) (conflicts ,conflicts)) ,tail)
     (check-true (set=? (get-neighbors conflicts 'x.1) (list 'x.2)))
     (check-true (set=? (get-neighbors conflicts 'x.2) (list 'x.1)))])

  (match (conflict-analysis (undead-analysis (uncover-locals '(module ()
                                                                (begin (set! x.1 1)
                                                                       (set! x.2 x.1)
                                                                       (set! x.2 (+ x.2 x.1))
                                                                       (if (> x.2 2)
                                                                           (set! x.3 3)
                                                                           (set! x.3 4))
                                                                       (set! x.2 (+ x.2 x.3))
                                                                       (halt x.2))))))
    [`(module ((locals ,ls) (conflicts ,conflicts)) ,tail)
     (check-true (set=? (get-neighbors conflicts 'x.1) empty))
     (check-true (set=? (get-neighbors conflicts 'x.2) (list 'x.3)))
     (check-true (set=? (get-neighbors conflicts 'x.3) (list 'x.2)))])

  (match (conflict-analysis (undead-analysis (uncover-locals '(module ()
                                                                (if (if (begin (set! x.1 1)
                                                                               (set! x.2 x.1)
                                                                               (set! x.3 x.1)
                                                                               (set! x.3 (+ x.3 x.2))
                                                                               (true))
                                                                        (true)
                                                                        (false))
                                                                    (halt x.3)
                                                                    (halt x.2))))))
    [`(module ((locals ,ls) (conflicts ,conflicts)) ,tail)
     (check-true (set=? (get-neighbors conflicts 'x.1) empty))
     (check-true (set=? (get-neighbors conflicts 'x.2) (list 'x.3)))
     (check-true (set=? (get-neighbors conflicts 'x.3) (list 'x.2)))]))
