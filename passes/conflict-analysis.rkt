#lang racket

(require "common.rkt"
         "undead-analysis.rkt"
         "uncover-locals.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v7
  rackunit)

(provide conflict-analysis)

;; asm-pred-lang-v7/undead -> asm-pred-lang-v7/conflicts
;; compiles p to Asm-pred-lang v5/conflicts by performing conflict analysis and
;; decorating programs with their conflict graph
(define/contract (conflict-analysis p)
  (-> asm-pred-lang-v7/undead? asm-pred-lang-v7/conflicts?)

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

(module+ test
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
                     (jump L.fact.4 rbp r15 fv0))))
  (check-equal? (conflict-analysis '(module
                                        ((new-frames ())
                                         (locals ())
                                         (call-undead ())
                                         (undead-out (rbp)))
                                      (define L.f.1
                                        ((new-frames ())
                                         (locals (tmp.1))
                                         (call-undead ())
                                         (undead-out ((tmp.1) (tmp.1) ())))
                                        (begin (set! tmp.1 1) (set! tmp.1 (* tmp.1 2)) (jump ra.13 rbp rax)))
                                      (jump L.f.1 rbp)))
                '(module
                     ((new-frames ())
                      (locals ())
                      (call-undead ())
                      (undead-out (rbp))
                      (conflicts ()))
                   (define L.f.1
                     ((new-frames ())
                      (locals (tmp.1))
                      (call-undead ())
                      (undead-out ((tmp.1) (tmp.1) ()))
                      (conflicts ((tmp.1 ()))))
                     (begin (set! tmp.1 1) (set! tmp.1 (* tmp.1 2)) (jump ra.13 rbp rax)))
                   (jump L.f.1 rbp)))
  (check-equal? (conflict-analysis '(module
                                        ((locals ())
                                         (new-frames ())
                                         (call-undead ())
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
                                         (new-frames ())
                                         (call-undead ())
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
                                          (jump ra.13 a.1)))
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
                      (new-frames ())
                      (call-undead ())
                      (undead-out
                       ((r9 rbp)
                        (r9 r8 rbp)
                        (r9 r8 rcx rbp)
                        (r9 r8 rcx rdx rbp)
                        (r9 r8 rcx rdx rsi rbp)
                        (r9 r8 rcx rdx rsi rdi rbp)
                        (r9 r8 rcx rdx rsi rdi rbp)))
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
                      (new-frames ())
                      (call-undead ())
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
                        ()))
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
                       (jump ra.13 a.1)))
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
                                         (new-frames ())
                                         (call-undead ())
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
                                         (new-frames ())
                                         (call-undead ())
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
                                          (jump done)))
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
                      (new-frames ())
                      (call-undead ())
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
                        (fv4 fv3 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)))
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
                      (new-frames ())
                      (call-undead ())
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
                        ()))
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
                       (jump done)))
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
  (check-equal? (conflict-analysis '(module ((locals ())
                                             (new-frames ())
                                             (call-undead ())
                                             (undead-out ((rdi rbp) (rdi rbp))))
                                      (define L.f.1
                                        ((locals (x.1))
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out ((x.1) ())))
                                        (begin (set! x.1 rdi) (jump done x.1)))
                                      (begin (set! rdi 1) (jump L.f.1 rbp rdi))))
                '(module
                     ((locals ())
                      (new-frames ())
                      (call-undead ())
                      (undead-out ((rdi rbp) (rdi rbp)))
                      (conflicts ((rdi (rbp)) (rbp (rdi)))))
                   (define L.f.1
                     ((locals (x.1))
                      (new-frames ())
                      (call-undead ())
                      (undead-out ((x.1) ()))
                      (conflicts ((x.1 ()))))
                     (begin (set! x.1 rdi) (jump done x.1)))
                   (begin (set! rdi 1) (jump L.f.1 rbp rdi))))
  (check-equal? (conflict-analysis '(module
                                        ((locals (a.1))
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out ((rbp a.1) (rdi rbp a.1) (rdi rbp a.1))))
                                      (define L.f.1
                                        ((locals (x.1))
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out ((x.1) ())))
                                        (begin (set! x.1 rdi) (jump done x.1)))
                                      (begin (set! a.1 L.f.1) (set! rdi 1) (jump a.1 rbp rdi))))
                '(module
                     ((locals (a.1))
                      (new-frames ())
                      (call-undead ())
                      (undead-out ((rbp a.1) (rdi rbp a.1) (rdi rbp a.1)))
                      (conflicts ((a.1 (rdi rbp)) (rbp (rdi a.1)) (rdi (rbp a.1)))))
                   (define L.f.1
                     ((locals (x.1))
                      (new-frames ())
                      (call-undead ())
                      (undead-out ((x.1) ()))
                      (conflicts ((x.1 ()))))
                     (begin (set! x.1 rdi) (jump done x.1)))
                   (begin (set! a.1 L.f.1) (set! rdi 1) (jump a.1 rbp rdi))))
  (check-equal? (conflict-analysis '(module
                                        ((locals ())
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out ((rbp r13) (rdi rbp r13) (rdi rbp r13))))
                                      (define L.f.1
                                        ((locals (x.1))
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out ((x.1) ())))
                                        (begin (set! x.1 rdi) (jump done x.1)))
                                      (begin (set! r13 L.f.1) (set! rdi 1) (jump r13 rbp rdi))))
                '(module
                     ((locals ())
                      (new-frames ())
                      (call-undead ())
                      (undead-out ((rbp r13) (rdi rbp r13) (rdi rbp r13)))
                      (conflicts ((r13 (rdi rbp)) (rbp (rdi r13)) (rdi (rbp r13)))))
                   (define L.f.1
                     ((locals (x.1))
                      (new-frames ())
                      (call-undead ())
                      (undead-out ((x.1) ()))
                      (conflicts ((x.1 ()))))
                     (begin (set! x.1 rdi) (jump done x.1)))
                   (begin (set! r13 L.f.1) (set! rdi 1) (jump r13 rbp rdi))))
  (check-equal? (conflict-analysis '(module
                                        ((locals ())
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out
                                          ((rbp)
                                           ((rdx rbp) (rdx rsi rbp) (rdx rsi rdi rbp) (rdx rsi rdi rbp))
                                           ((rdi rbp) (rdi rbp)))))
                                      (define L.f.1
                                        ((locals (x.1))
                                         (new-frames ())
                                         (call-undead ())
                                         (undead-out ((x.1) ())))
                                        (begin (set! x.1 rdi) (jump done x.1)))
                                      (define L.g.1
                                        ((locals (y.1 x.1 z.1))
                                         (new-frames ())
                                         (call-undead ())
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
                      (new-frames ())
                      (call-undead ())
                      (undead-out
                       ((rbp)
                        ((rdx rbp) (rdx rsi rbp) (rdx rsi rdi rbp) (rdx rsi rdi rbp))
                        ((rdi rbp) (rdi rbp))))
                      (conflicts
                       ((rdx (rdi rsi rbp))
                        (rbp (rdi rsi rdx))
                        (rsi (rdi rdx rbp))
                        (rdi (rdx rsi rbp)))))
                   (define L.f.1
                     ((locals (x.1))
                      (new-frames ())
                      (call-undead ())
                      (undead-out ((x.1) ()))
                      (conflicts ((x.1 ()))))
                     (begin (set! x.1 rdi) (jump done x.1)))
                   (define L.g.1
                     ((locals (y.1 x.1 z.1))
                      (new-frames ())
                      (call-undead ())
                      (undead-out
                       ((rsi rdx x.1 rbp) (rdx x.1 rbp) (x.1 rbp) (rdi rbp) (rdi rbp)))
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
  (check-equal? (conflict-analysis '(module ((locals (x.1))
                                             (new-frames ())
                                             (call-undead ())
                                             (undead-out ((x.1) ())))
                                      (begin (set! x.1 42) (jump done x.1))))
                '(module ((locals (x.1))
                          (new-frames ())
                          (call-undead ())
                          (undead-out ((x.1) ()))
                          (conflicts ((x.1 ()))))
                   (begin (set! x.1 42) (jump done x.1))))

  (match (conflict-analysis '(module ((locals (v.1 w.2 x.3 y.4 z.5 t.6 p.1))
                                      (new-frames ())
                                      (call-undead ())
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
                                      (jump done z.5))))
    [`(module ((locals ,ls) (new-frames ()) (call-undead ()) (undead-out ,udt) (conflicts ,conflicts)) ,tail)
     (check-true (set=? (get-neighbors conflicts 'v.1) (list 'w.2)))
     (check-true (set=? (get-neighbors conflicts 'w.2) (list 'z.5 'y.4 'p.1 'x.3 'v.1)))
     (check-true (set=? (get-neighbors conflicts 'x.3) (list 'y.4 'p.1 'w.2)))
     (check-true (set=? (get-neighbors conflicts 'y.4) (list 'z.5 'x.3 'p.1 'w.2)))
     (check-true (set=? (get-neighbors conflicts 'z.5) (list 'p.1 't.6 'w.2 'y.4)))
     (check-true (set=? (get-neighbors conflicts 't.6) (list 'p.1 'z.5)))
     (check-true (set=? (get-neighbors conflicts 'p.1) (list 'z.5 't.6 'y.4 'x.3 'w.2)))])
  (check-equal? (conflict-analysis '(module
                                        ((new-frames ())
                                         (locals (x.3 tmp-ra.2 x.2))
                                         (call-undead ())
                                         (undead-out
                                          ((tmp-ra.2 rbp)
                                           (x.2 tmp-ra.2 rbp)
                                           (((x.3 x.2 tmp-ra.2 rbp) (x.2 tmp-ra.2 rbp))
                                            ((tmp-ra.2 rdi rbp) (rdi r15 rbp) (rbp r15 rdi))
                                            ((tmp-ra.2 rdi rbp) (rdi r15 rbp) (rbp r15 rdi))))))
                                      (define L.f.1
                                        ((new-frames ())
                                         (locals (tmp-ra.1 b.1 y.1 x.1 z.1 a.1))
                                         (undead-out
                                          ((rdi rbp tmp-ra.1)
                                           (x.1 rbp tmp-ra.1)
                                           (y.1 x.1 rbp tmp-ra.1)
                                           (y.1 z.1 x.1 rbp tmp-ra.1)
                                           (a.1 z.1 x.1 rbp tmp-ra.1)
                                           (z.1 x.1 a.1 rbp tmp-ra.1)
                                           (x.1 b.1 a.1 rbp tmp-ra.1)
                                           (b.1 a.1 rbp tmp-ra.1)
                                           (a.1 rbp tmp-ra.1)
                                           (rax rbp tmp-ra.1)
                                           (rax rbp tmp-ra.1)
                                           (rbp rax)))
                                         (call-undead ()))
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
                '(module
                     ((new-frames ())
                      (locals (x.3 tmp-ra.2 x.2))
                      (call-undead ())
                      (undead-out
                       ((tmp-ra.2 rbp)
                        (x.2 tmp-ra.2 rbp)
                        (((x.3 x.2 tmp-ra.2 rbp) (x.2 tmp-ra.2 rbp))
                         ((tmp-ra.2 rdi rbp) (rdi r15 rbp) (rbp r15 rdi))
                         ((tmp-ra.2 rdi rbp) (rdi r15 rbp) (rbp r15 rdi)))))
                      (conflicts
                       ((x.2 (x.3 tmp-ra.2 rbp))
                        (tmp-ra.2 (rdi x.3 x.2 rbp))
                        (x.3 (x.2 tmp-ra.2 rbp))
                        (rbp (r15 rdi x.3 x.2 tmp-ra.2))
                        (rdi (r15 tmp-ra.2 rbp))
                        (r15 (rdi rbp)))))
                   (define L.f.1
                     ((new-frames ())
                      (locals (tmp-ra.1 b.1 y.1 x.1 z.1 a.1))
                      (undead-out
                       ((rdi rbp tmp-ra.1)
                        (x.1 rbp tmp-ra.1)
                        (y.1 x.1 rbp tmp-ra.1)
                        (y.1 z.1 x.1 rbp tmp-ra.1)
                        (a.1 z.1 x.1 rbp tmp-ra.1)
                        (z.1 x.1 a.1 rbp tmp-ra.1)
                        (x.1 b.1 a.1 rbp tmp-ra.1)
                        (b.1 a.1 rbp tmp-ra.1)
                        (a.1 rbp tmp-ra.1)
                        (rax rbp tmp-ra.1)
                        (rax rbp tmp-ra.1)
                        (rbp rax)))
                      (call-undead ())
                      (conflicts
                       ((a.1 (b.1 z.1 x.1 rbp tmp-ra.1))
                        (z.1 (a.1 y.1 x.1 rbp tmp-ra.1))
                        (x.1 (b.1 a.1 z.1 y.1 rbp tmp-ra.1))
                        (y.1 (z.1 x.1 rbp tmp-ra.1))
                        (b.1 (x.1 a.1 rbp tmp-ra.1))
                        (tmp-ra.1 (rax b.1 a.1 z.1 y.1 x.1 rdi rbp))
                        (rbp (rax b.1 a.1 z.1 y.1 x.1 tmp-ra.1))
                        (rdi (tmp-ra.1))
                        (rax (rbp tmp-ra.1)))))
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
  (check-equal? (conflict-analysis '(module
                                        ((new-frames ())
                                         (locals (tmp-ra.238))
                                         (call-undead ())
                                         (undead-out
                                          ((tmp-ra.238 rbp)
                                           (tmp-ra.238 rdi rbp)
                                           (tmp-ra.238 rsi rdi rbp)
                                           (tmp-ra.238 rdx rsi rdi rbp)
                                           (tmp-ra.238 rcx rdx rsi rdi rbp)
                                           (tmp-ra.238 r8 rcx rdx rsi rdi rbp)
                                           (tmp-ra.238 r9 r8 rcx rdx rsi rdi rbp)
                                           (tmp-ra.238 fv0 r9 r8 rcx rdx rsi rdi rbp)
                                           (fv0 r9 r8 rcx rdx rsi rdi r15 rbp)
                                           (fv0 r9 r8 rcx rdx rsi rdi r15 rbp))))
                                      (define L.+.31
                                        ((new-frames ())
                                         (locals (tmp.185 tmp.96 tmp.186 tmp-ra.232 tmp.184 tmp.97 tmp.183))
                                         (undead-out
                                          ((rdi rsi tmp-ra.232 rbp)
                                           (rsi tmp.96 tmp-ra.232 rbp)
                                           (tmp.96 tmp.97 tmp-ra.232 rbp)
                                           (((((tmp.184 tmp.96 tmp.97 tmp-ra.232 rbp)
                                               (tmp.184 tmp.96 tmp.97 tmp-ra.232 rbp)
                                               (tmp.96 tmp.97 tmp-ra.232 rbp))
                                              (tmp.183 tmp.96 tmp.97 tmp-ra.232 rbp)
                                              (tmp.183 tmp.96 tmp.97 tmp-ra.232 rbp))
                                             (tmp.96 tmp.97 tmp-ra.232 rbp))
                                            (((((tmp.186 tmp.96 tmp.97 tmp-ra.232 rbp)
                                                (tmp.186 tmp.96 tmp.97 tmp-ra.232 rbp)
                                                (tmp.96 tmp.97 tmp-ra.232 rbp))
                                               (tmp.185 tmp.96 tmp.97 tmp-ra.232 rbp)
                                               (tmp.185 tmp.96 tmp.97 tmp-ra.232 rbp))
                                              (tmp.96 tmp.97 tmp-ra.232 rbp))
                                             ((tmp.97 rax tmp-ra.232 rbp) (tmp-ra.232 rax rbp) (rax rbp))
                                             ((tmp-ra.232 rax rbp) (rax rbp)))
                                            ((tmp-ra.232 rax rbp) (rax rbp)))))
                                         (call-undead ()))
                                        (begin
                                          (set! tmp-ra.232 r15)
                                          (set! tmp.96 rdi)
                                          (set! tmp.97 rsi)
                                          (if (begin
                                                (if (begin
                                                      (set! tmp.184 tmp.97)
                                                      (set! tmp.184 (bitwise-and tmp.184 7))
                                                      (= tmp.184 0))
                                                    (set! tmp.183 14)
                                                    (set! tmp.183 6))
                                                (!= tmp.183 6))
                                              (if (begin
                                                    (if (begin
                                                          (set! tmp.186 tmp.96)
                                                          (set! tmp.186 (bitwise-and tmp.186 7))
                                                          (= tmp.186 0))
                                                        (set! tmp.185 14)
                                                        (set! tmp.185 6))
                                                    (!= tmp.185 6))
                                                  (begin
                                                    (set! rax tmp.96)
                                                    (set! rax (+ rax tmp.97))
                                                    (jump tmp-ra.232 rbp rax))
                                                  (begin (set! rax 574) (jump tmp-ra.232 rbp rax)))
                                              (begin (set! rax 574) (jump tmp-ra.232 rbp rax)))))
                                      (define L.F.6
                                        ((new-frames ())
                                         (locals
                                          (tmp.187 nfv.234 nfv.235 g.25 f.24 e.23 d.22 c.21 b.20 a.19 tmp-ra.233))
                                         (undead-out
                                          ((rdi rsi rdx rcx r8 r9 fv0 tmp-ra.233 rbp)
                                           (rsi rdx rcx r8 r9 fv0 a.19 tmp-ra.233 rbp)
                                           (rdx rcx r8 r9 fv0 b.20 a.19 tmp-ra.233 rbp)
                                           (rcx r8 r9 fv0 c.21 b.20 a.19 tmp-ra.233 rbp)
                                           (r8 r9 fv0 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                                           (r9 fv0 e.23 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                                           (fv0 f.24 e.23 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                                           (g.25 f.24 e.23 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                                           ((rax tmp-ra.233 rbp)
                                            ((b.20 c.21 d.22 e.23 f.24 g.25 rdi rbp)
                                             (c.21 d.22 e.23 f.24 g.25 rsi rdi rbp)
                                             (d.22 e.23 f.24 g.25 rdx rsi rdi rbp)
                                             (e.23 f.24 g.25 rcx rdx rsi rdi rbp)
                                             (f.24 g.25 r8 rcx rdx rsi rdi rbp)
                                             (g.25 r9 r8 rcx rdx rsi rdi rbp)
                                             (nfv.234 r9 r8 rcx rdx rsi rdi rbp)
                                             (nfv.235 nfv.234 r9 r8 rcx rdx rsi rdi rbp)
                                             (nfv.235 nfv.234 r9 r8 rcx rdx rsi rdi r15 rbp)
                                             (nfv.235 nfv.234 r9 r8 rcx rdx rsi rdi r15 rbp)))
                                           (tmp.187 tmp-ra.233 rbp)
                                           (tmp.187 tmp-ra.233 rdi rbp)
                                           (tmp-ra.233 rsi rdi rbp)
                                           (rsi rdi r15 rbp)
                                           (rsi rdi r15 rbp)))
                                         (call-undead (tmp-ra.233)))
                                        (begin
                                          (set! tmp-ra.233 r15)
                                          (set! a.19 rdi)
                                          (set! b.20 rsi)
                                          (set! c.21 rdx)
                                          (set! d.22 rcx)
                                          (set! e.23 r8)
                                          (set! f.24 r9)
                                          (set! g.25 fv0)
                                          (return-point L.rp.47
                                                        (begin
                                                          (set! rdi a.19)
                                                          (set! rsi b.20)
                                                          (set! rdx c.21)
                                                          (set! rcx d.22)
                                                          (set! r8 e.23)
                                                          (set! r9 f.24)
                                                          (set! nfv.234 g.25)
                                                          (set! nfv.235 64)
                                                          (set! r15 L.rp.47)
                                                          (jump L.G.7 rbp r15 rdi rsi rdx rcx r8 r9 nfv.234 nfv.235)))
                                          (set! tmp.187 rax)
                                          (set! rdi 80)
                                          (set! rsi tmp.187)
                                          (set! r15 tmp-ra.233)
                                          (jump L.+.31 rbp r15 rdi rsi)))
                                      (define L.G.7
                                        ((new-frames ())
                                         (locals (h.33 g.32 f.31 e.30 d.29 c.28 b.27 a.26 tmp-ra.236))
                                         (undead-out
                                          ((rdi rsi rdx rcx r8 r9 fv0 fv1 tmp-ra.236 rbp)
                                           (rsi rdx rcx r8 r9 fv0 fv1 a.26 tmp-ra.236 rbp)
                                           (rdx rcx r8 r9 fv0 fv1 a.26 b.27 tmp-ra.236 rbp)
                                           (rcx r8 r9 fv0 fv1 a.26 b.27 c.28 tmp-ra.236 rbp)
                                           (r8 r9 fv0 fv1 a.26 b.27 c.28 d.29 tmp-ra.236 rbp)
                                           (r9 fv0 fv1 a.26 b.27 c.28 d.29 e.30 tmp-ra.236 rbp)
                                           (fv0 fv1 a.26 b.27 c.28 d.29 e.30 f.31 tmp-ra.236 rbp)
                                           (fv1 a.26 b.27 c.28 d.29 e.30 f.31 g.32 tmp-ra.236 rbp)
                                           (a.26 b.27 c.28 d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rbp)
                                           (b.27 c.28 d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rdi rbp)
                                           (c.28 d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rsi rdi rbp)
                                           (d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rdx rsi rdi rbp)
                                           (e.30 f.31 g.32 h.33 tmp-ra.236 rcx rdx rsi rdi rbp)
                                           (f.31 g.32 h.33 tmp-ra.236 r8 rcx rdx rsi rdi rbp)
                                           (g.32 h.33 tmp-ra.236 r9 r8 rcx rdx rsi rdi rbp)
                                           (h.33 tmp-ra.236 fv0 r9 r8 rcx rdx rsi rdi rbp)
                                           (tmp-ra.236 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
                                           (tmp-ra.236 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
                                           (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi r15 rbp)
                                           (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi r15 rbp)))
                                         (call-undead ()))
                                        (begin
                                          (set! tmp-ra.236 r15)
                                          (set! a.26 rdi)
                                          (set! b.27 rsi)
                                          (set! c.28 rdx)
                                          (set! d.29 rcx)
                                          (set! e.30 r8)
                                          (set! f.31 r9)
                                          (set! g.32 fv0)
                                          (set! h.33 fv1)
                                          (set! rdi a.26)
                                          (set! rsi b.27)
                                          (set! rdx c.28)
                                          (set! rcx d.29)
                                          (set! r8 e.30)
                                          (set! r9 f.31)
                                          (set! fv0 g.32)
                                          (set! fv1 h.33)
                                          (set! fv2 72)
                                          (set! r15 tmp-ra.236)
                                          (jump L.H.8 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2)))
                                      (define L.H.8
                                        ((new-frames ())
                                         (locals
                                          (r7.49
                                           r6.48
                                           r5.47
                                           r4.46
                                           r3.45
                                           r2.44
                                           r1.43
                                           j.42
                                           h.41
                                           g.40
                                           f.39
                                           e.38
                                           d.37
                                           c.36
                                           b.35
                                           a.34
                                           tmp-ra.237))
                                         (undead-out
                                          ((rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.237 rbp)
                                           (rsi rdx rcx r8 r9 fv0 fv1 fv2 a.34 tmp-ra.237 rbp)
                                           (rdx rcx r8 r9 fv0 fv1 fv2 b.35 a.34 tmp-ra.237 rbp)
                                           (rcx r8 r9 fv0 fv1 fv2 b.35 a.34 c.36 tmp-ra.237 rbp)
                                           (r8 r9 fv0 fv1 fv2 b.35 a.34 c.36 d.37 tmp-ra.237 rbp)
                                           (r9 fv0 fv1 fv2 b.35 a.34 c.36 d.37 e.38 tmp-ra.237 rbp)
                                           (fv0 fv1 fv2 b.35 a.34 c.36 d.37 e.38 f.39 tmp-ra.237 rbp)
                                           (fv1 fv2 b.35 a.34 c.36 d.37 e.38 f.39 g.40 tmp-ra.237 rbp)
                                           (fv2 b.35 a.34 c.36 d.37 e.38 f.39 g.40 h.41 tmp-ra.237 rbp)
                                           (b.35 a.34 c.36 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                           ((rax c.36 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                            ((b.35 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                           (c.36 r1.43 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                           ((rax d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                            ((c.36 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                           (d.37 r2.44 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                           ((rax e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                            ((d.37 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                           (e.38 r3.45 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                           ((rax f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                                            ((e.38 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                           (f.39 r4.46 g.40 h.41 j.42 tmp-ra.237 rbp)
                                           ((rax g.40 h.41 j.42 tmp-ra.237 rbp)
                                            ((f.39 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                           (g.40 r5.47 h.41 j.42 tmp-ra.237 rbp)
                                           ((rax h.41 j.42 tmp-ra.237 rbp)
                                            ((g.40 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                           (h.41 r6.48 j.42 tmp-ra.237 rbp)
                                           ((rax j.42 tmp-ra.237 rbp)
                                            ((h.41 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                                           (r7.49 j.42 tmp-ra.237 rbp)
                                           (j.42 tmp-ra.237 rdi rbp)
                                           (tmp-ra.237 rsi rdi rbp)
                                           (rsi rdi r15 rbp)
                                           (rsi rdi r15 rbp)))
                                         (call-undead (c.36 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237)))
                                        (begin
                                          (set! tmp-ra.237 r15)
                                          (set! a.34 rdi)
                                          (set! b.35 rsi)
                                          (set! c.36 rdx)
                                          (set! d.37 rcx)
                                          (set! e.38 r8)
                                          (set! f.39 r9)
                                          (set! g.40 fv0)
                                          (set! h.41 fv1)
                                          (set! j.42 fv2)
                                          (return-point L.rp.48
                                                        (begin
                                                          (set! rdi a.34)
                                                          (set! rsi b.35)
                                                          (set! r15 L.rp.48)
                                                          (jump L.+.31 rbp r15 rdi rsi)))
                                          (set! r1.43 rax)
                                          (return-point L.rp.49
                                                        (begin
                                                          (set! rdi r1.43)
                                                          (set! rsi c.36)
                                                          (set! r15 L.rp.49)
                                                          (jump L.+.31 rbp r15 rdi rsi)))
                                          (set! r2.44 rax)
                                          (return-point L.rp.50
                                                        (begin
                                                          (set! rdi r2.44)
                                                          (set! rsi d.37)
                                                          (set! r15 L.rp.50)
                                                          (jump L.+.31 rbp r15 rdi rsi)))
                                          (set! r3.45 rax)
                                          (return-point L.rp.51
                                                        (begin
                                                          (set! rdi r3.45)
                                                          (set! rsi e.38)
                                                          (set! r15 L.rp.51)
                                                          (jump L.+.31 rbp r15 rdi rsi)))
                                          (set! r4.46 rax)
                                          (return-point L.rp.52
                                                        (begin
                                                          (set! rdi r4.46)
                                                          (set! rsi f.39)
                                                          (set! r15 L.rp.52)
                                                          (jump L.+.31 rbp r15 rdi rsi)))
                                          (set! r5.47 rax)
                                          (return-point L.rp.53
                                                        (begin
                                                          (set! rdi r5.47)
                                                          (set! rsi g.40)
                                                          (set! r15 L.rp.53)
                                                          (jump L.+.31 rbp r15 rdi rsi)))
                                          (set! r6.48 rax)
                                          (return-point L.rp.54
                                                        (begin
                                                          (set! rdi r6.48)
                                                          (set! rsi h.41)
                                                          (set! r15 L.rp.54)
                                                          (jump L.+.31 rbp r15 rdi rsi)))
                                          (set! r7.49 rax)
                                          (set! rdi r7.49)
                                          (set! rsi j.42)
                                          (set! r15 tmp-ra.237)
                                          (jump L.+.31 rbp r15 rdi rsi)))
                                      (begin
                                        (set! tmp-ra.238 r15)
                                        (set! rdi 8)
                                        (set! rsi 16)
                                        (set! rdx 24)
                                        (set! rcx 32)
                                        (set! r8 40)
                                        (set! r9 48)
                                        (set! fv0 56)
                                        (set! r15 tmp-ra.238)
                                        (jump L.F.6 rbp r15 rdi rsi rdx rcx r8 r9 fv0))))
                '(module
                     ((new-frames ())
                      (locals (tmp-ra.238))
                      (call-undead ())
                      (undead-out
                       ((tmp-ra.238 rbp)
                        (tmp-ra.238 rdi rbp)
                        (tmp-ra.238 rsi rdi rbp)
                        (tmp-ra.238 rdx rsi rdi rbp)
                        (tmp-ra.238 rcx rdx rsi rdi rbp)
                        (tmp-ra.238 r8 rcx rdx rsi rdi rbp)
                        (tmp-ra.238 r9 r8 rcx rdx rsi rdi rbp)
                        (tmp-ra.238 fv0 r9 r8 rcx rdx rsi rdi rbp)
                        (fv0 r9 r8 rcx rdx rsi rdi r15 rbp)
                        (fv0 r9 r8 rcx rdx rsi rdi r15 rbp)))
                      (conflicts
                       ((tmp-ra.238 (fv0 r9 r8 rcx rdx rsi rdi rbp))
                        (rbp (r15 fv0 r9 r8 rcx rdx rsi rdi tmp-ra.238))
                        (rdi (r15 fv0 r9 r8 rcx rdx rsi rbp tmp-ra.238))
                        (rsi (r15 fv0 r9 r8 rcx rdx rbp rdi tmp-ra.238))
                        (rdx (r15 fv0 r9 r8 rcx rbp rdi rsi tmp-ra.238))
                        (rcx (r15 fv0 r9 r8 rbp rdi rsi rdx tmp-ra.238))
                        (r8 (r15 fv0 r9 rbp rdi rsi rdx rcx tmp-ra.238))
                        (r9 (r15 fv0 rbp rdi rsi rdx rcx r8 tmp-ra.238))
                        (fv0 (r15 rbp rdi rsi rdx rcx r8 r9 tmp-ra.238))
                        (r15 (rbp rdi rsi rdx rcx r8 r9 fv0)))))
                   (define L.+.31
                     ((new-frames ())
                      (locals (tmp.185 tmp.96 tmp.186 tmp-ra.232 tmp.184 tmp.97 tmp.183))
                      (undead-out
                       ((rdi rsi tmp-ra.232 rbp)
                        (rsi tmp.96 tmp-ra.232 rbp)
                        (tmp.96 tmp.97 tmp-ra.232 rbp)
                        (((((tmp.184 tmp.96 tmp.97 tmp-ra.232 rbp)
                            (tmp.184 tmp.96 tmp.97 tmp-ra.232 rbp)
                            (tmp.96 tmp.97 tmp-ra.232 rbp))
                           (tmp.183 tmp.96 tmp.97 tmp-ra.232 rbp)
                           (tmp.183 tmp.96 tmp.97 tmp-ra.232 rbp))
                          (tmp.96 tmp.97 tmp-ra.232 rbp))
                         (((((tmp.186 tmp.96 tmp.97 tmp-ra.232 rbp)
                             (tmp.186 tmp.96 tmp.97 tmp-ra.232 rbp)
                             (tmp.96 tmp.97 tmp-ra.232 rbp))
                            (tmp.185 tmp.96 tmp.97 tmp-ra.232 rbp)
                            (tmp.185 tmp.96 tmp.97 tmp-ra.232 rbp))
                           (tmp.96 tmp.97 tmp-ra.232 rbp))
                          ((tmp.97 rax tmp-ra.232 rbp) (tmp-ra.232 rax rbp) (rax rbp))
                          ((tmp-ra.232 rax rbp) (rax rbp)))
                         ((tmp-ra.232 rax rbp) (rax rbp)))))
                      (call-undead ())
                      (conflicts
                       ((tmp.183 (rbp tmp-ra.232 tmp.97 tmp.96))
                        (tmp.97 (rbp tmp-ra.232 tmp.96 tmp.184 tmp.183 tmp.186 tmp.185 rax))
                        (tmp.184 (tmp.97 rbp tmp-ra.232 tmp.96))
                        (tmp-ra.232
                         (tmp.97 tmp.96 rbp rsi rdi tmp.184 tmp.183 tmp.186 tmp.185 rax))
                        (tmp.186 (tmp.96 rbp tmp-ra.232 tmp.97))
                        (tmp.96 (tmp.97 rbp tmp-ra.232 rsi tmp.184 tmp.183 tmp.186 tmp.185))
                        (tmp.185 (rbp tmp-ra.232 tmp.97 tmp.96))
                        (rax (tmp.97 rbp tmp-ra.232))
                        (rbp (tmp.97 tmp.96 tmp-ra.232 tmp.184 tmp.183 tmp.186 tmp.185 rax))
                        (rdi (tmp-ra.232))
                        (rsi (tmp.96 tmp-ra.232)))))
                     (begin
                       (set! tmp-ra.232 r15)
                       (set! tmp.96 rdi)
                       (set! tmp.97 rsi)
                       (if (begin
                             (if (begin
                                   (set! tmp.184 tmp.97)
                                   (set! tmp.184 (bitwise-and tmp.184 7))
                                   (= tmp.184 0))
                                 (set! tmp.183 14)
                                 (set! tmp.183 6))
                             (!= tmp.183 6))
                           (if (begin
                                 (if (begin
                                       (set! tmp.186 tmp.96)
                                       (set! tmp.186 (bitwise-and tmp.186 7))
                                       (= tmp.186 0))
                                     (set! tmp.185 14)
                                     (set! tmp.185 6))
                                 (!= tmp.185 6))
                               (begin
                                 (set! rax tmp.96)
                                 (set! rax (+ rax tmp.97))
                                 (jump tmp-ra.232 rbp rax))
                               (begin (set! rax 574) (jump tmp-ra.232 rbp rax)))
                           (begin (set! rax 574) (jump tmp-ra.232 rbp rax)))))
                   (define L.F.6
                     ((new-frames ())
                      (locals
                       (tmp.187 nfv.234 nfv.235 g.25 f.24 e.23 d.22 c.21 b.20 a.19 tmp-ra.233))
                      (undead-out
                       ((rdi rsi rdx rcx r8 r9 fv0 tmp-ra.233 rbp)
                        (rsi rdx rcx r8 r9 fv0 a.19 tmp-ra.233 rbp)
                        (rdx rcx r8 r9 fv0 b.20 a.19 tmp-ra.233 rbp)
                        (rcx r8 r9 fv0 c.21 b.20 a.19 tmp-ra.233 rbp)
                        (r8 r9 fv0 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                        (r9 fv0 e.23 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                        (fv0 f.24 e.23 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                        (g.25 f.24 e.23 d.22 c.21 b.20 a.19 tmp-ra.233 rbp)
                        ((rax tmp-ra.233 rbp)
                         ((b.20 c.21 d.22 e.23 f.24 g.25 rdi rbp)
                          (c.21 d.22 e.23 f.24 g.25 rsi rdi rbp)
                          (d.22 e.23 f.24 g.25 rdx rsi rdi rbp)
                          (e.23 f.24 g.25 rcx rdx rsi rdi rbp)
                          (f.24 g.25 r8 rcx rdx rsi rdi rbp)
                          (g.25 r9 r8 rcx rdx rsi rdi rbp)
                          (nfv.234 r9 r8 rcx rdx rsi rdi rbp)
                          (nfv.235 nfv.234 r9 r8 rcx rdx rsi rdi rbp)
                          (nfv.235 nfv.234 r9 r8 rcx rdx rsi rdi r15 rbp)
                          (nfv.235 nfv.234 r9 r8 rcx rdx rsi rdi r15 rbp)))
                        (tmp.187 tmp-ra.233 rbp)
                        (tmp.187 tmp-ra.233 rdi rbp)
                        (tmp-ra.233 rsi rdi rbp)
                        (rsi rdi r15 rbp)
                        (rsi rdi r15 rbp)))
                      (call-undead (tmp-ra.233))
                      (conflicts
                       ((tmp-ra.233
                         (tmp.187
                          g.25
                          f.24
                          e.23
                          d.22
                          c.21
                          b.20
                          a.19
                          rbp
                          fv0
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          rdi))
                        (a.19
                         (g.25 f.24 e.23 d.22 c.21 b.20 rbp tmp-ra.233 fv0 r9 r8 rcx rdx rsi))
                        (b.20
                         (rdi g.25 f.24 e.23 d.22 c.21 rbp tmp-ra.233 a.19 fv0 r9 r8 rcx rdx))
                        (c.21
                         (rsi rdi g.25 f.24 e.23 d.22 rbp tmp-ra.233 a.19 b.20 fv0 r9 r8 rcx))
                        (d.22
                         (rdx rsi rdi g.25 f.24 e.23 rbp tmp-ra.233 a.19 b.20 c.21 fv0 r9 r8))
                        (e.23
                         (rcx rdx rsi rdi g.25 f.24 rbp tmp-ra.233 a.19 b.20 c.21 d.22 fv0 r9))
                        (f.24
                         (r8 rcx rdx rsi rdi g.25 rbp tmp-ra.233 a.19 b.20 c.21 d.22 e.23 fv0))
                        (g.25
                         (r9 r8 rcx rdx rsi rdi rbp tmp-ra.233 a.19 b.20 c.21 d.22 e.23 f.24))
                        (nfv.235 (r15 rbp rdi rsi rdx rcx r8 r9 nfv.234))
                        (nfv.234 (r15 nfv.235 rbp rdi rsi rdx rcx r8 r9))
                        (tmp.187 (rdi rbp tmp-ra.233))
                        (rdi
                         (tmp.187
                          r15
                          nfv.235
                          nfv.234
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          rbp
                          g.25
                          f.24
                          e.23
                          d.22
                          c.21
                          b.20
                          tmp-ra.233))
                        (rsi
                         (r15
                          nfv.235
                          nfv.234
                          r9
                          r8
                          rcx
                          rdx
                          rbp
                          rdi
                          g.25
                          f.24
                          e.23
                          d.22
                          c.21
                          a.19
                          tmp-ra.233))
                        (rdx
                         (r15
                          nfv.235
                          nfv.234
                          r9
                          r8
                          rcx
                          rbp
                          rdi
                          rsi
                          g.25
                          f.24
                          e.23
                          d.22
                          b.20
                          a.19
                          tmp-ra.233))
                        (rcx
                         (r15
                          nfv.235
                          nfv.234
                          r9
                          r8
                          rbp
                          rdi
                          rsi
                          rdx
                          g.25
                          f.24
                          e.23
                          c.21
                          b.20
                          a.19
                          tmp-ra.233))
                        (r8
                         (r15
                          nfv.235
                          nfv.234
                          r9
                          rbp
                          rdi
                          rsi
                          rdx
                          rcx
                          g.25
                          f.24
                          d.22
                          c.21
                          b.20
                          a.19
                          tmp-ra.233))
                        (r9
                         (r15
                          nfv.235
                          nfv.234
                          rbp
                          rdi
                          rsi
                          rdx
                          rcx
                          r8
                          g.25
                          e.23
                          d.22
                          c.21
                          b.20
                          a.19
                          tmp-ra.233))
                        (fv0 (f.24 e.23 d.22 c.21 b.20 a.19 tmp-ra.233))
                        (rbp
                         (tmp.187
                          r15
                          nfv.235
                          nfv.234
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          rdi
                          g.25
                          f.24
                          e.23
                          d.22
                          c.21
                          b.20
                          a.19
                          tmp-ra.233))
                        (r15 (rbp rdi rsi rdx rcx r8 r9 nfv.234 nfv.235)))))
                     (begin
                       (set! tmp-ra.233 r15)
                       (set! a.19 rdi)
                       (set! b.20 rsi)
                       (set! c.21 rdx)
                       (set! d.22 rcx)
                       (set! e.23 r8)
                       (set! f.24 r9)
                       (set! g.25 fv0)
                       (return-point L.rp.47
                                     (begin
                                       (set! rdi a.19)
                                       (set! rsi b.20)
                                       (set! rdx c.21)
                                       (set! rcx d.22)
                                       (set! r8 e.23)
                                       (set! r9 f.24)
                                       (set! nfv.234 g.25)
                                       (set! nfv.235 64)
                                       (set! r15 L.rp.47)
                                       (jump L.G.7 rbp r15 rdi rsi rdx rcx r8 r9 nfv.234 nfv.235)))
                       (set! tmp.187 rax)
                       (set! rdi 80)
                       (set! rsi tmp.187)
                       (set! r15 tmp-ra.233)
                       (jump L.+.31 rbp r15 rdi rsi)))
                   (define L.G.7
                     ((new-frames ())
                      (locals (h.33 g.32 f.31 e.30 d.29 c.28 b.27 a.26 tmp-ra.236))
                      (undead-out
                       ((rdi rsi rdx rcx r8 r9 fv0 fv1 tmp-ra.236 rbp)
                        (rsi rdx rcx r8 r9 fv0 fv1 a.26 tmp-ra.236 rbp)
                        (rdx rcx r8 r9 fv0 fv1 a.26 b.27 tmp-ra.236 rbp)
                        (rcx r8 r9 fv0 fv1 a.26 b.27 c.28 tmp-ra.236 rbp)
                        (r8 r9 fv0 fv1 a.26 b.27 c.28 d.29 tmp-ra.236 rbp)
                        (r9 fv0 fv1 a.26 b.27 c.28 d.29 e.30 tmp-ra.236 rbp)
                        (fv0 fv1 a.26 b.27 c.28 d.29 e.30 f.31 tmp-ra.236 rbp)
                        (fv1 a.26 b.27 c.28 d.29 e.30 f.31 g.32 tmp-ra.236 rbp)
                        (a.26 b.27 c.28 d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rbp)
                        (b.27 c.28 d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rdi rbp)
                        (c.28 d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rsi rdi rbp)
                        (d.29 e.30 f.31 g.32 h.33 tmp-ra.236 rdx rsi rdi rbp)
                        (e.30 f.31 g.32 h.33 tmp-ra.236 rcx rdx rsi rdi rbp)
                        (f.31 g.32 h.33 tmp-ra.236 r8 rcx rdx rsi rdi rbp)
                        (g.32 h.33 tmp-ra.236 r9 r8 rcx rdx rsi rdi rbp)
                        (h.33 tmp-ra.236 fv0 r9 r8 rcx rdx rsi rdi rbp)
                        (tmp-ra.236 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
                        (tmp-ra.236 fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi rbp)
                        (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi r15 rbp)
                        (fv2 fv1 fv0 r9 r8 rcx rdx rsi rdi r15 rbp)))
                      (call-undead ())
                      (conflicts
                       ((tmp-ra.236
                         (fv2
                          h.33
                          g.32
                          f.31
                          e.30
                          d.29
                          c.28
                          b.27
                          a.26
                          rbp
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          rdi))
                        (a.26
                         (h.33
                          g.32
                          f.31
                          e.30
                          d.29
                          c.28
                          b.27
                          rbp
                          tmp-ra.236
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rdx
                          rsi))
                        (b.27
                         (rdi
                          h.33
                          g.32
                          f.31
                          e.30
                          d.29
                          c.28
                          rbp
                          tmp-ra.236
                          a.26
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rdx))
                        (c.28
                         (rsi
                          rdi
                          h.33
                          g.32
                          f.31
                          e.30
                          d.29
                          rbp
                          tmp-ra.236
                          b.27
                          a.26
                          fv1
                          fv0
                          r9
                          r8
                          rcx))
                        (d.29
                         (rdx
                          rsi
                          rdi
                          h.33
                          g.32
                          f.31
                          e.30
                          rbp
                          tmp-ra.236
                          c.28
                          b.27
                          a.26
                          fv1
                          fv0
                          r9
                          r8))
                        (e.30
                         (rcx
                          rdx
                          rsi
                          rdi
                          h.33
                          g.32
                          f.31
                          rbp
                          tmp-ra.236
                          d.29
                          c.28
                          b.27
                          a.26
                          fv1
                          fv0
                          r9))
                        (f.31
                         (r8
                          rcx
                          rdx
                          rsi
                          rdi
                          h.33
                          g.32
                          rbp
                          tmp-ra.236
                          e.30
                          d.29
                          c.28
                          b.27
                          a.26
                          fv1
                          fv0))
                        (g.32
                         (r9
                          r8
                          rcx
                          rdx
                          rsi
                          rdi
                          h.33
                          rbp
                          tmp-ra.236
                          f.31
                          e.30
                          d.29
                          c.28
                          b.27
                          a.26
                          fv1))
                        (h.33
                         (fv0
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          rdi
                          rbp
                          tmp-ra.236
                          g.32
                          f.31
                          e.30
                          d.29
                          c.28
                          b.27
                          a.26))
                        (rdi
                         (r15
                          fv2
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          rbp
                          h.33
                          g.32
                          f.31
                          e.30
                          d.29
                          c.28
                          b.27
                          tmp-ra.236))
                        (rsi
                         (r15
                          fv2
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rdx
                          rbp
                          rdi
                          h.33
                          g.32
                          f.31
                          e.30
                          d.29
                          c.28
                          a.26
                          tmp-ra.236))
                        (rdx
                         (r15
                          fv2
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rbp
                          rdi
                          rsi
                          h.33
                          g.32
                          f.31
                          e.30
                          d.29
                          b.27
                          a.26
                          tmp-ra.236))
                        (rcx
                         (r15
                          fv2
                          fv1
                          fv0
                          r9
                          r8
                          rbp
                          rdi
                          rsi
                          rdx
                          h.33
                          g.32
                          f.31
                          e.30
                          c.28
                          b.27
                          a.26
                          tmp-ra.236))
                        (r8
                         (r15
                          fv2
                          fv1
                          fv0
                          r9
                          rbp
                          rdi
                          rsi
                          rdx
                          rcx
                          h.33
                          g.32
                          f.31
                          d.29
                          c.28
                          b.27
                          a.26
                          tmp-ra.236))
                        (r9
                         (r15
                          fv2
                          fv1
                          fv0
                          rbp
                          rdi
                          rsi
                          rdx
                          rcx
                          r8
                          h.33
                          g.32
                          e.30
                          d.29
                          c.28
                          b.27
                          a.26
                          tmp-ra.236))
                        (fv0
                         (r15
                          fv2
                          fv1
                          rbp
                          rdi
                          rsi
                          rdx
                          rcx
                          r8
                          r9
                          h.33
                          f.31
                          e.30
                          d.29
                          c.28
                          b.27
                          a.26
                          tmp-ra.236))
                        (fv1
                         (r15
                          fv2
                          rbp
                          rdi
                          rsi
                          rdx
                          rcx
                          r8
                          r9
                          fv0
                          g.32
                          f.31
                          e.30
                          d.29
                          c.28
                          b.27
                          a.26
                          tmp-ra.236))
                        (rbp
                         (r15
                          fv2
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          rdi
                          h.33
                          g.32
                          f.31
                          e.30
                          d.29
                          c.28
                          b.27
                          a.26
                          tmp-ra.236))
                        (fv2 (r15 rbp rdi rsi rdx rcx r8 r9 fv0 fv1 tmp-ra.236))
                        (r15 (rbp rdi rsi rdx rcx r8 r9 fv0 fv1 fv2)))))
                     (begin
                       (set! tmp-ra.236 r15)
                       (set! a.26 rdi)
                       (set! b.27 rsi)
                       (set! c.28 rdx)
                       (set! d.29 rcx)
                       (set! e.30 r8)
                       (set! f.31 r9)
                       (set! g.32 fv0)
                       (set! h.33 fv1)
                       (set! rdi a.26)
                       (set! rsi b.27)
                       (set! rdx c.28)
                       (set! rcx d.29)
                       (set! r8 e.30)
                       (set! r9 f.31)
                       (set! fv0 g.32)
                       (set! fv1 h.33)
                       (set! fv2 72)
                       (set! r15 tmp-ra.236)
                       (jump L.H.8 rbp r15 rdi rsi rdx rcx r8 r9 fv0 fv1 fv2)))
                   (define L.H.8
                     ((new-frames ())
                      (locals
                       (r7.49
                        r6.48
                        r5.47
                        r4.46
                        r3.45
                        r2.44
                        r1.43
                        j.42
                        h.41
                        g.40
                        f.39
                        e.38
                        d.37
                        c.36
                        b.35
                        a.34
                        tmp-ra.237))
                      (undead-out
                       ((rdi rsi rdx rcx r8 r9 fv0 fv1 fv2 tmp-ra.237 rbp)
                        (rsi rdx rcx r8 r9 fv0 fv1 fv2 a.34 tmp-ra.237 rbp)
                        (rdx rcx r8 r9 fv0 fv1 fv2 b.35 a.34 tmp-ra.237 rbp)
                        (rcx r8 r9 fv0 fv1 fv2 b.35 a.34 c.36 tmp-ra.237 rbp)
                        (r8 r9 fv0 fv1 fv2 b.35 a.34 c.36 d.37 tmp-ra.237 rbp)
                        (r9 fv0 fv1 fv2 b.35 a.34 c.36 d.37 e.38 tmp-ra.237 rbp)
                        (fv0 fv1 fv2 b.35 a.34 c.36 d.37 e.38 f.39 tmp-ra.237 rbp)
                        (fv1 fv2 b.35 a.34 c.36 d.37 e.38 f.39 g.40 tmp-ra.237 rbp)
                        (fv2 b.35 a.34 c.36 d.37 e.38 f.39 g.40 h.41 tmp-ra.237 rbp)
                        (b.35 a.34 c.36 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                        ((rax c.36 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                         ((b.35 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                        (c.36 r1.43 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                        ((rax d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                         ((c.36 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                        (d.37 r2.44 e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                        ((rax e.38 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                         ((d.37 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                        (e.38 r3.45 f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                        ((rax f.39 g.40 h.41 j.42 tmp-ra.237 rbp)
                         ((e.38 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                        (f.39 r4.46 g.40 h.41 j.42 tmp-ra.237 rbp)
                        ((rax g.40 h.41 j.42 tmp-ra.237 rbp)
                         ((f.39 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                        (g.40 r5.47 h.41 j.42 tmp-ra.237 rbp)
                        ((rax h.41 j.42 tmp-ra.237 rbp)
                         ((g.40 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                        (h.41 r6.48 j.42 tmp-ra.237 rbp)
                        ((rax j.42 tmp-ra.237 rbp)
                         ((h.41 rdi rbp) (rsi rdi rbp) (rsi rdi r15 rbp) (rsi rdi r15 rbp)))
                        (r7.49 j.42 tmp-ra.237 rbp)
                        (j.42 tmp-ra.237 rdi rbp)
                        (tmp-ra.237 rsi rdi rbp)
                        (rsi rdi r15 rbp)
                        (rsi rdi r15 rbp)))
                      (call-undead (c.36 d.37 e.38 f.39 g.40 h.41 j.42 tmp-ra.237))
                      (conflicts
                       ((tmp-ra.237
                         (r7.49
                          r6.48
                          r5.47
                          r4.46
                          r3.45
                          r2.44
                          r1.43
                          j.42
                          h.41
                          g.40
                          f.39
                          e.38
                          d.37
                          c.36
                          b.35
                          a.34
                          rbp
                          fv2
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rdx
                          rsi
                          rdi))
                        (a.34
                         (j.42
                          h.41
                          g.40
                          f.39
                          e.38
                          d.37
                          c.36
                          b.35
                          rbp
                          tmp-ra.237
                          fv2
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rdx
                          rsi))
                        (b.35
                         (rdi
                          j.42
                          h.41
                          g.40
                          f.39
                          e.38
                          d.37
                          c.36
                          rbp
                          tmp-ra.237
                          a.34
                          fv2
                          fv1
                          fv0
                          r9
                          r8
                          rcx
                          rdx))
                        (c.36
                         (rdi
                          r1.43
                          j.42
                          h.41
                          g.40
                          f.39
                          e.38
                          d.37
                          rbp
                          tmp-ra.237
                          a.34
                          b.35
                          fv2
                          fv1
                          fv0
                          r9
                          r8
                          rcx))
                        (d.37
                         (rdi
                          r2.44
                          r1.43
                          j.42
                          h.41
                          g.40
                          f.39
                          e.38
                          rbp
                          tmp-ra.237
                          c.36
                          a.34
                          b.35
                          fv2
                          fv1
                          fv0
                          r9
                          r8))
                        (e.38
                         (rdi
                          r3.45
                          r2.44
                          r1.43
                          j.42
                          h.41
                          g.40
                          f.39
                          rbp
                          tmp-ra.237
                          d.37
                          c.36
                          a.34
                          b.35
                          fv2
                          fv1
                          fv0
                          r9))
                        (f.39
                         (rdi
                          r4.46
                          r3.45
                          r2.44
                          r1.43
                          j.42
                          h.41
                          g.40
                          rbp
                          tmp-ra.237
                          e.38
                          d.37
                          c.36
                          a.34
                          b.35
                          fv2
                          fv1
                          fv0))
                        (g.40
                         (rdi
                          r5.47
                          r4.46
                          r3.45
                          r2.44
                          r1.43
                          j.42
                          h.41
                          rbp
                          tmp-ra.237
                          f.39
                          e.38
                          d.37
                          c.36
                          a.34
                          b.35
                          fv2
                          fv1))
                        (h.41
                         (rdi
                          r6.48
                          r5.47
                          r4.46
                          r3.45
                          r2.44
                          r1.43
                          j.42
                          rbp
                          tmp-ra.237
                          g.40
                          f.39
                          e.38
                          d.37
                          c.36
                          a.34
                          b.35
                          fv2))
                        (j.42
                         (rdi
                          r7.49
                          r6.48
                          r5.47
                          r4.46
                          r3.45
                          r2.44
                          r1.43
                          rbp
                          tmp-ra.237
                          h.41
                          g.40
                          f.39
                          e.38
                          d.37
                          c.36
                          a.34
                          b.35))
                        (r1.43 (rbp tmp-ra.237 j.42 h.41 g.40 f.39 e.38 d.37 c.36))
                        (r2.44 (rbp tmp-ra.237 j.42 h.41 g.40 f.39 e.38 d.37))
                        (r3.45 (rbp tmp-ra.237 j.42 h.41 g.40 f.39 e.38))
                        (r4.46 (rbp tmp-ra.237 j.42 h.41 g.40 f.39))
                        (r5.47 (rbp tmp-ra.237 j.42 h.41 g.40))
                        (r6.48 (rbp tmp-ra.237 j.42 h.41))
                        (r7.49 (rbp tmp-ra.237 j.42))
                        (rdi (j.42 h.41 g.40 f.39 e.38 d.37 c.36 r15 rsi rbp b.35 tmp-ra.237))
                        (rsi (r15 rbp rdi a.34 tmp-ra.237))
                        (rdx (b.35 a.34 tmp-ra.237))
                        (rcx (c.36 b.35 a.34 tmp-ra.237))
                        (r8 (d.37 c.36 b.35 a.34 tmp-ra.237))
                        (r9 (e.38 d.37 c.36 b.35 a.34 tmp-ra.237))
                        (fv0 (f.39 e.38 d.37 c.36 b.35 a.34 tmp-ra.237))
                        (fv1 (g.40 f.39 e.38 d.37 c.36 b.35 a.34 tmp-ra.237))
                        (fv2 (h.41 g.40 f.39 e.38 d.37 c.36 b.35 a.34 tmp-ra.237))
                        (rbp
                         (r7.49
                          r6.48
                          r5.47
                          r4.46
                          r3.45
                          r2.44
                          r1.43
                          r15
                          rsi
                          rdi
                          j.42
                          h.41
                          g.40
                          f.39
                          e.38
                          d.37
                          c.36
                          b.35
                          a.34
                          tmp-ra.237))
                        (r15 (rbp rdi rsi)))))
                     (begin
                       (set! tmp-ra.237 r15)
                       (set! a.34 rdi)
                       (set! b.35 rsi)
                       (set! c.36 rdx)
                       (set! d.37 rcx)
                       (set! e.38 r8)
                       (set! f.39 r9)
                       (set! g.40 fv0)
                       (set! h.41 fv1)
                       (set! j.42 fv2)
                       (return-point L.rp.48
                                     (begin
                                       (set! rdi a.34)
                                       (set! rsi b.35)
                                       (set! r15 L.rp.48)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r1.43 rax)
                       (return-point L.rp.49
                                     (begin
                                       (set! rdi r1.43)
                                       (set! rsi c.36)
                                       (set! r15 L.rp.49)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r2.44 rax)
                       (return-point L.rp.50
                                     (begin
                                       (set! rdi r2.44)
                                       (set! rsi d.37)
                                       (set! r15 L.rp.50)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r3.45 rax)
                       (return-point L.rp.51
                                     (begin
                                       (set! rdi r3.45)
                                       (set! rsi e.38)
                                       (set! r15 L.rp.51)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r4.46 rax)
                       (return-point L.rp.52
                                     (begin
                                       (set! rdi r4.46)
                                       (set! rsi f.39)
                                       (set! r15 L.rp.52)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r5.47 rax)
                       (return-point L.rp.53
                                     (begin
                                       (set! rdi r5.47)
                                       (set! rsi g.40)
                                       (set! r15 L.rp.53)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r6.48 rax)
                       (return-point L.rp.54
                                     (begin
                                       (set! rdi r6.48)
                                       (set! rsi h.41)
                                       (set! r15 L.rp.54)
                                       (jump L.+.31 rbp r15 rdi rsi)))
                       (set! r7.49 rax)
                       (set! rdi r7.49)
                       (set! rsi j.42)
                       (set! r15 tmp-ra.237)
                       (jump L.+.31 rbp r15 rdi rsi)))
                   (begin
                     (set! tmp-ra.238 r15)
                     (set! rdi 8)
                     (set! rsi 16)
                     (set! rdx 24)
                     (set! rcx 32)
                     (set! r8 40)
                     (set! r9 48)
                     (set! fv0 56)
                     (set! r15 tmp-ra.238)
                     (jump L.F.6 rbp r15 rdi rsi rdx rcx r8 r9 fv0))))
  #;
  (match (conflict-analysis (undead-analysis (uncover-locals '(module ((new-frames (())))
                                                                (begin (set! x.6 2)
                                                                       (set! x.6 (+ x.6 3))
                                                                       (set! x.7 x.6)
                                                                       (set! x.7 (+ x.7 x.6))
                                                                       (begin (set! y.2 5)
                                                                              (jump L.f.1 x.6)))))))
    [`(module ((locals ,ls) (conflicts ,conflicts)) ,tail)
     (check-true (set=? (get-neighbors conflicts 'y.2) (list 'x.6)))
     (check-true (set=? (get-neighbors conflicts 'x.6) (list 'y.2 'x.7))
                 (format "unexpected conflict graph: ~a" (get-neighbors conflicts 'x.6)))
     (check-true (set=? (get-neighbors conflicts 'x.7) (list 'x.6)))])
  #;
  (match (conflict-analysis (undead-analysis (uncover-locals '(module ()
                                                                (begin (set! x.1 1)
                                                                       (set! x.2 x.1)
                                                                       (set! x.1 (+ x.1 x.1))
                                                                       (halt x.2))))))
    [`(module ((locals ,ls) (conflicts ,conflicts)) ,tail)
     (check-true (set=? (get-neighbors conflicts 'x.1) (list 'x.2)))
     (check-true (set=? (get-neighbors conflicts 'x.2) (list 'x.1)))])
  #;
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
  #;
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
