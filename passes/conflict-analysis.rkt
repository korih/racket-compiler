#lang racket

(require "undead-analysis.rkt"
         "uncover-locals.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v4
  rackunit)

(provide conflict-analysis)

;; Exercise 2
;; asm-pred-lang-v4/undead -> asm-pred-lang-v4/conflicts
;; compiles p by decorating p with its conflict graph
(define/contract (conflict-analysis p)
  (-> asm-pred-lang-v4/undead? asm-pred-lang-v4/conflicts?)

  ;; acc is Graph
  ;; the conflict graphs of abstract locations
  (define conflict-graph (void))

  ;; undead-set-tree asm-pred-lang-v4/undead-tail -> asm-pred-lang-v4/conflicts-tail
  ;; produce the tail of the program while adding conflicts to the conflict graph
  (define (conflict-analysis/tail udt tail)
    (match (cons udt tail)
      [(cons '() `(halt ,triv)) `(halt ,triv)]
      [(cons `(,undead-set-trees ... ,undead-set-tree-tail) `(begin ,fx ... ,inner-tail))
       (define compiled-effects (for/list ([e fx]
                                           [udt-e undead-set-trees])
                                  (conflict-analysis/effect udt-e e)))
       `(begin ,@compiled-effects ,(conflict-analysis/tail undead-set-tree-tail inner-tail))]
      [(cons `(,undead-set-tree-p ,undead-set-tree-t ,undead-set-tree-f) `(if ,pred ,tail-t ,tail-f))
       `(if ,(conflict-analysis/predicate undead-set-tree-p pred)
            ,(conflict-analysis/tail undead-set-tree-t tail-t)
            ,(conflict-analysis/tail undead-set-tree-f tail-f))]))

  ;; undead-set-tree asm-pred-lang-v4-pred -> asm-pred-lang-v4-pred
  ;; produce the predicate while adding conflicts to the conflict graph
  (define (conflict-analysis/predicate udt pred)
    (match (cons udt pred)
      [(cons '() '(true)) '(true)]
      [(cons '() '(false)) '(false)]
      [(cons undead-set-tree `(not ,pred)) `(not ,(conflict-analysis/predicate undead-set-tree pred))]
      [(cons `(,udts ... ,udt-p) `(begin ,fx ... ,pred))
       `(begin ,@(for/list ([e fx]
                            [udt-e udts])
                   (conflict-analysis/effect udt-e e))
               (conflict-analysis/predicate udt-p pred))]
      [(cons `(,udt-p ,udt-t ,udt-f) `(if ,pred ,pred ,pred))
       `(if ,(conflict-analysis/predicate udt-p pred)
            ,(conflict-analysis/predicate udt-t pred)
            ,(conflict-analysis/predicate udt-f pred))]
      [(cons '() `(,relop ,aloc ,triv))
       `(,relop ,aloc ,triv)]))

  ;; undead-set-tree asm-pred-lang-v4/undead-effect -> asm-pred-lang-v4/conflicts-effect
  ;; interp. identify abstract location conflicts and add them to the conflict graph
  (define (conflict-analysis/effect udt e)
    (match (cons udt e)
      [(cons `(,undead-set-trees ... ,last-undead-set-tree) `(begin ,fx ... ,effect))
       (define analyzed-fx (for/list ([e fx]
                                      [ust undead-set-trees])
                             (conflict-analysis/effect ust e)))
       `(begin ,@analyzed-fx ,(conflict-analysis/effect last-undead-set-tree effect))]
      [(cons `(,undead-out ...) `(set! ,aloc_1 (,binop ,aloc_1 ,triv)))
       (analyze-move-instruction undead-out
                                 aloc_1
                                 empty)
       `(set! ,aloc_1 (,binop ,aloc_1 ,triv))]
      [(cons `(,undead-out ...) `(set! ,aloc ,triv))
       (analyze-move-instruction undead-out
                                 aloc
                                 (get-aloc-set triv))
       `(set! ,aloc ,triv)]
      [(cons `(,udt-p ,udt-t ,udt-f) `(if ,pred ,t-e ,f-e))
       `(if ,(conflict-analysis/predicate udt-p pred)
            ,(conflict-analysis/effect udt-t t-e)
            ,(conflict-analysis/effect udt-f f-e))]))

  ;; undead-set-tree aloc? (listof aloc?) -> (void)
  ;; interp. track the conflict resulting from the move instruction to the dest aloc in the conflict graph
  (define (analyze-move-instruction udt dest src)
    (set! conflict-graph (add-edges conflict-graph dest (set-subtract udt (list dest) src))))

  ;; asm-pred-lang-v4/undead-triv -> (setof aloc?)
  ;; interp. get the abstract location if this triv is an aloc, otherwise return an empty list
  (define (get-aloc-set triv)
    (match triv
      [`,triv #:when (aloc? triv) (list triv)]
      [_ empty]))

  (match p
    [`(module ,info ,tail)
     (set! conflict-graph (new-graph (info-ref info 'locals)))
     (define udt (info-ref info 'undead-out))
     (define analyzed-tail (conflict-analysis/tail udt tail))
     (define updated-info (info-set (info-remove info 'undead-out)
                                    'conflicts
                                    conflict-graph))
     `(module ,updated-info ,analyzed-tail)]))

(module+ test
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
                                                                       (set! x2 (+ x.2 x.1))
                                                                       (if (> x.2 2)
                                                                           (set! x.3 3)
                                                                           (set! x.3 4))
                                                                       (set! x.2 (+ x.2 x.3))
                                                                       (halt x.2))))))
    [`(module ((locals ,ls) (conflicts ,conflicts)) ,tail)
     (check-true (set=? (get-neighbors conflicts 'x.1) empty))
     (check-true (set=? (get-neighbors conflicts 'x.2) (list 'x.1)))
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
     (check-true (set=? (get-neighbors conflicts 'x.2) (list 'x.1)))
     (check-true (set=? (get-neighbors conflicts 'x.3) (list 'x.2)))]))
