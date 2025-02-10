#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/graph-lib
  cpsc411/langs/v2
  cpsc411/langs/v2-reg-alloc)

(provide uncover-locals
         assign-fvars
         replace-locations
         assign-homes
         undead-analysis
         conflict-analysis
         assign-registers
         assign-homes-opt)

;; asm-lang-v2 -> asm-lang-v2/locals
;; compiles p to asm-lang-v2/locals by analysing which abstract locations are
;; used in the program and decorating the program with the set of variables in
;; an info field
(define/contract (uncover-locals p)
  (-> asm-lang-v2? asm-lang-v2/locals?)

  ;; acc is (Set-of aloc)
  ;; the unique abstract locations used in the program p
  (define unique-alocs (mutable-set))

  (define (uncover-locals-tail t)
    (match t
      [`(halt ,tr)
       (uncover-locals-triv tr)]
      [`(begin ,ef ... ,ta)
       (for-each uncover-locals-effect ef)
       (uncover-locals-tail ta)]))

  (define (uncover-locals-effect e)
    (match e
      [`(set! ,aloc1 (,binop ,aloc1 ,triv))
       (set-add! unique-alocs aloc1)
       (uncover-locals-triv triv)]
      [`(set! ,aloc ,triv)
       (set-add! unique-alocs aloc)
       (uncover-locals-triv triv)]
      [`(begin ,ef ...)
       (for-each uncover-locals-effect ef)]))

  (define (uncover-locals-triv t)
    (match t
      [aloc #:when (aloc? aloc) (set-add! unique-alocs aloc)]
      [int64 #:when (int64? int64) (void)]))

  (match p
    [`(module () ,t)
     (uncover-locals-tail t)
     `(module ,(info-set '() 'locals (set->list unique-alocs)) ,t)]))

;; asm-lang-v2/locals -> asm-lang-v2/assignments
;; compiles p to asm-lang-v2/assignments by assigning each abstract location
;; from the locals info field to a fresh frame variable
(define/contract (assign-fvars p)
  (-> asm-lang-v2/locals? asm-lang-v2/assignments?)
  
  ;; interp. annotate abstract locations with frame variables
  (define (assign-fvars p)
    (match p
      [`(module ,info ,tail)
       (define assignments (for/list ([l (info-ref info 'locals)]
                                      [i (in-naturals)])
                             `(,l ,(make-fvar i))))
       (define updated-info (info-set info 'assignment assignments))
       `(module ,updated-info ,tail)]))
  
  (assign-fvars p))

;; asm-lang-v2/assignments -> nested-asm-lang-v2
;; interp. replaces the abstract locations with the concrete locations
(define (replace-locations p)
  (-> asm-lang-v2/assignments? nested-asm-lang-v2?)

  ;; acc is (Map-of aloc reg)
  ;; the abstract locations mapped to its physical location
  (define assignments (make-hash))
  
  ;; interp. replaces the abstract locations with the concrete locations
  (define (replace-locations/tail t)
    (match t
      [`(halt ,triv)
       `(halt ,(replace-locations/triv triv))]
      [`(begin ,fx ... ,tail)
       (define compiled-fx (for/list ([e fx])
                             (replace-locations/effect e)))
       (define compiled-tail (replace-locations/tail tail))
       `(begin ,@compiled-fx ,compiled-tail)]))

  ;; interp. replaces the abstract locations with the concrete locations
  (define (replace-locations/effect e)
    (match e
      [`(set! ,x (,binop ,x ,v))
       (define reg (dict-ref assignments x))
       `(set! ,reg (,binop ,reg ,(replace-locations/triv v)))]
      [`(set! ,x ,v)
       `(set! ,(dict-ref assignments x) ,(replace-locations/triv v))]
      [`(begin ,fx ... ,e)
       (define compiled-fx (for/list ([e fx]) (replace-locations/effect e)))
       (define compiled-e (replace-locations/effect e))
       `(begin ,@compiled-fx ,compiled-e)]))

  ;; interp. replaces any abstract locations with the concrete locations
  (define (replace-locations/triv t)
    (match t
      [`,x #:when (aloc? x) (dict-ref assignments x)]
      [`,x x]))

  (match p
    [`(module ,info ,tail)
     (for ([pair (info-ref info 'assignment)])
       (dict-set! assignments (first pair) (second pair)))
     (replace-locations/tail tail)]))

;; asm-lang-v2 -> nested-asm-lang-v2
;; interp. compiles p and replaces abstract locations with concrete locations
(define (assign-homes p)
  (-> asm-lang-v2? nested-asm-lang-v2?)
  
  (replace-locations
   (assign-fvars
    (uncover-locals p))))

;; Exercise 1
;; asm-lang-v2/locals -> asm-lang-v2/undead
;; compiles p to asm-lang-v2/undead by performing undeadness analysis,
;; decorating the program with undead-set tree
(define/contract (undead-analysis p)
  (-> asm-lang-v2/locals? asm-lang-v2/undead?)

  (define (analyze-tail t)
    (match t
      [`(begin ,effects ... ,tail)

       (define-values (t-ust undead-out)
         (analyze-tail tail))

       (define-values (rev-ust undead-in)
         (for/foldr ([rev-ust (list t-ust)]
                     [undead-out undead-out])
           ([effect effects])
           (define-values (ust undead-in)
             (analyze-effects effect undead-out))
           (values (cons ust rev-ust) undead-in)))
       (values rev-ust undead-in)]
      [`(halt ,triv) (define undead-in
                       (analyze-triv triv))
                     (values '() undead-in)]))

  (define (analyze-effects e undead-out)
    (match e
      [`(begin ,effects ...)
       (define-values (rev-ust undead-in)
         (for/foldr ([rev-ust '()]
                     [undead-out undead-out])
           ([effect effects])
           (define-values (ust undead-in)
             (analyze-effects effect undead-out))
           (values (cons ust rev-ust) undead-in)))
       (values rev-ust undead-in)]
      [`(set! ,aloc_1 (,binop ,aloc_1 ,triv))
       (define undead-in (set-union
                          (set-add
                           (set-remove undead-out aloc_1)
                           aloc_1)
                          (analyze-triv triv)))
       (values undead-out undead-in)]
      [`(set! ,aloc ,triv)
       (define undead-in (set-union
                          (set-remove undead-out aloc)
                          (analyze-triv triv)))
       (values undead-out undead-in)]))

  (define (analyze-triv triv)
    (match triv
      [x #:when (aloc? x) (list x)]
      [_ '()]))

  (define (compile-info i tail)
    (match i
      [`,info
       (define-values (ust^ _)
         (analyze-tail tail))
       (info-set info 'undead-out ust^)]))

  (match p
    [`(module ,info ,tail)
     `(module ,(compile-info info tail) ,tail)]))

;; Exercise 2
;; asm-lang-v2/undead -> asm-lang-v2/conflicts
;; compiles p to asm-lang-v2/conflicts by decorating p with its conflict graph
(define/contract (conflict-analysis p)
  (-> asm-lang-v2/undead? asm-lang-v2/conflicts?)

  ;; acc is Graph
  ;; the conflict graphs of abstract locations
  (define conflict-graph (void))
  
  ;; asm-lang-v2/undead-tail -> asm-lang-v2/conflicts-tail
  ;; produce the tail of the program while adding conflicts to the conflict graph
  (define (conflict-analysis/tail udt tail)
    (match (cons udt tail)
      [(cons '() `(halt ,triv)) `(halt ,triv)]
      [(cons `(,undead-set-trees ... ,undead-set-tree-tail) `(begin ,fx ... ,inner-tail))
       (define compiled-effects (for/list ([e fx]
                                           [udt-e undead-set-trees])
                                  (conflict-analysis/effect udt-e e)))
       `(begin ,@compiled-effects ,(conflict-analysis/tail undead-set-tree-tail inner-tail))]))
  
  ;; undead-set-tree asm-lang-v2/undead-effect -> asm-lang-v2/conflicts-effect
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
       `(set! ,aloc ,triv)]))
  
  ;; undead-set-tree aloc? (listof aloc?) -> (void)
  ;; interp. track the conflict resulting from the move instruction to the dest aloc in the conflict graph
  (define (analyze-move-instruction udt dest src)
    (set! conflict-graph (add-edges conflict-graph dest (set-subtract udt (list dest) src))))
  
  ;; asm-lang-v2/undead-triv -> (setof aloc?)
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

;; Exercise 3
;; asm-lang-v2/conflicts -> asm-lang-v2/assignments
;; compiles p to asm-lang-v2/assignments by attempting to fit each of the
;; abstract location declared in the locals set into a register, and if one
;; cannot be found, assigns it a frame variable instead
(define/contract (assign-registers p)
  (-> asm-lang-v2/conflicts? asm-lang-v2/assignments?)

  (define (graph-colouring-register-allocation conflict-graph registers)
    (define fvar-index 0)
  
    (define (make-fvar-spill)
      (let ([fvar (make-fvar fvar-index)])
        (set! fvar-index (+ fvar-index 1))
        fvar))
  
    (define (assign-registers-helper remaining-graph assignment)
      (define sorted-nodes (sort (map car remaining-graph)
                                 (lambda (a b)
                                   (< (length (get-neighbors conflict-graph a))
                                      (length (get-neighbors conflict-graph b))))))
      (define chosen-node (car sorted-nodes))
      (define conflicting (get-neighbors conflict-graph chosen-node))
      (define used-registers (map (lambda (conflict) (info-ref assignment conflict #f)) conflicting))
      (define available-registers (filter (lambda (r) (not (member r used-registers))) registers))
      (define new-location (if (null? available-registers)
                               (make-fvar-spill)
                               (car available-registers)))
      (if (null? remaining-graph)
          assignment
          (assign-registers-helper (remove-vertex remaining-graph chosen-node)
                                   (info-set assignment chosen-node new-location))))
  
    (assign-registers-helper conflict-graph '()))

  (match p
    [`(module ,info ,tail)
     (define assignments (graph-colouring-register-allocation (info-ref info 'conflicts)
                                                              (current-assignable-registers)))
     `(module ,(info-set (info-remove info 'conflicts)
                         'assignment
                         assignments)
        ,tail)]))

;; Exercise 4
;; asm-lang-v2 -> nested-asm-lang-v2
;; compiles p to nested-asm-lang-v2 by replacing each abstract location with a
;; physical location through a graph-colouring register allocation algorithm
(define/contract (assign-homes-opt p)
  (-> asm-lang-v2? nested-asm-lang-v2?)

  (replace-locations
   (assign-registers
    (conflict-analysis
     (undead-analysis
      (uncover-locals p))))))
