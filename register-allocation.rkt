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

;; decorate the program with the local variables being used
(define/contract (uncover-locals p)
  (-> asm-lang-v2? asm-lang-v2/locals?)
  (define locals (mutable-set))
  ;; interp. produce an annotated program with the local variables being used
  (define/contract (uncover-locals p)
    (-> asm-lang-v2? asm-lang-v2/locals?)
    (match p
      [`(module ,_ ,tail) (let ([uncovered-tail (uncover-locals/tail tail)])
                            `(module ((locals ,(set->list locals))) ,uncovered-tail))]))
  ;; asm-lang-v2-tail -> asm-lang-v2-tail
  ;; interp. discovers the local variables being used in the program
  (define (uncover-locals/tail t)
    (match t
      [`(halt ,t) `(halt ,(uncover-locals/triv t))]
      [`(begin ,fx ... ,tail) (let ([compiled-fx (for/list ([e fx]) (uncover-locals/effect e))]
                                    [compiled-tail (uncover-locals/tail tail)])
                                `(begin ,@compiled-fx ,compiled-tail))]))
  ;; asm-lang-v2-effect -> asm-lang-v2-effect
  ;; interp. discovers the local variables being used in the program
  (define (uncover-locals/effect e)
    (match e
      [`(set! ,x ,v) (set-add! locals x) `(set! ,x ,(uncover-locals/triv v))]
      [`(begin ,fx ... ,e) (let ([compiled-fx (for/list ([e fx]) (uncover-locals/effect e))]
                                 [compiled-e (uncover-locals/effect e)])
                             `(begin ,@compiled-fx ,compiled-e))]
      [`(set! ,x (,binop ,x ,v)) (set-add! locals x) `(set! ,x (,binop ,x ,(uncover-locals/triv v)))]))
  ;; asm-lang-v2-triv -> asm-lang-v2-triv
  ;; interp. discovers the local variables being used in the program
  (define (uncover-locals/triv t)
    (match t
      [x #:when (aloc? x) (set-add! locals x) x]
      [x x]))
  (uncover-locals p))

;; interp. annotate abstract locations with frame variables
(define/contract (assign-fvars p)
  (-> asm-lang-v2/locals? asm-lang-v2/assignments?)
  ;; interp. annotate abstract locations with frame variables
  (define (assign-fvars p)
    (-> asm-lang-v2/locals? asm-lang-v2/assignments?)
    (match p
      [`(module ((locals (,ls ...))) ,tail)
       (let ([assignments (for/list ([l ls] [i (in-naturals)]) `(,l ,(make-fvar i)))])
         `(module ((locals ,ls) (assignment ,assignments)) ,tail))]))
  (assign-fvars p))

;; interp. replaces the abstract locations with the concrete locations
(define (replace-locations p)
  (-> asm-lang-v2/assignments? nested-asm-lang-v2?)
  (define assignments (make-hash))

  ;; interp. replaces the abstract locations with the concrete locations
  (define/contract (replace-locations p)
    (-> asm-lang-v2/assignments? nested-asm-lang-v2?)
    (match p
      [`(module ((locals (,_ ...)) (assignment ((,xs ,regx) ...))) ,tail)
       (for ([x xs] [reg regx])
         (dict-set! assignments x reg))
       (replace-locations/tail tail)]))
  ;; (asm-lang-v2/assignments-tail) -> (nested-asm-lang-v2-tail)
  ;; interp. replaces the abstract locations with the concrete locations
  (define (replace-locations/tail t)
    (match t
      [`(halt ,triv) `(halt ,(replace-locations/triv triv))]
      [`(begin ,fx ... ,tail) (let ([compiled-fx (for/list ([e fx]) (replace-locations/effect e))]
                                    [compiled-tail (replace-locations/tail tail)])
                                `(begin ,@compiled-fx ,compiled-tail))]))
  ;; (asm-lang-v2/assignments-effect) -> (nested-asm-lang-v2-effect)
  ;; interp. replaces the abstract locations with the concrete locations
  (define (replace-locations/effect e)
    (match e
      [`(set! ,x (,binop ,x ,v)) (let ([reg (dict-ref assignments x)])
                                   `(set! ,reg (,binop ,reg ,(replace-locations/triv v))))]
      [`(set! ,x ,v) `(set! ,(dict-ref assignments x) ,(replace-locations/triv v))]
      [`(begin ,fx ... ,e) (let ([compiled-fx (for/list ([e fx]) (replace-locations/effect e))]
                                 [compiled-e (replace-locations/effect e)])
                             `(begin ,@compiled-fx ,compiled-e))]))
  ;; (asm-lang-v2-triv) -> (nested-asm-lang-v2-triv)
  ;; interp. replaces any abstract locations with the concrete locations
  (define (replace-locations/triv t)
    (match t
      [`,x #:when (aloc? x) (dict-ref assignments x)]
      [`,x x]))

  (replace-locations p))

;; interp. compiles program and replaces abstract locations with concrete locations
(define (assign-homes p)
  (-> asm-lang-v2? nested-asm-lang-v2?)
  (replace-locations (assign-fvars (uncover-locals p))))

;; asm-lang-v2/locals -> asm-lang-v2/undead
;; compiles p to asm-lang-v2/undead by performing undeadness analysis,
;; decorating the program with undead-set tree
(define/contract (undead-analysis p)
  (-> asm-lang-v2/locals? asm-lang-v2/undead?)

  ;; asm-lang-v2/locals? tail -> undead-out set
  ;; analyze the tail data non-terminal
  (define (analyze-tail t)
    (match t
      ;; handle the effects, accumulate through all of them
      [`(begin ,effects ... ,tail)

       ;; handle tail to get undead-in
       (define-values (t-ust undead-out)
         (analyze-tail tail))

       ;; handle the effects
       (define-values (rev-ust undead-in)
         (for/foldr ([rev-ust (list t-ust)]
                     [undead-out undead-out])
           ([effect effects])
           (define-values (ust undead-in)
             (analyze-effects effect undead-out))
           (values (cons ust rev-ust) undead-in)))
       (values rev-ust undead-in)]

      ;; tail must be halt, pass empty ust and undead-in if applicable
      [`(halt ,triv) (define undead-in
                       (analyze-triv triv))
                     (values '() undead-in)]))


  ;; asm-lang-v2/locals? -> undead-out set
  ;; analyze the effects data non-terminal
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
       (let ([undead-in (set-union (set-add (set-remove undead-out aloc_1) aloc_1) (analyze-triv triv))])
         (values undead-out undead-in))]

      [`(set! ,aloc ,triv)
       (let ([undead-in (set-union (set-remove undead-out aloc) (analyze-triv triv))])
         (values undead-out undead-in))]))

  (define (analyze-triv triv)
    (match triv
      [(? aloc?) (list triv)]
      [_ '()]))

  (define (compile-info i tail)
    (match i
      [`,info
       (define-values (ust^ _)
         (analyze-tail tail))
       (info-set info 'undead-out ust^)]))

  (match p
    [`(module ,info ,tail) `(module ,(compile-info info tail) ,tail)]))

;; asm-lang-v2/undead -> asm-lang-v2/conflicts
;; compiles p to asm-lang-v2/conflicts by decorating p with its conflict graph
(define/contract (conflict-analysis p)
  (-> asm-lang-v2/undead? asm-lang-v2/conflicts?)
  (define conflict-graph (void))

  ;; interp. creates a conflict graph for the given program
  (define/contract (conflict-analysis p)
    (-> asm-lang-v2/undead? asm-lang-v2/conflicts?)
    (match p
      [`(module ,info ,tail) (define udt (init-conflict-graph info))
                             (define analyzed-tail (conflict-analysis/tail udt tail))
                             `(module ,(conflict-analysis/info info) ,analyzed-tail)]))
  ;; asm-lang-v2/undead-info -> undead-set-tree
  ;; interp. initialize the conflict graph with the locals
  (define (init-conflict-graph info)
    (match info
      [`((locals (,ls ...)) (undead-out ,udt))
       (set! conflict-graph (new-graph ls))
       udt]))
  ;; asm-lang-v2/undead-info -> asm-lang-v2/conflicts-info
  ;; produce the conflict graph for the given program
  (define (conflict-analysis/info info)
    (match info
      [`((locals (,ls ...)) (undead-out ,udt)) `((locals ,ls) (conflicts ,conflict-graph))]))
  ;; asm-lang-v2/undead-tail -> asm-lang-v2/conflicts-tail
  ;; produce the tail of the program while adding conflicts to the conflict graph
  (define (conflict-analysis/tail udt tail)
    (match (cons udt tail)
      [(cons '() `(halt ,triv)) `(halt ,triv)]
      [(cons `(,undead-set-trees ... ,undead-set-tree-tail) `(begin ,fx ... ,inner-tail))
       (let ([compiled-effects (for/list ([e fx]
                                          [udt-e undead-set-trees])
                                 (conflict-analysis/effect udt-e e))])
         `(begin ,@compiled-effects ,(conflict-analysis/tail undead-set-tree-tail inner-tail)))]))
  ;; undead-set-tree asm-lang-v2/undead-effect -> asm-lang-v2/conflicts-effect
  ;; interp. identify abstract location conflicts and add them to the conflict graph
  (define (conflict-analysis/effect udt e)
    (match (cons udt e)
      [(cons `(,undead-set-trees ... ,last-undead-set-tree) `(begin ,fx ... ,effect))
       (define analyzed-fx (for/list ([e fx] [ust undead-set-trees]) (conflict-analysis/effect ust e)))
       `(begin ,@analyzed-fx ,(conflict-analysis/effect last-undead-set-tree effect))]
      [(cons `(,undead-out ...) `(set! ,aloc_1 (,binop ,aloc_1 ,triv)))
       (analyze-move-instruction undead-out
                                 aloc_1
                                 (get-aloc-set triv))
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
  (conflict-analysis p))

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
      (if (null? remaining-graph)
          assignment
          (let* ([sorted-nodes (sort (map car remaining-graph)
                                     (lambda (a b)
                                       (< (length (get-neighbors conflict-graph a))
                                          (length (get-neighbors conflict-graph b)))))]
                 [chosen-node (car sorted-nodes)]
                 [conflicting (get-neighbors conflict-graph chosen-node)]
                 [used-registers (map (lambda (conflict) (info-ref assignment conflict #f)) conflicting)]
                 [available-registers (filter (lambda (r) (not (member r used-registers))) registers)]
                 [new-location (if (null? available-registers)
                                   (make-fvar-spill)
                                   (car available-registers))])
            (assign-registers-helper (remove-vertex remaining-graph chosen-node)
                                     (info-set assignment chosen-node new-location)))))

    (assign-registers-helper conflict-graph '()))

  (match p
    [`(module ,info ,tail)
     (let ([assignments (graph-colouring-register-allocation (info-ref info 'conflicts)
                                                             (current-assignable-registers))])
       `(module ,(info-set (info-remove info 'conflicts) 'assignment assignments) ,tail))]))

;; asm-lang-v2 -> nested-asm-lang-v2
;; compiles p to nested-asm-lang-v2 by replacing each abstract location with a
;; physical location through a graph-colouring register allocation algorithm
(define/contract (assign-homes-opt p)
  (-> asm-lang-v2? nested-asm-lang-v2?)

  (replace-locations (assign-registers (conflict-analysis (undead-analysis (uncover-locals p))))))
