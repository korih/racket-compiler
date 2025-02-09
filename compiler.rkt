#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/2c-run-time
  cpsc411/langs/v2-reg-alloc
  cpsc411/langs/v2
  cpsc411/langs/v3
  cpsc411/graph-lib
  rackunit)

(provide
 check-values-lang
 uniquify
 sequentialize-let
 normalize-bind
 select-instructions
 uncover-locals
 undead-analysis
 conflict-analysis
 assign-registers
 replace-locations
 assign-homes-opt
 assign-homes
 flatten-begins
 patch-instructions
 implement-fvars
 generate-x64
 compile-m2
 compile-m3)

;; STUBS; delete when you've begun to implement the passes or replaced them with
;; your own stubs.
(define-values (check-values-lang
                normalize-bind
                select-instructions
                uncover-locals
                replace-locations
                assign-homes
                flatten-begins
                implement-fvars
                generate-x64
                compile-m2
                compile-m3)
  (values
   values
   values
   values
   values
   values
   values
   values
   values
   values
   values
   values))


;; values-lang-v3 -> values-unique-lang-v3
;; resolve identifiers to abstract locations
(define/contract (uniquify p)
  (-> values-lang-v3? values-unique-lang-v3?)

  ;; Env
  ;; empty environment for holding variable mappings
  (define (empty-env) (lambda (x) (error "Value not in environment!" x)))

  ;; env aloc -> triv
  ;; lookup variable in environment
  (define (lookup-env env x)
    (env x))

  ;; env aloc triv -> env
  ;; extend env mapping with x to v
  (define (extend-env env x v)
    (lambda (x0)
      (if (equal? x0 x)
          v
          (env x0))))

  ;; (values-lang-v3 tail) -> (values-unique-lang-v3 tail)
  ;; compile the tail from identifiers to abstract locations
  (define (compile-tail tail)
    (match tail
      [`(,values ...)
       (for/list ([value values])
         (compile-value value (empty-env)))]))

  ;; (values-lang-v3 value) (HashSetof Bindings) -> (values-unique-lang-v3 value)
  ;; compile the values from identifiers to abstract locations
  (define (compile-value value env)
    (match value
      [`(let (,bindings ...) ,v)
       (define-values (bindings^ env^)

         (for/fold ([binding-acc '()]
                    [env-acc env])
                   ([binding bindings])

           (match binding
             [`(,x ,value)
              (define value^ (compile-value value env))
              (define aloc (fresh x))
              (define env-new (extend-env env-acc x aloc))
              (define binding-new (cons `(,aloc ,value^) binding-acc))
              (values (reverse binding-new) env-new)])))

       (define compiled-value
         (compile-value v env^))

       `(let ,bindings^ ,compiled-value)]
      [`(,binop ,t1 ,t2) (let ([t1^ (if (name? t1) (lookup-env env t1) t1)]
                               [t2^ (if (name? t2) (lookup-env env t2) t2)])
                           `(,binop ,t1^ ,t2^))]
      [triv (if (name? triv)
                (lookup-env env triv)
                triv)]))

  (match p
    [`(module ,tails ...) `(module ,@(compile-tail tails))]))

;; interp. sequentialize the let statements into set! statements
(define/contract (sequentialize-let p)
  (-> values-unique-lang-v3? imp-mf-lang-v3?)
  ;; interp. compiler that sequentializes the let statements into set! statements
  (define/contract (sequentialize-let p)
    (-> values-unique-lang-v3? imp-mf-lang-v3?)
    (match p
      [`(module ,tail) `(module ,(sequentialize-let/tail tail))]))
  ;; values-unique-lang-v3-tail -> imp-mf-lang-v3-tail
  ;; interp. compiler that converts the tail and sequentializes the let statements
  (define (sequentialize-let/tail t)
    (match t
      [`(let ([,xs ,vs] ...) ,tail) (let ([sequentialize-let-values (for/list ([x xs] [v vs])
                                                                      `(set! ,x ,(sequentialize-let/value v)))])
                                      `(begin ,@sequentialize-let-values ,(sequentialize-let/tail tail)))]
      [`,value (sequentialize-let/value value)]))
  ;; values-unique-lang-v3-value -> imp-mf-lang-v3-value
  ;; interp. compiler that converts from values-unique-lang-v3-value to imp-mf-lang-v3-value
  (define (sequentialize-let/value v)
    (match v
      [`(let ([,xs ,vs] ...) ,v) (let ([sequentialize-let-values (for/list ([x xs] [v vs])
                                                                   `(set! ,x ,(sequentialize-let/value v)))])
                                   `(begin ,@sequentialize-let-values ,(sequentialize-let/value v)))]
      [_ v]))
  (sequentialize-let p))


;; para-asm-lang-v2 -> paren-x64-fvars-v2
;; compile program by patching instructions that have to x64 equivilent
;; into sequences of equivilent instructions
(define/contract (patch-instructions p)
  (-> para-asm-lang-v2? paren-x64-fvars-v2?)

  ;; (para-asm-lang-v2 effect) -> (paren-x64-fvars-v2 s)
  ;; compiles effectful operations in para-asm-lang-v2 to sequence of
  ;; instructions equivilent in paren-x64-fvars-v2
  (define (compile-effect e)
    (match e
      [`(set! ,loc (,binop ,loc ,triv))
       (cond

         ;; check if fvar and triv are valid in there positions
         [(and (fvar? loc) (not (int32? triv)))
          (define patch-reg-1 (first (current-patch-instructions-registers)))
          (define patch-reg-2 (second (current-patch-instructions-registers)))
          `((set! ,patch-reg-1 ,loc)
            (set! ,patch-reg-2 ,triv)
            (set! ,patch-reg-1 (,binop ,patch-reg-1 ,patch-reg-2))
            (set! ,loc ,patch-reg-1))]

         ;; check fvar since we know triv is now int32 when loc is fvar
         [(and (fvar? loc) (int32? triv))
          (define patch-reg-1 (first (current-patch-instructions-registers)))
          `((set! ,patch-reg-1 ,loc)
            (set! ,patch-reg-1 (,binop ,patch-reg-1 ,triv))
            (set! ,loc ,patch-reg-1))]

         ;; check triv since we know loc is not fvar
         [(and (not (int32? triv)) (int64? triv))
          (define patch-reg-1 (first (current-patch-instructions-registers)))
          `((set! ,patch-reg-1 ,triv)
            (set! ,loc (,binop ,loc ,patch-reg-1)))]

         [else
          `((set! ,loc (,binop ,loc ,triv)))])]

      [`(set! ,loc ,triv) (cond
                            ;; if loc is fvar and we are not moving an int32 or reg there
                            [(and (fvar? loc)
                                  (or (and (not (int32? triv)) (int64? triv))
                                      (fvar? triv)))
                             (define patch-reg (first (current-patch-instructions-registers)))
                             `((set! ,patch-reg ,triv)
                               (set! ,loc ,patch-reg))]

                            [else (list `(set! ,loc ,triv))])]))


  ;; (para-asm-lang-v2 p) -> (paren-x64-fvars-v2 p)
  ;; compiles para-asm-lang-v2 halt to set return register in paren-x64-fvars-v2
  (define (compile-p p)
    (match p
      [`(halt ,triv) (define ret (current-return-value-register))
                     `(set! ,ret ,triv)]))

  (match p
    [`(begin ,effects ... ,halt)
     (define effects^ (for/list ([effect effects])
                        (compile-effect effect)))
     (define halt^ (compile-p halt))
     `(begin ,@(apply append effects^) ,halt^)]))

;; Exercise 1
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

;; Exercise 2
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

;; Exercise 4
;; asm-lang-v2 -> nested-asm-lang-v2
;; compiles p to nested-asm-lang-v2 by replacing each abstract location with a
;; physical location through a graph-colouring register allocation algorithm
(define/contract (assign-homes-opt p)
  (-> asm-lang-v2? nested-asm-lang-v2?)

  (assign-registers (conflict-analysis (undead-analysis (uncover-locals p)))))

(module+ test
  (require
    ;rackunit
    rackunit/text-ui
    cpsc411/langs/v3
    ;cpsc411/langs/v2-reg-alloc
    cpsc411/langs/v2
    cpsc411/test-suite/public/v3
    cpsc411/test-suite/public/v2-reg-alloc)

  ;; You can modify this pass list, e.g., by adding check-assignment, or other
  ;; debugging and validation passes.
  ;; Doing this may provide additional debugging info when running the rest
  ;; suite.
  ;; If you modify, you must modify the corresponding interpreter in the
  ;; interp-ls, at least by interesting #f as the interpreter for the new pass.
  ;; See the documentation for v3-public-test-suite for details on the structure
  ;; of the interpreter list.
  (current-pass-list (list
                      check-values-lang
                      uniquify
                      sequentialize-let
                      normalize-bind
                      select-instructions
                      assign-homes-opt
                      flatten-begins
                      patch-instructions
                      implement-fvars
                      generate-x64
                      wrap-x64-run-time
                      wrap-x64-boilerplate))

  (define interp-ls (list
                     interp-values-lang-v3
                     interp-values-lang-v3
                     interp-values-unique-lang-v3
                     interp-imp-mf-lang-v3
                     interp-imp-cmf-lang-v3
                     interp-asm-lang-v2
                     interp-nested-asm-lang-v2
                     interp-para-asm-lang-v2
                     interp-paren-x64-fvars-v2
                     interp-paren-x64-v2
                     #f #f))

  (run-tests (v3-public-test-sutie (current-pass-list) interp-ls))
  (run-tests (v2-reg-alloc-public-test-suite undead-analysis conflict-analysis assign-registers)))
