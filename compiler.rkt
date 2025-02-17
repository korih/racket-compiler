#lang racket

(require
  "register-allocation.rkt"
  "x64-wrapper.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/2c-run-time
  cpsc411/langs/v2
  cpsc411/langs/v3)

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
(define-values (check-values-lang)
  (values
   values))


;; values-lang-v3 -> values-unique-lang-v3
;; resolve identifiers to abstract locations
(define/contract (uniquify p)
  (-> values-lang-v3? values-unique-lang-v3?)

  ;; Env : (env-of X) is (listof (list Symbol X))

  ;; empty-env : Env
  ;; empty environment for holding variable mappings
  (define (empty-env) (lambda (x) (error "Value not in environment!" x)))

  ;; (env-of X) Symbol -> X
  ;; lookup variable in environment
  (define (lookup-env env x)
    (env x))

  ;; (env-of X) Symbol X -> (env-of X)
  ;; extend env mapping with x to v
  (define (extend-env env x v)
    (lambda (x0)
      (if (equal? x0 x)
          v
          (env x0))))
  
  (define (compile-tail tail)
    (match tail
      [`(,values ...)
       (for/list ([value values])
         (compile-value value (empty-env)))]))

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
      [`(,binop ,t1 ,t2)
       (define t1^ (if (name? t1) (lookup-env env t1) t1))
       (define t2^ (if (name? t2) (lookup-env env t2) t2))
       `(,binop ,t1^ ,t2^)]
      [triv
       (if (name? triv)
           (lookup-env env triv)
           triv)]))

  (match p
    [`(module ,tails ...) `(module ,@(compile-tail tails))]))

;; values-unique-lang-v3 -> imp-mf-lang-v3
;; interp. sequentialize the let statements into set! statements
(define/contract (sequentialize-let p)
  (-> values-unique-lang-v3? imp-mf-lang-v3?)
  
  (define (sequentialize-let/tail t)
    (match t
      [`(let ([,xs ,vs] ...) ,tail)
       (define sequentialize-let-values (for/list ([x xs] [v vs])
                                          `(set! ,x ,(sequentialize-let/value v))))
       `(begin ,@sequentialize-let-values ,(sequentialize-let/tail tail))]
      [value (sequentialize-let/value value)]))
  
  (define (sequentialize-let/value v)
    (match v
      [`(let ([,xs ,vs] ...) ,v)
       (define sequentialize-let-values (for/list ([x xs] [v vs])
                                         `(set! ,x ,(sequentialize-let/value v))))
       `(begin ,@sequentialize-let-values ,(sequentialize-let/value v))]
      ;; Using wildcard collapse case because in the other two cases, the
      ;; expression is already in imp-mf-lang-v3-value form
      [_ v]))

  (match p
      [`(module ,tail)
       `(module ,(sequentialize-let/tail tail))]))

;; imp-mf-lang-v3 -> imp-cmf-lang-v3
;; compiles p to imp-cmf-lang-v3 by ensuring the right-hand-side of each set! is
;; a simple value-producing operation
(define/contract (normalize-bind p)
  (-> imp-mf-lang-v3? imp-cmf-lang-v3?)

  (define (normalize-bind-tail tail)
    (match tail
      [`(begin ,e ... ,t)
       `(begin ,@(map normalize-bind-effect e) ,(normalize-bind-tail t))]
      [v (normalize-bind-value v (lambda (v) v))]))

  (define (normalize-bind-value value cont)
    (match value
      [`(begin ,e ... ,v)
       `(begin ,@(map normalize-bind-effect e) ,(normalize-bind-value v cont))]
      [triv (cont triv)]))

  (define (normalize-bind-effect effect)
    (match effect
      [`(set! ,aloc ,v)
       (normalize-bind-value v (lambda (simple-v)
                                 `(set! ,aloc ,simple-v)))]
      [`(begin ,e ...)
       `(begin ,@(map normalize-bind-effect e))]
      ;; Using a wildcard collapse case as it captures all other well-formed
      ;; expressions without transformation
      [_ effect]))

  (match p
    [`(module ,tail)
     `(module ,(normalize-bind-tail tail))]))

;; imp-cmf-lang-v3 -> asm-lang-v2
;; intep. compile value abstractions into a sequence of instructions
(define/contract (select-instructions p)
  (-> imp-cmf-lang-v3? asm-lang-v2?)

  ;; (imp-cmf-lang-v3-tail) -> bool
  ;; interp. compiler that returns true if the expression is a valid value in tail position
  (define (tail-value? e)
    (match e
      [`(begin ,fx ... ,tail) #f]
      [_ #t]))

  ; (Imp-cmf-lang-v3 value) -> (List-of (Asm-lang-v2 effect)) and (Asm-lang-v2 aloc)
  ; Assigns the value v to a fresh temporary, returning two values: the list of
  ; statements the implement the assignment in Loc-lang, and the aloc that the
  ; value is stored in.
  (define (assign-tmp v)
    (match v
      [`(,binop ,op1 ,op2)
       (match-let ([`(,stmts1 ,loc1) (select-triv op1)]
                   [`(,stmts2 ,loc2) (select-triv op2)])
         (list (append stmts1 stmts2 (list `(set! ,loc1 (,binop ,loc1 ,loc2)))) loc1))]
      [triv (select-triv triv)]))


  ;; imp-cmf-lang-v3-tail -> asm-lang-v2-tail
  ;; interp. produce the asm-lang-v2-tail and halt with the trivial value
  (define (select-tail e)
    (match e
      [`(begin ,fx ... ,tail)
       (define compiled-fx (for/foldr ([instructions empty])
                             ([e fx])
                             (append (select-effect e) instructions)))
       (define tail-compiled (select-tail tail))
       (match tail-compiled
         [`(begin ,inner-compiled-fx ... ,inner-compiled-tail)
          #:when (tail-value? tail)
          `(begin ,@compiled-fx ,@inner-compiled-fx ,inner-compiled-tail)]
         [_ `(begin ,@compiled-fx ,tail-compiled)])]
      [value (match-let ([`(,stmts ,loc) (select-value value)])
               (if (empty? stmts)
                   `(halt ,loc)
                   `(begin ,@stmts (halt ,loc))))]))

  ;; imp-cmf-lang-v3-value -> (list (listof asm-lang-v2-effect) asm-lang-v2-aloc)
  ;; interp. compiles value expression and creates temporary abstract locations
  ;; to store intermediate values
  (define (select-value e)
    (match e
      [`(,binop ,op1 ,op2)
       (define op1-tmp (fresh 'tmp))
       (define op2-tmp (fresh 'tmp))
       (list (list `(set! ,op1-tmp ,op1)
                   `(set! ,op2-tmp ,op2)
                   `(set! ,op1-tmp (,binop ,op1-tmp ,op2-tmp)))
             op1-tmp)]
      [triv (select-triv triv)]))

  ;; imp-cmf-lang-v3-effect -> (listof asm-lang-v2-effect)
  ;; interp. convert expressions of the form (set! x v) into (set! x triv) and
  ;; (set! x (binop x triv))
  (define (convert-set-expr x v)
    (match v
      [`(,binop ,op1 ,op2)
       (list `(set! ,x ,op1) `(set! ,x (,binop ,x ,op2)))]
      [triv (list `(set! ,x ,triv))]))

  ;; imp-cmf-lang-v3-effect -> (listof asm-lang-v2-effect)
  ;; interp. compiles effect expression into a sequence of instructions,
  ;; resolving values to abstract locations
  (define (select-effect e)
    (match e
      [`(set! ,x ,v) (convert-set-expr x v)]
      [`(begin ,fx ... ,e)
       (define compiled-fx (for/foldr ([fx-acc empty])
                             ([e fx])
                             (append (select-effect e) fx-acc)))
       (list `(begin ,@compiled-fx ,@(select-effect e)))]))

  ;; imp-cmf-lang-v3-triv -> (list (listof asm-lang-v2-effect) asm-lang-v2-aloc)
  ;; interp. compiles trivial expressions into a sequence of instructions and
  ;; returns the abstract location
  (define (select-triv t)
    (match t
      [x #:when (aloc? x) (list empty x)]
      [x
       (define tmp (fresh 'tmp))
       (list (list `(set! ,tmp ,x)) tmp)]))

  (match p
    [`(module ,tail)
     `(module () ,(select-tail tail))]))

;; nested-asm-lang-v2 -> para-asm-lang-v2
;; interp. flatten begin statements in the program
(define/contract (flatten-begins p)
  (-> nested-asm-lang-v2? para-asm-lang-v2?)
  
  ;; interp. flatten begin statements in the program into a list of effect
  ;; statements
  (define (flatten-begins/effect e)
    (match e
      [`(set! ,x (,binop ,x ,v)) (list `(set! ,x (,binop ,x ,v)))]
      [`(set! ,x ,v) (list `(set! ,x ,v))]
      [`(begin ,fx ... ,e)
       (define compiled-fx (for/foldr ([fx-acc empty])
                             ([e fx])
                             (append (flatten-begins/effect e) fx-acc)))
       (append compiled-fx (flatten-begins/effect e))]))

  (match p
    [`(halt ,triv) `(begin (halt ,triv))]
    [`(begin ,fx ... ,tail)
     (define compiled-fx (for/foldr ([fx-acc empty])
                           ([e fx])
                           (append (flatten-begins/effect e) fx-acc)))
     (match (flatten-begins tail)
       [`(begin ,inner-fx ... ,inner-tail)
        `(begin ,@compiled-fx ,@inner-fx ,inner-tail)])]))

;; para-asm-lang-v2 -> paren-x64-fvars-v2
;; compile program by patching instructions that have to x64 equivilent
;; into sequences of equivilent instructions
(define/contract (patch-instructions p)
  (-> para-asm-lang-v2? paren-x64-fvars-v2?)

  ;; compiles effectful operations in para-asm-lang-v2 to sequence of
  ;; instructions equivilent in paren-x64-fvars-v2
  (define (compile-effect e)
    (match e
      [`(set! ,loc (,binop ,loc ,triv))
       #:when (and (fvar? loc) (not (int32? triv)))
       (define patch-reg-1 (first (current-patch-instructions-registers)))
       (define patch-reg-2 (second (current-patch-instructions-registers)))
       `((set! ,patch-reg-1 ,loc)
         (set! ,patch-reg-2 ,triv)
         (set! ,patch-reg-1 (,binop ,patch-reg-1 ,patch-reg-2))
         (set! ,loc ,patch-reg-1))]
      [`(set! ,loc (,binop ,loc ,triv))
       #:when (and (fvar? loc) (int32? triv))
       (define patch-reg-1 (first (current-patch-instructions-registers)))
       `((set! ,patch-reg-1 ,loc)
         (set! ,patch-reg-1 (,binop ,patch-reg-1 ,triv))
         (set! ,loc ,patch-reg-1))]
      [`(set! ,loc (,binop ,loc ,triv))
       #:when (and (not (int32? triv)) (int64? triv))
       (define patch-reg-1 (first (current-patch-instructions-registers)))
       `((set! ,patch-reg-1 ,triv)
         (set! ,loc (,binop ,loc ,patch-reg-1)))]
      [`(set! ,loc (,binop ,loc ,triv))
       `((set! ,loc (,binop ,loc ,triv)))]
      [`(set! ,loc ,triv)
       #:when (and (fvar? loc)
                   (or (and (not (int32? triv)) (int64? triv))
                       (fvar? triv)))
       (define patch-reg (first (current-patch-instructions-registers)))
       `((set! ,patch-reg ,triv)
         (set! ,loc ,patch-reg))]
      [`(set! ,loc ,triv)
       (list `(set! ,loc ,triv))]))

  ;; compiles para-asm-lang-v2 halt to set return register in paren-x64-fvars-v2
  (define (compile-p p)
    (match p
      [`(halt ,triv)
       (define ret (current-return-value-register))
       `(set! ,ret ,triv)]))

  (match p
    [`(begin ,effects ... ,halt)
     (define effects^ (for/list ([effect effects])
                        (compile-effect effect)))
     (define halt^ (compile-p halt))
     `(begin ,@(apply append effects^) ,halt^)]))

;; paren-x64-fvars-v2 -> paren-x64-v2
;; interp. convert fvars into displacement mode operands
(define/contract (implement-fvars p)
  (-> paren-x64-fvars-v2? paren-x64-v2?)

  ;; fvar -> addr
  ;; convert fvar into displacement mode operand
  (define (fvar->addr fvar)
    `(,(current-frame-base-pointer-register) - ,(* (fvar->index fvar) (current-word-size-bytes))))

  ;; (param-asm-fvars-v2-s) -> (paren-x64-v2-s)
  ;; interp. convert fvars into displacement mode operands
  (define (implement-fvars/s s)
    (match s
      [`(set! ,fvar ,v)
       #:when (fvar? fvar)
       `(set! ,(fvar->addr fvar) ,v)]
      [`(set! ,x ,fvar)
       #:when (fvar? fvar)
       `(set! ,x ,(fvar->addr fvar))]
      [`(set! ,x (,binop ,x ,fvar))
       #:when (fvar? fvar)
       `(set! ,x (,binop ,x ,(fvar->addr fvar)))]
      [_ s]))

  (match p
    [`(begin ,ss ...)
     (define compiled-s (for/list ([s ss]) (implement-fvars/s s)))
     `(begin ,@compiled-s)]))

;; values-unique-lang-v3 -> string
;; interp. compile values-lang-v3 to x64 assembly without register allocation
(define/contract (compile-m2 p)
  (-> values-unique-lang-v3? string?)
  (parameterize ([current-pass-list (list uniquify
                                          sequentialize-let
                                          normalize-bind
                                          select-instructions
                                          assign-homes
                                          flatten-begins
                                          patch-instructions
                                          implement-fvars
                                          generate-x64
                                          wrap-x64-run-time
                                          wrap-x64-boilerplate)])
    (compile p)))

;; values-unique-lang-v3 -> string
;; interp. compile values-lang-v3 to x64 assembly with register allocation
(define/contract (compile-m3 p)
  (-> values-unique-lang-v3? string?)
  (parameterize ([current-pass-list (list uniquify
                                          sequentialize-let
                                          normalize-bind
                                          select-instructions
                                          assign-homes-opt
                                          flatten-begins
                                          patch-instructions
                                          implement-fvars
                                          generate-x64
                                          wrap-x64-run-time
                                          wrap-x64-boilerplate)])
    (compile p)))

(module+ test
  (require
    rackunit/text-ui
    cpsc411/langs/v3
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
