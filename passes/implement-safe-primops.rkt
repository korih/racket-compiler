#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9)

(provide implement-safe-primops)

;; exprs-unique-lang-v9 -> exprs-unsafe-data-lang-v9
;; compiles p to Exprs-unsafe-data-lang v9 by implementing safe primitive
;; operations by inserting procedure definitions for each primitive operation
;; which perform dynamic tag checking, to ensure type safety
(define/contract (implement-safe-primops p)
  (-> exprs-unique-lang-v9? exprs-unsafe-data-lang-v9?)

  ;; parameter-type is one-of:
  ;; - fixnum?
  ;; - vector?
  ;; - any?
  ;; interp. represents a runtime type predicate used to check the type of an
  ;;argument

  ;; func is `(define ,label (lambda (,alocs ...) ,value))
  ;; interp. a function definition

  ;; new-funcs is (Map-of exprs-unique-lang-v9.binop (list label func))
  ;; interp. keeps track of new funcs that were created ...
  (define new-funcs (make-hash))

  ;; primop-spec is `(Symbol Symbol (List-of parameter-type) Natural)
  ;; interp. represents the safety specifications for primitive operations, the
  ;; first symbol is the safe label, the second symbol is the unsafe label, the
  ;; list of parameter types are the types that the operation must receive, and
  ;; the natural is the error code for that operation
  (define primop-spec-table
    `((*  unsafe-fx*  (fixnum? fixnum?) 1)
      (+  unsafe-fx+  (fixnum? fixnum?) 2)
      (-  unsafe-fx-  (fixnum? fixnum?) 3)
      (<  unsafe-fx<  (fixnum? fixnum?) 4)
      (<= unsafe-fx<= (fixnum? fixnum?) 5)
      (>  unsafe-fx>  (fixnum? fixnum?) 6)
      (>= unsafe-fx>= (fixnum? fixnum?) 7)

      (procedure-arity unsafe-procedure-arity (procedure?) 26)

      (make-vector   unsafe-make-vector   (fixnum?)               8)
      (vector-length unsafe-vector-length (vector?)               9)
      (vector-set!   unsafe-vector-set!   (vector? fixnum? any?) 10)
      (vector-ref    unsafe-vector-ref    (vector? fixnum?)      11)

      (car unsafe-car (pair?) 12)
      (cdr unsafe-cdr (pair?) 13)

      ;; No error code if the value isn't as expected, just produce false
      ,@(map (lambda (x) `(,x ,x (any?) 0))
             '(fixnum? boolean? empty? void? ascii-char? error? pair?
                       vector? not procedure?))

      ;; No error code if the value isn't as expected, just produce false
      ,@(map (lambda (x) `(,x ,x (any? any?) 0))
             '(cons eq?))))

  ;; primop-spec-map is (Map-of Symbol (list Symbol (List-of parameter-type) Natural))
  ;; interp. maps each safe primitive operation to a tuple of its corresponding
  ;; unsafe primitive name, a list of parameter type checks, and a unique error
  ;; code used for dynamic safety enforcement
  (define primop-spec-map
    (for/fold ([h (hash)]) ([prim-table^ primop-spec-table])
      (hash-set h (first prim-table^) (rest prim-table^))))

  ;; Symbol (List-of aloc) (List-of aloc) (List-of parameter-type) Natural (exprs-unsafe-data-lang-v9.value -> func) -> func
  ;; interp: builds a nested series of type checks for a safe primitive, where
  ;; unsafe-primop is the underlying unsafe version of the primitive op, args
  ;; is the list of arguments used in dynamic type checks, args-cons is the
  ;; original argument list for constructing the actual call, p-types is the
  ;; expected types for each argument (fixnum?, vector?, etc.), error-code is
  ;; the code to use if the type check fails, and k is the continuation
  ;; representing the computation to perform after passing the checks
  (define (primop-safety-builder unsafe-primop args args-const p-types error-code k)
    (cond
      ;; if there is only one type left or the next type is any? then we can just
      ;; use the unsafe primop
      [(or (empty? args) (eq? (first p-types) 'any?))
       (k `(,unsafe-primop ,@args-const))]
      ;; else we need to generate a safety check and pass the continuation along
      ;; for the rest of the computation
      [else
       ;; build the continuation
       (define kont (lambda (inner)
                      (k `(if (,(first p-types) ,(first args))
                              ,inner
                              (error ,error-code)))))
       ;; tail call the rest of the args CPS style
       (primop-safety-builder unsafe-primop
                              (rest args)
                              args-const
                              (rest p-types)
                              error-code
                              kont)]))

  ;; label prim-f (list Symbol (List-of parameter-type) Natural) -> void
  ;; interp: generates a safe wrapper function for prim-f using its unsafe form
  ;; EFFECTS: mutates new-funcs by inserting a binding from prim-f to its safe
  ;; wrapper definition
  (define (generate-safety-checks label prim-f primop-spec)
    ;; Destructuring the primop spec
    (define-values (unsafe-primop p-types error-code)
      (match primop-spec
        [`(,unsafe-primop ,p-types ,error-code)
         (values unsafe-primop p-types error-code)]))

    ;; Creating fresh variables for the parameter type args
    (define args (map (lambda (_) (fresh)) p-types))
    (define safety-check-fun (primop-safety-builder unsafe-primop
                                                    args
                                                    args
                                                    p-types
                                                    error-code
                                                    (lambda (inner)
                                                      `(define ,label (lambda (,@args)
                                                                        ,inner)))))
    (hash-set! new-funcs prim-f (list label safety-check-fun)))

  ;; exprs.safe-data-lang-v9.func -> exprs-unsafe-data-lang-v9.func
  ;; interp: recursively transforms a function body to replace primitive ops
  ;; with their safe counterparts
  (define (implement-safe-primops-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,value))
       `(define ,label (lambda (,@alocs) ,(implement-safe-primops-value value)))]))

  ;; exprs-unique-lang-v9.value -> exprs-unsafe-data-lang-v9.value
  ;; interp: transforms a value expression, recursively replacing primitive ops
  ;; with their safe counterparts
  (define (implement-safe-primops-value value)
    (match value
      [`(call ,vs ...)
       `(call ,@(map implement-safe-primops-value vs))]
      [`(let ([,alocs ,vs] ...) ,v)
       (define bindings
         (map (lambda (aloc val)
                (list aloc (implement-safe-primops-value val)))
              alocs vs))
       `(let (,@bindings) ,(implement-safe-primops-value v))]
      [`(if ,v1 ,v2 ,v3)
       `(if ,(implement-safe-primops-value v1)
            ,(implement-safe-primops-value v2)
            ,(implement-safe-primops-value v3))]
      [triv (implement-safe-primops-triv triv)]))

  ;; exprs-unique-lang-v9.triv -> exprs-unsafe-data-lang-v9.triv
  ;; interp: transforms trivials to their safe forms when needed
  (define (implement-safe-primops-triv triv)
    (match triv
      [prim-f
       #:when (hash-has-key? primop-spec-map prim-f)
       (if (hash-has-key? new-funcs prim-f)
           (car (hash-ref new-funcs prim-f))
           (implement-safe-primops-prim prim-f))]
      [`(lambda (,alocs ...) ,value)
       `(lambda ,alocs ,(implement-safe-primops-value value))]
      ;; Wildcard collapse case used because they are terminal cases with no
      ;; transformation
      [_ triv]))

  ;; exprs-unique-lang-v9.prim-f -> exprs-unsafe-data-lang-v9.prim-f
  ;; interp: constructs the complete safety wrapper for known primitives
  ;; EFFECTS: mutates new-funcs to insert multiple helper functions as needed
  (define (implement-safe-primops-prim prim-f)
    (match prim-f
      ['make-vector
       (define-values (unsafe-primop param-types error-code)
         (match (hash-ref primop-spec-map prim-f)
           [`(,unsafe ,params ,err) (values unsafe params err)]))
       (define make-label (fresh 'make-vector))
       (define init-label (fresh 'make-init-vector))
       (define loop-label (fresh 'vector-init-loop))

       ;; Generate safety check for: is length a fixnum?
       (define len1 (fresh 'tmp))
       (define make-vector-func `(define ,make-label
                                   (lambda (,len1)
                                     (if (fixnum? ,len1)
                                         (call ,init-label ,len1)
                                         (error ,error-code)))))

       ;; Generate check: is length ≥ 0
       (define len2 (fresh 'tmp))
       (define vec (fresh 'tmp))
       (define init-vector-func  `(define ,init-label
                                    (lambda (,len2)
                                      (if (unsafe-fx>= ,len2 0)
                                          (let ([,vec (unsafe-make-vector ,len2)])
                                            (call ,loop-label ,len2 0 ,vec))
                                          (error 12)))))

       ;; Loop: initialize all elements to 0
       (define len3 (fresh 'len))
       (define i (fresh 'i))
       (define vec3 (fresh 'vec))
       (define loop-func `(define ,loop-label
                            (lambda (,len3 ,i ,vec3)
                              (if (eq? ,len3 ,i)
                                  ,vec3
                                  (begin
                                    (unsafe-vector-set! ,vec3 ,i 0)
                                    (call ,loop-label ,len3 (unsafe-fx+ ,i 1) ,vec3))))))

       (hash-set! new-funcs loop-label (list loop-func))
       (hash-set! new-funcs init-label (list init-vector-func))
       (hash-set! new-funcs prim-f (list make-label make-vector-func))
       make-label]
      ['vector-ref
       (define-values (unsafe-primop param-types error-code)
         (match (hash-ref primop-spec-map prim-f)
           [`(,unsafe ,params ,err) (values unsafe params err)]))

       (define safe-label (fresh 'vector-ref))
       (define unsafe-label (fresh unsafe-primop))

       ;; First safety check: argument types (vector? and fixnum?)
       (define vec (fresh 'vec))
       (define index (fresh 'index))
       (define fun1 `(define ,safe-label
                       (lambda (,vec ,index)
                         (if (fixnum? ,index)
                             (if (vector? ,vec)
                                 (call ,unsafe-label ,vec ,index)
                                 (error ,error-code))
                             (error ,error-code)))))

       ;; Second safety check: bounds check on the index
       (define vec^ (fresh 'vec))
       (define index^ (fresh 'index))
       (define fun2 `(define ,unsafe-label
                       (lambda (,vec^ ,index^)
                         (if (unsafe-fx< ,index^ (unsafe-vector-length ,vec^))
                             (if (unsafe-fx>= ,index^ 0)
                                 (unsafe-vector-ref ,vec^ ,index^)
                                 (error ,error-code))
                             (error ,error-code)))))

       (hash-set! new-funcs prim-f (list safe-label fun1))
       (hash-set! new-funcs fun1 (list unsafe-label fun2))
       safe-label]
      ['vector-set!
       (define-values (unsafe-primop param-types error-code)
         (match (hash-ref primop-spec-map prim-f)
           [`(,unsafe ,params ,err) (values unsafe params err)]))

       (define safe-label (fresh 'vector-set!))
       (define unsafe-label (fresh unsafe-primop))

       ;; First safety check: type checks for (vector? vec) and (fixnum? index)
       (define vec (fresh 'vec))
       (define index (fresh 'index))
       (define val (fresh 'val))
       (define fun1 `(define ,safe-label
                       (lambda (,vec ,index ,val)
                         (if (fixnum? ,index)
                             (if (vector? ,vec)
                                 (call ,unsafe-label ,vec ,index ,val)
                                 (error ,error-code))
                             (error ,error-code)))))

       ;; Second safety check: bounds check for index in vector
       (define vec^ (fresh 'vec))
       (define index^ (fresh 'index))
       (define val^ (fresh 'val))
       (define fun2 `(define ,unsafe-label
                       (lambda (,vec^ ,index^ ,val^)
                         (if (unsafe-fx< ,index^ (unsafe-vector-length ,vec^))
                             (if (unsafe-fx>= ,index^ 0)
                                 (begin (unsafe-vector-set! ,vec^ ,index^ ,val^) (void))
                                 (error ,error-code))
                             (error ,error-code)))))

       (hash-set! new-funcs prim-f (list safe-label fun1))
       (hash-set! new-funcs fun1 (list unsafe-label fun2))
       safe-label]
      ;; Wildcard collapse case used for all other primitive operations as they 
      ;; have the same pattern of generating a safety check
      [_ (define primop (hash-ref primop-spec-map prim-f))
         (define label (fresh prim-f))
         (generate-safety-checks label prim-f primop)
         label]))

  (match p
    [`(module ,funcs ... ,value)
     (define funcs^ (map implement-safe-primops-func funcs))
     (define value^ (implement-safe-primops-value value))
     `(module ,@(map second (hash-values new-funcs)) ,@funcs^ ,value^)]))

