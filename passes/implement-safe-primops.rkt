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

  ;; TODO: change the values and stuff for call

  ;; func is `(define ,label (lambda (,alocs ...) ,value))
  ;; interp. a function definition

  ;; primop-spec is `(Symbol Symbol (Listof Parameter-types) Natural)
  ;; interp. represents the safety specifications for primitive operations, the
  ;; first symbol is the safe label, the second symbol is the unsafe label, the
  ;; list of parameter types are the types that the operation, and the natural
  ;; is the error code for that operation
  (define primop-spec-table
    `((*  unsafe-fx*  (fixnum? fixnum?) 1)
      (+  unsafe-fx+  (fixnum? fixnum?) 2)
      (-  unsafe-fx-  (fixnum? fixnum?) 3)
      (<  unsafe-fx<  (fixnum? fixnum?) 4)
      (<= unsafe-fx<= (fixnum? fixnum?) 5)
      (>  unsafe-fx>  (fixnum? fixnum?) 6)
      (>= unsafe-fx>= (fixnum? fixnum?) 7)

      (procedure-arity unsafe-procedure-arity (procedure?) 26)

      (make-vector   make-init-vector-label   (fixnum?)               8)
      (vector-length unsafe-vector-length     (vector?)               9)
      (vector-set!   unsafe-vector-set!-label (vector? fixnum? any?) 10)
      (vector-ref    unsafe-vector-ref-label  (vector? fixnum?)      11)

      (car unsafe-car (pair?) 12)
      (cdr unsafe-cdr (pair?) 13)

      ;; No error code if the value isn't as expected, just produce false
      ,@(map (lambda (x) `(,x ,x (any?) 0))
             '(fixnum? boolean? empty? void? ascii-char? error? pair?
                       vector? not procedure?))

      ;; No error code if the value isn't as expected, just produce false
      ,@(map (lambda (x) `(,x ,x (any? any?) 0))
             '(cons eq?))))

  ;; (Immutable Map-of exprs-unique-lang-v9.prim-f) -> (Listof exprs-unsafe-data-lang-v9.prim-f unsafe-prim-f (Listof Paramter-Type) Natural)
  ;; interp. map of data sturcture allocations to unsafe counter parts with specifications such as the types of the parameters and
  ;; the natural represents the error code
  (define primop-spec-map
    (for/fold ([h (hash)]) ([prim-table^ primop-spec-table])
      (hash-set h (first prim-table^) (rest prim-table^))))

  ;; new-funcs is (Mutable Map-of exprs-unique-lang-v9.binop (list label func))
  ;; interp. keeps track of new funcs that were created by compiling binop or unops
  (define new-funcs (make-hash))

  ;; exprs.safe-data-lang-v9.func -> exprs-unsafe-data-lang-v9.func
  ;; produce exprs-unsafe-data-lang-v9 of function definitions
  (define (implement-safe-primops-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,value))
       `(define ,label (lambda (,@alocs) ,(implement-safe-primops-value value)))]))

  ;; exprs-unique-lang-v9.value -> exprs-unsafe-data-lang-v9.value
  ;; produce unsafe values from exprs-unique-lang-v9
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
  ;; GLOBAL VARIABLE: new-funcs maps prim-f expressions to (Listof Label Safety-Check-Funtion)
  ;; interp. produce unsafe triv from exprs-unique-lang-v9.triv
  (define (implement-safe-primops-triv triv)
    (match triv
      [prim-f #:when (hash-has-key? primop-spec-map prim-f)
              (if (hash-has-key? new-funcs prim-f)
                  (car (hash-ref new-funcs prim-f)) ; returns the label
                  (implement-safe-primops-prim prim-f))]
      [`(lambda (,alocs ...) ,value)
       `(lambda ,alocs ,(implement-safe-primops-value value))]
      ;; Wildcard collapse case used because they are terminal cases with no transformation
      [_ triv]))

  ;; prim-f (Listof tmp) (Listof tmp) (Listof Parameter Types) Natural kont -> prim-f
  ;; GLOBAL VARIABLE: new-funs maps prim-f expressions to (Listof Label Safety-Check-Funtion)
  ;; interp. builds a safety check for a primitive operation
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

  ;; label prim-f primop-spec -> label
  ;; interp. helper generates safety checks for unsafe primops and setting it in the new-funcs hash
  ;; GLOBAL VARIABLE: new-funs maps prim-f expressions to (Listof Label Safety-Check-Funtion)
  ;; EFFECTS: mutates new-funcs to include the new safety check functions
  (define (generate-safety-checks label prim-f primop-spec)
    ;; Destructuring the primop spec
    (define-values (unsafe-primop p-types error-code)
      (match primop-spec
        [`(,unsafe-primop ,p-types ,error-code)
         (values unsafe-primop p-types error-code)]))

    ;; Creating fresh variables for the parameter type args
    (define arg (map (lambda (_) (fresh)) p-types))
    (define safety-check-fun (primop-safety-builder unsafe-primop
                                                    arg
                                                    arg
                                                    p-types
                                                    error-code
                                                    (lambda (inner)
                                                      `(define ,label (lambda (,@arg)
                                                                        ,inner)))))
    (hash-set! new-funcs prim-f (list label safety-check-fun)))

  ;; exprs-unique-lang-v9.prim-f -> exprs-unsafe-data-lang-v9.prim-f
  ;; produce safety checks for unsafe primops
  ;; GLOBAL VARIABLE: new-funs maps prim-f expressions to (Listof Label Safety-Check-Funtion)
  ;; EFFECTS: mutates new-funcs to include the new safety check functions
  (define (implement-safe-primops-prim prim-f)
    (match prim-f
      ['make-vector (define make-label (fresh 'make-vector))
                    (define init-label (fresh 'make-init-vector))
                    (define loop-label (fresh 'vector-init-loop))

                    ;; Generate safty check for if argument is fixnum?
                    (define arg1 (fresh))
                    (define fun1 `(define ,make-label
                                    (lambda (,arg1) (if (fixnum? ,arg1) (call ,init-label ,arg1) (error 8)))))


                    ;; Generate safety check for if argument is >= 0
                    (define arg2 (fresh))
                    (define arg3 (fresh))
                    (define fun2 `(define ,init-label
                                    (lambda (,arg2) (if (unsafe-fx>= ,arg2 0)
                                                        (let ((,arg3 (unsafe-make-vector ,arg2)))
                                                          (call ,loop-label ,arg2 0 ,arg3))
                                                        (error 12)))))

                    ;; Generate loop for initializing vector to 0
                    (define arg4 (fresh 'len))
                    (define arg5 (fresh 'i))
                    (define arg6 (fresh 'vec))
                    (define fun3 `(define ,loop-label
                                    (lambda (,arg4 ,arg5 ,arg6) (if (eq? ,arg4 ,arg5)
                                                                    ,arg6
                                                                    (begin
                                                                      (unsafe-vector-set! ,arg6 ,arg5 0)
                                                                      (call ,loop-label ,arg4 (unsafe-fx+ ,arg5 1) ,arg6))))))

                    (hash-set! new-funcs fun2 (list loop-label fun3))
                    (hash-set! new-funcs fun1 (list init-label fun2))
                    (hash-set! new-funcs prim-f (list make-label fun1))
                    make-label]
      ['vector-ref (define safe-label (fresh 'vector-ref))
                   (define unsafe-label (fresh 'unsafe-vector-ref))

                   ;; Generate safety check for if arguments are vector? and fixnum?
                   (define arg1 (fresh))
                   (define arg2 (fresh))
                   (define fun1 `(define ,safe-label
                                   (lambda (,arg1 ,arg2)
                                     (if (fixnum? ,arg2)
                                         (if (vector? ,arg1)
                                             (call ,unsafe-label ,arg1 ,arg2)
                                             (error 11))
                                         (error 11)))))

                   ;; Generate safety check for if fixnum is within bounds of vector
                   (define arg3 (fresh))
                   (define arg4 (fresh))
                   (define fun2 `(define ,unsafe-label
                                   (lambda (,arg3 ,arg4)
                                     (if (unsafe-fx< ,arg4 (unsafe-vector-length ,arg3))
                                         (if (unsafe-fx>= ,arg4 0)
                                             (unsafe-vector-ref ,arg3 ,arg4)
                                             (error 11))
                                         (error 11)))))

                   (hash-set! new-funcs prim-f (list safe-label fun1))
                   (hash-set! new-funcs fun1 (list unsafe-label fun2))
                   safe-label]
      ['vector-set! (define safe-label (fresh 'vector-set!))
                    (define unsafe-label (fresh 'unsafe-vector-set!))

                    ;; Generate safety check for if arguments are vector? and fixnum?
                    (define arg1 (fresh))
                    (define arg2 (fresh))
                    (define arg3 (fresh))
                    (define fun1 `(define ,safe-label
                                    (lambda (,arg1 ,arg2 ,arg3)
                                      (if (fixnum? ,arg2)
                                          (if (vector? ,arg1)
                                              (call ,unsafe-label ,arg1 ,arg2 ,arg3)
                                              (error 10))
                                          (error 10)))))

                    ;; Generate safety check for if fixnum is within bounds of vector
                    (define arg4 (fresh))
                    (define arg5 (fresh))
                    (define arg6 (fresh))
                    (define fun2 `(define ,unsafe-label
                                    (lambda (,arg4 ,arg5 ,arg6)
                                      (if (unsafe-fx< ,arg5 (unsafe-vector-length ,arg4))
                                          (if (unsafe-fx>= ,arg5 0)
                                              (begin (unsafe-vector-set! ,arg4 ,arg5 ,arg6) (void))
                                              (error 10))
                                          (error 10)))))

                    (hash-set! new-funcs prim-f (list safe-label fun1))
                    (hash-set! new-funcs fun1 (list unsafe-label fun2))
                    safe-label]

      ['vector-length (define primop-spec (hash-ref primop-spec-map prim-f))
                      (define label (fresh 'vector-length))
                      (generate-safety-checks label prim-f primop-spec)
                      label]

      ;; Wildcard case for all other primitive operations as they have the same pattern
      ;; of generating a safety check
      [_ (define primop (hash-ref primop-spec-map prim-f))
         (define label (fresh prim-f))
         (generate-safety-checks label prim-f primop)
         label]))

  (match p
    [`(module ,funcs ... ,value)
     (define funcs^ (map implement-safe-primops-func funcs))
     (define value^ (implement-safe-primops-value value))
     `(module ,@(map second (hash-values new-funcs)) ,@funcs^ ,value^)]))

