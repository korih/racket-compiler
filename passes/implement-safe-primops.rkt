#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8
  rackunit)

(provide implement-safe-primops)

;; exprs-unique-lang-v8 -> exprs-unsafe-data-lang-v8
;; compiles p to Exprs-unsafe-data-lang v8 by implementing safe primitive
;; operations by inserting procedure definitions for each primitive operation
;; which perform dynamic tag checking, to ensure type safety
(define/contract (implement-safe-primops p)
  (-> exprs-unique-lang-v8? exprs-unsafe-data-lang-v8?)

  ;; func is `(define ,label (lambda (,alocs ...) ,value))
  ;; interp. a function definition

  ;; Symbol Symbol (Listof Parameter-types) Natural
  ;; interp. represents the safety specifications for primitive operations, the
  ;; first symbol is the safe label, the second symbol is the unsafe label, the
  ;; list of parameter types are the types that the operation, and the natural
  ;; is the error code for that operation
  (define primop-spec-table
    `((* unsafe-fx*   (fixnum? fixnum?) 1)
      (+ unsafe-fx+   (fixnum? fixnum?) 2)
      (- unsafe-fx-   (fixnum? fixnum?) 3)
      (< unsafe-fx<   (fixnum? fixnum?) 4)
      (<= unsafe-fx<= (fixnum? fixnum?) 5)
      (> unsafe-fx>   (fixnum? fixnum?) 6)
      (>= unsafe-fx>= (fixnum? fixnum?) 7)

      ;; TODO: use these for the vector prim-ops
      (make-vector   make-init-vector-label   (fixnum?)               8)
      (vector-length unsafe-vector-length     (vector?)               9)
      (vector-set!   unsafe-vector-set!-label (vector? fixnum? any?) 10)
      (vector-ref    unsafe-vector-ref-label  (vector? fixnum?)      11)

      (car unsafe-car (pair?) 12)
      (cdr unsafe-cdr (pair?) 13)

      ,@(map (lambda (x) `(,x ,x (any?) 0))
             '(fixnum? boolean? empty? void? ascii-char? error? pair?
                       vector? not))

      ,@(map (lambda (x) `(,x ,x (any? any?) 0))
             '(cons eq?))))

  ;; (Immutable Map-of exprs-unique-lang-v8.prim-f) -> (Listof exprs-unsafe-data-lang-v8.prim-f unsafe-prim-f (Listof Paramter-Type) Natural)
  ;; interp. map of data sturcture allocations to unsafe counter parts with specifications such as the types of the parameters and
  ;; the natural represents the error code
  (define primop-spec-map
    (for/fold ([h (hash)]) ([prim-table^ primop-spec-table])
      (hash-set h (first prim-table^) (rest prim-table^))))

  ;; new-funcs is (Mutable Map-of exprs-unique-lang-v8.binop (list label func))
  ;; interp. keeps track of new funcs that were created by compiling binop or unops
  (define new-funcs (make-hash))

  ;; exprs.safe-data-lang-v8.func -> exprs-unsafe-data-lang-v8.func
  ;; produce exprs-unsafe-data-lang-v8 of function definitions
  (define (implement-safe-primops-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,value))
       `(define ,label (lambda (,@alocs) ,(implement-safe-primops-value value)))]))

  ;; exprs-unique-lang-v8.value -> exprs-unsafe-data-lang-v8.value
  ;; produce unsafe values from exprs-unique-lang-v8
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

  ;; exprs-unique-lang-v8.triv -> exprs-unsafe-data-lang-v8.triv
  ;; GLOBAL VARIABLE: new-funcs maps prim-f expressions to (Listof Label Safety-Check-Funtion)
  ;; interp. produce unsafe triv from exprs-unique-lang-v8.triv
  (define (implement-safe-primops-triv triv)
    (match triv
      [prim-f #:when (hash-has-key? primop-spec-map prim-f)
              (if (hash-has-key? new-funcs prim-f)
                  (car (hash-ref new-funcs prim-f)) ; returns the label
                  (implement-safe-primops-prim prim-f))]
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

  ;; exprs-unique-lang-v8.prim-f -> exprs-unsafe-data-lang-v8.prim-f
  ;; produce safety checks for unsafe primops
  ;; GLOBAL VARIABLE: new-funs maps prim-f expressions to (Listof Label Safety-Check-Funtion)
  ;; EFFECTS: mutates new-funcs to include the new safety check functions
  ;; TODO: replace this with handle-prim (also change that name)
  ;; the hash isn't ever used here
  (define (implement-safe-primops-prim prim-f)
    (match prim-f
      ['make-vector (define make-label (fresh-label 'make-vector))
                    (define init-label (fresh-label 'make-init-vector))
                    (define loop-label (fresh-label 'vector-init-loop))

                    ;; Generate safty check for if argument is fixnum?
                    (define arg1 (fresh))
                    (define fun1 `(define ,make-label (lambda (,arg1) (if (fixnum? ,arg1) (call ,init-label ,arg1) (error 8)))))


                    ;; Generate safety check for if argument is >= 0
                    (define arg2 (fresh))
                    (define arg3 (fresh))
                    (define fun2 `(define ,init-label (lambda (,arg2) (if (unsafe-fx>= ,arg2 0)
                                                                          (let ((,arg3 (unsafe-make-vector ,arg2)))
                                                                            (call ,loop-label ,arg2 0 ,arg3))
                                                                          (error 12)))))

                    ;; Generate loop for initializing vector to 0
                    (define arg4 (fresh 'len))
                    (define arg5 (fresh 'i))
                    (define arg6 (fresh 'vec))
                    (define fun3 `(define ,loop-label (lambda (,arg4 ,arg5 ,arg6) (if (eq? ,arg4 ,arg5)
                                                                                      ,arg6
                                                                                      (begin
                                                                                        (unsafe-vector-set! ,arg6 ,arg5 0)
                                                                                        (call ,loop-label ,arg4 (unsafe-fx+ ,arg5 1) ,arg6))))))

                    (hash-set! new-funcs fun2 (list loop-label fun3))
                    (hash-set! new-funcs fun1 (list init-label fun2))
                    (hash-set! new-funcs prim-f (list make-label fun1))
                    make-label]
      ['vector-ref (define safe-label (fresh-label 'vector-ref))
                   (define unsafe-label (fresh-label 'unsafe-vector-ref))

                   ;; Generate safety check for if arguments are vector? and fixnum?
                   (define arg1 (fresh))
                   (define arg2 (fresh))
                   (define fun1 `(define ,safe-label (lambda (,arg1 ,arg2)
                                                       (if (fixnum? ,arg2)
                                                           (if (vector? ,arg1)
                                                               (call ,unsafe-label ,arg1 ,arg2)
                                                               (error 11))
                                                           (error 11)))))

                   ;; Generate safety check for if fixnum is within bounds of vector
                   (define arg3 (fresh))
                   (define arg4 (fresh))
                   (define fun2 `(define ,unsafe-label (lambda (,arg3 ,arg4)
                                                         (if (unsafe-fx< ,arg4 (unsafe-vector-length ,arg3))
                                                             (if (unsafe-fx>= ,arg4 0)
                                                                 (unsafe-vector-ref ,arg3 ,arg4)
                                                                 (error 11))
                                                             (error 11)))))

                   (hash-set! new-funcs prim-f (list safe-label fun1))
                   (hash-set! new-funcs fun1 (list unsafe-label fun2))
                   safe-label]
      ['vector-set! (define safe-label (fresh-label 'vector-set!))
                    (define unsafe-label (fresh-label 'unsafe-vector-set!))

                    ;; Generate safety check for if arguments are vector? and fixnum?
                    (define arg1 (fresh))
                    (define arg2 (fresh))
                    (define arg3 (fresh))
                    (define fun1 `(define ,safe-label (lambda (,arg1 ,arg2 ,arg3)
                                                        (if (fixnum? ,arg2)
                                                            (if (vector? ,arg1)
                                                                (call ,unsafe-label ,arg1 ,arg2 ,arg3)
                                                                (error 10))
                                                            (error 10)))))

                    ;; Generate safety check for if fixnum is within bounds of vector
                    (define arg4 (fresh))
                    (define arg5 (fresh))
                    (define arg6 (fresh))
                    (define fun2 `(define ,unsafe-label (lambda (,arg4 ,arg5 ,arg6)
                                                          (if (unsafe-fx>= ,arg5 (unsafe-vector-length ,arg4))
                                                              (if (unsafe-fx>= ,arg5 0)
                                                                  (begin (unsafe-vector-set! ,arg4 ,arg5 ,arg6) (void))
                                                                  (error 10))
                                                              (error 10)))))

                    (hash-set! new-funcs prim-f (list safe-label fun1))
                    (hash-set! new-funcs fun1 (list unsafe-label fun2))
                    safe-label]

      ['vector-length (define primop-spec (hash-ref primop-spec-map prim-f))
                      (define label (fresh-label 'vector-length))
                      (generate-safety-checks label prim-f primop-spec)
                      label]

      ;; Wildcard case for all other primitive operations as they have the same pattern
      ;; of generating a safety check
      [_ (define primop (hash-ref primop-spec-map prim-f))
         (define label (fresh-label prim-f))
         (generate-safety-checks label prim-f primop)
         label]))

  (match p
    [`(module ,funcs ... ,value)
     (define funcs^ (map implement-safe-primops-func funcs))
     (define value^ (implement-safe-primops-value value))
     `(module ,@(map second (hash-values new-funcs)) ,@funcs^ ,value^)]))


(module+ test
  (check-equal? (implement-safe-primops '(module (call + 1 2)))
                '(module
                     (define L.+.1
                       (lambda (tmp.1 tmp.2)
                         (if (fixnum? tmp.1)
                             (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2) (error 2))
                             (error 2))))
                   (call L.+.1 1 2)))
  (check-equal? (implement-safe-primops '(module (call * 1 2)))
                '(module
                     (define L.*.2
                       (lambda (tmp.3 tmp.4)
                         (if (fixnum? tmp.3)
                             (if (fixnum? tmp.4) (unsafe-fx* tmp.3 tmp.4) (error 1))
                             (error 1))))
                   (call L.*.2 1 2)))
  (check-equal? (implement-safe-primops '(module (call - 1 2)))
                '(module
                     (define L.-.3
                       (lambda (tmp.5 tmp.6)
                         (if (fixnum? tmp.5)
                             (if (fixnum? tmp.6) (unsafe-fx- tmp.5 tmp.6) (error 3))
                             (error 3))))
                   (call L.-.3 1 2)))
  (check-equal? (implement-safe-primops '(module (call eq? 1 2)))
                '(module
                     (define L.eq?.4 (lambda (tmp.7 tmp.8) (eq? tmp.7 tmp.8)))
                   (call L.eq?.4 1 2)))
  (check-equal? (implement-safe-primops '(module (call > 1 2)))
                '(module
                     (define L.>.5
                       (lambda (tmp.9 tmp.10)
                         (if (fixnum? tmp.9)
                             (if (fixnum? tmp.10) (unsafe-fx> tmp.9 tmp.10) (error 6))
                             (error 6))))
                   (call L.>.5 1 2)))
  (check-equal? (implement-safe-primops '(module (call >= 1 2)))
                '(module
                     (define L.>=.6
                       (lambda (tmp.11 tmp.12)
                         (if (fixnum? tmp.11)
                             (if (fixnum? tmp.12) (unsafe-fx>= tmp.11 tmp.12) (error 7))
                             (error 7))))
                   (call L.>=.6 1 2)))
  (check-equal? (implement-safe-primops '(module (call < 1 2)))
                '(module
                     (define L.<.7
                       (lambda (tmp.13 tmp.14)
                         (if (fixnum? tmp.13)
                             (if (fixnum? tmp.14) (unsafe-fx< tmp.13 tmp.14) (error 4))
                             (error 4))))
                   (call L.<.7 1 2)))
  (check-equal? (implement-safe-primops '(module (call <= 1 2)))
                '(module
                     (define L.<=.8
                       (lambda (tmp.15 tmp.16)
                         (if (fixnum? tmp.15)
                             (if (fixnum? tmp.16) (unsafe-fx<= tmp.15 tmp.16) (error 5))
                             (error 5))))
                   (call L.<=.8 1 2)))
  (check-equal? (implement-safe-primops '(module (call fixnum? 1)))
                '(module
                     (define L.fixnum?.9 (lambda (tmp.17) (fixnum? tmp.17)))
                   (call L.fixnum?.9 1)))
  (check-equal? (implement-safe-primops '(module
                                             (define L.odd?.4
                                               (lambda (x.45)
                                                 (if (call eq? x.45 0)
                                                     0
                                                     (let ((y.46 (call + x.45 -1))) (call L.even?.5 y.46)))))
                                           (define L.even?.5
                                             (lambda (x.47)
                                               (if (call eq? x.47 0)
                                                   1
                                                   (let ((y.48 (call + x.47 -1))) (call L.odd?.4 y.48)))))
                                           (call L.even?.5 5)))
                '(module
                     (define L.+.11
                       (lambda (tmp.20 tmp.21)
                         (if (fixnum? tmp.20)
                             (if (fixnum? tmp.21) (unsafe-fx+ tmp.20 tmp.21) (error 2))
                             (error 2))))
                   (define L.eq?.10 (lambda (tmp.18 tmp.19) (eq? tmp.18 tmp.19)))
                   (define L.odd?.4
                     (lambda (x.45)
                       (if (call L.eq?.10 x.45 0)
                           0
                           (let ((y.46 (call L.+.11 x.45 -1))) (call L.even?.5 y.46)))))
                   (define L.even?.5
                     (lambda (x.47)
                       (if (call L.eq?.10 x.47 0)
                           1
                           (let ((y.48 (call L.+.11 x.47 -1))) (call L.odd?.4 y.48)))))
                   (call L.even?.5 5)))
  (check-equal? (implement-safe-primops '(module (let ([x.1 #t]
                                                       [x.2 #f]
                                                       [x.3 empty]
                                                       [x.4 (void)]
                                                       [x.5 (error 255)]
                                                       [x.6 #\x])
                                                   (if (call not (call boolean? x.1))
                                                       (call + (call error? x.5) (call empty? x.3))
                                                       (call ascii-char? x.6)))))
                '(module
                     (define L.error?.15 (lambda (tmp.26) (error? tmp.26)))
                   (define L.boolean?.13 (lambda (tmp.23) (boolean? tmp.23)))
                   (define L.ascii-char?.17 (lambda (tmp.28) (ascii-char? tmp.28)))
                   (define L.empty?.16 (lambda (tmp.27) (empty? tmp.27)))
                   (define L.+.14
                     (lambda (tmp.24 tmp.25)
                       (if (fixnum? tmp.24)
                           (if (fixnum? tmp.25) (unsafe-fx+ tmp.24 tmp.25) (error 2))
                           (error 2))))
                   (define L.not.12 (lambda (tmp.22) (not tmp.22)))
                   (let ((x.1 #t)
                         (x.2 #f)
                         (x.3 empty)
                         (x.4 (void))
                         (x.5 (error 255))
                         (x.6 #\x))
                     (if (call L.not.12 (call L.boolean?.13 x.1))
                         (call L.+.14 (call L.error?.15 x.5) (call L.empty?.16 x.3))
                         (call L.ascii-char?.17 x.6)))))
  (check-equal? (implement-safe-primops '(module
                                             (define L.add.10
                                               (lambda (a.61 b.62 c.63 d.64 e.65 f.66 g.67 h.68)
                                                 (call
                                                  +
                                                  a.61
                                                  (call
                                                   +
                                                   b.62
                                                   (call
                                                    +
                                                    c.63
                                                    (call + d.64 (call + e.65 (call + f.66 (call + g.67 h.68)))))))))
                                           (define L.add-and-multiply.11
                                             (lambda (a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76 i.77)
                                               (let ((sum.78 (call L.add.10 a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76)))
                                                 (call * sum.78 i.77))))
                                           (call L.add-and-multiply.11 1 2 3 4 5 6 7 8 2)))
                '(module
                     (define L.*.19
                       (lambda (tmp.31 tmp.32)
                         (if (fixnum? tmp.31)
                             (if (fixnum? tmp.32) (unsafe-fx* tmp.31 tmp.32) (error 1))
                             (error 1))))
                   (define L.+.18
                     (lambda (tmp.29 tmp.30)
                       (if (fixnum? tmp.29)
                           (if (fixnum? tmp.30) (unsafe-fx+ tmp.29 tmp.30) (error 2))
                           (error 2))))
                   (define L.add.10
                     (lambda (a.61 b.62 c.63 d.64 e.65 f.66 g.67 h.68)
                       (call
                        L.+.18
                        a.61
                        (call
                         L.+.18
                         b.62
                         (call
                          L.+.18
                          c.63
                          (call
                           L.+.18
                           d.64
                           (call L.+.18 e.65 (call L.+.18 f.66 (call L.+.18 g.67 h.68)))))))))
                   (define L.add-and-multiply.11
                     (lambda (a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76 i.77)
                       (let ((sum.78 (call L.add.10 a.69 b.70 c.71 d.72 e.73 f.74 g.75 h.76)))
                         (call L.*.19 sum.78 i.77))))
                   (call L.add-and-multiply.11 1 2 3 4 5 6 7 8 2)))

  (check-equal? (implement-safe-primops '(module (call make-vector L.a.1 2)))
                '(module
                     (define L.vector-init-loop.22
                       (lambda (len.36 i.37 vec.38)
                         (if (eq? len.36 i.37)
                             vec.38
                             (begin
                               (unsafe-vector-set! vec.38 i.37 0)
                               (call L.vector-init-loop.22 len.36 (unsafe-fx+ i.37 1) vec.38)))))
                   (define L.make-vector.20
                     (lambda (tmp.33)
                       (if (fixnum? tmp.33) (call L.make-init-vector.21 tmp.33) (error 8))))
                   (define L.make-init-vector.21
                     (lambda (tmp.34)
                       (if (unsafe-fx>= tmp.34 0)
                           (let ((tmp.35 (unsafe-make-vector tmp.34)))
                             (call L.vector-init-loop.22 tmp.34 0 tmp.35))
                           (error 12))))
                   (call L.make-vector.20 L.a.1 2)))
  (check-equal? (implement-safe-primops '(module (let ([a.1 (call make-vector 1)] [b.1 (call make-vector 2)])
                                                   (call vector-length (call make-vector 2)))))
                '(module
                     (define L.vector-length.26
                       (lambda (tmp.45)
                         (if (vector? tmp.45) (unsafe-vector-length tmp.45) (error 9))))
                   (define L.make-init-vector.24
                     (lambda (tmp.40)
                       (if (unsafe-fx>= tmp.40 0)
                           (let ((tmp.41 (unsafe-make-vector tmp.40)))
                             (call L.vector-init-loop.25 tmp.40 0 tmp.41))
                           (error 12))))
                   (define L.make-vector.23
                     (lambda (tmp.39)
                       (if (fixnum? tmp.39) (call L.make-init-vector.24 tmp.39) (error 8))))
                   (define L.vector-init-loop.25
                     (lambda (len.42 i.43 vec.44)
                       (if (eq? len.42 i.43)
                           vec.44
                           (begin
                             (unsafe-vector-set! vec.44 i.43 0)
                             (call L.vector-init-loop.25 len.42 (unsafe-fx+ i.43 1) vec.44)))))
                   (let ((a.1 (call L.make-vector.23 1)) (b.1 (call L.make-vector.23 2)))
                     (call L.vector-length.26 (call L.make-vector.23 2)))))
  (check-equal? (implement-safe-primops '(module
                                             (let ([a.1 (call make-vector 1)] [b.1 (call make-vector 2)])
                                               (call vector-set!
                                                     (call make-vector 2)
                                                     (call vector-length b.1)
                                                     9))))
                '(module
                     (define L.vector-length.32
                       (lambda (tmp.58)
                         (if (vector? tmp.58) (unsafe-vector-length tmp.58) (error 9))))
                   (define L.vector-set!.30
                     (lambda (tmp.52 tmp.53 tmp.54)
                       (if (fixnum? tmp.53)
                           (if (vector? tmp.52)
                               (call L.unsafe-vector-set!.31 tmp.52 tmp.53 tmp.54)
                               (error 10))
                           (error 10))))
                   (define L.make-vector.27
                     (lambda (tmp.46)
                       (if (fixnum? tmp.46) (call L.make-init-vector.28 tmp.46) (error 8))))
                   (define L.make-init-vector.28
                     (lambda (tmp.47)
                       (if (unsafe-fx>= tmp.47 0)
                           (let ((tmp.48 (unsafe-make-vector tmp.47)))
                             (call L.vector-init-loop.29 tmp.47 0 tmp.48))
                           (error 12))))
                   (define L.vector-init-loop.29
                     (lambda (len.49 i.50 vec.51)
                       (if (eq? len.49 i.50)
                           vec.51
                           (begin
                             (unsafe-vector-set! vec.51 i.50 0)
                             (call L.vector-init-loop.29 len.49 (unsafe-fx+ i.50 1) vec.51)))))
                   (define L.unsafe-vector-set!.31
                     (lambda (tmp.55 tmp.56 tmp.57)
                       (if (unsafe-fx>= tmp.56 (unsafe-vector-length tmp.55))
                           (if (unsafe-fx>= tmp.56 0)
                               (begin (unsafe-vector-set! tmp.55 tmp.56 tmp.57) (void))
                               (error 10))
                           (error 10))))
                   (let ((a.1 (call L.make-vector.27 1)) (b.1 (call L.make-vector.27 2)))
                     (call
                      L.vector-set!.30
                      (call L.make-vector.27 2)
                      (call L.vector-length.32 b.1)
                      9))))
  (check-equal? (implement-safe-primops '(module (let ([a.1 (call cons 1 2)]
                                                       [b.1 (call make-vector 1)])
                                                   (call vector-set!
                                                         (call make-vector 2)
                                                         (call vector-length b.1)
                                                         (call car a.1) ))))

                '(module
                     (define L.car.40
                       (lambda (tmp.74) (if (pair? tmp.74) (unsafe-car tmp.74) (error 12))))
                   (define L.vector-length.39
                     (lambda (tmp.73)
                       (if (vector? tmp.73) (unsafe-vector-length tmp.73) (error 9))))
                   (define L.unsafe-vector-set!.38
                     (lambda (tmp.70 tmp.71 tmp.72)
                       (if (unsafe-fx>= tmp.71 (unsafe-vector-length tmp.70))
                           (if (unsafe-fx>= tmp.71 0)
                               (begin (unsafe-vector-set! tmp.70 tmp.71 tmp.72) (void))
                               (error 10))
                           (error 10))))
                   (define L.vector-set!.37
                     (lambda (tmp.67 tmp.68 tmp.69)
                       (if (fixnum? tmp.68)
                           (if (vector? tmp.67)
                               (call L.unsafe-vector-set!.38 tmp.67 tmp.68 tmp.69)
                               (error 10))
                           (error 10))))
                   (define L.make-vector.34
                     (lambda (tmp.61)
                       (if (fixnum? tmp.61) (call L.make-init-vector.35 tmp.61) (error 8))))
                   (define L.vector-init-loop.36
                     (lambda (len.64 i.65 vec.66)
                       (if (eq? len.64 i.65)
                           vec.66
                           (begin
                             (unsafe-vector-set! vec.66 i.65 0)
                             (call L.vector-init-loop.36 len.64 (unsafe-fx+ i.65 1) vec.66)))))
                   (define L.make-init-vector.35
                     (lambda (tmp.62)
                       (if (unsafe-fx>= tmp.62 0)
                           (let ((tmp.63 (unsafe-make-vector tmp.62)))
                             (call L.vector-init-loop.36 tmp.62 0 tmp.63))
                           (error 12))))
                   (define L.cons.33 (lambda (tmp.59 tmp.60) (cons tmp.59 tmp.60)))
                   (let ((a.1 (call L.cons.33 1 2)) (b.1 (call L.make-vector.34 1)))
                     (call
                      L.vector-set!.37
                      (call L.make-vector.34 2)
                      (call L.vector-length.39 b.1)
                      (call L.car.40 a.1)))))
  (check-equal? (implement-safe-primops '(module (call make-vector 2)))
                '(module
                     (define L.make-init-vector.42
                       (lambda (tmp.76)
                         (if (unsafe-fx>= tmp.76 0)
                             (let ((tmp.77 (unsafe-make-vector tmp.76)))
                               (call L.vector-init-loop.43 tmp.76 0 tmp.77))
                             (error 12))))
                   (define L.vector-init-loop.43
                     (lambda (len.78 i.79 vec.80)
                       (if (eq? len.78 i.79)
                           vec.80
                           (begin
                             (unsafe-vector-set! vec.80 i.79 0)
                             (call L.vector-init-loop.43 len.78 (unsafe-fx+ i.79 1) vec.80)))))
                   (define L.make-vector.41
                     (lambda (tmp.75)
                       (if (fixnum? tmp.75) (call L.make-init-vector.42 tmp.75) (error 8))))
                   (call L.make-vector.41 2)))
  (check-equal? (implement-safe-primops '(module (call vector-ref (call make-vector 2) 0)))
                '(module
                     (define L.vector-init-loop.48
                       (lambda (len.88 i.89 vec.90)
                         (if (eq? len.88 i.89)
                             vec.90
                             (begin
                               (unsafe-vector-set! vec.90 i.89 0)
                               (call L.vector-init-loop.48 len.88 (unsafe-fx+ i.89 1) vec.90)))))
                   (define L.vector-ref.44
                     (lambda (tmp.81 tmp.82)
                       (if (fixnum? tmp.82)
                           (if (vector? tmp.81)
                               (call L.unsafe-vector-ref.45 tmp.81 tmp.82)
                               (error 11))
                           (error 11))))
                   (define L.make-vector.46
                     (lambda (tmp.85)
                       (if (fixnum? tmp.85) (call L.make-init-vector.47 tmp.85) (error 8))))
                   (define L.make-init-vector.47
                     (lambda (tmp.86)
                       (if (unsafe-fx>= tmp.86 0)
                           (let ((tmp.87 (unsafe-make-vector tmp.86)))
                             (call L.vector-init-loop.48 tmp.86 0 tmp.87))
                           (error 12))))
                   (define L.unsafe-vector-ref.45
                     (lambda (tmp.83 tmp.84)
                       (if (unsafe-fx< tmp.84 (unsafe-vector-length tmp.83))
                           (if (unsafe-fx>= tmp.84 0)
                               (unsafe-vector-ref tmp.83 tmp.84)
                               (error 11))
                           (error 11))))
                   (call L.vector-ref.44 (call L.make-vector.46 2) 0))))
