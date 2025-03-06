#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v6
  rackunit)

(provide check-values-lang)

;; Any -> values-lang-v6
;; checks the typing requirements from values-lang-v6 with p
(define/contract (check-types-lang p)
  (-> any/c values-lang-v6?)

  ;; func-args is (Map-of name natural)
  ;; keeps track of the number of arguments associated with a procedure
  (define func-args (make-hash))

  ;; Any ->
  ;; EFFECTS: raises an error if the typing requirements are not met and binds the
  ;; number of arguments and function name in func-args
  (define (check-types-lang-func-args func)
    (match func
      [`(define ,funcName (lambda (,args ...) ,tail))
       (when (hash-has-key? func-args funcName)
         (error 'check-types-lang (format "Duplicate function name: ~a" funcName)))
       (unless (equal? (length args) (length (remove-duplicates args)))
         (error 'check-types-lang (format "Duplicate argument names in function ~a: ~a" funcName args)))
       (hash-set! func-args funcName (length args))]))

  ;; Any ->
  ;; EFFECTS: raises an error if the typing requirements are not met
  (define (check-types-lang-func-tail func)
    (match func
      [`(define ,funcName (lambda (,args ...) ,tail))
       (check-types-lang-tail tail args (extend-env* empty-env (hash-keys func-args) (hash-keys func-args)))]))

  ;; Any ->
  ;; EFFECTS: raises an error if the typing requirements are not met
  (define (check-types-lang-tail tail args env)
    (match tail
      [`(let ([,xs ,vs] ...) ,t)
       (unless (equal? (length xs) (length (remove-duplicates xs)))
         (error 'check-types-lang "Parallel bindings of the same variable are not allowed"))
       (define vs^ (map (lambda (v) (check-types-lang-value v args env)) vs))
       (map (lambda (x v) (if (hash-has-key? func-args x)
                              (void)
                              (hash-set! func-args x v))) xs vs^)
       (define new-env (extend-env* env xs vs^))
       (check-types-lang-tail t args new-env)]
      [`(if ,pred ,t1 ,t2)
       (check-types-lang-pred pred args env)
       (check-types-lang-tail t1 args env)
       (check-types-lang-tail t2 args env)]
      [`(call ,x ,trivs ...)
       (cond
         [(not (hash-has-key? func-args (lookup-env env x)))
          (error 'check-types-lang (format "Function not defined: ~a in env: ~a with hash: ~a" x env func-args))]
         [(not (eq? (hash-ref func-args (lookup-env env x)) (length trivs)))
          (error 'check-types-lang "Wrong number of arguments")]
         [else (for-each (lambda (t)
                           (unless (int64? (check-types-lang-triv t args env))
                             (error 'check-types-lang (format "Expected int64 but got: ~a" t))))
                         trivs)])]
      [value
       (unless (int64? (check-types-lang-value value args env))
         (error 'check-types-lang (format "Must end with int64: ~a" tail)))]))

  ;; Any -> Natural
  ;; EFFECTS: raises an error if the typing requirements are not met
  (define (check-types-lang-value value args env)
    (match value
      [`(let ([,xs ,vs] ...) ,v)
       (unless (equal? (length xs) (length (remove-duplicates xs)))
         (error 'check-types-lang "Parallel bindings of the same variable are not allowed"))
       (define vs^ (map (lambda (v) (check-types-lang-value v args env)) vs))
       (define new-env (extend-env* env xs vs^))
       (check-types-lang-value v args new-env)
       0]
      [`(if ,pred ,v1 ,v2)
       (check-types-lang-pred pred args env)
       (check-types-lang-value v1 args env)
       (check-types-lang-value v2 args env)
       (if (and (name? v1) (name? v2))
           (let* ([v1-length (hash-ref func-args v1)]
                  [v2-length (hash-ref func-args v2)])
             (if (equal? v1-length v2-length)
                 v1
                 0))
           0)]
      [`(call ,x ,trivs ...)
       (cond
         [(not (hash-has-key? func-args (lookup-env env x)))
          (error 'check-types-lang (format "Function not defined: ~a in env: ~a with hash: ~a" x env func-args))]
         [(not (eq? (hash-ref func-args (lookup-env env x)) (length trivs)))
          (error 'check-types-lang "Wrong number of arguments")]
         [else (for-each (lambda (t)
                           (unless (int64? (check-types-lang-triv t args env))
                             (error 'check-types-lang (format "Expected int64 but got: ~a" t))))
                         trivs)])]
      [`(,binop ,triv1 ,triv2)
       (unless (and (int64? (check-types-lang-triv triv1 args env))
                    (int64? (check-types-lang-triv triv2 args env)))
         (error 'check-types-lang (format "Binop must have two integer operands: ~a" value)))
       0]
      [triv (check-types-lang-triv triv args env)]))

  ;; Any ->
  ;; EFFECTS: raises an error if the typing requirements are not met
  (define (check-types-lang-pred pred args env)
    (match pred
      ['(true) pred]
      ['(false) pred]
      [`(not ,p) (check-types-lang-pred p args env)]
      [`(let ([,xs ,vs] ...) ,p)
       (unless (equal? (length xs) (length (remove-duplicates xs)))
         (error 'check-types-lang "Parallel bindings of the same variable are not allowed"))
       (define vs^ (map (lambda (v) (check-types-lang-value v args env)) vs))
       (define new-env (extend-env* env xs vs^))
       (check-types-lang-pred p args new-env)]
      [`(if ,p1 ,p2 ,p3)
       (check-types-lang-pred p1 args env)
       (check-types-lang-pred p2 args env)
       (check-types-lang-pred p3 args env)]
      [`(,relop ,triv1 ,triv2)
       (unless (and (int64? (check-types-lang-triv triv1 args env))
                    (int64? (check-types-lang-triv triv2 args env)))
         (error 'check-types-lang (format "Relop must have two integer operands: ~a" pred)))]))

  ;; Any -> Natural
  ;; EFFECTS: raises an error if the typing requirements are not met
  (define (check-types-lang-triv triv args env)
    (match triv
      [int64 #:when (int64? int64) int64]
      [x #:when (name? x)
         (if (member x args)
             0
             (lookup-env env x))]))

  (match p
    [`(module ,funcs ... ,tail)
     (for-each check-types-lang-func-args funcs)
     (for-each check-types-lang-func-tail funcs)
     (check-types-lang-tail tail '() (extend-env* empty-env (hash-keys func-args) (hash-keys func-args)))
     p]))

;; Any -> Any
;; interp. checks the syntax of p with the requirements for values-lang-v5
(define/contract (check-syntax-lang p)
  (-> any/c any/c)

  ;; Any -> Any
  (define (check-syntax-lang-func func)
    (match func
      [`(define ,funcName (lambda (,args ...) ,tail))
       #:when (and (name? funcName) (andmap name? args))
       (check-syntax-lang-tail tail)
       func]
      [else (error 'check-syntax-lang "Invalid function definition")]))

  ;; Any -> Any
  (define (check-syntax-lang-tail tail)
    (match tail
      [`(let ([,xs ,vs] ...) ,t)
       #:when (andmap name? xs)
       (for-each check-syntax-lang-value vs)
       (check-syntax-lang-tail t)
       tail]
      [`(if ,pred ,t1 ,t2)
       (check-syntax-lang-pred pred)
       (check-syntax-lang-tail t1)
       (check-syntax-lang-tail t2)
       tail]
      [`(call ,funcName ,trivs ...)
       #:when (name? funcName)
       (for-each check-syntax-lang-triv trivs)
       tail]
      [value
       (check-syntax-lang-value value)
       tail]
      [_ (error 'check-syntax-lang (format "Invalid tail structure: ~a" tail))]))

  ;; Any -> Any
  (define (check-syntax-lang-value value)
    (match value
      [`(let ([,xs ,vs] ...) ,v)
       #:when (andmap name? xs)
       (for-each check-syntax-lang-value vs)
       (check-syntax-lang-value v)
       value]
      [`(if ,pred ,v1 ,v2)
       (check-syntax-lang-pred pred)
       (check-syntax-lang-value v1)
       (check-syntax-lang-value v2)
       value]
      [`(call ,funcName ,trivs ...)
       #:when (name? funcName)
       (for-each check-syntax-lang-triv trivs)
       value]
      [`(,binop ,triv1 ,triv2)
       #:when (binop? binop)
       (check-syntax-lang-triv triv1)
       (check-syntax-lang-triv triv2)
       value]
      [triv
       (check-syntax-lang-triv triv)
       value]
      [else (error 'check-syntax-lang (format "Invalid value structure: ~a" value))]))

  ;; Any -> Any
  (define (check-syntax-lang-pred pred)
    (match pred
      ['(true) pred]
      ['(false) pred]
      [`(not ,p) (check-syntax-lang-pred p)]
      [`(let ([,xs ,vs] ...) ,p)
       #:when (andmap name? xs)
       (for-each check-syntax-lang-value vs)
       (check-syntax-lang-pred p)
       pred]
      [`(if ,p1 ,p2 ,p3)
       (check-syntax-lang-pred p1)
       (check-syntax-lang-pred p2)
       (check-syntax-lang-pred p3)
       pred]
      [`(,relop ,triv1 ,triv2)
       #:when (relop? relop)
       (check-syntax-lang-triv triv1)
       (check-syntax-lang-triv triv2)
       pred]
      [else (error 'check-syntax-lang (format "Invalid pred structure: ~a" pred))]))

  ;; Any -> Any
  (define (check-syntax-lang-triv triv)
    (match triv
      [int64 #:when (int64? int64) (void)]
      [x #:when (name? x) (void)]
      [else (error 'check-syntax-lang (format "Invalid triv: ~a" triv))]))

  (match p
    [`(module ,funcs ... ,tail)
     `(module ,@(map check-syntax-lang-func funcs) ,(check-syntax-lang-tail tail))]
    [_ (error 'check-syntax-lang (format "Invalid program structure: ~a" p))]))

;; any -> values-lang-v6
;; validates that p is a syntactically well-formed, well bound and well
;; typed Values-lang v6: all procedure calls pass the correct number of
;; arguments, and all binop and relop are never used with labels
(define/contract (check-values-lang p)
  (-> any/c values-lang-v6?)

  (check-types-lang (check-syntax-lang p)))

(module+ test
  (check-exn exn:fail? (lambda () (check-values-lang '(module
                                                          (let ([x 1]
                                                                [x 2])
                                                            x)))))
  (check-equal? (check-values-lang '(module
                                        (define odd?
                                          (lambda (x)
                                            (if (= x 0)
                                                0
                                                (let ([y (+ x -1)])
                                                  (call even? y)))))
                                      (define even?
                                        (lambda (x)
                                          (if (= x 0)
                                              1
                                              (let ([y (+ x -1)])
                                                (call odd? y)))))
                                      (call even? 5)))
                '(module
                     (define odd? (lambda (x) (if (= x 0) 0 (let ((y (+ x -1))) (call even? y)))))
                   (define even? (lambda (x) (if (= x 0) 1 (let ((y (+ x -1))) (call odd? y)))))
                   (call even? 5)))
  (check-exn exn:fail? (lambda () (check-values-lang '(module (define f (lambda (x) x))
                                                        (let ([x 1])
                                                          (call x 1))))))
  (check-equal? (check-values-lang '(module (define f (lambda (x) x))
                                      (let ([f f])
                                        (call f 1))))
                '(module (define f (lambda (x) x)) (let ((f f)) (call f 1))))
  (check-exn exn:fail? (lambda () (check-values-lang '(module (define f (lambda (x) x))
                                                        (let ([f 1])
                                                          (call f f))))))
  (check-equal? (check-values-lang '(module (define f (lambda (x) x))
                                      (let ([x f])
                                        (call x 1))))
                '(module (define f (lambda (x) x)) (let ((x f)) (call x 1))))
  (check-equal? (check-values-lang '(module (define f (lambda (x) x))
                                      (let ([x f]
                                            [y 1])
                                        (call x y))))
                '(module (define f (lambda (x) x)) (let ((x f) (y 1)) (call x y))))
  (check-exn exn:fail? (lambda () (check-values-lang '(module
                                                          (define g (lambda (x) (call f x)))
                                                        (define f (lambda (x) x))
                                                        (call g x)))))
  (check-equal? (check-values-lang '(module
                                        (define g (lambda (x) (call f x)))
                                      (define f (lambda (x) x))
                                      (call g 1)))
                '(module
                     (define g (lambda (x) (call f x)))
                   (define f (lambda (x) x))
                   (call g 1)))
  (check-equal? (check-values-lang '(module 5))
                '(module 5))
  (check-equal? (check-values-lang '(module (+ 1 2)))
                '(module (+ 1 2)))
  (check-equal? (check-values-lang '(module (let ([x 1]) x)))
                '(module (let ([x 1]) x)))
  (check-equal? (check-values-lang '(module (let ([x 1] [y 2]) (+ x y))))
                '(module (let ([x 1] [y 2]) (+ x y))))
  (check-equal? (check-values-lang '(module
                                        (define f (lambda (x)
                                                    (let ([y 5])
                                                      (* x y))))
                                      (if (if (let ([x 1] [y 2]) (> x y)) (not (true)) (!= 5 1))
                                          (let ([x (let ([y 1]) (+ y 2))])
                                            (if (< x 10)
                                                (+ x 5)
                                                (* 10 x)))
                                          (call f 10))))
                '(module
                     (define f (lambda (x) (let ((y 5)) (* x y))))
                   (if (if (let ((x 1) (y 2)) (> x y)) (not (true)) (!= 5 1))
                       (let ((x (let ((y 1)) (+ y 2)))) (if (< x 10) (+ x 5) (* 10 x)))
                       (call f 10))))
  (check-equal? (check-values-lang '(module (define x (lambda (y) y)) (call x 1)))
                '(module (define x (lambda (y) y)) (call x 1)))
  (check-equal? (check-values-lang '(module (define f (lambda (x y) (+ x y))) (call f 1 2)))
                '(module (define f (lambda (x y) (+ x y))) (call f 1 2)))
  (check-equal? (check-values-lang '(module (define x (lambda () 42)) (call x)))
                '(module (define x (lambda () 42)) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (let ([y 10]) y))) (call x)))
                '(module (define x (lambda () (let ([y 10]) y))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (if (< 2 3) 5 10))) (call x)))
                '(module (define x (lambda () (if (< 2 3) 5 10))) (call x)))
  (check-equal? (check-values-lang '(module (define f (lambda (y z) (+ y z))) (define x (lambda () (call f 1 2))) (call x)))
                '(module (define f (lambda (y z) (+ y z))) (define x (lambda () (call f 1 2))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (+ 3 4))) (call x)))
                '(module (define x (lambda () (+ 3 4))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (* 2 5))) (call x)))
                '(module (define x (lambda () (* 2 5))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (let ([y 10]) (+ y 5)))) (call x)))
                '(module (define x (lambda () (let ([y 10]) (+ y 5)))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () (if (true) 1 0))) (call x)))
                '(module (define x (lambda () (if (true) 1 0))) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda () 100)) (call x)))
                '(module (define x (lambda () 100)) (call x)))
  (check-equal? (check-values-lang '(module (define x (lambda (y) y)) (call x 1)))
                '(module (define x (lambda (y) y)) (call x 1)))
  (check-equal? (check-values-lang '(module
                                        (define f (lambda () 1))
                                      (define g (lambda () (call f)))
                                      (call g)))
                '(module (define f (lambda () 1)) (define g (lambda () (call f))) (call g)))
  (check-equal? (check-values-lang '(module
                                        (define id1 (lambda (x) x))
                                      (define id2 (lambda (x) x))
                                      (let ((y (if (true) id1 id2))) (call y 5))))
                '(module
                     (define id1 (lambda (x) x))
                   (define id2 (lambda (x) x))
                   (let ((y (if (true) id1 id2))) (call y 5))))

  (check-exn exn:fail? (lambda ()
                         (check-values-lang '(module (define x (lambda (y) y)) x))))
  (check-exn exn:fail? (lambda ()
                         (check-values-lang '(module (define x (lambda (y) x)) (call x)))))
  (check-exn exn:fail? (lambda ()
                         (check-values-lang '(module
                                                 (define f (lambda (x) x))
                                               (define g (lambda (x) x))
                                               (call f g)))))
  (check-exn exn:fail? (lambda ()
                         (check-values-lang '(module
                                                 (define f (lambda (x) x))
                                               (call f)))))
  (check-exn exn:fail? (lambda ()
                         (check-values-lang '(module
                                                 (define f (lambda (x y z) x))
                                               (call f 1 2 3 4 5)))))
  (check-exn exn:fail? (lambda ()
                         (check-values-lang '(module
                                                 (define f (lambda (x) x))
                                               (define g (lambda (x) x))
                                               (+ f g)))))
  (check-exn exn:fail? (lambda ()
                         (check-values-lang '(module
                                                 (define f (lambda (x) x))
                                               (define g (lambda (x) x))
                                               (if (> f g)
                                                   1
                                                   2))))))
