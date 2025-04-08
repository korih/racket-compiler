#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9
  rackunit)

(provide uniquify)

;; exprs-lang-v9 -> exprs-unique-lang-v9
;; compiles p to Exprs-unique-lang v9 by resolving top-level lexical
;; identifiers into unique labels, and all other lexical identifiers into
;; unique abstract locations
(define/contract (uniquify p)
  (-> exprs-lang-v9? exprs-unique-lang-v9?)

  ;; func is `(define ,label (lambda (,alocs ...) ,value))
  ;; interp. a function definition

  ;; (List-of func) -> (Env-of exprs-unique-lang-v9.triv)
  ;; interp. creates an environment with all the unique function labels
  (define (initialize-env funcs)
    (for/fold ([env empty-env])
              ([fun funcs])
      (match fun
        [`(define ,funcName (lambda (,args ...) ,tail))
         (define unique-label (fresh funcName))
         (define env^ (extend-env env funcName unique-label))
         env^])))

  ;; (List-of func) (Env-of exprs-unique-lang-v9.triv) -> (values (List-of func) (Env-of exprs-unique-lang-v9.triv))
  ;; interp. processes each function definition by assigning lexical identifiers with unique labels and abstract locations
  (define (identify-function-labels funcs env)
    (for/fold ([updated-funcs '()]
               [updated-env env])
              ([func funcs])
      (define-values (updated-func new-env) (uniquify-func func updated-env))
      (values (cons updated-func updated-funcs) new-env)))

  ;; func (Env-of exprs-unique-lang-v9.triv) -> func (Env-of exprs-unique-lang-v9.triv)
  ;; interp. for a given function definition, go through its args and body and produce uniquified version
  (define (uniquify-func func env)
    (match func
      [`(define ,funcName (lambda (,args ...) ,value))
       (define unique-label (lookup-env env funcName))
       (define unique-args (map fresh args))
       (define new-env (extend-env* env args unique-args))
       (values `(define ,unique-label (lambda (,@unique-args) ,(uniquify-value value new-env)))
               env)]))

  ;; exprs-lang-v9.value (Env-of exprs-unique-lang-v9.triv) -> exprs-unique-lang-v9.value
  ;; interp. rom a given value and environment, produce the uniquified version of it
  (define (uniquify-value value env)
    (match value
      [`(let ([,xs ,vs] ...) ,v)
       (define unique-names (map fresh xs))
       (define new-env (extend-env* env xs unique-names))
       (define unique-binds
         (map (lambda (uname value)
                (list uname (uniquify-value value env)))
              unique-names vs))
       `(let (,@unique-binds) ,(uniquify-value v new-env))]
      [`(if ,p-value ,t-value ,f-value)
       `(if ,(uniquify-value p-value env)
            ,(uniquify-value t-value env)
            ,(uniquify-value f-value env))]
      [`(call ,vs ...)
       `(call ,@(map (lambda (v) (uniquify-value v env)) vs))]
      [triv (uniquify-triv triv env)]))

  ;; exprs-lang-v9.triv (Env-of exprs-unique-lang-v9.triv) -> exprs-unique-lang-v9.triv
  ;; interp. resolves triv as terminal case, or x
  (define (uniquify-triv triv env)
    (match triv
      [#t #t]
      [#f #f]
      ['empty 'empty]
      ['(void) '(void)]
      [`(error ,n) `(error ,n)]
      [asci #:when (ascii-char-literal? asci) asci]
      [fixnum #:when (fixnum? fixnum) fixnum]
      [`(lambda (,xs ...) ,value)
       (define unique-names (map fresh xs))
       (define new-env (extend-env* env xs unique-names))
       (define value^ (uniquify-value value new-env))
       `(lambda ,unique-names ,value^)]
      [x (uniquify-x x env)]))

  ;; exprs-unique-lang-v9.x (Env-of exprs-unique-lang-v9.triv) -> exprs-unique-lang-v9.triv
  ;; interp. resolves x primitive function or a unique variable definition
  (define (uniquify-x x env)
    (match x
      [prim-f
       #:when (prim-f? prim-f)
       (if (assoc prim-f env)
           (lookup-env env prim-f)
           prim-f)]
      [name #:when (name? name) (lookup-env env name)]))

  (match p
    [`(module ,funcs ... ,value)
     (define defined-funs (initialize-env funcs))
     (define-values (updated-funcs updated-env) (identify-function-labels funcs defined-funs))
     `(module ,@(reverse updated-funcs) ,(uniquify-value value updated-env))]))

(module+ test
  (define (test-helper interp input expected)
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (printf "Test Error: ~a\nWith input: ~a\n" (exn-message e) input))])
      (let ([actual (interp input)]
            [expected-val (interp expected)])
        (check-equal? actual expected-val "Test Helper"))))

  (check-equal? (uniquify '(module (call + 2 2)))
                '(module (call + 2 2)))
  (check-equal? (uniquify '(module (let ([x 5]) x)))
                '(module (let ((x.1 5)) x.1)))
  (check-equal? (uniquify '(module (let ([x 5]) (call + x 1))))
                '(module (let ((x.2 5)) (call + x.2 1))))
  (check-equal? (uniquify '(module (let ([x 5] [y 10]) (call + x y))))
                '(module (let ((x.3 5) (y.4 10)) (call + x.3 y.4))))
  (check-equal? (uniquify '(module (let ([x 5] [y (call + 3 2)]) (call * x y))))
                '(module (let ((x.5 5) (y.6 (call + 3 2))) (call * x.5 y.6))))
  (check-equal? (uniquify '(module (define + (lambda (x y) (call + x y)))
                             (call + 1 2)))
                '(module
                     (define |+.7| (lambda (x.8 y.9) (call |+.7| x.8 y.9)))
                   (call |+.7| 1 2)))
  (check-equal? (uniquify '(module (let ([x 1])
                                     (let ([x #t])
                                       (let ([x #f])
                                         (let ([x empty])
                                           (let ([x (void)])
                                             (let ([x (error 255)])
                                               (let ([x #\space])
                                                 x)))))))))
                '(module
                     (let ((x.10 1))
                       (let ((x.11 #t))
                         (let ((x.12 #f))
                           (let ((x.13 empty))
                             (let ((x.14 (void)))
                               (let ((x.15 (error 255))) (let ((x.16 #\space)) x.16)))))))))
  (check-equal? (uniquify '(module (let ([x 5]) (let ([y x]) (call + y 1)))))
                '(module (let ((x.17 5)) (let ((y.18 x.17)) (call + y.18 1)))))
  (check-equal? (uniquify '(module (let ([x 5]) (let ([y x]) (let ([z y]) (call + z 2))))))
                '(module
                     (let ((x.19 5)) (let ((y.20 x.19)) (let ((z.21 y.20)) (call + z.21 2))))))
  (check-equal? (uniquify '(module (let ([x 5]) (let ([x 6]) x))))
                '(module (let ((x.22 5)) (let ((x.23 6)) x.23))))
  (check-equal? (uniquify '(module (let ([x 5]) (let ([x (call + x 1)]) (let ([x (call + x 2)]) x)))))
                '(module
                     (let ((x.24 5))
                       (let ((x.25 (call + x.24 1))) (let ((x.26 (call + x.25 2))) x.26)))))
  (check-equal? (uniquify `(module (let ([x ,(- (expt 2 60))] [y ,(- (expt 2 60) 1)]) (call + x y))))
                `(module (let ((x.27 ,(- (expt 2 60))) (y.28 ,(- (expt 2 60) 1))) (call + x.27 y.28))))
  (check-equal? (uniquify '(module (let ([x 5]) (let ([y x]) (let ([x y]) (call + x y))))))
                '(module
                     (let ((x.29 5)) (let ((y.30 x.29)) (let ((x.31 y.30)) (call + x.31 y.30))))))
  (check-equal? (uniquify '(module (let ([a 1]) (let ([b (call + a 1)]) (let ([c (call + b 1)]) (call + c 2))))))
                '(module
                     (let ((a.32 1))
                       (let ((b.33 (call + a.32 1)))
                         (let ((c.34 (call + b.33 1))) (call + c.34 2))))))
  (check-equal? (uniquify '(module (let ([x 1]) (let ([y (call + x 2)]) (let ([x (call + y 3)]) (call + x y))))))
                '(module
                     (let ((x.35 1))
                       (let ((y.36 (call + x.35 2)))
                         (let ((x.37 (call + y.36 3))) (call + x.37 y.36))))))
  (check-equal? (uniquify '(module (if (let ([x #t]) x) (let ([x 5]) x) -1)))
                '(module (if (let ((x.38 #t)) x.38) (let ((x.39 5)) x.39) -1)))
  (check-equal? (uniquify '(module (let ([y (if (if #t #f #t)
                                                (call * 3 3)
                                                0)])
                                     (call + y -1))))
                '(module (let ((y.40 (if (if #t #f #t) (call * 3 3) 0))) (call + y.40 -1))))
  (check-equal? (uniquify '(module
                               (define f (lambda (x y) (call + x y)))
                             (define x (lambda (z) (let ([x 1])
                                                     (call + x z))))
                             (if #t
                                 (call f 1 2)
                                 (call x 1))))
                '(module
                     (define f.41 (lambda (x.43 y.44) (call + x.43 y.44)))
                   (define x.42 (lambda (z.45) (let ((x.46 1)) (call + x.46 z.45))))
                   (if #t (call f.41 1 2) (call x.42 1))))
  (check-equal? (uniquify '(module
                               (define odd? (lambda (x) (if (call eq? x 0) 0 (let ((y (call + x -1))) (call even? y)))))
                             (define even? (lambda (x) (if (call eq? x 0) 1 (let ((y (call + x -1))) (call odd? y)))))
                             (call even? 5)))
                '(module
                     (define odd?.47
                       (lambda (x.49)
                         (if (call eq? x.49 0)
                             0
                             (let ((y.50 (call + x.49 -1))) (call even?.48 y.50)))))
                   (define even?.48
                     (lambda (x.51)
                       (if (call eq? x.51 0)
                           1
                           (let ((y.52 (call + x.51 -1))) (call odd?.47 y.52)))))
                   (call even?.48 5)))
  (check-equal? (uniquify '(module
                               (define f (lambda (x y) (call + x y)))
                             (define x (lambda (z) (let ([x 1])
                                                     (call + x z))))
                             (let ([x (call f 1 2)])
                               (let ([y (call x x)])
                                 y))))
                '(module
                     (define f.53 (lambda (x.55 y.56) (call + x.55 y.56)))
                   (define x.54 (lambda (z.57) (let ((x.58 1)) (call + x.58 z.57))))
                   (let ((x.59 (call f.53 1 2))) (let ((y.60 (call x.59 x.59))) y.60))))
  (test-helper
   interp-exprs-unique-lang-v9
   (uniquify '(module
                  (define f (lambda (x y) (call + x y)))
                (define x (lambda (z) (let ([x 1])
                                        (call + x z))))
                (let ([a (call f 1 2)])
                  (let ([y (call x a)])
                    y))))
   '(module
        (define L.f.8 (lambda (x.55 y.56) (call + x.55 y.56)))
      (define L.x.9 (lambda (z.57) (let ((x.58 1)) (call + x.58 z.57))))
      (let ((a.59 (call L.f.8 1 2))) (let ((y.60 (call L.x.9 a.59))) y.60))))
  (check-equal? (uniquify '(module
                               (define add
                                 (lambda (a b c d e f g h)
                                   (call + a (call + b (call + c (call + d (call + e (call + f (call + g h)))))))))
                             (define add-and-multiply (lambda (a b c d e f g h i)
                                                        (let ([sum (call add a b c d e f g h)])
                                                          (call * sum i))))
                             (call add-and-multiply 1 2 3 4 5 6 7 8 2)))
                '(module
                     (define add.69
                       (lambda (a.71 b.72 c.73 d.74 e.75 f.76 g.77 h.78)
                         (call
                          +
                          a.71
                          (call
                           +
                           b.72
                           (call
                            +
                            c.73
                            (call + d.74 (call + e.75 (call + f.76 (call + g.77 h.78)))))))))
                   (define add-and-multiply.70
                     (lambda (a.79 b.80 c.81 d.82 e.83 f.84 g.85 h.86 i.87)
                       (let ((sum.88 (call add.69 a.79 b.80 c.81 d.82 e.83 f.84 g.85 h.86)))
                         (call * sum.88 i.87))))
                   (call add-and-multiply.70 1 2 3 4 5 6 7 8 2)))
  (check-equal? (uniquify '(module (call eq? 1 1)))
                '(module (call eq? 1 1)))
  (check-equal? (uniquify '(module (call fixnum? 1)))
                '(module (call fixnum? 1)))
  (check-equal? (uniquify '(module (call boolean? 1)))
                '(module (call boolean? 1)))
  (check-equal? (uniquify '(module (call empty? 1)))
                '(module (call empty? 1)))
  (check-equal? (uniquify '(module (call void? 1)))
                '(module (call void? 1)))
  (check-equal? (uniquify '(module (call ascii-char? #\x)))
                '(module (call ascii-char? #\x)))
  (check-equal? (uniquify '(module (call error? 1)))
                '(module (call error? 1)))
  (check-equal? (uniquify '(module (call cons 1 2)))
                '(module (call cons 1 2)))
  (check-equal? (uniquify '(module (call car (call cons 1 2))))
                '(module (call car (call cons 1 2))))
  (check-equal? (uniquify '(module (call make-vector 1)))
                '(module (call make-vector 1)))
  (check-equal? (uniquify '(module (call make-vector 2)))
                '(module (call make-vector 2)))
  (check-equal? (interp-exprs-unique-lang-v9 (uniquify '(module
                                                            (define v (lambda () (call make-vector 3)))
                                                          (define set-first (lambda (vec) (call vector-set! vec 0 42)))
                                                          (define get-first (lambda (vec) (call vector-ref vec 0)))

                                                          (let ([vec (call v)])
                                                            (call + (if (call void? (call set-first vec)) 0 (error 1))
                                                                  (call get-first vec))))))
                (interp-exprs-unique-lang-v9 '(module
                                                  (define L.v.4 (lambda () (call make-vector 3)))
                                                (define L.set-first.5 (lambda (vec.1) (call vector-set! vec.1 0 42)))
                                                (define L.get-first.6 (lambda (vec.2) (call vector-ref vec.2 0)))
                                                (let ((vec.3 (call L.v.4)))
                                                  (call
                                                   +
                                                   (if (call void? (call L.set-first.5 vec.3)) 0 (error 1))
                                                   (call L.get-first.6 vec.3))))))

  (check-equal? (uniquify '(module (lambda (x y) (call + x y))))
                '(module (lambda (x.95 y.96) (call + x.95 y.96))))
  (check-equal? (uniquify '(module (define add (lambda (x y) (call + x y)))
                             (call + 1 2)))
                '(module (define add.97 (lambda (x.98 y.99) (call + x.98 y.99))) (call + 1 2))
                "Check with definition with names")

  (test-helper interp-exprs-unique-lang-v9
               (uniquify '(module (lambda (x y) (call + x y))))
               '(module (lambda (x.5 y.4) (call + x.5 y.4)))))
