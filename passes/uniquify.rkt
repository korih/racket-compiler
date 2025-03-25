#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8
  rackunit)

(provide uniquify)

;; exprs-lang-v8 -> exprs-unique-lang-v8
;; compiles p to Exprs-unique-lang v8 by resolving top-level lexical
;; identifiers into unique labels, and all other lexical identifiers into
;; unique abstract locations
(define/contract (uniquify p)
  (-> exprs-lang-v8? exprs-unique-lang-v8?)

  ;; func is `(define ,label (lambda (,alocs ...) ,value))
  ;; interp. a function definition

  ;; (List-of func) -> (Env-of exprs-unique-lang-v8.triv)
  ;; interp. creates an environment with all the unique function labels
  (define (initialize-env funcs)
    (for/fold ([env empty-env])
              ([fun funcs])
      (match fun
        [`(define ,funcName (lambda (args ...) tail))
         (define unique-label (fresh-label funcName))
         (define env^ (extend-env env funcName unique-label))
         env^])))

  ;; (List-of func) (Env-of exprs-unique-lang-v8.triv) -> (values (List-of func) (Env-of exprs-unique-lang-v8.triv))
  ;; interp. processes each function definition by assigning lexical identifiers with unique labels and abstract locations
  (define (process-functions funcs env)
    (for/fold ([updated-funcs '()]
               [updated-env env])
              ([func funcs])
      (define-values (updated-func new-env) (uniquify-func func updated-env))
      (values (cons updated-func updated-funcs) new-env)))

  ;; func (Env-of exprs-unique-lang-v8.triv) -> (values func (Env-of exprs-unique-lang-v8.triv))
  (define (uniquify-func func env)
    (match func
      [`(define ,funcName (lambda (,args ...) ,value))
       (define unique-label (lookup-env env funcName))
       (define unique-args (map fresh args))
       (define new-env (extend-env* env args unique-args))
       (values `(define ,unique-label (lambda (,@unique-args) ,(uniquify-value value new-env)))
               env)]))

  ;; exprs-lang-v8.value (Env-of exprs-unique-lang-v8.triv) -> exprs-unique-lang-v8.value
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

  ;; exprs-lang-v8.triv (Env-of exprs-unique-lang-v8.triv) -> exprs-unique-lang-v8.triv
  (define (uniquify-triv triv env)
    (match triv
      ['empty triv] ; italics means something else?
      [x #:when (name? x) ; name or primf
         (lookup-env env x)]
      #;
      [x #:when (or (name? x) (safe-binop? x) (unop? x))
         (cond
           [(and (or (safe-binop? x) (unop? x)) (not (assoc x env))) x]
           [else (lookup-env env x)])]
      ;; Wildcard collapse case used because all terminal triv values do not
      ;; require any further processing or transformation, allowing them to be
      ;; returned as-is
      ;; primf case
      [_ triv]))

  (match p
    [`(module ,funcs ... ,value)
     (define defined-funs (initialize-env funcs))
     (define-values (updated-funcs updated-env) (process-functions funcs defined-funs))
     `(module ,@(reverse updated-funcs) ,(uniquify-value value updated-env))]))

(module+ test
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
                     (define L.+.1 (lambda (x.7 y.8) (call L.+.1 x.7 y.8)))
                   (call L.+.1 1 2)))
  (check-equal? (uniquify '(module (let ([x 1])
                                     (let ([x #t])
                                       (let ([x #f])
                                         (let ([x empty])
                                           (let ([x (void)])
                                             (let ([x (error 255)])
                                               (let ([x #\space])
                                                 x)))))))))
                '(module
                     (let ((x.9 1))
                       (let ((x.10 #t))
                         (let ((x.11 #f))
                           (let ((x.12 empty))
                             (let ((x.13 (void)))
                               (let ((x.14 (error 255))) (let ((x.15 #\space)) x.15)))))))))
  (check-equal? (uniquify '(module (let ([x 5]) (let ([y x]) (call + y 1)))))
                '(module (let ((x.16 5)) (let ((y.17 x.16)) (call + y.17 1)))))
  (check-equal? (uniquify '(module (let ([x 5]) (let ([y x]) (let ([z y]) (call + z 2))))))
                '(module
                     (let ((x.18 5)) (let ((y.19 x.18)) (let ((z.20 y.19)) (call + z.20 2))))))
  (check-equal? (uniquify '(module (let ([x 5]) (let ([x 6]) x))))
                '(module (let ((x.21 5)) (let ((x.22 6)) x.22))))
  (check-equal? (uniquify '(module (let ([x 5]) (let ([x (call + x 1)]) (let ([x (call + x 2)]) x)))))
                '(module
                     (let ((x.23 5))
                       (let ((x.24 (call + x.23 1))) (let ((x.25 (call + x.24 2))) x.25)))))
  (check-equal? (uniquify '(module (let ([x 5]) 42)))
                '(module (let ((x.26 5)) 42)))
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
                     (define L.f.2 (lambda (x.41 y.42) (call + x.41 y.42)))
                   (define L.x.3 (lambda (z.43) (let ((x.44 1)) (call + x.44 z.43))))
                   (if #t (call L.f.2 1 2) (call L.x.3 1))))
  (check-equal? (uniquify '(module
                               (define odd? (lambda (x) (if (call eq? x 0) 0 (let ((y (call + x -1))) (call even? y)))))
                             (define even? (lambda (x) (if (call eq? x 0) 1 (let ((y (call + x -1))) (call odd? y)))))
                             (call even? 5)))
                '(module
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
  (check-equal? (uniquify '(module
                               (define f (lambda (x y) (call + x y)))
                             (define x (lambda (z) (let ([x 1])
                                                     (call + x z))))
                             (let ([x (call f 1 2)])
                               (let ([y (call x x)])
                                 y))))
                '(module
                     (define L.f.6 (lambda (x.49 y.50) (call + x.49 y.50)))
                   (define L.x.7 (lambda (z.51) (let ((x.52 1)) (call + x.52 z.51))))
                   (let ((x.53 (call L.f.6 1 2))) (let ((y.54 (call x.53 x.53))) y.54))))
  (check-equal? (uniquify '(module
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
  (check-equal? (uniquify '(module (define add (lambda (a b c d e f g h) (call + a (call + b (call + c (call + d (call + e (call + f (call + g h)))))))))
                             (define add-and-multiply (lambda (a b c d e f g h i)
                                                        (let ([sum (call add a b c d e f g h)])
                                                          (call * sum i))))
                             (call add-and-multiply 1 2 3 4 5 6 7 8 2)))
                '(module
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
                   (call L.add-and-multiply.11 1 2 3 4 5 6 7 8 2))))
