#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v3
  rackunit)

(provide uniquify)

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

(test-case
 "uniquify tests"
 (check-equal? (uniquify '(module (+ 2 2)))
               '(module (+ 2 2)))

 (check-equal? (uniquify '(module (let ([x 5]) x)))
               '(module (let ((x.1 5)) x.1)))

 (check-equal? (uniquify '(module (let ([x 5]) (+ x 1))))
               '(module (let ((x.2 5)) (+ x.2 1))))
   
 (check-equal? (uniquify '(module (let ([x 5] [y 10]) (+ x y))))
               '(module (let ((x.3 5) (y.4 10)) (+ x.3 y.4))))

 (check-equal? (uniquify '(module (let ([x 5] [y (+ 3 2)]) (* x y))))
               '(module (let ((x.5 5) (y.6 (+ 3 2))) (* x.5 y.6))))
   
 ;; Nested let bindings
 (check-equal? (uniquify '(module (let ([x 5]) (let ([y x]) (+ y 1)))))
               '(module (let ((x.7 5)) (let ((y.8 x.7)) (+ y.8 1)))))

 ;; Multiple nested let expressions
 (check-equal? (uniquify '(module (let ([x 5]) (let ([y x]) (let ([z y]) (+ z 2))))))
               '(module (let ((x.9 5)) (let ((y.10 x.9)) (let ((z.11 y.10)) (+ z.11 2))))))
   
 ;; Variable shadowing (inner `x` should be renamed)
 (check-equal? (uniquify '(module (let ([x 5]) (let ([x 6]) x))))
               '(module (let ((x.12 5)) (let ((x.13 6)) x.13))))

 ;; Multiple shadowing variables
 (check-equal? (uniquify '(module (let ([x 5]) (let ([x (+ x 1)]) (let ([x (+ x 2)]) x)))))
               '(module (let ((x.14 5)) (let ((x.15 (+ x.14 1))) (let ((x.16 (+ x.15 2))) x.16)))))

 ;; Unused variable
 (check-equal? (uniquify '(module (let ([x 5]) 42)))
               '(module (let ((x.17 5)) 42)))

 ;; Unbound variable (should fail)
 (check-exn exn:fail? (λ () (uniquify '(module (let ([x y]) x)))))

 ;; Large numbers and edge values
 (check-equal? (uniquify `(module (let ([x ,(- (expt 2 63))] [y ,(- (expt 2 63) 1)]) (+ x y))))
               `(module (let ((x.18 ,(- (expt 2 63))) (y.19 ,(- (expt 2 63) 1))) (+ x.18 y.19))))
   
 ;; Shadowing with external references
 (check-equal? (uniquify '(module (let ([x 5]) (let ([y x]) (let ([x y]) (+ x y))))))
               '(module (let ((x.20 5)) (let ((y.21 x.20)) (let ((x.22 y.21)) (+ x.22 y.21))))))

 ;; Nested variable dependencies
 (check-equal? (uniquify '(module (let ([a 1]) (let ([b (+ a 1)]) (let ([c (+ b 1)]) (+ c 2))))))
               '(module (let ((a.23 1)) (let ((b.24 (+ a.23 1))) (let ((c.25 (+ b.24 1))) (+ c.25 2))))))

 ;; Reusing names inside different let blocks
 (check-equal? (uniquify '(module (let ([x 1]) (let ([y (+ x 2)]) (let ([x (+ y 3)]) (+ x y))))))
               '(module (let ((x.26 1)) (let ((y.27 (+ x.26 2))) (let ((x.28 (+ y.27 3))) (+ x.28 y.27)))))))