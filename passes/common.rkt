#lang racket

(require
  cpsc411/compiler-lib
  rackunit)

(provide empty-env
         lookup-env
         extend-env*
         extend-env
         binop?
         addr?)

;; Env : (Env-of X) is (List-of (list Symbol X))

;; empty-env : Env
;; empty environment for holding variable mappings
(define empty-env '())

;; (Env-of X) Symbol -> X
;; Produce the binding for the given symbol
;; Effect: Signals an error if no binding is found
(define (lookup-env env x)
  (cond [(empty? env) (error 'lookup-env "unbound identifier: ~a" x)]
        [else (if (symbol=? x (car (first env)))
                  (cadr (first env))
                  (lookup-env (rest env) x))]))

;; (Env-of X) Symbol X -> (or/c (Env-of X) #f)
;; Produce the binding for the given symbol, or #f if no binding is found
(define (lookup-env2 env x)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (lookup-env env x)))

;; (Env-of X) (List-of Symbol) (List-of X) -> (Env-of X)
;; Produce an environment that binds distinct symbols in x* to values in v*.
;; ASSUME: (= (length x*) (length v*))
;; ASSUME: (unique? x*)
(define (extend-env* env x* v*)
  (append (map list x* v*) env))

;; (Env-of X) Symbol X -> (Env-of X)
;; extend the given environment to bind x to v
(define (extend-env env x v)
  (extend-env* env (list x) (list v)))

;; any -> boolean
;; produces true if op is a valid binop, which is either * or +
(define (binop? op)
  (and (member op '(* +)) #t))

;; any -> boolean
;; produces true if x is a valid address of the form (fbp - dispoffset)
;; where fbp is the frame base pointer register and dispoffset is a displacement
;; mode offset for x86-64
(define (addr? x)
  (match x
    [`(,fbp - ,dispoffset)
     (and (frame-base-pointer-register? fbp)
          (dispoffset? dispoffset))]
    [_ #f]))

(module+ test
  (test-case "extend-env*"
             (check-equal? (extend-env*
                            (extend-env* empty-env '(x y z) '(5 6 7))
                            '(a x c) '(5 6 7))
                           '((a 5)
                             (x 6)
                             (c 7)
                             (x 5)
                             (y 6)
                             (z 7))))

  (test-case "lookup-env"
             (check-equal? (lookup-env (extend-env*
                                        (extend-env* empty-env '(x y z) '(5 6 7))
                                        '(a x c) '(5 6 7))
                                       'z)
                           7))
  
  (test-case "binop?"
             (check-true (binop? '*))
             (check-true (binop? '+))
             (check-false (binop? '-))
             (check-false (binop? ""))
             (check-false (binop? "*"))
             (check-false (binop? "+"))
             (check-false (binop? 1)))

  (test-case "addr?"
             (check-true (addr? `(rbp - 8)))
             (check-true (addr? `(rbp - 0)))
             (check-false (addr? `(rbp - 2147483647)))
             (check-false (addr? `(rax - 8))) 
             (check-false (addr? `(foo - 8)))
             (check-false (addr? `(rbp - "8")))
             (check-false (addr? `(rbp - 3.14)))
             (check-false (addr? `(rbp + 8)))
             (check-false (addr? '(rbp 8)))
             (check-false (addr? '(rbp - 8 10)))
             (check-false (addr? 42))))
