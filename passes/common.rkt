#lang racket

(require
  cpsc411/compiler-lib
  rackunit)

(provide empty-env
         lookup-env
         extend-env*
         extend-env
         binop?
         unsafe-binop?
         safe-binop?
         unop?
         relop?
         addr?
         rloc?)              

;; ================================================
;; Environment
;; ================================================

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

;; ================================================
;; Helper Functions
;; ================================================

;; any -> boolean
;; produces true if op is a valid binop
(define (binop? op)
  (and (member op '(* + - < eq? <= > >=)) #t))

;; any -> boolean
;; produces true if op is a valid unsafe binop
(define (unsafe-binop? op)
  (and (member op '(unsafe-fx* unsafe-fx+ unsafe-fx- eq? unsafe-fx< unsafe-fx<= unsafe-fx> unsafe-fx>=)) #t))

;; any -> boolean
;; produces true if op is a valid safe binop
(define (safe-binop? op)
  (and (member op '(* + - bitwise-and bitwise-ior bitwise-xor arithmetic-shift-right)) #t))

;; any -> boolean
;; produces true if op is a valid unop
(define (unop? op)
  (and (member op '(fixnum? boolean? empty? void? ascii-char? error? not)) #t))

;; any -> boolean
;; produces true if op is a valid relop
(define (relop? op)
  (and (member op '(< <= = >= > !=)) #t))

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

;; any -> boolean
;; produces true if rloc is a valid rloc, which is either a register or fvar
(define (rloc? rloc)
  (or (register? rloc) (fvar? rloc)))

;; ================================================
;; Tests
;; ================================================

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
             (check-true (binop? '-))
             (check-true (binop? '>))
             (check-true (binop? '>=))
             (check-true (binop? 'eq?))
             (check-true (binop? '<))
             (check-true (binop? '<=))
             (check-false (binop? '^))
             (check-false (binop? ""))
             (check-false (binop? "*"))
             (check-false (binop? "+"))
             (check-false (binop? 1)))

  (test-case "unsafe-binop?"
             (check-true (unsafe-binop? 'unsafe-fx*))
             (check-true (unsafe-binop? 'unsafe-fx+))
             (check-true (unsafe-binop? 'unsafe-fx-))
             (check-true (unsafe-binop? 'eq?))
             (check-true (unsafe-binop? 'unsafe-fx<))
             (check-true (unsafe-binop? 'unsafe-fx<=))
             (check-true (unsafe-binop? 'unsafe-fx>))
             (check-true (unsafe-binop? 'unsafe-fx>=))
             (check-false (unsafe-binop? '*))
             (check-false (unsafe-binop? '+))
             (check-false (unsafe-binop? '-))
             (check-false (unsafe-binop? 'bitwise-and))
             (check-false (unsafe-binop? "unsafe-fx*"))
             (check-false (unsafe-binop? 1))
             (check-false (unsafe-binop? 'random-symbol)))

  (test-case "safe-binop?"
             (check-true (safe-binop? '*))
             (check-true (safe-binop? '+))
             (check-true (safe-binop? '-))
             (check-true (safe-binop? 'bitwise-and))
             (check-true (safe-binop? 'bitwise-ior))
             (check-true (safe-binop? 'bitwise-xor))
             (check-true (safe-binop? 'arithmetic-shift-right))
             (check-false (safe-binop? 'eq?))
             (check-false (safe-binop? 'unsafe-fx*))
             (check-false (safe-binop? 'unsafe-fx+))
             (check-false (safe-binop? 'unsafe-fx-))
             (check-false (safe-binop? "bitwise-and"))
             (check-false (safe-binop? 2))
             (check-false (safe-binop? 'random-op)))

  (test-case "unop?"
             (check-true (unop? 'fixnum?))
             (check-true (unop? 'boolean?))
             (check-true (unop? 'empty?))
             (check-true (unop? 'void?))
             (check-true (unop? 'ascii-char?))
             (check-true (unop? 'error?))
             (check-true (unop? 'not))
             (check-false (unop? 'eq?))
             (check-false (unop? '*))
             (check-false (unop? '+))
             (check-false (unop? 'unsafe-fx*))
             (check-false (unop? "boolean?"))
             (check-false (unop? 3))
             (check-false (unop? 'random-unary)))

  (test-case "relop?"
             (check-true (relop? '<=))
             (check-true (relop? '<))
             (check-true (relop? '>))
             (check-true (relop? '>=))
             (check-true (relop? '=))
             (check-true (relop? '!=))
             (check-false (relop? '==))
             (check-false (relop? '!))
             (check-false (relop? "<="))
             (check-false (relop? #t))
             (check-false (relop? 1)))  

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
             (check-false (addr? 42)))

  (test-case "rloc?"
             (check-true (rloc? 'r15))
             (check-true (rloc? 'rax))
             (check-true (rloc? 'rdi))
             (check-true (rloc? 'fv0))
             (check-true (rloc? 'fv100))
             (check-false (rloc? "r15"))
             (check-false (rloc? "15"))
             (check-false (rloc? '15))
             (check-false (rloc? 'fv.1))
             (check-false (rloc? "fv0"))))
