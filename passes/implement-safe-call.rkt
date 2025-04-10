#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v9)

(provide implement-safe-call)

;; exprs-unsafe-data-lang-v9 -> exprs-unsafe-lang-v9
;; compiles p to Exprs-unsafe-lang v9 by implementing call as an unsafe
;; procedure call with dynamic checks
(define/contract (implement-safe-call p)
  (-> exprs-unsafe-data-lang-v9? exprs-unsafe-lang-v9?)

  ;; func is `(define ,label (lambda (,alocs ...) ,value))
  ;; interp. a function definition

  ;; bad-arity-error is an error raised when procedure call has wrong number of
  ;; arguments
  (define bad-arity-error `(error 42))

  ;; bad-proc-error is an error raised when a non-procedure value is called
  (define bad-proc-error `(error 43))

  ;; func-map is (Map-of Label Integer)
  ;; interp. keeps track of function arities by label
  (define func-map (make-hash))

  ;; exprs-unsafe-data-lang-v9.triv (List-of exprs-unsafe-data-lang-v9.value) -> exprs-unsafe-lang-v9.value
  ;; interp. transforms a call expression with dynamic checks for arity and
  ;; procedure validity
  (define (expand-safe-call target args)
    (define arg-count (length args))
    (match target
      ;; Case 1: direct lambda expression
      [`(lambda (,params ...) ,body)
       (define lambda^ `(lambda ,params ,(implement-safe-call-value body)))
       (if (= (length params) arg-count)
           `(unsafe-procedure-call ,lambda^ ,@args)
           bad-arity-error)]
      ;; Case 2: direct label reference with known arity
      [label #:when (hash-has-key? func-map label)
             (define arity (hash-ref func-map label))
             (if (= arity arg-count)
                 `(unsafe-procedure-call ,label ,@args)
                 bad-arity-error)]
      ;; Case 3: any other expression
      [_ (define (procedure-check tmp)
           `(if (procedure? ,tmp)
                (if (eq? (unsafe-procedure-arity ,tmp) ,(length args))
                    (unsafe-procedure-call ,tmp ,@args)
                    ,bad-arity-error)
                ,bad-proc-error))
         (define fresh-var (fresh 'call-tmp))
         (if (aloc? target)
             (procedure-check target)
             `(let ((,fresh-var ,target))
                ,(procedure-check fresh-var)))]))

  ;; func -> void
  ;; interp. saves a function's arity in the global function map
  ;; EFFECTS: mutates func-map by inserting (label -> arity) entry
  (define (save-function-arity func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,value))
       (hash-set! func-map label (length alocs))]))

  ;; func -> func
  ;; interp. recursively implements safe call transformation in a function
  ;; definition
  (define (implement-safe-call-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,value))
       `(define ,label (lambda (,@alocs) ,(implement-safe-call-value value)))]))

  ;; exprs-unsafe-data-lang-v9.value -> exprs-unsafe-lang-v9.value
  ;; interp. recursively transforms value-level expressions to implement safe
  ;; calls
  (define (implement-safe-call-value value)
    (match value
      [`(call ,p ,args ...)
       (expand-safe-call (implement-safe-call-value p) (map implement-safe-call-value args))]
      [`(let ([,alocs ,vs] ...) ,v)
       (define bindings
         (map (lambda (aloc val)
                (list aloc (implement-safe-call-value val)))
              alocs vs))
       `(let ,bindings ,(implement-safe-call-value v))]
      [`(if ,v1 ,v2 ,v3)
       `(if ,(implement-safe-call-value v1)
            ,(implement-safe-call-value v2)
            ,(implement-safe-call-value v3))]
      [`(begin ,effects ... ,value)
       `(begin ,@(map implement-safe-call-effect effects) ,(implement-safe-call-value value))]
      [triv (implement-safe-call-triv triv)]))

  ;; exprs-unsafe-data-lang-v9.effect -> exprs-unsafe-lang-v9.effect
  ;; interp. recursively transforms effects to implement safe calls
  (define (implement-safe-call-effect effect)
    (match effect
      [`(begin ,effs ...)
       `(begin ,@(map implement-safe-call-effect effs))]
      [`(,primop ,vs ...)
       `(,primop ,@(map implement-safe-call-value vs))]))

  ;; exprs-unsafe-data-lang-v9.triv -> exprs-unsafe-lang-v9.triv
  ;; interp. transforms trivial values (e.g. inline lambdas) recursively
  (define (implement-safe-call-triv triv)
    (match triv
      [`(lambda (,alocs ...) ,value)
       `(lambda ,alocs ,(implement-safe-call-value value))]
      ;; Wildcard collapse case used because they are terminal cases with no
      ;; transformation
      [_ triv]))

  (match p
    [`(module ,funcs ... ,value)
     (for-each save-function-arity funcs)
     `(module ,@(map implement-safe-call-func funcs) ,(implement-safe-call-value value))]))

