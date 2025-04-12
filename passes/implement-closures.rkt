#lang racket

(require
  cpsc411/langs/v9)

(provide implement-closures)

;; hoisted-lang-v9 -> proc-exposed-lang-v9
;; compiles p to Proc-exposed-lang v9 by implementing closures in terms of the
;; procedure data structure
(define/contract (implement-closures p)
  (-> hoisted-lang-v9? proc-exposed-lang-v9?)

  ;; func is (define label (lambda (alocs ...) value))
  ;; interp. a function definition

  ;; func -> func
  ;; interp. transforms a lambda function from hoisted to proc-exposed form
  (define (implement-closures-func f)
    (match f
      [`(define ,label (lambda (,alocs ...) ,body))
       `(define ,label (lambda (,@alocs) ,(implement-closures-value body)))]))

  ;; hoisted-lang-v9.value -> proc-exposed-lang-v9.value
  ;; interp. recursively rewrites closure constructs in hoisted-lang values into procedure objects
  (define (implement-closures-value value)
    (match value
      [`(cletrec ([,alocs (make-closure ,labels ,arities ,vss ...)] ...) ,body)
       (define-values (bindings procs)
         (for/fold ([bindings '()]
                    [procs '()])
                   ([aloc alocs]
                    [label labels]
                    [arity arities]
                    [vs vss])
           (define vs^ (map implement-closures-value vs))
           (define new-binding `(,aloc (make-procedure ,label ,arity ,(length vs^))))
           (define unsafe-procs
             (for/list ([v vs^] [i (in-naturals)])
               `(unsafe-procedure-set! ,aloc ,i ,v)))
           (values (cons new-binding bindings)
                   (append procs unsafe-procs))))
       `(let ,(reverse bindings)
          (begin ,@procs ,(implement-closures-value body)))]
      [`(begin ,effects ... ,value)
       `(begin ,@(map implement-closures-effect effects) ,(implement-closures-value value))]
      [`(if ,v1 ,v2 ,v3)
       `(if ,(implement-closures-value v1)
            ,(implement-closures-value v2)
            ,(implement-closures-value v3))]
      [`(let ([,alocs ,vs] ...) ,body)
       (define bindings
         (map (lambda (aloc v)
                (list aloc (implement-closures-value v)))
              alocs vs))
       `(let (,@bindings) ,(implement-closures-value body))]
      [`(call ,vs ...)
       `(call ,@(map implement-closures-value vs))]
      [`(closure-call ,c ,vs ...)
       `(call (unsafe-procedure-label ,(implement-closures-value c)) ,@(map implement-closures-value vs))]
      [`(closure-ref ,v1 ,v2)
       `(unsafe-procedure-ref ,(implement-closures-value v1) ,(implement-closures-value v2))]
      [`(,primops ,vs ...)
       `(,primops ,@(map implement-closures-value vs))]
      [triv triv]))

  ;; hoisted-lang-v9.effect -> proc-exposed-lang-v9.effect
  ;; interp. rewrites closure-related constructs inside effects
  (define (implement-closures-effect effect)
    (match effect
      [`(,primops ,vs ...)
       `(,primops ,@(map implement-closures-value vs))]
      [`(begin ,effects ...)
       `(begin ,@(map implement-closures-effect effects))]))
  
  (match p
    [`(module ,funcs ... ,value)
     `(module ,@(map implement-closures-func funcs) ,(implement-closures-value value))]))

