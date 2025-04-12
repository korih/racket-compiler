#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8
  cpsc411/langs/v9)

(provide specify-representation)

;; proc-exposed-lang-v9 -> exprs-bits-lang-v8
;; compiles p to Exprs-bit-lang v8 by compiling immediate data and primitive
;; operations into their implementations as ptrs and primitive bitwise
;; operations on ptrs
(define/contract (specify-representation p)
  (-> proc-exposed-lang-v9? exprs-bits-lang-v8?)

  ;; func is `(define ,label (lambda (,alocs ...) ,value))
  ;; interp. a function definition

  ;; relop is one-of:
  ;; - '<
  ;; - '<=
  ;; - '>
  ;; - '>=
  ;; - '=
  ;; interp. a relational operator used for comparing two values

  ;; relop proc-exposed-lang-v9.value proc-exposed-lang-v9.value -> exprs-bits-lang-v8.value
  ;; interp. generates a boolean pointer from a relational operator comparison
  (define (safe-relop-check relop op1 op2)
    `(if (,relop ,(specify-representation-value op1) ,(specify-representation-value op2))
         ,(current-true-ptr)
         ,(current-false-ptr)))

  ;; Integer Integer proc-exposed-lang-v9.value -> exprs-bits-lang-v8.value
  ;; interp. checks whether a value's tag matches the expected tag
  (define (tag-check mask tag value)
    `(if (= (bitwise-and ,(specify-representation-value value) ,mask)
            ,tag)
         ,(current-true-ptr)
         ,(current-false-ptr)))

  ;; proc-exposed-lang-v9.value Integer Integer -> exprs-bits-lang-v8.value
  ;; interp. computes the byte offset for an indexed memory access 
  (define (compute-offset index displacement tag)
    (cond
      [(int61? index) (+ (* index (current-word-size-bytes)) (- displacement tag))]
      [(not (or (label? index) (aloc? index)))
       (+ (* (arithmetic-shift (specify-representation-value index) (- (current-fixnum-shift)))
             (current-word-size-bytes))
          (- displacement tag))]
      [else `(+ (* (arithmetic-shift-right ,(specify-representation-value index) ,(current-fixnum-shift))
                   ,(current-word-size-bytes))
                ,(- displacement tag))]))

  ;; Integer proc-exposed-lang-v9.value Integer Integer -> exprs-bits-lang-v8.value
  ;; interp. generates an mref expression with a computed offset
  (define (generate-mref base index displacement tag)
    `(mref ,(specify-representation-value base)
           ,(compute-offset index displacement tag)))

  ;; Integer proc-exposed-lang-v9.value proc-exposed-lang-v9.value Integer Integer -> exprs-bits-lang-v8.value
  ;; interp. generates an mset! expression with a computed offset and value
  (define (generate-mset! base index val displacement tag)
    `(mset! ,(specify-representation-value base)
            ,(compute-offset index displacement tag)
            ,(specify-representation-value val)))

  ;; Integer proc-exposed-lang-v9.value (List-of (list Integer exprs-bits-lang-v8.value)) -> exprs-bits-lang-v8.value
  ;; interp. allocates a memory region with a tag and sets metadata fields
  (define (allocate-tagged-struct tag alloc-size metadata)
    (define tmp (fresh 'tmp))
    `(let ([,tmp (+ (alloc ,alloc-size) ,tag)])
       (begin
         ,@(for/list ([field metadata])
             `(mset! ,tmp ,(car field) ,(cdr field)))
         ,tmp)))

  ;; proc-exposed-lang-v9.value Integer Integer -> exprs-bits-lang-v8.value
  ;; interp. computes the memory allocation size for a vector or procedure
  (define (compute-alloc-size raw-size base-displacement shift)
    (cond
      [(int61? raw-size) (+ (* raw-size (current-word-size-bytes)) base-displacement)]
      [(not (or (label? raw-size) (aloc? raw-size)))
       (* (+ 1 (arithmetic-shift (specify-representation-value raw-size) (- shift)))
          (current-word-size-bytes))]
      [else `(* (+ 1 (arithmetic-shift-right ,(specify-representation-value raw-size) ,shift))
                ,(current-word-size-bytes))]))

  ;; func -> func
  ;; interp. transforms a function by specifying representations for its body
  (define (specify-representation-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,value))
       `(define ,label (lambda (,@alocs) ,(specify-representation-value value)))]))

  ;; proc-exposed-lang-v9 -> exprs-bits-lang-v8.value
  ;; interp. recursively transforms values by specifying representation
  (define (specify-representation-value value)
    (match value
      [`(let ([,alocs ,vs] ...) ,v)
       (define bindings
         (map (lambda (aloc val)
                (list aloc (specify-representation-value val)))
              alocs vs))
       `(let (,@bindings) ,(specify-representation-value v))]
      [`(call ,vs ...)
       `(call ,@(map specify-representation-value vs))]
      [`(if ,v1 ,v2 ,v3)
       `(if (!= ,(specify-representation-value v1) ,(current-false-ptr))
            ,(specify-representation-value v2)
            ,(specify-representation-value v3))]
      [`(begin ,es ... ,v)
       `(begin ,@(map specify-representation-effect es) ,(specify-representation-value v))]
      [`(,primop ,vs ...)
       #:when (unsafe-primop? primop)
       ((specify-representation-primop primop) vs)]
      [triv (specify-representation-triv triv)]))

  ;; proc-exposed-lang-v9 -> exprs-bits-lang-v8.effect
  ;; interp. recursively transforms effects by specifying representation
  (define (specify-representation-effect effect)
    (match effect
      [`(,primop ,vs ...)
       #:when (unsafe-primop? primop)
       ((specify-representation-primop primop) vs)]
      [`(begin ,es ...)
       `(begin ,@(map specify-representation-effect es))]))

  ;; proc-exposed-lang-v9 -> exprs-bits-lang-v8.triv
  ;; interp. transforms trivial values like constants or variable references 
  (define (specify-representation-triv triv)
    (match triv
      ['#t (current-true-ptr)]
      ['#f (current-false-ptr)]
      ['empty (current-empty-ptr)]
      ['(void) (current-void-ptr)]
      [`(error ,uint8)
       (bitwise-ior (arithmetic-shift uint8 (current-error-shift))
                    (current-error-tag))]
      [label #:when (label? label) label]
      [aloc #:when (aloc? aloc) aloc]
      [fixnum #:when (int61? fixnum)
              (arithmetic-shift fixnum (current-fixnum-shift))]
      [char #:when (ascii-char-literal? char)
            (bitwise-ior (arithmetic-shift (char->integer char) (current-ascii-char-shift))
                         (current-ascii-char-tag))]))

  ;; proc-exposed-lang-v9 -> ((List-of exprs-bits-lang-v8.value) -> exprs-bits-lang-v8.value)
  ;; interp. returns a function that transforms a primitive operation to its
  ;; bit-level representation
  (define (specify-representation-primop primop)
    (match primop
      ['unsafe-fx*
       (lambda (vs)
         (if (int61? (second vs))
             `(* ,(first vs) ,(specify-representation-value (second vs)))
             `(* ,(specify-representation-value (first vs)) (arithmetic-shift-right ,(specify-representation-value (second vs)) ,(current-fixnum-shift)))))]
      ['unsafe-fx+
       (lambda (vs) `(+ ,(specify-representation-value (first vs))
                        ,(specify-representation-value (second vs))))]
      ['unsafe-fx-
       (lambda (vs) `(- ,(specify-representation-value (first vs))
                        ,(specify-representation-value (second vs))))]
      ['unsafe-fx<  (lambda (vs) (safe-relop-check '<  (first vs) (second vs)))]
      ['unsafe-fx<= (lambda (vs) (safe-relop-check '<= (first vs) (second vs)))]
      ['unsafe-fx>  (lambda (vs) (safe-relop-check '>  (first vs) (second vs)))]
      ['unsafe-fx>= (lambda (vs) (safe-relop-check '>= (first vs) (second vs)))]
      ['eq?         (lambda (vs) (safe-relop-check '=  (first vs) (second vs)))]
      ['fixnum?     (lambda (vs) (tag-check (current-fixnum-mask)     (current-fixnum-tag)     (first vs)))]
      ['boolean?    (lambda (vs) (tag-check (current-boolean-mask)    (current-boolean-tag)    (first vs)))]
      ['empty?      (lambda (vs) (tag-check (current-empty-mask)      (current-empty-tag)      (first vs)))]
      ['void?       (lambda (vs) (tag-check (current-void-mask)       (current-void-tag)       (first vs)))]
      ['ascii-char? (lambda (vs) (tag-check (current-ascii-char-mask) (current-ascii-char-tag) (first vs)))]
      ['error?      (lambda (vs) (tag-check (current-error-mask)      (current-error-tag)      (first vs)))]
      ['pair?       (lambda (vs) (tag-check (current-pair-mask)       (current-pair-tag)       (first vs)))]
      ['vector?     (lambda (vs) (tag-check (current-vector-mask)     (current-vector-tag)     (first vs)))]
      ['procedure?  (lambda (vs) (tag-check (current-procedure-mask)  (current-procedure-tag)  (first vs)))]
      ['not
       (lambda (vs)
         `(if (!= ,(specify-representation-value (first vs)) ,(current-false-ptr))
              ,(current-false-ptr)
              ,(current-true-ptr)))]
      ['cons
       (define tmp (fresh 'tmp))
       (lambda (vs)
         `(let ([,tmp (+ (alloc ,(current-pair-size)) ,(current-pair-tag))])
            (begin
              (mset! ,tmp ,(car-offset) ,(specify-representation-value (first vs)))
              (mset! ,tmp ,(cdr-offset) ,(specify-representation-value (second vs)))
              ,tmp)))]
      ['unsafe-car
       (lambda (vs) `(mref ,(specify-representation-value (first vs)) ,(car-offset)))]
      ['unsafe-cdr
       (lambda (vs) `(mref ,(specify-representation-value (first vs)) ,(cdr-offset)))]
      ['unsafe-make-vector
       (lambda (vs)
         (define raw-size (first vs))
         (define size-expr (specify-representation-value raw-size))
         (define alloc-size
           (compute-alloc-size raw-size
                               (current-vector-base-displacement)
                               (current-vector-shift)))
         (allocate-tagged-struct (current-vector-tag)
                                 alloc-size
                                 (list (cons (- (current-vector-tag)) size-expr))))]
      ['unsafe-vector-length
       (lambda (vs)
         `(mref ,(specify-representation-value (first vs)) ,(- (current-vector-length-displacement) (current-vector-tag))))]
      ['unsafe-vector-set!
       (lambda (vs)
         (generate-mset! (first vs) (second vs) (third vs)
                         (current-vector-base-displacement)
                         (current-vector-tag)))]
      ['unsafe-vector-ref
       (lambda (vs)
         (generate-mref (first vs) (second vs)
                        (current-vector-base-displacement)
                        (current-vector-tag)))]
      ['make-procedure
       (lambda (vs)
         (define label (specify-representation-value (first vs)))
         (define arity (specify-representation-value (second vs)))
         (define raw-size (third vs))
         (define alloc-size
           (compute-alloc-size raw-size
                               (current-procedure-environment-displacement)
                               (current-fixnum-shift)))
         (allocate-tagged-struct (current-procedure-tag)
                                 alloc-size
                                 (list (cons (- (current-procedure-label-displacement)
                                                (current-procedure-tag)) label)
                                       (cons (- (current-procedure-arity-displacement)
                                                (current-procedure-tag)) arity))))]
      ['unsafe-procedure-arity
       (lambda (vs)
         `(mref ,(specify-representation-value (first vs)) ,(- (current-procedure-arity-displacement) (current-procedure-tag))))]
      ['unsafe-procedure-label
       (lambda (vs)
         `(mref ,(specify-representation-value (first vs)) ,(- (current-procedure-label-displacement) (current-procedure-tag))))]
      ['unsafe-procedure-ref
       (lambda (vs)
         (generate-mref (first vs) (second vs)
                        (current-procedure-environment-displacement)
                        (current-procedure-tag)))]
      ['unsafe-procedure-set!
       (lambda (vs)
         (generate-mset! (first vs) (second vs) (third vs)
                         (current-procedure-environment-displacement)
                         (current-procedure-tag)))]))

  (match p
    [`(module ,funcs ... ,value)
     `(module ,@(map specify-representation-func funcs) ,(specify-representation-value value))]))