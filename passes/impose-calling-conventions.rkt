#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide impose-calling-conventions)

;; proc-imp-cmf-lang-v8 -> imp-cmf-lang-v8
;; compiles p to Imp-cmf-lang v8 by imposing calling conventions on all calls
;; and procedure definitions
(define/contract (impose-calling-conventions p)
  (-> proc-imp-cmf-lang-v8? imp-cmf-lang-v8?)

  ;; func-lambda is `(define ,label (lambda (,alocs ...) ,entry))
  ;; interp. a function definition that uses lambdas

  ;; func is `(define ,label ,info ,tail)
  ;; interp. a function definition that uses calling conventions for arguments

  ;; (List-of opand) (List-of register) aloc -> (values (List-of effect) (List-of rloc))
  ;; interp. converts a list of arguments from a tail call into a sequence of
  ;; register assignments, following calling conventions
  (define (transform-tail-call ops regs return-aloc)
    (define-values (effects rlocs _)
      (for/fold ([effects '()]
                 [rlocs '()]
                 [fvidx 0])
                ([op (in-list ops)]
                 [maybe-reg (in-list (append regs (make-list (max 0 (- (length ops) (length regs))) #f)))])
        (if maybe-reg
            (values (cons `(set! ,maybe-reg ,op) effects)
                    (cons maybe-reg rlocs)
                    fvidx)
            (let ([fvar (make-fvar fvidx)])
              (values (cons `(set! ,fvar ,op) effects)
                      (cons fvar rlocs)
                      (add1 fvidx))))))
    (values (reverse (cons `(set! ,(current-return-address-register) ,return-aloc) effects))
            (reverse rlocs)))

  ;; (List-of opand) (List-of register) aloc -> (List-of imp-cmf-lang-v8.effect) (List-of imp-cmf-lang-v8.rloc) (List-of aloc)
  ;; interp. converts a list of arguments from a non-tail call into a sequence
  ;; of register assignments, following calling conventions
  (define (transform-non-tail-call ops regs return-label)
    (define-values (effects rlocs nfvars)
      (for/fold ([effects '()]
                 [rlocs '()]
                 [nfvars '()])
                ([op (in-list ops)]
                 [maybe-reg (in-list (append regs (make-list (max 0 (- (length ops) (length regs))) #f)))])
        (if maybe-reg
            (values (cons `(set! ,maybe-reg ,op) effects)
                    (cons maybe-reg rlocs)
                    nfvars)
            (let ([nfvar (fresh 'nfv)])
              (values (cons `(set! ,nfvar ,op) effects)
                      (cons nfvar rlocs)
                      (cons nfvar nfvars))))))
    (values (reverse (cons `(set! ,(current-return-address-register) ,return-label) effects))
            (reverse rlocs)
            (reverse nfvars)))

  ;; (List-of aloc) (List-of register) -> (List-of imp-cmf-lang-v8.effect)
  ;; interp. transforms procedure parameter allocations by assigning them to registers based on calling conventions
  (define (transform-procedure alocs regs)
    (define-values (effects _)
      (for/foldr ([effects '()]
                  [next-fvidx 0])
        ([aloc (in-list alocs)]
         [maybe-reg (in-list (append regs (make-list (max 0 (- (length alocs) (length regs))) #f)))])
        (if maybe-reg
            (values (cons `(set! ,aloc ,maybe-reg) effects) next-fvidx)
            (let ([fvar (make-fvar next-fvidx)])
              (values (cons `(set! ,aloc ,fvar) effects) (add1 next-fvidx))))))
    effects)

  ;; info is imp-cmf-lang-v8.info
  ;; interp. keeps track of new frames for a given entry
  (define info (info-set '() 'new-frames '()))

  ;; imp-cmf-lang-v8.info (List-of aloc) -> imp-cmf-lang-v8.info
  ;; EFFECTS: adds the frame to the end of the new-frames's info
  (define (add-frame! frame)
    (set! info (info-set '() 'new-frames (cons frame (info-ref info 'new-frames)))))

  ;; void -> void
  ;; EFFECTS: resets the new-frames of new-frames of info to an empty list
  (define (reset-info!)
    (set! info (info-set info 'new-frames '())))

  ;; func-lambda -> func
  (define (impose-calling-conventions-func func)
    (match func
      [`(define ,label (lambda (,alocs ...) ,entry))
       (reset-info!)
       (define entry^ (impose-calling-conventions-proc-entry entry alocs))
       `(define ,label ,info ,entry^)]))

  ;; proc-imp-cmf-lang-v8.entry (List-of aloc) -> imp-cmf-lang-v8.tail
  ;; interp. transforms the entry point of a procedure by assigning arguments
  ;; according to the calling convention and setting up the return address
  ;; register
  (define (impose-calling-conventions-proc-entry entry alocs)
    (define return-aloc (fresh 'tmp-ra))
    (define call-setup
      (if (empty? alocs)
          '()
          (transform-procedure alocs (current-parameter-registers))))
    `(begin
       (set! ,return-aloc ,(current-return-address-register))
       (begin
         ,@call-setup
         ,(impose-calling-conventions-tail entry return-aloc))))

  ;; proc-imp-cmf-lang-v8.entry -> imp-cmf-lang-v8.tail
  ;; interp. transforms the module-level entry point by setting up the return
  ;; address register
  (define (impose-calling-conventions-module-entry entry)
    (define return-aloc (fresh 'tmp-ra))
    `(begin
       (set! ,return-aloc ,(current-return-address-register))
       ,(impose-calling-conventions-tail entry return-aloc)))

  ;; proc-imp-cmf-lang-v8.tail aloc -> imp-cmf-lang-v8.tail
  (define (impose-calling-conventions-tail tail return-aloc)
    (match tail
      [`(begin ,es ... ,t)
       `(begin ,@(map impose-calling-conventions-effect es) ,(impose-calling-conventions-tail t return-aloc))]
      [`(if ,pred ,t1 ,t2)
       `(if ,(impose-calling-conventions-pred pred)
            ,(impose-calling-conventions-tail t1 return-aloc)
            ,(impose-calling-conventions-tail t2 return-aloc))]
      [`(call ,triv ,ops ...)
       (define-values (effects used-rlocs) (transform-tail-call ops (current-parameter-registers) return-aloc))
       `(begin
          ,@effects
          (jump ,triv ,(current-frame-base-pointer-register) ,(current-return-address-register) ,@used-rlocs))]
      [value
       (define-values (effect^ value^) (impose-calling-conventions-value value))
       (if (empty? effect^)
           `(begin
              (set! ,(current-return-value-register) ,value^)
              (jump ,return-aloc ,(current-frame-base-pointer-register) ,(current-return-value-register)))
           effect^)]))

  ;; proc-imp-cmf-lang-v8.effect -> imp-cmf-lang-v8.effect
  (define (impose-calling-conventions-effect effect)
    (match effect
      [`(set! ,aloc ,value)
       (define-values (effect^ value^) (impose-calling-conventions-value value))
       (if (empty? effect^)
           `(set! ,aloc ,value^)
           `(begin
              (return-point ,value^ ,effect^)
              (set! ,aloc ,(current-return-value-register))))]
      [`(mset! ,aloc ,opand ,triv) effect]
      [`(begin ,es ...)
       `(begin ,@(map impose-calling-conventions-effect es))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(impose-calling-conventions-pred pred)
            ,(impose-calling-conventions-effect e1)
            ,(impose-calling-conventions-effect e2))]))

  ;; proc-imp-cmf-lang-v8.pred -> imp-cmf-lang-v8.pred
  (define (impose-calling-conventions-pred pred)
    (match pred
      ['(true) pred]
      ['(false) pred]
      [`(not ,p)
       `(not ,(impose-calling-conventions-pred p))]
      [`(begin ,es ... ,p)
       `(begin ,@(map impose-calling-conventions-effect es) ,(impose-calling-conventions-pred p))]
      [`(if ,p1 ,p2 ,p3)
       `(if ,(impose-calling-conventions-pred p1)
            ,(impose-calling-conventions-pred p2)
            ,(impose-calling-conventions-pred p3))]
      [`(,relop ,op1 ,op2) pred]))

  ;; proc-imp-cmf-lang-v8.value -> imp-cmf-lang-v8.effect imp-cmf-lang-v8.value
  (define (impose-calling-conventions-value value)
    (match value
      [`(call ,triv ,ops ...)
       (define return-label (fresh-label 'rp))
       (define-values (effects used-rlocs nfvar-list) (transform-non-tail-call ops (current-parameter-registers) return-label))
       (add-frame! nfvar-list)
       (define effect^ `(begin
                          ,@effects
                          (jump ,triv ,(current-frame-base-pointer-register) ,(current-return-address-register) ,@used-rlocs)))
       (values effect^ return-label)]
      ;; Using wildcard collapse case because in all other cases, the
      ;; expression is already in imp-cmf-lang-v8.value form
      [_ (values '() value)]))

  (match p
    [`(module ,funcs ... ,entry)
     (define funcs^ (map impose-calling-conventions-func funcs))
     (reset-info!)
     (define entry^ (impose-calling-conventions-module-entry entry))
     `(module ,info ,@funcs^ ,entry^)]))

