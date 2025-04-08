#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide expose-basic-blocks)

;; nested-asm-lang-v8 -> block-pred-lang-v8
;; compiles p to Block-pred-lang v4 by eliminating all nested expressions by
;; generating fresh basic blocks and jumps
(define/contract (expose-basic-blocks p)
  (-> nested-asm-lang-v8? block-pred-lang-v8?)

  ;; blocks is (Box (List-of block-pred-lang-v8.b))
  ;; stores new block definitions created
  (define blocks (box '()))

  ;; block-pred-lang-v8.b ->
  ;; adds blk to blocks
  (define (add-block blk)
    (set-box! blocks (cons blk (unbox blocks))))

  ;; block-pred-lang-v8.tail (list-of block-pred-lang-v8.effect) -> block-pred-lang-v8.tail
  ;; creates a new tail with the given effects
  (define (make-new-tail tail fx)
    (match tail
      [`(begin ,fx^ ... ,tail^) (make-new-tail tail^ (append fx fx^))]
      ;; Other cases are handled in the same way:
      [_ (if (empty? fx)
             tail
             `(begin ,@fx ,tail))]))

  ;; nested-asm-lang-v8.tail -> block-pred-lang-v8.tail
  ;; interp. converts the tail program into a list of basic blocks
  (define (expose-basic-blocks-tail tail)
    (match tail
      [`(begin ,fx ... ,tail)
       (for/foldr ([bpl-tail (expose-basic-blocks-tail tail)])
         ([e fx])
         (expose-basic-blocks-effect e bpl-tail))]
      [`(if ,pred ,t-tail ,f-tail)
       (let* ([tail1-label (fresh-label)]
              [tail2-label (fresh-label)])
         (add-block `(define ,tail1-label ,(expose-basic-blocks-tail t-tail)))
         (add-block `(define ,tail2-label ,(expose-basic-blocks-tail f-tail)))
         ((expose-basic-blocks-pred pred) tail1-label tail2-label))]
      [`(jump ,trg) `(jump ,trg)]))

  ;; interp. returns the effects and tails of the current block
  ;; nested-asm-lang-v8.effect block-pred-lang-v8.tail -> block-pred-lang-v8.tail
  (define (expose-basic-blocks-effect e bpl-tail)
    (match e
      [`(set! ,loc1 (mref ,loc2 ,index))
       (make-begin (list `(set! ,loc1 (mref ,loc2 ,index))) bpl-tail)]
      [`(set! ,loc (,binop ,loc ,triv))
       (make-begin (list `(set! ,loc (,binop ,loc ,triv))) bpl-tail)]
      [`(set! ,loc ,triv)
       (make-begin (list `(set! ,loc ,triv)) bpl-tail)]
      [`(mset! ,loc ,index ,triv)
       (make-begin (list `(mset! ,loc ,index ,triv)) bpl-tail)]
      [`(begin ,fx ...)
       (for/foldr ([bpl-tail bpl-tail])
         ([e fx])
         (expose-basic-blocks-effect e bpl-tail))]
      [`(if ,pred ,t-e ,f-e)
       (define merge-label (fresh-label))
       (define true-label (fresh-label))
       (define false-label (fresh-label))
       (define true-tail
         (expose-basic-blocks-effect t-e
                                     `(jump ,merge-label)))
       (define false-tail
         (expose-basic-blocks-effect f-e
                                     `(jump ,merge-label)))
       (add-block `(define ,true-label ,true-tail))
       (add-block `(define ,false-label ,false-tail))

       (define merge-tail bpl-tail)
       (add-block `(define ,merge-label ,merge-tail))

       (define pred-creator (expose-basic-blocks-pred pred))
       (pred-creator true-label false-label)]
      [`(return-point ,label ,tail)
       (add-block `(define ,label ,bpl-tail))
       tail]))

  ;; nested-asm-lang-v8.pred
  ;; -> (block-pred-lang-v8.trg block-pred-lang-v8.trg -> block-pred-lang-v8.tail)
  (define (expose-basic-blocks-pred p)
    (match p
      ['(true)
       (lambda (t f) `(if ,p (jump ,t) (jump ,f)))]
      ['(false)
       (lambda (t f) `(if ,p (jump ,t) (jump ,f)))]
      [`(not ,pred)
       (lambda (t f)
         ((expose-basic-blocks-pred pred) f t))]
      [`(begin ,e ... ,pred)
       (define pred-fn (expose-basic-blocks-pred pred))
       (lambda (t f)
         (for/foldr ([tail-acc (pred-fn t f)])
           ([e e])
           (expose-basic-blocks-effect e tail-acc)))]
      [`(if ,pred1 ,pred2 ,pred3)
       (lambda (t f)
         (let* ([label2 (fresh-label)]
                [label3 (fresh-label)])
           (add-block `(define ,label3 ,((expose-basic-blocks-pred pred3) t f)))
           (add-block `(define ,label2 ,((expose-basic-blocks-pred pred2) t f)))
           ((expose-basic-blocks-pred pred1) label2 label3)))]
      [`(,relop ,loc ,triv)
       (lambda (t f) `(if (,relop ,loc ,triv) (jump ,t) (jump ,f)))]))

  (match p
    [`(module ,funs ... ,tail)
     (for ([fun funs])
       (match fun
         [`(define ,label ,tail^)
          (add-block `(define ,label ,(expose-basic-blocks-tail tail^)))]))
     (add-block `(define ,(fresh-label) ,(expose-basic-blocks-tail tail)))
     `(module ,@(unbox blocks))]))


