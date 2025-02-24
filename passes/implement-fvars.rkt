#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4
  rackunit)

(provide implement-fvars)

;; paren-x64-fvars-v4 -> paren-x64-v4
;; interp. convert fvars into displacement mode operands
(define/contract (implement-fvars p)
  (-> paren-x64-fvars-v4? paren-x64-v4?)

  ;; fvar -> addr
  ;; convert fvar into displacement mode operand
  (define (fvar->addr fvar)
    `(,(current-frame-base-pointer-register) - ,(* (fvar->index fvar)
                                                   (current-word-size-bytes))))

  ;; paren-x64-fvars-v4.s -> paren-x64-v4.s
  (define (implement-fvars-s s)
    (match s
      [`(set! ,fvar ,v)
       #:when (fvar? fvar)
       `(set! ,(fvar->addr fvar) ,v)]
      [`(set! ,x ,fvar)
       #:when (fvar? fvar)
       `(set! ,x ,(fvar->addr fvar))]
      [`(set! ,x (,binop ,x ,fvar))
       #:when (fvar? fvar)
       `(set! ,x (,binop ,x ,(fvar->addr fvar)))]
      [`(with-label ,label ,s) 
       `(with-label ,label ,(implement-fvars-s s))]
      [`(jump ,trg) `(jump ,trg)]
      [`(compare ,reg ,op) `(compare ,reg ,op)]
      [`(jump-if ,relop ,label) `(jump-if ,relop ,label)]
      ;; Using a wildcard collapse case as it captures all other well-formed
      ;; expressions without transformation
      [_ s]))

  (match p
    [`(begin ,ss ...)
     (define compiled-s (for/list ([s ss]) (implement-fvars-s s)))
     `(begin ,@compiled-s)]))

(module+ test
  (check-equal? (implement-fvars '(begin
                                    (set! rsi L.label.1)
                                    (with-label L.label.1 (set! rbx 18))
                                    (set! rax rbx)
                                    (jump done)))
                '(begin
                   (set! rsi L.label.1)
                   (with-label L.label.1 (set! rbx 18))
                   (set! rax rbx)
                   (jump done)))
  (check-equal? (implement-fvars '(begin
                                    (with-label L.tmp.52 (set! fv0 5))
                                    (set! rax fv0)
                                    (jump done)))
                '(begin
                   (with-label L.tmp.52 (set! (rbp - 0) 5))
                   (set! rax (rbp - 0))
                   (jump done)))
  (check-equal? (implement-fvars '(begin
                                    (with-label L.tmp.47 (set! fv1 12))
                                    (set! fv2 12)
                                    (set! fv3 0)
                                    (set! rax fv3)
                                    (jump done)))
                '(begin
                   (with-label L.tmp.47 (set! (rbp - 8) 12))
                   (set! (rbp - 16) 12)
                   (set! (rbp - 24) 0)
                   (set! rax (rbp - 24))
                   (jump done)))
  (check-equal? (implement-fvars '(begin
                                    (with-label L.tmp.33 (set! fv16 0))
                                    (set! fv0 0)
                                    (set! r10 fv16)
                                    (set! fv8 r10)
                                    (set! r10 fv0)
                                    (set! r11 fv16)
                                    (set! r10 (+ r10 r11))
                                    (set! fv0 r10)
                                    (set! r10 fv0)
                                    (set! r11 fv8)
                                    (set! r10 (+ r10 r11))
                                    (set! fv0 r10)
                                    (set! rax fv0)
                                    (jump done)))
                '(begin
                   (with-label L.tmp.33 (set! (rbp - 128) 0))
                   (set! (rbp - 0) 0)
                   (set! r10 (rbp - 128))
                   (set! (rbp - 64) r10)
                   (set! r10 (rbp - 0))
                   (set! r11 (rbp - 128))
                   (set! r10 (+ r10 r11))
                   (set! (rbp - 0) r10)
                   (set! r10 (rbp - 0))
                   (set! r11 (rbp - 64))
                   (set! r10 (+ r10 r11))
                   (set! (rbp - 0) r10)
                   (set! rax (rbp - 0))
                   (jump done)))
  (check-equal? (implement-fvars '(begin (set! fv0 0)))
                '(begin (set! (rbp - 0) 0)))
  (check-equal? (implement-fvars `(begin (set! fv0 0) (set! fv1 ,(max-int 32))))
                `(begin (set! (rbp - 0) 0) (set! (rbp - 8) ,(max-int 32))))
  (check-equal? (implement-fvars '(begin (set! fv0 5)
                                         (set! rax fv0)))
                '(begin (set! (rbp - 0) 5)
                        (set! rax (rbp - 0)))))