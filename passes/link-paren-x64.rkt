#lang racket

(require "common.rkt")

(require
  cpsc411/compiler-lib
  cpsc411/langs/v4)

(provide link-paren-x64)

;; Exercise 2
;; paren-x64-v4 -> paren-x64-rt-v4
;; compiles p to Paren-x64-rt v4 by resolving all labels to their position in
;; the instruction sequence
(define/contract (link-paren-x64 p)
  (-> paren-x64-v4? paren-x64-rt-v4?)

  ;; label-table is (Map-of label pc-addr)
  ;; keeps track of the index for each label
  (define label-table (make-hash))

  ;; paren-x64-v4.s ->
  ;; EFFECTS: assign labels with an idx and add it to label-table
  (define (link-label-to-pc-addr s idx)
    (match s
      [`(with-label ,label ,s)
       (hash-set! label-table label idx)
       (void)]
      ;; Using wildcard collapse case because all the other cases do not create
      ;; a new label
      [_ (void)]))

  ;; paren-x64-v4.s -> paren-x64-rt-v4.s
  (define (link-paren-x64-s s)
    (match s
      [`(with-label ,label ,exp)
       (link-paren-x64-s exp)]
      [`(jump ,trg)
       `(jump ,(link-paren-x64-trg trg))]
      [`(jump-if ,relop ,label)
       `(jump-if ,relop ,(hash-ref label-table label))]
      [`(set! ,addr ,trg)
       #:when (and (addr? addr) (or (register? trg) (label? trg)))
       `(set! ,addr ,(link-paren-x64-trg trg))]
      [_ s]))

  (define (link-paren-x64-trg trg)
    (match trg
      [reg #:when (register? reg) reg]
      [label #:when (label? label) (hash-ref label-table label)]))

  (match p
    [`(begin ,s ...)
     (for ([exp s]
           [idx (in-naturals)])
       (link-label-to-pc-addr exp idx))
     `(begin ,@(map link-paren-x64-s s))]))

