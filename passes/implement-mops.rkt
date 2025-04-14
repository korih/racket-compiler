#lang racket

(require
  cpsc411/langs/v8)

(provide implement-mops)

;; paren-x64-mops-v8 -> paren-x64-v8
;; compiles p to Paren-x64 v8 by compiling mops to instructions on pointers with
;; index- and displacement-mode operands
(define/contract (implement-mops p)
  (-> paren-x64-mops-v8? paren-x64-v8?)

  ;; paren-x64-mops-v8.s -> paren-x64-v8.s
  ;; interp. compiles a single instruction s by rewriting memory operations 
  ;; into pointer arithmetic using index/displacement mode syntax 
  (define (implement-mops-s s)
    (match s
      [`(with-label ,label ,s)
       `(with-label ,label ,(implement-mops-s s))]
      [`(set! ,reg_1 (mref ,reg_2 ,index))
       `(set! ,reg_1 (,reg_2 + ,index))]
      [`(mset! ,reg_1 ,index ,int32)
       `(set! (,reg_1 + ,index) ,int32)]
      [`(mset! ,reg_1 ,index ,trg)
       `(set! (,reg_1 + ,index) ,trg)]
      ;; Wildcard collapse case used because the remaining terminal cases of s
      ;; are not mops and do not require a transformation
      [_ s]))

  (match p
    [`(begin ,s ...)
     `(begin ,@(map implement-mops-s s))]))

