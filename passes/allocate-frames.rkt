#lang racket

(require
  cpsc411/compiler-lib
  cpsc411/langs/v8)

(provide allocate-frames)

;; asm-pred-lang-v8/pre-framed -> asm-pred-lang-v8/framed
;; compiles p to Asm-pred-lang v8/framed by allocating frames for each non-tail
;; call, and assigning all new-frame variables to frame variables in the new
;; frame
(define/contract (allocate-frames p)
  (-> asm-pred-lang-v8/pre-framed? asm-pred-lang-v8/framed?)

  ;; func is `(define ,label ,info ,tail)
  ;; interp. a function definition

  ;; asm-pred-lang-v8/pre-framed -> integer
  ;; interp. computes the number of bytes needed to allocate the frame by
  ;; calculating the maximum of assigned frame variable slots and call-undead
  ;; size, multiplied by the word size
  (define (compute-frame-size info)
    (define assignments (info-ref info 'assignment))
    (define max-idx
      (for/fold ([max-idx -1]) ([a assignments])
        (if (fvar? (second a))
            (max max-idx (fvar->index (second a)))
            max-idx)))
    (* (max (add1 max-idx) (length (info-ref info 'call-undead)))
       (current-word-size-bytes)))

  ;; asm-pred-lang-v8/pre-framed.info -> asm-pred-lang-v8/framed.info
  (define (allocate-frames-info info)
    (define new-frames (if (empty? (info-ref info 'new-frames))
                           '()
                           (first (info-ref info 'new-frames))))

    ;; Remove new-frames from locals since they're now frame allocated
    (define locals^ (remove* new-frames (info-ref info 'locals)))

    (define existing-assignments (info-ref info 'assignment))
    (define existing-fvars (map cadr existing-assignments))

    (define start-index
      (if (empty? existing-fvars)
          0
          (add1 (apply max (map fvar->index existing-fvars)))))

    ;; Assign fresh fvars to new-frames
    (define-values (final-assignments _)
      (for/fold ([assignments^ existing-assignments]
                 [next-idx start-index])
                ([frame new-frames])
        (define-values (fvar idx)
          (let loop ([i next-idx])
            (define candidate (make-fvar i))
            (if (member candidate existing-fvars)
                (loop (add1 i))
                (values candidate (add1 i)))))
        (values (cons (list frame fvar) assignments^) idx)))

    (define cleaned-info
      (for/fold ([acc info]) ([field '(undead-out call-undead new-frames)])
        (info-remove acc field)))

    (info-set
     (info-set cleaned-info 'locals (reverse locals^))
     'assignment final-assignments))

  ;; func -> func
  (define (allocate-frames-func f)
    (match f
      [`(define ,label ,info ,tail)
       `(define ,label ,(allocate-frames-info info) ,(allocate-frames-tail tail info))]))

  ;; asm-pred-lang-v8/pre-framed.tail asm-pred-lang-v8/pre-framed.info -> asm-pred-lang-v8/framed.tail
  (define (allocate-frames-tail t info)
    (match t
      [`(jump ,trg ,locs ...) `(jump ,trg ,@locs)]
      [`(begin ,effects ... ,tail)
       (define effects^ (for/fold ([compiled-effects empty])
                                  ([effect effects])
                          (cons (allocate-frames-effect effect info) compiled-effects)))
       `(begin ,@(reverse effects^) ,(allocate-frames-tail tail info))]
      [`(if ,pred ,t1 ,t2)
       `(if ,(allocate-frames-pred pred info)
            ,(allocate-frames-tail t1 info)
            ,(allocate-frames-tail t2 info))]))

  ;; asm-pred-lang-v8/pre-framed.effect asm-pred-lang-v8/pre-framed.info -> asm-pred-lang-v8/framed.effect
  (define (allocate-frames-effect e info)
    (match e
      [`(begin ,effects ...)
       (define effects^ (for/fold ([compiled-effects empty])
                                  ([effect effects])
                          (cons (allocate-frames-effect effect info) compiled-effects)))
       `(begin ,@(reverse effects^))]
      [`(return-point ,label ,tail)
       (define frame-size (compute-frame-size info))
       (define fbp (current-frame-base-pointer-register))
       `(begin
          (set! ,fbp (- ,fbp ,frame-size))
          (return-point ,label ,(allocate-frames-tail tail info))
          (set! ,fbp (+ ,fbp ,frame-size)))]
      [`(if ,pred ,e1 ,e2)
       `(if ,(allocate-frames-pred pred info)
            ,(allocate-frames-effect e1 info)
            ,(allocate-frames-effect e2 info))]
      ;; Wildcard collapse case used because any other effect does not require
      ;; frame allocation and can be returned unchanged
      [_ e]))

  ;; asm-pred-lang-v8/pre-framed.pred asm-pred-lang-v8/pre-framed.info -> asm-pred-lang-v8/framed.pred
  (define (allocate-frames-pred p info)
    (match p
      [`(begin ,effects ... ,pred)
       (define effects^ (for/fold ([compiled-effects '()])
                                  ([effect effects])
                          (append compiled-effects (list (allocate-frames-effect effect info)))))
       `(begin ,@effects^ ,(allocate-frames-pred pred info))]
      ;; Wildcard collapse case used because all other predicates do not contain
      ;; nested effects or require frame allocation and can be returned as-is
      [_ p]))

  (match p
    [`(module ,info ,funcs ... ,tail)
     `(module ,(allocate-frames-info info) ,@(map allocate-frames-func funcs) ,(allocate-frames-tail tail info))]))

