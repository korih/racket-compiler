#lang racket

(require
  rackunit
  "../passes/optimize-known-calls.rkt")


(module+ test
  (check-equal?
   (optimize-known-calls '(module
                              (letrec ((L.+.1.7
                                        (lambda (c.4 tmp.1 tmp.2)
                                          (let ((tmp.3 (closure-ref c.4 0)))
                                            (if (fixnum? tmp.1)
                                                (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2 tmp.3) (error 2))
                                                (error 2)))))
                                       (L.+.2.8
                                        (lambda (c.5 tmp.3 tmp.4)
                                          (let ((|+.1| (closure-ref c.5 0)))
                                            (closure-call |+.1| |+.1| tmp.3 tmp.4)))))
                                (cletrec
                                 ((|+.1| (make-closure L.+.1.7 2 tmp.3))
                                  (|+.2| (make-closure L.+.2.8 2 |+.1|)))
                                 (closure-call |+.2| |+.2| 1 2 tmp.3)))))
   '(module
        (letrec ((L.+.1.7
                  (lambda (c.4 tmp.1 tmp.2)
                    (let ((tmp.3 (closure-ref c.4 0)))
                      (if (fixnum? tmp.1)
                          (if (fixnum? tmp.2) (unsafe-fx+ tmp.1 tmp.2 tmp.3) (error 2))
                          (error 2)))))
                 (L.+.2.8
                  (lambda (c.5 tmp.3 tmp.4)
                    (let ((|+.1| (closure-ref c.5 0)))
                      (call L.+.1.7 |+.1| tmp.3 tmp.4)))))
          (cletrec
           ((|+.1| (make-closure L.+.1.7 2 tmp.3))
            (|+.2| (make-closure L.+.2.8 2 |+.1|)))
           (call L.+.2.8 |+.2| 1 2 tmp.3))))
   "Convert closure call to call")
  (check-equal?
   (optimize-known-calls '(module
                              (letrec ((L.x.1.7
                                        (lambda (c.4)
                                          (let ((x.1 (closure-ref c.4 0))) (closure-call x.1 x.1)))))
                                (cletrec ((x.1 (make-closure L.x.1.7 0 x.1))) x.1))))
   '(module
        (letrec ((L.x.1.7
                  (lambda (c.4)
                    (let ((x.1 (closure-ref c.4 0))) (call L.x.1.7 x.1)))))
          (cletrec ((x.1 (make-closure L.x.1.7 0 x.1))) x.1)))
   "Convert closure call with no call")
  (check-equal?
   (optimize-known-calls '(module
                              (letrec ((L.x.1.7
                                        (lambda (c.4)
                                          (let ((x.1 (closure-ref c.4 0))) (closure-call x.1 x.1)))))
                                (cletrec ((x.1 (make-closure L.x.1.7 0 x.1)))
                                         (closure-call x.1 x.1 1 2)))))
   '(module
        (letrec ((L.x.1.7
                  (lambda (c.4)
                    (let ((x.1 (closure-ref c.4 0))) (call L.x.1.7 x.1)))))
          (cletrec ((x.1 (make-closure L.x.1.7 0 x.1))) (call L.x.1.7 x.1 1 2))))
   "Convert closure with call that has extra args")
  (check-equal? (optimize-known-calls '(module (letrec ((L.tmp.49 (lambda (tmp.330 tmp.255) (let () (error? tmp.255)))) (L.tmp.50 (lambda (tmp.331 tmp.248 tmp.249 tmp.250) (let ((unsafe-vector-set!.247 (closure-ref tmp.331 0))) (if (fixnum? tmp.249) (if (vector? tmp.248) (closure-call unsafe-vector-set!.247 unsafe-vector-set!.247 tmp.248 tmp.249 tmp.250) (error 10)) (error 10))))) (L.tmp.51 (lambda (tmp.332 tmp.240) (let ((make-init-vector.238 (closure-ref tmp.332 0))) (if (fixnum? tmp.240) (closure-call make-init-vector.238 make-init-vector.238 tmp.240) (error 8))))) (L.tmp.52 (lambda (tmp.333 tmp.251 tmp.252 tmp.253) (let () (if (unsafe-fx< tmp.252 (unsafe-vector-length tmp.251)) (if (unsafe-fx>= tmp.252 0) (begin (unsafe-vector-set! tmp.251 tmp.252 tmp.253) (void)) (error 10)) (error 10))))) (L.tmp.53 (lambda (tmp.334 len.243 i.244 vec.245) (let ((vector-init-loop.239 (closure-ref tmp.334 0))) (if (eq? len.243 i.244) vec.245 (begin (unsafe-vector-set! vec.245 i.244 0) (closure-call vector-init-loop.239 vector-init-loop.239 len.243 (unsafe-fx+ i.244 1) vec.245)))))) (L.tmp.54 (lambda (tmp.335 tmp.241) (let ((vector-init-loop.239 (closure-ref tmp.335 0))) (if (unsafe-fx>= tmp.241 0) (let ((tmp.242 (unsafe-make-vector tmp.241))) (closure-call vector-init-loop.239 vector-init-loop.239 tmp.241 0 tmp.242)) (error 12)))))) (cletrec ((error?.254 (make-closure L.tmp.49 1)) (vector-set!.246 (make-closure L.tmp.50 3 unsafe-vector-set!.247)) (make-vector.237 (make-closure L.tmp.51 1 make-init-vector.238)) (unsafe-vector-set!.247 (make-closure L.tmp.52 3)) (vector-init-loop.239 (make-closure L.tmp.53 3 vector-init-loop.239)) (make-init-vector.238 (make-closure L.tmp.54 1 vector-init-loop.239))) (let ((tmp.7.67 (closure-call make-vector.237 make-vector.237 11))) (let ((tmp.8.68 (closure-call vector-set!.246 vector-set!.246 tmp.7.67 0 #\h))) (if (closure-call error?.254 error?.254 tmp.8.68) tmp.8.68 (let ((tmp.9.69 (closure-call vector-set!.246 vector-set!.246 tmp.7.67 1 #\e))) (if (closure-call error?.254 error?.254 tmp.9.69) tmp.9.69 (let ((tmp.10.70 (closure-call vector-set!.246 vector-set!.246 tmp.7.67 2 #\l))) (if (closure-call error?.254 error?.254 tmp.10.70) tmp.10.70 (let ((tmp.11.71 (closure-call vector-set!.246 vector-set!.246 tmp.7.67 3 #\l))) (if (closure-call error?.254 error?.254 tmp.11.71) tmp.11.71 (let ((tmp.12.72 (closure-call vector-set!.246 vector-set!.246 tmp.7.67 4 #\o))) (if (closure-call error?.254 error?.254 tmp.12.72) tmp.12.72 (let ((tmp.13.73 (closure-call vector-set!.246 vector-set!.246 tmp.7.67 5 #\space))) (if (closure-call error?.254 error?.254 tmp.13.73) tmp.13.73 (let ((tmp.14.74 (closure-call vector-set!.246 vector-set!.246 tmp.7.67 6 #\w))) (if (closure-call error?.254 error?.254 tmp.14.74) tmp.14.74 (let ((tmp.15.75 (closure-call vector-set!.246 vector-set!.246 tmp.7.67 7 #\o))) (if (closure-call error?.254 error?.254 tmp.15.75) tmp.15.75 (let ((tmp.16.76 (closure-call vector-set!.246 vector-set!.246 tmp.7.67 8 #\r))) (if (closure-call error?.254 error?.254 tmp.16.76) tmp.16.76 (let ((tmp.17.77 (closure-call vector-set!.246 vector-set!.246 tmp.7.67 9 #\l))) (if (closure-call error?.254 error?.254 tmp.17.77) tmp.17.77 (let ((tmp.18.78 (closure-call vector-set!.246 vector-set!.246 tmp.7.67 10 #\d))) (if (closure-call error?.254 error?.254 tmp.18.78) tmp.18.78 tmp.7.67)))))))))))))))))))))))))))
                '(module
                     (letrec ((L.tmp.49 (lambda (tmp.330 tmp.255) (let () (error? tmp.255))))
                              (L.tmp.50
                               (lambda (tmp.331 tmp.248 tmp.249 tmp.250)
                                 (let ((unsafe-vector-set!.247 (closure-ref tmp.331 0)))
                                   (if (fixnum? tmp.249)
                                       (if (vector? tmp.248)
                                           (call
                                            L.tmp.52
                                            unsafe-vector-set!.247
                                            tmp.248
                                            tmp.249
                                            tmp.250)
                                           (error 10))
                                       (error 10)))))
                              (L.tmp.51
                               (lambda (tmp.332 tmp.240)
                                 (let ((make-init-vector.238 (closure-ref tmp.332 0)))
                                   (if (fixnum? tmp.240)
                                       (call L.tmp.54 make-init-vector.238 tmp.240)
                                       (error 8)))))
                              (L.tmp.52
                               (lambda (tmp.333 tmp.251 tmp.252 tmp.253)
                                 (let ()
                                   (if (unsafe-fx< tmp.252 (unsafe-vector-length tmp.251))
                                       (if (unsafe-fx>= tmp.252 0)
                                           (begin (unsafe-vector-set! tmp.251 tmp.252 tmp.253) (void))
                                           (error 10))
                                       (error 10)))))
                              (L.tmp.53
                               (lambda (tmp.334 len.243 i.244 vec.245)
                                 (let ((vector-init-loop.239 (closure-ref tmp.334 0)))
                                   (if (eq? len.243 i.244)
                                       vec.245
                                       (begin
                                         (unsafe-vector-set! vec.245 i.244 0)
                                         (call
                                          L.tmp.53
                                          vector-init-loop.239
                                          len.243
                                          (unsafe-fx+ i.244 1)
                                          vec.245))))))
                              (L.tmp.54
                               (lambda (tmp.335 tmp.241)
                                 (let ((vector-init-loop.239 (closure-ref tmp.335 0)))
                                   (if (unsafe-fx>= tmp.241 0)
                                       (let ((tmp.242 (unsafe-make-vector tmp.241)))
                                         (call L.tmp.53 vector-init-loop.239 tmp.241 0 tmp.242))
                                       (error 12))))))
                       (cletrec
                        ((error?.254 (make-closure L.tmp.49 1))
                         (vector-set!.246 (make-closure L.tmp.50 3 unsafe-vector-set!.247))
                         (make-vector.237 (make-closure L.tmp.51 1 make-init-vector.238))
                         (unsafe-vector-set!.247 (make-closure L.tmp.52 3))
                         (vector-init-loop.239 (make-closure L.tmp.53 3 vector-init-loop.239))
                         (make-init-vector.238 (make-closure L.tmp.54 1 vector-init-loop.239)))
                        (let ((tmp.7.67 (call L.tmp.51 make-vector.237 11)))
                          (let ((tmp.8.68 (call L.tmp.50 vector-set!.246 tmp.7.67 0 #\h)))
                            (if (call L.tmp.49 error?.254 tmp.8.68)
                                tmp.8.68
                                (let ((tmp.9.69 (call L.tmp.50 vector-set!.246 tmp.7.67 1 #\e)))
                                  (if (call L.tmp.49 error?.254 tmp.9.69)
                                      tmp.9.69
                                      (let ((tmp.10.70
                                             (call L.tmp.50 vector-set!.246 tmp.7.67 2 #\l)))
                                        (if (call L.tmp.49 error?.254 tmp.10.70)
                                            tmp.10.70
                                            (let ((tmp.11.71
                                                   (call L.tmp.50 vector-set!.246 tmp.7.67 3 #\l)))
                                              (if (call L.tmp.49 error?.254 tmp.11.71)
                                                  tmp.11.71
                                                  (let ((tmp.12.72
                                                         (call L.tmp.50 vector-set!.246 tmp.7.67 4 #\o)))
                                                    (if (call L.tmp.49 error?.254 tmp.12.72)
                                                        tmp.12.72
                                                        (let ((tmp.13.73
                                                               (call
                                                                L.tmp.50
                                                                vector-set!.246
                                                                tmp.7.67
                                                                5
                                                                #\space)))
                                                          (if (call L.tmp.49 error?.254 tmp.13.73)
                                                              tmp.13.73
                                                              (let ((tmp.14.74
                                                                     (call
                                                                      L.tmp.50
                                                                      vector-set!.246
                                                                      tmp.7.67
                                                                      6
                                                                      #\w)))
                                                                (if (call L.tmp.49 error?.254 tmp.14.74)
                                                                    tmp.14.74
                                                                    (let ((tmp.15.75
                                                                           (call
                                                                            L.tmp.50
                                                                            vector-set!.246
                                                                            tmp.7.67
                                                                            7
                                                                            #\o)))
                                                                      (if (call L.tmp.49 error?.254 tmp.15.75)
                                                                          tmp.15.75
                                                                          (let ((tmp.16.76
                                                                                 (call
                                                                                  L.tmp.50
                                                                                  vector-set!.246
                                                                                  tmp.7.67
                                                                                  8
                                                                                  #\r)))
                                                                            (if (call
                                                                                 L.tmp.49
                                                                                 error?.254
                                                                                 tmp.16.76)
                                                                                tmp.16.76
                                                                                (let ((tmp.17.77
                                                                                       (call
                                                                                        L.tmp.50
                                                                                        vector-set!.246
                                                                                        tmp.7.67
                                                                                        9
                                                                                        #\l)))
                                                                                  (if (call
                                                                                       L.tmp.49
                                                                                       error?.254
                                                                                       tmp.17.77)
                                                                                      tmp.17.77
                                                                                      (let ((tmp.18.78
                                                                                             (call
                                                                                              L.tmp.50
                                                                                              vector-set!.246
                                                                                              tmp.7.67
                                                                                              10
                                                                                              #\d)))
                                                                                        (if (call
                                                                                             L.tmp.49
                                                                                             error?.254
                                                                                             tmp.18.78)
                                                                                            tmp.18.78
                                                                                            tmp.7.67)))))))))))))))))))))))))))
  (check-equal? (optimize-known-calls '(module (letrec ((L.tmp.5 (lambda (tmp.285 tmp.150) (let () (error? tmp.150)))) (L.tmp.6 (lambda (tmp.286 tmp.133) (let ((vector-init-loop.131 (closure-ref tmp.286 0))) (if (unsafe-fx>= tmp.133 0) (let ((tmp.134 (unsafe-make-vector tmp.133))) (closure-call vector-init-loop.131 vector-init-loop.131 tmp.133 0 tmp.134)) (error 12))))) (L.tmp.7 (lambda (tmp.287 len.135 i.136 vec.137) (let ((vector-init-loop.131 (closure-ref tmp.287 0))) (if (eq? len.135 i.136) vec.137 (begin (unsafe-vector-set! vec.137 i.136 0) (closure-call vector-init-loop.131 vector-init-loop.131 len.135 (unsafe-fx+ i.136 1) vec.137)))))) (L.tmp.8 (lambda (tmp.288 tmp.140 tmp.141 tmp.142) (let ((unsafe-vector-set!.139 (closure-ref tmp.288 0))) (if (fixnum? tmp.141) (if (vector? tmp.140) (closure-call unsafe-vector-set!.139 unsafe-vector-set!.139 tmp.140 tmp.141 tmp.142) (error 10)) (error 10))))) (L.tmp.9 (lambda (tmp.289 tmp.132) (let ((make-init-vector.130 (closure-ref tmp.289 0))) (if (fixnum? tmp.132) (closure-call make-init-vector.130 make-init-vector.130 tmp.132) (error 8))))) (L.tmp.10 (lambda (tmp.290 tmp.147 tmp.148) (let () (if (fixnum? tmp.147) (if (fixnum? tmp.148) (unsafe-fx+ tmp.147 tmp.148) (error 2)) (error 2))))) (L.tmp.11 (lambda (tmp.291 tmp.143 tmp.144 tmp.145) (let () (if (unsafe-fx< tmp.144 (unsafe-vector-length tmp.143)) (if (unsafe-fx>= tmp.144 0) (begin (unsafe-vector-set! tmp.143 tmp.144 tmp.145) (void)) (error 10)) (error 10)))))) (cletrec ((error?.149 (make-closure L.tmp.5 1)) (make-init-vector.130 (make-closure L.tmp.6 1 vector-init-loop.131)) (vector-init-loop.131 (make-closure L.tmp.7 3 vector-init-loop.131)) (vector-set!.138 (make-closure L.tmp.8 3 unsafe-vector-set!.139)) (make-vector.129 (make-closure L.tmp.9 1 make-init-vector.130)) (|+.146| (make-closure L.tmp.10 2)) (unsafe-vector-set!.139 (make-closure L.tmp.11 3))) (let ((tmp.19.62 (closure-call make-vector.129 make-vector.129 4))) (let ((tmp.20.63 (closure-call vector-set!.138 vector-set!.138 tmp.19.62 0 (closure-call |+.146| |+.146| 1 2)))) (if (closure-call error?.149 error?.149 tmp.20.63) tmp.20.63 (let ((tmp.21.64 (closure-call vector-set!.138 vector-set!.138 tmp.19.62 1 3))) (if (closure-call error?.149 error?.149 tmp.21.64) tmp.21.64 (let ((tmp.22.65 (closure-call vector-set!.138 vector-set!.138 tmp.19.62 2 2))) (if (closure-call error?.149 error?.149 tmp.22.65) tmp.22.65 (let ((tmp.23.66 (closure-call vector-set!.138 vector-set!.138 tmp.19.62 3 3))) (if (closure-call error?.149 error?.149 tmp.23.66) tmp.23.66 tmp.19.62)))))))))))))
                '(module
                     (letrec ((L.tmp.5 (lambda (tmp.285 tmp.150) (let () (error? tmp.150))))
                              (L.tmp.6
                               (lambda (tmp.286 tmp.133)
                                 (let ((vector-init-loop.131 (closure-ref tmp.286 0)))
                                   (if (unsafe-fx>= tmp.133 0)
                                       (let ((tmp.134 (unsafe-make-vector tmp.133)))
                                         (call L.tmp.7 vector-init-loop.131 tmp.133 0 tmp.134))
                                       (error 12)))))
                              (L.tmp.7
                               (lambda (tmp.287 len.135 i.136 vec.137)
                                 (let ((vector-init-loop.131 (closure-ref tmp.287 0)))
                                   (if (eq? len.135 i.136)
                                       vec.137
                                       (begin
                                         (unsafe-vector-set! vec.137 i.136 0)
                                         (call
                                          L.tmp.7
                                          vector-init-loop.131
                                          len.135
                                          (unsafe-fx+ i.136 1)
                                          vec.137))))))
                              (L.tmp.8
                               (lambda (tmp.288 tmp.140 tmp.141 tmp.142)
                                 (let ((unsafe-vector-set!.139 (closure-ref tmp.288 0)))
                                   (if (fixnum? tmp.141)
                                       (if (vector? tmp.140)
                                           (call
                                            L.tmp.11
                                            unsafe-vector-set!.139
                                            tmp.140
                                            tmp.141
                                            tmp.142)
                                           (error 10))
                                       (error 10)))))
                              (L.tmp.9
                               (lambda (tmp.289 tmp.132)
                                 (let ((make-init-vector.130 (closure-ref tmp.289 0)))
                                   (if (fixnum? tmp.132)
                                       (call L.tmp.6 make-init-vector.130 tmp.132)
                                       (error 8)))))
                              (L.tmp.10
                               (lambda (tmp.290 tmp.147 tmp.148)
                                 (let ()
                                   (if (fixnum? tmp.147)
                                       (if (fixnum? tmp.148) (unsafe-fx+ tmp.147 tmp.148) (error 2))
                                       (error 2)))))
                              (L.tmp.11
                               (lambda (tmp.291 tmp.143 tmp.144 tmp.145)
                                 (let ()
                                   (if (unsafe-fx< tmp.144 (unsafe-vector-length tmp.143))
                                       (if (unsafe-fx>= tmp.144 0)
                                           (begin (unsafe-vector-set! tmp.143 tmp.144 tmp.145) (void))
                                           (error 10))
                                       (error 10))))))
                       (cletrec
                        ((error?.149 (make-closure L.tmp.5 1))
                         (make-init-vector.130 (make-closure L.tmp.6 1 vector-init-loop.131))
                         (vector-init-loop.131 (make-closure L.tmp.7 3 vector-init-loop.131))
                         (vector-set!.138 (make-closure L.tmp.8 3 unsafe-vector-set!.139))
                         (make-vector.129 (make-closure L.tmp.9 1 make-init-vector.130))
                         (|+.146| (make-closure L.tmp.10 2))
                         (unsafe-vector-set!.139 (make-closure L.tmp.11 3)))
                        (let ((tmp.19.62 (call L.tmp.9 make-vector.129 4)))
                          (let ((tmp.20.63
                                 (call
                                  L.tmp.8
                                  vector-set!.138
                                  tmp.19.62
                                  0
                                  (call L.tmp.10 |+.146| 1 2))))
                            (if (call L.tmp.5 error?.149 tmp.20.63)
                                tmp.20.63
                                (let ((tmp.21.64 (call L.tmp.8 vector-set!.138 tmp.19.62 1 3)))
                                  (if (call L.tmp.5 error?.149 tmp.21.64)
                                      tmp.21.64
                                      (let ((tmp.22.65 (call L.tmp.8 vector-set!.138 tmp.19.62 2 2)))
                                        (if (call L.tmp.5 error?.149 tmp.22.65)
                                            tmp.22.65
                                            (let ((tmp.23.66
                                                   (call L.tmp.8 vector-set!.138 tmp.19.62 3 3)))
                                              (if (call L.tmp.5 error?.149 tmp.23.66)
                                                  tmp.23.66
                                                  tmp.19.62)))))))))))))
  (check-equal? (optimize-known-calls '(module (letrec ((L.tmp.64 (lambda (tmp.345 tmp.260 tmp.261) (let () (if (unsafe-fx< tmp.261 (unsafe-vector-length tmp.260)) (if (unsafe-fx>= tmp.261 0) (unsafe-vector-ref tmp.260 tmp.261) (error 11)) (error 11))))) (L.tmp.65 (lambda (tmp.346 tmp.258 tmp.259) (let ((unsafe-vector-ref.257 (closure-ref tmp.346 0))) (if (fixnum? tmp.259) (if (vector? tmp.258) (closure-call unsafe-vector-ref.257 unsafe-vector-ref.257 tmp.258 tmp.259) (error 11)) (error 11))))) (L.tmp.66 (lambda (tmp.347 tmp.265) (let ((make-init-vector.263 (closure-ref tmp.347 0))) (if (fixnum? tmp.265) (closure-call make-init-vector.263 make-init-vector.263 tmp.265) (error 8))))) (L.tmp.67 (lambda (tmp.348 len.268 i.269 vec.270) (let ((vector-init-loop.264 (closure-ref tmp.348 0))) (if (eq? len.268 i.269) vec.270 (begin (unsafe-vector-set! vec.270 i.269 0) (closure-call vector-init-loop.264 vector-init-loop.264 len.268 (unsafe-fx+ i.269 1) vec.270)))))) (L.tmp.68 (lambda (tmp.349 tmp.266) (let ((vector-init-loop.264 (closure-ref tmp.349 0))) (if (unsafe-fx>= tmp.266 0) (let ((tmp.267 (unsafe-make-vector tmp.266))) (closure-call vector-init-loop.264 vector-init-loop.264 tmp.266 0 tmp.267)) (error 12)))))) (cletrec ((unsafe-vector-ref.257 (make-closure L.tmp.64 2)) (vector-ref.256 (make-closure L.tmp.65 2 unsafe-vector-ref.257)) (make-vector.262 (make-closure L.tmp.66 1 make-init-vector.263)) (vector-init-loop.264 (make-closure L.tmp.67 3 vector-init-loop.264)) (make-init-vector.263 (make-closure L.tmp.68 1 vector-init-loop.264))) (closure-call vector-ref.256 vector-ref.256 (closure-call make-vector.262 make-vector.262 2) 0)))))
                '(module
                     (letrec ((L.tmp.64
                               (lambda (tmp.345 tmp.260 tmp.261)
                                 (let ()
                                   (if (unsafe-fx< tmp.261 (unsafe-vector-length tmp.260))
                                       (if (unsafe-fx>= tmp.261 0)
                                           (unsafe-vector-ref tmp.260 tmp.261)
                                           (error 11))
                                       (error 11)))))
                              (L.tmp.65
                               (lambda (tmp.346 tmp.258 tmp.259)
                                 (let ((unsafe-vector-ref.257 (closure-ref tmp.346 0)))
                                   (if (fixnum? tmp.259)
                                       (if (vector? tmp.258)
                                           (call L.tmp.64 unsafe-vector-ref.257 tmp.258 tmp.259)
                                           (error 11))
                                       (error 11)))))
                              (L.tmp.66
                               (lambda (tmp.347 tmp.265)
                                 (let ((make-init-vector.263 (closure-ref tmp.347 0)))
                                   (if (fixnum? tmp.265)
                                       (call L.tmp.68 make-init-vector.263 tmp.265)
                                       (error 8)))))
                              (L.tmp.67
                               (lambda (tmp.348 len.268 i.269 vec.270)
                                 (let ((vector-init-loop.264 (closure-ref tmp.348 0)))
                                   (if (eq? len.268 i.269)
                                       vec.270
                                       (begin
                                         (unsafe-vector-set! vec.270 i.269 0)
                                         (call
                                          L.tmp.67
                                          vector-init-loop.264
                                          len.268
                                          (unsafe-fx+ i.269 1)
                                          vec.270))))))
                              (L.tmp.68
                               (lambda (tmp.349 tmp.266)
                                 (let ((vector-init-loop.264 (closure-ref tmp.349 0)))
                                   (if (unsafe-fx>= tmp.266 0)
                                       (let ((tmp.267 (unsafe-make-vector tmp.266)))
                                         (call L.tmp.67 vector-init-loop.264 tmp.266 0 tmp.267))
                                       (error 12))))))
                       (cletrec
                        ((unsafe-vector-ref.257 (make-closure L.tmp.64 2))
                         (vector-ref.256 (make-closure L.tmp.65 2 unsafe-vector-ref.257))
                         (make-vector.262 (make-closure L.tmp.66 1 make-init-vector.263))
                         (vector-init-loop.264 (make-closure L.tmp.67 3 vector-init-loop.264))
                         (make-init-vector.263 (make-closure L.tmp.68 1 vector-init-loop.264)))
                        (call L.tmp.65 vector-ref.256 (call L.tmp.66 make-vector.262 2) 0)))))
  (check-equal? (optimize-known-calls '(module (letrec ((L.tmp.44 (lambda (tmp.325 tmp.114) (let ((make-init-vector.112 (closure-ref tmp.325 0))) (if (fixnum? tmp.114) (closure-call make-init-vector.112 make-init-vector.112 tmp.114) (error 8))))) (L.tmp.45 (lambda (tmp.326 tmp.115) (let ((vector-init-loop.113 (closure-ref tmp.326 0))) (if (unsafe-fx>= tmp.115 0) (let ((tmp.116 (unsafe-make-vector tmp.115))) (closure-call vector-init-loop.113 vector-init-loop.113 tmp.115 0 tmp.116)) (error 12))))) (L.tmp.46 (lambda (tmp.327 len.117 i.118 vec.119) (let ((vector-init-loop.113 (closure-ref tmp.327 0))) (if (eq? len.117 i.118) vec.119 (begin (unsafe-vector-set! vec.119 i.118 0) (closure-call vector-init-loop.113 vector-init-loop.113 len.117 (unsafe-fx+ i.118 1) vec.119))))))) (cletrec ((make-vector.111 (make-closure L.tmp.44 1 make-init-vector.112)) (make-init-vector.112 (make-closure L.tmp.45 1 vector-init-loop.113)) (vector-init-loop.113 (make-closure L.tmp.46 3 vector-init-loop.113))) (closure-call make-vector.111 make-vector.111 2)))))
                '(module
                     (letrec ((L.tmp.44
                               (lambda (tmp.325 tmp.114)
                                 (let ((make-init-vector.112 (closure-ref tmp.325 0)))
                                   (if (fixnum? tmp.114)
                                       (call L.tmp.45 make-init-vector.112 tmp.114)
                                       (error 8)))))
                              (L.tmp.45
                               (lambda (tmp.326 tmp.115)
                                 (let ((vector-init-loop.113 (closure-ref tmp.326 0)))
                                   (if (unsafe-fx>= tmp.115 0)
                                       (let ((tmp.116 (unsafe-make-vector tmp.115)))
                                         (call L.tmp.46 vector-init-loop.113 tmp.115 0 tmp.116))
                                       (error 12)))))
                              (L.tmp.46
                               (lambda (tmp.327 len.117 i.118 vec.119)
                                 (let ((vector-init-loop.113 (closure-ref tmp.327 0)))
                                   (if (eq? len.117 i.118)
                                       vec.119
                                       (begin
                                         (unsafe-vector-set! vec.119 i.118 0)
                                         (call
                                          L.tmp.46
                                          vector-init-loop.113
                                          len.117
                                          (unsafe-fx+ i.118 1)
                                          vec.119)))))))
                       (cletrec
                        ((make-vector.111 (make-closure L.tmp.44 1 make-init-vector.112))
                         (make-init-vector.112 (make-closure L.tmp.45 1 vector-init-loop.113))
                         (vector-init-loop.113 (make-closure L.tmp.46 3 vector-init-loop.113)))
                        (call L.tmp.44 make-vector.111 2)))))
  (check-equal? (optimize-known-calls '(module (letrec ((L.tmp.61 (lambda (tmp.342 tmp.94) (let ((vector-init-loop.92 (closure-ref tmp.342 0))) (if (unsafe-fx>= tmp.94 0) (let ((tmp.95 (unsafe-make-vector tmp.94))) (closure-call vector-init-loop.92 vector-init-loop.92 tmp.94 0 tmp.95)) (error 12))))) (L.tmp.62 (lambda (tmp.343 len.96 i.97 vec.98) (let ((vector-init-loop.92 (closure-ref tmp.343 0))) (if (eq? len.96 i.97) vec.98 (begin (unsafe-vector-set! vec.98 i.97 0) (closure-call vector-init-loop.92 vector-init-loop.92 len.96 (unsafe-fx+ i.97 1) vec.98)))))) (L.tmp.63 (lambda (tmp.344 tmp.93) (let ((make-init-vector.91 (closure-ref tmp.344 0))) (if (fixnum? tmp.93) (closure-call make-init-vector.91 make-init-vector.91 tmp.93) (error 8)))))) (cletrec ((make-init-vector.91 (make-closure L.tmp.61 1 vector-init-loop.92)) (vector-init-loop.92 (make-closure L.tmp.62 3 vector-init-loop.92)) (make-vector.90 (make-closure L.tmp.63 1 make-init-vector.91))) (closure-call make-vector.90 make-vector.90 0)))))
                '(module
                     (letrec ((L.tmp.61
                               (lambda (tmp.342 tmp.94)
                                 (let ((vector-init-loop.92 (closure-ref tmp.342 0)))
                                   (if (unsafe-fx>= tmp.94 0)
                                       (let ((tmp.95 (unsafe-make-vector tmp.94)))
                                         (call L.tmp.62 vector-init-loop.92 tmp.94 0 tmp.95))
                                       (error 12)))))
                              (L.tmp.62
                               (lambda (tmp.343 len.96 i.97 vec.98)
                                 (let ((vector-init-loop.92 (closure-ref tmp.343 0)))
                                   (if (eq? len.96 i.97)
                                       vec.98
                                       (begin
                                         (unsafe-vector-set! vec.98 i.97 0)
                                         (call
                                          L.tmp.62
                                          vector-init-loop.92
                                          len.96
                                          (unsafe-fx+ i.97 1)
                                          vec.98))))))
                              (L.tmp.63
                               (lambda (tmp.344 tmp.93)
                                 (let ((make-init-vector.91 (closure-ref tmp.344 0)))
                                   (if (fixnum? tmp.93)
                                       (call L.tmp.61 make-init-vector.91 tmp.93)
                                       (error 8))))))
                       (cletrec
                        ((make-init-vector.91 (make-closure L.tmp.61 1 vector-init-loop.92))
                         (vector-init-loop.92 (make-closure L.tmp.62 3 vector-init-loop.92))
                         (make-vector.90 (make-closure L.tmp.63 1 make-init-vector.91)))
                        (call L.tmp.63 make-vector.90 0)))))
  (check-equal? (optimize-known-calls '(module (letrec ((L.tmp.71 (lambda (tmp.352 tmp.212) (let () (error? tmp.212)))) (L.tmp.72 (lambda (tmp.353 len.200 i.201 vec.202) (let ((vector-init-loop.196 (closure-ref tmp.353 0))) (if (eq? len.200 i.201) vec.202 (begin (unsafe-vector-set! vec.202 i.201 0) (closure-call vector-init-loop.196 vector-init-loop.196 len.200 (unsafe-fx+ i.201 1) vec.202)))))) (L.tmp.73 (lambda (tmp.354 tmp.205 tmp.206 tmp.207) (let ((unsafe-vector-set!.204 (closure-ref tmp.354 0))) (if (fixnum? tmp.206) (if (vector? tmp.205) (closure-call unsafe-vector-set!.204 unsafe-vector-set!.204 tmp.205 tmp.206 tmp.207) (error 10)) (error 10))))) (L.tmp.74 (lambda (tmp.355 tmp.197) (let ((make-init-vector.195 (closure-ref tmp.355 0))) (if (fixnum? tmp.197) (closure-call make-init-vector.195 make-init-vector.195 tmp.197) (error 8))))) (L.tmp.75 (lambda (tmp.356 tmp.198) (let ((vector-init-loop.196 (closure-ref tmp.356 0))) (if (unsafe-fx>= tmp.198 0) (let ((tmp.199 (unsafe-make-vector tmp.198))) (closure-call vector-init-loop.196 vector-init-loop.196 tmp.198 0 tmp.199)) (error 12))))) (L.tmp.76 (lambda (tmp.357 tmp.208 tmp.209 tmp.210) (let () (if (unsafe-fx< tmp.209 (unsafe-vector-length tmp.208)) (if (unsafe-fx>= tmp.209 0) (begin (unsafe-vector-set! tmp.208 tmp.209 tmp.210) (void)) (error 10)) (error 10)))))) (cletrec ((error?.211 (make-closure L.tmp.71 1)) (vector-init-loop.196 (make-closure L.tmp.72 3 vector-init-loop.196)) (vector-set!.203 (make-closure L.tmp.73 3 unsafe-vector-set!.204)) (make-vector.194 (make-closure L.tmp.74 1 make-init-vector.195)) (make-init-vector.195 (make-closure L.tmp.75 1 vector-init-loop.196)) (unsafe-vector-set!.204 (make-closure L.tmp.76 3))) (let ((tmp.2.81 (closure-call make-vector.194 make-vector.194 3))) (let ((tmp.3.82 (closure-call vector-set!.203 vector-set!.203 tmp.2.81 0 1))) (if (closure-call error?.211 error?.211 tmp.3.82) tmp.3.82 (let ((tmp.4.83 (closure-call vector-set!.203 vector-set!.203 tmp.2.81 1 2))) (if (closure-call error?.211 error?.211 tmp.4.83) tmp.4.83 (let ((tmp.5.84 (closure-call vector-set!.203 vector-set!.203 tmp.2.81 2 3))) (if (closure-call error?.211 error?.211 tmp.5.84) tmp.5.84 tmp.2.81)))))))))))
                '(module
                     (letrec ((L.tmp.71 (lambda (tmp.352 tmp.212) (let () (error? tmp.212))))
                              (L.tmp.72
                               (lambda (tmp.353 len.200 i.201 vec.202)
                                 (let ((vector-init-loop.196 (closure-ref tmp.353 0)))
                                   (if (eq? len.200 i.201)
                                       vec.202
                                       (begin
                                         (unsafe-vector-set! vec.202 i.201 0)
                                         (call
                                          L.tmp.72
                                          vector-init-loop.196
                                          len.200
                                          (unsafe-fx+ i.201 1)
                                          vec.202))))))
                              (L.tmp.73
                               (lambda (tmp.354 tmp.205 tmp.206 tmp.207)
                                 (let ((unsafe-vector-set!.204 (closure-ref tmp.354 0)))
                                   (if (fixnum? tmp.206)
                                       (if (vector? tmp.205)
                                           (call
                                            L.tmp.76
                                            unsafe-vector-set!.204
                                            tmp.205
                                            tmp.206
                                            tmp.207)
                                           (error 10))
                                       (error 10)))))
                              (L.tmp.74
                               (lambda (tmp.355 tmp.197)
                                 (let ((make-init-vector.195 (closure-ref tmp.355 0)))
                                   (if (fixnum? tmp.197)
                                       (call L.tmp.75 make-init-vector.195 tmp.197)
                                       (error 8)))))
                              (L.tmp.75
                               (lambda (tmp.356 tmp.198)
                                 (let ((vector-init-loop.196 (closure-ref tmp.356 0)))
                                   (if (unsafe-fx>= tmp.198 0)
                                       (let ((tmp.199 (unsafe-make-vector tmp.198)))
                                         (call L.tmp.72 vector-init-loop.196 tmp.198 0 tmp.199))
                                       (error 12)))))
                              (L.tmp.76
                               (lambda (tmp.357 tmp.208 tmp.209 tmp.210)
                                 (let ()
                                   (if (unsafe-fx< tmp.209 (unsafe-vector-length tmp.208))
                                       (if (unsafe-fx>= tmp.209 0)
                                           (begin (unsafe-vector-set! tmp.208 tmp.209 tmp.210) (void))
                                           (error 10))
                                       (error 10))))))
                       (cletrec
                        ((error?.211 (make-closure L.tmp.71 1))
                         (vector-init-loop.196 (make-closure L.tmp.72 3 vector-init-loop.196))
                         (vector-set!.203 (make-closure L.tmp.73 3 unsafe-vector-set!.204))
                         (make-vector.194 (make-closure L.tmp.74 1 make-init-vector.195))
                         (make-init-vector.195 (make-closure L.tmp.75 1 vector-init-loop.196))
                         (unsafe-vector-set!.204 (make-closure L.tmp.76 3)))
                        (let ((tmp.2.81 (call L.tmp.74 make-vector.194 3)))
                          (let ((tmp.3.82 (call L.tmp.73 vector-set!.203 tmp.2.81 0 1)))
                            (if (call L.tmp.71 error?.211 tmp.3.82)
                                tmp.3.82
                                (let ((tmp.4.83 (call L.tmp.73 vector-set!.203 tmp.2.81 1 2)))
                                  (if (call L.tmp.71 error?.211 tmp.4.83)
                                      tmp.4.83
                                      (let ((tmp.5.84 (call L.tmp.73 vector-set!.203 tmp.2.81 2 3)))
                                        (if (call L.tmp.71 error?.211 tmp.5.84)
                                            tmp.5.84
                                            tmp.2.81))))))))))))
