#lang racket

(require rackunit
         "../passes/generate-x64.rkt")

(module+ test
  (check-equal? (generate-x64 '(begin (set! rax (- rax 1))))
                "sub rax, 1")
  (check-equal? (generate-x64 '(begin (set! rsi L.label.1) (with-label L.label.1 (set! rbx 18)) (set! rax rbx) (jump done)))
                (string-join
                 (list "lea rsi, [rel L.label.1]"
                       "L.label.1:"
                       "mov rbx, 18"
                       "mov rax, rbx"
                       "jmp done")
                 "\n"))
  (check-equal? (generate-x64 '(begin (set! (rbp - 8) L.label.1) (with-label L.label.1 (set! rbx 18)) (set! rax rbx) (jump done)))
                (string-join
                 (list "mov QWORD [rbp - 8], L.label.1"
                       "L.label.1:"
                       "mov rbx, 18"
                       "mov rax, rbx"
                       "jmp done")
                 "\n"))
  (check-equal? (generate-x64 '(begin))
                "")
  (check-equal? (generate-x64 '(begin (set! (rbp - 8) 42)))
                "mov QWORD [rbp - 8], 42")
  (check-equal? (generate-x64 '(begin (set! (rbp - 16) rax)))
                "mov QWORD [rbp - 16], rax")
  (check-equal? (generate-x64 '(begin (set! rax rbx)))
                "mov rax, rbx")
  (check-equal? (generate-x64 '(begin (set! rax (rbp - 8))))
                "mov rax, QWORD [rbp - 8]")
  (check-equal? (generate-x64 '(begin (set! rax 170679)))
                "mov rax, 170679")
  (check-equal? (generate-x64 '(begin (set! rax (+ rax 5))))
                "add rax, 5")
  (check-equal? (generate-x64 '(begin (set! rax (+ rax rbx))))
                "add rax, rbx")
  (check-equal? (generate-x64 '(begin (set! rax (+ rax (rbp - 8)))))
                "add rax, QWORD [rbp - 8]")
  (check-equal?
   (generate-x64
    '(begin
       (set! rax 1)
       (set! rax (+ rax 2))
       (set! rax (+ rax 3))
       (set! rax (+ rax 4))
       (set! rax (+ rax 5))))
   (string-join
    (list "mov rax, 1"
          "add rax, 2"
          "add rax, 3"
          "add rax, 4"
          "add rax, 5")
    "\n"))
  (check-equal?
   (generate-x64
    '(begin
       (set! rax (rbp - 8))
       (set! rbx (rbp - 16))
       (set! rax (+ rax (rbp - 8)))
       (set! rax (+ rax rbx))))
   (string-join
    (list "mov rax, QWORD [rbp - 8]"
          "mov rbx, QWORD [rbp - 16]"
          "add rax, QWORD [rbp - 8]"
          "add rax, rbx")
    "\n"))
  (check-equal? (generate-x64 '(begin (jump rax)))
                "jmp rax")
  (check-equal? (generate-x64 '(begin (jump-if > L.start.1)))
                "jg L.start.1")
  (check-equal? (generate-x64 '(begin (with-label L.start.1 (jump L.start.1))))
                "L.start.1:\njmp L.start.1")
  (check-equal? (generate-x64 '(begin (compare rax 2)))
                "cmp rax, 2")
  (check-equal? (generate-x64 '(begin
                                 (set! rbx 10)
                                 (compare rbx 0)
                                 (with-label L.start.1 (jump-if = L.end.1))
                                 (set! rbx (+ rbx -1))
                                 (jump L.start.1)
                                 (with-label L.end.1 (set! rax rbx))))
                (string-join
                 (list "mov rbx, 10"
                       "cmp rbx, 0"
                       "L.start.1:"
                       "je L.end.1"
                       "add rbx, -1"
                       "jmp L.start.1"
                       "L.end.1:"
                       "mov rax, rbx")
                 "\n"))
  (check-equal? (generate-x64 '(begin
                                 (with-label L.tmp.99 (set! rbx r15))
                                 (set! rcx 10)
                                 (set! rsp 100)
                                 (compare rcx rsp)
                                 (jump-if != L.tmp.101)
                                 (jump L.tmp.100)
                                 (with-label L.tmp.101 (set! rdi 1000))
                                 (set! r15 rbx)
                                 (jump L.f.2)
                                 (with-label L.tmp.100 (set! rdi rcx))
                                 (set! r15 rbx)
                                 (jump L.f.1)
                                 (with-label L.f.1 (set! rsp r15))
                                 (set! rcx rdi)
                                 (set! rdx 1)
                                 (set! rbx 2)
                                 (set! rdx rdx)
                                 (set! rdx (bitwise-and rdx rcx))
                                 (set! rbx rbx)
                                 (set! rbx (bitwise-ior rbx rcx))
                                 (set! rdx (bitwise-xor rdx rbx))
                                 (set! rax rdx)
                                 (set! rax (arithmetic-shift-right rax 3))
                                 (jump rsp)))
                (string-join
                 (list "L.tmp.99:"
                       "mov rbx, r15"
                       "mov rcx, 10"
                       "mov rsp, 100"
                       "cmp rcx, rsp"
                       "jne L.tmp.101"
                       "jmp L.tmp.100"
                       "L.tmp.101:"
                       "mov rdi, 1000"
                       "mov r15, rbx"
                       "jmp L.f.2"
                       "L.tmp.100:"
                       "mov rdi, rcx"
                       "mov r15, rbx"
                       "jmp L.f.1"
                       "L.f.1:"
                       "mov rsp, r15"
                       "mov rcx, rdi"
                       "mov rdx, 1"
                       "mov rbx, 2"
                       "mov rdx, rdx"
                       "and rdx, rcx"
                       "mov rbx, rbx"
                       "or rbx, rcx"
                       "xor rdx, rbx"
                       "mov rax, rdx"
                       "sar rax, 3"
                       "jmp rsp")
                 "\n"))
  (check-equal? (generate-x64 '(begin (with-label L.tmp.89 (set! (rbp - 8) r15)) (set! rbp (- rbp 16)) (set! rdi 40) (set! rsi 48) (set! r15 L.rp.19) (jump L.+.16) (with-label L.rp.19 (set! rbp (+ rbp 16))) (set! (rbp - 0) rax) (set! rbp (- rbp 16)) (set! rdi 32) (set! rsi 40) (set! r15 L.rp.20) (jump L.*.17) (with-label L.rp.20 (set! rbp (+ rbp 16))) (set! rsp rax) (set! rdi (rbp - 0)) (set! rsi rsp) (set! r15 (rbp - 8)) (jump L.+.16) (with-label L.+.16 (set! rcx r15)) (set! rsp rdi) (set! rdx rsi) (set! rbx rdx) (set! rbx (bitwise-and rbx 7)) (compare rbx 0) (jump-if = L.tmp.87) (jump L.tmp.88) (with-label L.tmp.86 (compare rbx 6)) (jump-if != L.tmp.79) (jump L.tmp.80) (with-label L.tmp.88 (set! rbx 6)) (jump L.tmp.86) (with-label L.tmp.87 (set! rbx 14)) (jump L.tmp.86) (with-label L.tmp.80 (set! rax 574)) (jump rcx) (with-label L.tmp.79 (set! rbx rsp)) (set! rbx (bitwise-and rbx 7)) (compare rbx 0) (jump-if = L.tmp.84) (jump L.tmp.85) (with-label L.tmp.83 (compare rbx 6)) (jump-if != L.tmp.81) (jump L.tmp.82) (with-label L.tmp.85 (set! rbx 6)) (jump L.tmp.83) (with-label L.tmp.84 (set! rbx 14)) (jump L.tmp.83) (with-label L.tmp.82 (set! rax 574)) (jump rcx) (with-label L.tmp.81 (set! rax rsp)) (set! rax (+ rax rdx)) (jump rcx) (with-label L.*.17 (set! rdx r15)) (set! rbx rdi) (set! rcx rsi) (set! rsp rcx) (set! rsp (bitwise-and rsp 7)) (compare rsp 0) (jump-if = L.tmp.77) (jump L.tmp.78) (with-label L.tmp.76 (compare rsp 6)) (jump-if != L.tmp.69) (jump L.tmp.70) (with-label L.tmp.78 (set! rsp 6)) (jump L.tmp.76) (with-label L.tmp.77 (set! rsp 14)) (jump L.tmp.76) (with-label L.tmp.70 (set! rax 318)) (jump rdx) (with-label L.tmp.69 (set! rsp rbx)) (set! rsp (bitwise-and rsp 7)) (compare rsp 0) (jump-if = L.tmp.74) (jump L.tmp.75) (with-label L.tmp.73 (compare rsp 6)) (jump-if != L.tmp.71) (jump L.tmp.72) (with-label L.tmp.75 (set! rsp 6)) (jump L.tmp.73) (with-label L.tmp.74 (set! rsp 14)) (jump L.tmp.73) (with-label L.tmp.72 (set! rax 318)) (jump rdx) (with-label L.tmp.71 (set! rsp rcx)) (set! rsp (arithmetic-shift-right rsp 3)) (set! rax rbx) (set! rax (* rax rsp)) (jump rdx)))
                (string-join
                 (list
                  "L.tmp.89:"
                  "mov QWORD [rbp - 8], r15"
                  "sub rbp, 16"
                  "mov rdi, 40"
                  "mov rsi, 48"
                  "lea r15, [rel L.rp.19]"
                  "jmp L.$2b$.16"
                  "L.rp.19:"
                  "add rbp, 16"
                  "mov QWORD [rbp - 0], rax"
                  "sub rbp, 16"
                  "mov rdi, 32"
                  "mov rsi, 40"
                  "lea r15, [rel L.rp.20]"
                  "jmp L.$2a$.17"
                  "L.rp.20:"
                  "add rbp, 16"
                  "mov rsp, rax"
                  "mov rdi, QWORD [rbp - 0]"
                  "mov rsi, rsp"
                  "mov r15, QWORD [rbp - 8]"
                  "jmp L.$2b$.16"
                  "L.$2b$.16:"
                  "mov rcx, r15"
                  "mov rsp, rdi"
                  "mov rdx, rsi"
                  "mov rbx, rdx"
                  "and rbx, 7"
                  "cmp rbx, 0"
                  "je L.tmp.87"
                  "jmp L.tmp.88"
                  "L.tmp.86:"
                  "cmp rbx, 6"
                  "jne L.tmp.79"
                  "jmp L.tmp.80"
                  "L.tmp.88:"
                  "mov rbx, 6"
                  "jmp L.tmp.86"
                  "L.tmp.87:"
                  "mov rbx, 14"
                  "jmp L.tmp.86"
                  "L.tmp.80:"
                  "mov rax, 574"
                  "jmp rcx"
                  "L.tmp.79:"
                  "mov rbx, rsp"
                  "and rbx, 7"
                  "cmp rbx, 0"
                  "je L.tmp.84"
                  "jmp L.tmp.85"
                  "L.tmp.83:"
                  "cmp rbx, 6"
                  "jne L.tmp.81"
                  "jmp L.tmp.82"
                  "L.tmp.85:"
                  "mov rbx, 6"
                  "jmp L.tmp.83"
                  "L.tmp.84:"
                  "mov rbx, 14"
                  "jmp L.tmp.83"
                  "L.tmp.82:"
                  "mov rax, 574"
                  "jmp rcx"
                  "L.tmp.81:"
                  "mov rax, rsp"
                  "add rax, rdx"
                  "jmp rcx"
                  "L.$2a$.17:"
                  "mov rdx, r15"
                  "mov rbx, rdi"
                  "mov rcx, rsi"
                  "mov rsp, rcx"
                  "and rsp, 7"
                  "cmp rsp, 0"
                  "je L.tmp.77"
                  "jmp L.tmp.78"
                  "L.tmp.76:"
                  "cmp rsp, 6"
                  "jne L.tmp.69"
                  "jmp L.tmp.70"
                  "L.tmp.78:"
                  "mov rsp, 6"
                  "jmp L.tmp.76"
                  "L.tmp.77:"
                  "mov rsp, 14"
                  "jmp L.tmp.76"
                  "L.tmp.70:"
                  "mov rax, 318"
                  "jmp rdx"
                  "L.tmp.69:"
                  "mov rsp, rbx"
                  "and rsp, 7"
                  "cmp rsp, 0"
                  "je L.tmp.74"
                  "jmp L.tmp.75"
                  "L.tmp.73:"
                  "cmp rsp, 6"
                  "jne L.tmp.71"
                  "jmp L.tmp.72"
                  "L.tmp.75:"
                  "mov rsp, 6"
                  "jmp L.tmp.73"
                  "L.tmp.74:"
                  "mov rsp, 14"
                  "jmp L.tmp.73"
                  "L.tmp.72:"
                  "mov rax, 318"
                  "jmp rdx"
                  "L.tmp.71:"
                  "mov rsp, rcx"
                  "sar rsp, 3"
                  "mov rax, rbx"
                  "imul rax, rsp"
                  "jmp rdx")
                 "\n"))
  (check-equal? (generate-x64 '(begin
                                 (with-label L.tmp.105 (set! rsp r15))
                                 (set! rdi 1)
                                 (set! rsi 2)
                                 (set! r15 rsp)
                                 (jump L.f.1)
                                 (with-label L.g.1 (set! rsp r15))
                                 (set! rax 8)
                                 (jump rsp)
                                 (with-label L.f.1 (set! (rbp - 24) r15))
                                 (set! (rbp - 8) rdi)
                                 (set! (rbp - 0) rsi)
                                 (set! rsp 10)
                                 (set! rsp (+ rsp 6))
                                 (set! (rbp - 16) r12)
                                 (set! r12 (+ r12 rsp))
                                 (set! rbp (- rbp 32))
                                 (set! r15 L.rp.21)
                                 (jump L.g.1)
                                 (with-label L.rp.21 (set! rbp (+ rbp 32)))
                                 (set! rsp rax)
                                 (jump L.tmp.103)
                                 (with-label L.tmp.102 (set! rbx 10))
                                 (set! rbx (+ rbx 6))
                                 (set! rsp r12)
                                 (set! r12 (+ r12 rbx))
                                 (set! rbx 8)
                                 (set! rbx (bitwise-and rbx 8))
                                 (set! rax (rsp + rbx))
                                 (set! r10 (rbp - 24))
                                 (jump r10)
                                 (with-label L.tmp.104 (set! r10 (rbp - 0)))
                                 (set! r11 (rbp - 16))
                                 (set! (r11 + rsp) r10)
                                 (jump L.tmp.102)
                                 (with-label L.tmp.103 (set! r10 (rbp - 8)))
                                 (set! r11 (rbp - 16))
                                 (set! (r11 + rsp) r10)
                                 (jump L.tmp.102)))
                (string-join
                 (list "L.tmp.105:"
                       "mov rsp, r15"
                       "mov rdi, 1"
                       "mov rsi, 2"
                       "mov r15, rsp"
                       "jmp L.f.1"
                       "L.g.1:"
                       "mov rsp, r15"
                       "mov rax, 8"
                       "jmp rsp"
                       "L.f.1:"
                       "mov QWORD [rbp - 24], r15"
                       "mov QWORD [rbp - 8], rdi"
                       "mov QWORD [rbp - 0], rsi"
                       "mov rsp, 10"
                       "add rsp, 6"
                       "mov QWORD [rbp - 16], r12"
                       "add r12, rsp"
                       "sub rbp, 32"
                       "lea r15, [rel L.rp.21]"
                       "jmp L.g.1"
                       "L.rp.21:"
                       "add rbp, 32"
                       "mov rsp, rax"
                       "jmp L.tmp.103"
                       "L.tmp.102:"
                       "mov rbx, 10"
                       "add rbx, 6"
                       "mov rsp, r12"
                       "add r12, rbx"
                       "mov rbx, 8"
                       "and rbx, 8"
                       "mov rax, QWORD [rsp + rbx]"
                       "mov r10, QWORD [rbp - 24]"
                       "jmp r10"
                       "L.tmp.104:"
                       "mov r10, QWORD [rbp - 0]"
                       "mov r11, QWORD [rbp - 16]"
                       "mov QWORD [r11 + rsp], r10"
                       "jmp L.tmp.102"
                       "L.tmp.103:"
                       "mov r10, QWORD [rbp - 8]"
                       "mov r11, QWORD [rbp - 16]"
                       "mov QWORD [r11 + rsp], r10"
                       "jmp L.tmp.102")
                 "\n")))
