# Milestone 3 Short-answer questions

## Exercise 5
### What are benefits and the disadvantages of each?
The benefits of the milestone 2 compiler is that it is simpler to implement and leads to faster compilation as it avoids using the quadratic time register assignment algorithm. It also leads to more understandable code as each abstract location maps onto a unique frame variable. A tradeoff is that faster compilation leads to slower runtime performance. Accessing frame variables requires memory access, and this latency leads to slower execution performance.

The benefits of the milestone 3 compiler is that it leads to faster runtime performance using the register assignment algorithm by reducing memory accesses in exchange for slower compilation. Implementing this compiler is also more complex because a single register can represent multiple abstract locations at different points in the program. This complexity can lead to the generated code being harder to understand.

### Is one optimal, and if so, in what sense?
The milestone 3 compiler is optimal in terms of runtime performance because it generates code that minimizes memory accesses. We could argue that the milestone 2 compiler is optimal in terms of compilation performance because it avoids using the quadratic-time register allocation algorithm.

### Try running and timing the same programs compiled with each compiler. Are there any surprises?
For small programs, something suprising was that the runtime performance of programs compiled with the milestone 2 and milestone 3 compilers were similar. We expect that as the size of the program increases, the runtime benefits of the milestone 3 compiler will become more apparent.
