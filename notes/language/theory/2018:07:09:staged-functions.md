# Partial evaluation

Given a program `prog : I_static x I_dynamic -> O`, where `I_static` is input known at compile time. Partial evaluation transforms `(prog, I_static)` into `prog* : I_dynamic -> O`. `prog*` is called a **residual program** and should run more effciently than the original program.

# Staged Computation
Given a function `f(m,n)` and `m`. By partial evaluation we obtain `f'(n)` automatically. This is the first stage. In the second stage the second argument `n` is provided to `f'` which does the rest of the job. This technique is commonly used in compilers to optimise a program.
