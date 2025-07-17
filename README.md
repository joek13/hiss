# Hiss

Hiss is my WIP hobby functional programming language. Read more on [my website](https://kerrigan.dev/hiss).

## Project structure

```
.
├── hissc - hiss compiler
├── hissvm - hiss virtual machine
└── README.md
```

## By example

```
% cat samples/fib.hiss
// computes nth fibonacci number
fib(n) = if n <= 1
         then 1
         else fib(n-1) + fib(n-2)

main() = fib(13)
% hissc samples/fib.hiss --asm
Successfully compiled samples/fib.hiss
Wrote assembly to fib.hissa
% cat fib.hissa
.constants {
        hfunc 0 $_init
        hint 1
        hint 2
        hint 13
        hfunc 0 $main
        hfunc 1 $fib
}

.code {
fib:
        pushc 1
        loadv 0
        isub
        icmp leq
        br $fib_then
        jmp $fib_else
fib_else:
        pushc 2
        loadv 0
        isub
        loadg 0
        call
        pushc 1
        loadv 0
        isub
        loadg 0
        call
        iadd
        jmp $fib_finally
fib_then:
        pushc 1
        jmp $fib_finally
fib_finally:
        ret
main:
        pushc 3
        loadg 0
        call
        ret
_init:
        pushc 5
        storeg 0
        pushc 4
        storeg 1
        loadg 1
        call
        print
        halt
}
% hissc samples/fib.hiss
Successfully compiled samples/fib.hiss
Wrote bytecode to fib.hissc
% hissvm fib.hissc
377
```