# hiss
Hiss is a hobby functional programming language.

## Features
- [x] Simple Haskell-like syntax
- [x] First-class functions
- [x] Simple tree-walk interpreter (see [src/Interpreter/TreeWalker.hs](src/Interpreter/TreeWalker.hs))
- [x] Lexical closures and partial function application
- [ ] Semantic pass with type checking
- [ ] Compiles to machine code
    - [ ] Lazy evaluation (maybe)

## Sample
A simple Hiss program (syntax is subject to change):
```
// computes nth fibonacci number
let fib n = if n <= 1
            then 1
            else fib (n-1) + fib (n-2)
in
    fib 10 // prints 89
```
## Commands
```bash
# build the project
stack build 
# run the project (cli in progress)
stack run
# run unit tests
stack test
```
