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
// computes factorial of n
let fac n = if n 
            then n * fac (n-1)
            else 1
in
    fac 6 // prints 720
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
