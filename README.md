# hiss
Hiss is a hobby functional programming language.

## Features
- [x] Simple Haskell-like syntax
- [x] First-class functions
- [x] Simple tree-walk interpreter (see [src/Interpreter/TreeWalker.hs](src/Interpreter/TreeWalker.hs))
- [x] Lexical closures and partial function application
- [ ] Semantic pass
    - [x] Name checker
    - [ ] Type checker
- [ ] Compiles to machine code
    - [ ] Lazy evaluation (maybe)

## Sample
A simple Hiss program, [collatz.hiss](samples/collatz.hiss):
```
// computes k mod n
let mod n k = if k < n
              then k
              else mod n (k-n)
in
// partial function application
let mod2 = mod 2 in
// computes total stopping time of n
let collatz n steps = if n == 1
                      then steps
                      else if mod2 n == 0
                           then collatz (n/2) (steps + 1)
                           else collatz (3*n + 1) (steps + 1)
in
    collatz 27 0 // should output 111
```
(syntax is subject to change)
## Commands
```bash
# build the project
stack build 
# run the project (cli in progress)
stack run
# run unit tests
stack test
```
