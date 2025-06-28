# hiss compiler
Hiss is my WIP hobby functional programming language. 
Read more about Hiss features and design goals on [my personal website](https://kerrigan.dev/hiss).

This repository contains the source code for the Hiss compiler `hissc`.

## Progress
- [x] Parser/lexer
- [x] Semantic passes
    - [x] Name checker
    - [x] Dependency checker
    - [x] Type checker
- [x] Codegen

## Commands
```bash
# build the project
stack build
# compile a hiss program
stack run -- samples/fib.hiss
# emit human readable assembly
stack run -- samples/fib.hiss --asm
# run unit tests
stack test
```
