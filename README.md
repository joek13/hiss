# hiss compiler
Hiss is my WIP hobby functional programming language. 
Read more about Hiss features and design goals on [my personal website](https://kerrigan.dev/hiss).

This repository contains the source code for the Hiss compiler `hissc`.

## Progress
- [x] Parser/lexer
- [ ] Semantic passes
    - [x] Name checker
    - [x] Dependency checker
    - [x] Type checker
- [x] Simple tree-walk interpreter
- [ ] Codegen

## Commands
```bash
# build the project
stack build
# interpret a hiss program
stack run -- eval samples/collatz.hiss
# start a hiss REPL
stack run -- repl samples/collatz.hiss
# run unit tests
stack test
```
