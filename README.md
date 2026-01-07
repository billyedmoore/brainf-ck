# Brainf-ck

Program to interpret and transpile code from the esoteric
language [BrainFuck](https://en.wikipedia.org/wiki/Brainfuck)
performing optimisation along the way.

Read about the optimisations [here](/notes/Optimisations.md).

## General Usage

To interpret programs simply provide the name of a file containing
brainfuck code.

```bash
brainf-ck your_program.bf 
```

To transpile programs provide an output file, the transpilation
language will be infered from the file extension of the output
file.

Supported languages:

| Language | Extensions |
| -------- | ---------- |
| bash     | `[".sh", ".bash"]` |
| c        | `[".c"]` |
| Haskell  | `[".hs"]` |
| LLVM ir | `[".ll"]` |
| [whitespace](https://esolangs.org/wiki/Whitespace) | `[".ws"]` |

Example:

```bash
brainf-ck your_program.bf -o your_program.c
```

## Run Tests

```bash
cabal test
```

