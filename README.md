# Brainf-ck

Program to interpret and transpile code from the esoteric
language [BrainFuck](https://en.wikipedia.org/wiki/Brainfuck)
performing minor optimisation along the way (grouping
sequential increments and decrements).

## General Usage

To interpret programs simply omit the output file.

```bash
brainf-ck your_program.bf 
```

To transpile programs, the transpilation language will be infered
from the file extension of the output file. Although since only bash 
is supported that extension better be (`.sh` or `.bash`).

```bash
brainf-ck your_program.bf -o your_program.sh
```

## Run Tests

```bash
cabal test
```

