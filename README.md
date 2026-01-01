# Brainf-ck

Program to transpile code from the esoteric language [BrainFuck](https://en.wikipedia.org/wiki/Brainfuck)
to (for now exclusively) Bash performing minor optimisation along the way (grouping
sequential increments and decrements).

## General Usage

```bash
# Generated Bash code is written to stdout
brainf-ck your_program.bf > out.sh
```

## Run Tests

```bash
cabal test
```

