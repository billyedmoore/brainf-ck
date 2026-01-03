# Brainf-ck

Program to interpret and transpile code from the esoteric
language [BrainFuck](https://en.wikipedia.org/wiki/Brainfuck)
performing minor optimisation along the way (grouping
sequential increments and decrements).

## General Usage

To interpret programs simply provide a `.bf` file.

```bash
brainf-ck your_program.bf 
```

To transpile programs provide an output file, the transpilation
language will be infered from the file extension of the output
file. Currently supported: `{"Bash":[".sh",".bash"],"C":[".c"]}`.

```bash
brainf-ck your_program.bf -o your_program.sh
```

## Run Tests

```bash
cabal test
```

