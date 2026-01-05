# Optimisations

The optimisation strategies in this project are not new ideas but
shamelessly stolen, some invaluable victims were:

+ [brainfuck optimization strategies](http://calmerthanyouare.org/2015/01/07/optimizing-brainfuck.html)
+ [An Optimising BF Compiler](https://www.wilfred.me.uk/blog/2015/08/29/an-optimising-bf-compiler/)

I will use C code for examples but all the transpilers in this project *should*
make use of these optimisations.

## Squashing

The brainfuck code `+++` by default would result in the C code
`tape[i]++;tape[i]++;tape[i]++;`. It is much more clean and likely quicker
to do `tape[i]+=3;`. The value of this is even more aparent when we
consider sequences of `+` and `-` such as `+++--` since we can look
at just the net change (`tape[i]++;tape[i]++;tape[i]++;tape[i]--;tape[i]--` 
becomes `tape[i]+=1;`), if a sequence has no net change it can be entirely
removed (for example `-+-+`).

## Clear Cells

A common idiom in brainfuck code is to use a loop `[-]` to clear a cell (set
it's value to `0`) in C we can better represent this as `tape[i]=0;`. Since
cells wrap around in brainfuck `[+]` does the same thing.

Performing this optimisation after squashing allows us to catch cases
like `[+-+]` (which is equivalent to `[+]`). 
