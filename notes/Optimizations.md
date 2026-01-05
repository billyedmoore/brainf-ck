# Optimisations

The optimisation strategies in this project are not new ideas but
shamelessly stolen, some invaluable victims were:

+ [brainfuck optimization strategies](http://calmerthanyouare.org/2015/01/07/optimizing-brainfuck.html)
+ [An Optimising BF Compiler](https://www.wilfred.me.uk/blog/2015/08/29/an-optimising-bf-compiler/)

I will use C code for examples but all the transpilers in this project *should*
make use of these optimisations.

## Squashing

The brainfuck code `+++` by default would result in the C code
`tape[i]++;tape[i]++;tape[i]++`. It is much more clean and likely quicker
to do `tape[i]+=3`. The value of this is even more aparent when we
consider sequences of `+` and `-` such as `+++--` since we can look
at just the net change (`tape[i]++;tape[i]++;tape[i]++;tape[i]--;tape[i]--` 
becomes `tape[i]+=1`), if a sequence has no net change it can be entirely
removed (for example `-+-+`).
