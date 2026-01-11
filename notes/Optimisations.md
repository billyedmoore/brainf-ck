# Optimisations

The optimisation strategies in this project are not new ideas but
shamelessly stolen, some invaluable victims were:

+ [brainfuck optimization strategies](http://calmerthanyouare.org/2015/01/07/optimizing-brainfuck.html)
+ [An Optimising BF Compiler](https://www.wilfred.me.uk/blog/2015/08/29/an-optimising-bf-compiler/)
+ [Optimizing brainfuck compiler](https://www.nayuki.io/page/optimizing-brainfuck-compiler)

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

## Pointless Loops

Brainfuck only has one type of loop `while (tape[i] != 0){}`, this means
at the point we exit a loop we can be sure that `tape[i] == 0`. As a result
the body of the second in a pair of back to back loops will never execute.

Similarly at the start of a brainfuck program the tape is initialised to
zeros (so `tape[0] == 0`). Therefore we can safely remove any loop before
the first non-loop instruction.

## DataArithmetic On Zeroed Cells

After certain operations (loops/clear cells) the tape cell currently pointed
at must be zero, this means rather than `tape[i]+=n` we can do `tape[i]=n`.

In the case of clear cells this means we can remove an instruction because
`tape[i]=0;tape[i]=n;` is equivalent to `tape[i]=n;`.

## Remove Redundant Instructions Before "Clobber"s

Certain Brainfuck instructions "clobber" the current cells value, i.e. they
set its value with no regard for the previous value. Before instruction that
clobber we can remove data arithmetic (`+` and `-`) as well as clear cells (`[-]`).

For example `[-]+++[-]---,` can just be `,`.

This is probably not very impactful because, one would assume, developers are not deliberately
including these pointless instructions in there programs. Every little helps though I 
suppose.

## Not (or Not Yet) Implemented Strategies

+ Multiply loops and other common loop patterns (such as moves).
+ Operation offsets for non-loop instructions.
+ Upper bound checking to set tape length (for implementations with limited tape length).
