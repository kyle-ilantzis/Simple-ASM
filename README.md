# Simple ASM

This is an exercise from [programming praxis](http://programmingpraxis.com/),
[Assembler Part 1](http://programmingpraxis.com/2014/04/15/assembler-part-1/) and
[Assembler Part 2](http://programmingpraxis.com/2014/04/18/assembler-part-2/).

## The Language

The possible opcodes can be seen in SimpleAsmCpu.hs at the top of the file.
Currently there are 10 opcodes. This simple architecture's words consist of 5
digits from 0 to 9. The first 2 digits are for the opcode and the last 3 for the
memory address. As a result, this architecture has 1000 addressable memory words.

## Part 1

The goal of part 1 is to create a parser for the language. The syntax rules are:

* The first field, if it exists, is a label.
* The second field, which is mandatory, is the opcode.
* The third field, which is used only with some opcodes, is the object.
*  The fourth field, which is optional, is a comment (which starts with #).

The modules involved are:

* Parser.hs
* SimpleAsmGrammer.hs
* SimpleAsmParser.hs
* SimpleAsmArchitecture.hs
* SimpleAsmCompiler.hs

Parser.hs is my custom parsing Monad that is a "mix" of the State and Error Monad.
I decided to make my own parsing Monad rather than to use the
[Parsec library](http://book.realworldhaskell.org/read/using-parsec.html).

SimpleAsmGrammer.hs is the "grammer" definition. Its purpose is to structuraly
represent a program. SimpleAsmParser.hs parses a string and returns its grammar
structure.

SimpleAsmCompiler.hs is the compiler for the simple assembly language. It
accepts an input file and produces an output file. It also provides simplistic
error reporting.

## Part 2

The goal of part 2 is to simulate the compiled assembly language. The modules
involved are:

* SimpleAsmArchitecture.hs
* SimpleAsmCpu.hs

SimpleAsmArchitecture.hs is the ALU of the simulator. Not mentioned in the
requirements is negative numbers. For the five digit words I've used tens
complement. The numbers are signed. A word with the fifth digit greater than or
equal to 5 is negative. Numbers can range from [-50000,49999].

SimpleAsmCpu.hs is the simulator for the simple opcodes. The opcodes **get** and
**put** which perform I/O haves as a prompts:

* **H<** for get and
* **H>** for put.

If an error occurs at runtime then the prompt **H!** with an error message will
appear and the program will halt.

## Compiling

To compile a simple assembly language source start with the haskell interpreter.
Currently there is no main function. This is because dealing with user errors
was not the focus of this exercise.

    ghci
    $> :l SimpleAsmCompiler
    $> main' "examples/sum.sasm" "examples/sum.slc"

In examples you should now see sum.slc (slc is for simple language code), a text
file containing only digits.

## Running

To simulate a program you also need to launch an interpreter. You should start
a new interpreter because both SimpleAsmCompiler and SimpleAsmCpu have functions
named **main'**.

    ghci
    $> :l SimpleAsmCpu
    $> main' "examples/sum.slc"

The simulator should now start up and run the program.

## Example Simulation

If you follow both the **Compiling** and **Running** sections properly then when
you enter

    $> main' "examples/sum.slc"

into ghci a simulation could go as follows:

    H< 5
    H< 2
    H> 7

The code for sum.sasm which instructs too accept two number and print their sum
has been properly simulated!

Next you could try out examples/mul.sasm which multiplies two input numbers.

## Problems

The simulator is rather slow. Try running examples/loop.sasm. It prints a 1
ten times but each 1 is printed after a busy loop of 49999 iterations. Each loop
from 0 to 49999 can take up to 3 seconds.

## Additions

Aside from the performance problem there are a few things that would be fun to
implement:

1. Support for subroutines
2. Support for arrays

I have yet to define the opcodes and syntax for these new functionalities.

## LICENSE

MIT
