# HaskF#ck

## A BrainFuck implementation in Haskell

### Written by Wyatt Whiting - ver. 1.1, 7 Nov 2021

HaskFuck is a naive BrainFuck interpreter written in Haskell, intended to be ran with `GHCi`. Please try it out and see what you think!

Being an interpreter, this implementation does not perform any sort of optimization. Nor does it produce a binary, like a compiler might. Don't expect any fast execution. I wrote this program as a way to learn more about Haskell rather than as a tool for seriously studying the BrainFuck language.

### Starting the Interpreter

`$ ghci bf.hs`

### Running a Program

`*Main> runProgram "program_string"`

where `program_string` consists of a sequence of ascii characters and is the source code of the BrainFuck program you wish to run. If the program requires input, type each input prompt one at a time and press the `enter` key.

Warning: the `program_string` cannot contain any return characters. This will be fixed in the future.
