# Arrow
A framework for the Arrow assignment of the course Talen & Compilers at the University of Utrecht.

## Alex and Happy
If you use an up-to-date Cabal, you should be able to simply code in the .x and .y files.
Running `cabal build` (or similar commands) will compile the alex and happy code into Haskell
files, and compile your project with those, without ever showing them to you.

You can also manually call the alex and happy executables to do this first step,
but then you have to remember to do so on each change...

## File structure
The exercises are spread out over various (source) files. Please adhere to this distribution for the sake of your grader.
 - [open-questions.md](open-questions.md): Exercises 4 and 10
 - [src/Model.hs](src/Model.hs): Exercises 1 and 2
 - [src/Lexer.x](src/Lexer.x): Exercise 1, generating [src/Lexer.hs](src/Lexer.hs) with Alex
 - [src/Parser.y](src/Parser.y): Exercise 3, generating [src/Parser.hs](src/Parser.hs) with Happy
 - [src/Algebra.hs](src/Algebra.hs): Exercises 5 and 6
 - [src/Interpreter.hs](src/Interpreter.hs): Exercises 7, 8 and 9
 - [src/Driver.hs](src/Driver.hs): Exercise 11
