# DepuTy
### Build Instructions:
 
From a command line:
 1. ```cd <path-to>/DepuTy/src```
 2. ```ocamllex lexer.mll```
 3. ```ocamlyacc parser.mly```
 4. ```make depend```
 5. ```make```

You should now have an executable file 'lpi' in the top level directory!
If you don't, maybe you don't have the necessary Ocaml toolchain or something?

### REPL
 1. ```cd <path-to>/Deputy/```
 2. ```./lpi```

This should enter the interactive read-eval-print-loop, where you can line-by-line enter lpi commands and see all of the syntax errors you will inevitably make :).

Alternatively, the following sequence of commands:
 1. ```cd <path-to>/Deputy/```
 2. ```./lpi file.lpi```,

will run the interpreter on the source file ```file.lpi``` and print the result of the final line in the file.  Eventually I'll get around to writing a std lib to issue ```print`` calls so that you're not restricted to printing the result of the final sequentially executed instruction.

### lpi
Using the language is pretty clunky.  The syntax is still horrendous and I've provided a fairly restricted set of capabilities.  That being said, lpi is modelled after the dependently-typed lambda calculus, and as such has some pretty cool features.  Current implementation only supports a repl, but the goal is to write a compiler for ARM, which will expose the true benefits of static type-checking in a dependent type system that are lost in an interpreter.  The project is intended to be a model for simple implementation of a complex system, in an approachable context.  Most study on and implementation of depend type systems is done in languages such as Haskell (which have their own merits, approachable-ness not one of them), and feature techniques such as higher-order abstract syntax and quoting.  

