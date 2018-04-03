# Modern Compiler Implement in ML

This is a compiler for the Tiger programming language, created from the projects in Modern Compiler Implementation in ML, by Appel.

## Milestone 1 -- Compiler Front-End


Please note that

       A) I have enriched it with actual booleans.  As a result,
                1) there is another built-in type, named `bool'

                2) there is another expression in absyn.sml:
                             exp = ...
                                 | BoolExp of bool
                   with concrete syntax `true' and `false'

                3) the `if' and 'while' statements no longer rely on
                   zero or non-zero to distinguish the cases

                4) the various equality and inequality operators
                   (=,<>,>,<,>=,<=) produce boolean results.

                5) the translations of and (`&') and or (`|') change:

                       e1 & e2   <==> if e1 then e2 else false
                       e1 | e2   <==> if e1 then true else e2

                6) the standard library function `not' inverts boolean
                   values, not integers.
               
          This change will cascade through the compiler, especially
          the type-checker.

       B) There are two middle-ends supplied:

                      -a will print the abstract syntax in
                                        constructor form

                      -u will print the program out in (unparsed)
                                        concrete syntax form

          The Makefile may require customization of paths to
          accommodate your system configuration.  The default

                  make

          target should build the compiler.

       C) Tests and a rudimentary testing harness are included with the
          starter files.  Please try the

                  make test

          target to see what happens.

       D) Note that there are many edge cases in the syntax: see Appendix A
          in the textbook for complete details.

                1) there are many escape sequences allowed in strings

                2) a sequence expression must contain at least two
                   sub-expressions

                3) integer arithmetic has the usual precedence and
                   associativity

                4) assignment is lower precedence than boolean
                   operators



## Milestone 2 -- Compiler Middle-End (Semantic Analysis to Intermediate Representations)

**Note that Milestone 2 folder contains all Milestone 1 files**

As the starter for Middle-End, the Tiger book supply additional files:

      absyn.sml    -- Absyn structure: the abstract syntax

      errormsg.sml -- ERRORMSG signature and ErrorMsg structure

      main.sml     -- Main structure that with a main that parses
                      Tiger source files to abstract syntax and either
                         -p => prints in SML constructors
                         -u => unparses into concrete syntax

                         -e => turns on escape analysis
                         -c => turns on type checking and translation

                         -t => turns on trace scheduling

			 -x => turns on an interpreter back end

       temp.sig -- the TEMP signature
       temp.sml -- the Temp structure

       types.sml -- the Types structure

       tree.sml -- the TREE signature and partial Tree structure

       printtree.sml -- functions to display IR trees
       unparsetree.sml -- functions to print IR trees

       frame.sig -- the FRAME signature

       interp.sml -- the interpreter

The files I completed:

    semant.sml -- the Semant structure, including support for
                     * mutually recursive functions
                     * mutually recursive types
                     * checks for break outside while/for
                  and appropriate calls to Translate.

    env.sml -- the ENV signature and Env structure extended
                 to support nesting levels and static links
                 and populated with initial entries

    translate.sml -- the TRANSLATE signature and Translate structure
                     including the Ex, Nx, and Cx constructors

    riscvframe.sml -- a Frame structure for the RV32I architecture
                     handling k=8 formals passed in registers, k=2
                     return values in registers, and the rest on the
                     stack.  The ABI is documented in chapter 20 and
                     the instructions in Chapter 2 of the User-Level
                     ISA specification at
                     https://riscv.org/specifications/. We will use
                     the sp and fp registers, and at your discretion,
                     the gp register.  I will post the summary
                     (chapters 2--3) of "The RISC-V Reader" because
                     much of the specification details are irrelevant:
                     we are not interested in the supervisor-level
                     instructions (control registers etc...) and we're
                     targeting assembly language, so we don't care
                     about the bits-level encoding of the
                     assmebly-language instructions or their
                     compression.

    escape.sml -- the FindEscape structure with appropriate
                     modifications to transDec


## Milestone 3 -- Compiler Bakc-End 

