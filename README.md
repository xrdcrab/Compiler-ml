#Modern Compiler Implement in ML

This is a compiler for the Tiger programming language, created from the projects in Modern Compiler Implementation in ML, by Appel.

##Milestone 1 -- Compiler Front-End


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



##Milestone 2 -- Compiler Middle-End


