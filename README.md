# Modern Compiler Implement in ML

This is a compiler for the Tiger programming language, created from the projects in Modern Compiler Implementation in ML, by Appel.

## Milestone 1 -- Compiler Front-End (Lexical analysis & Syntax analysis)


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


## Milestone 3 -- Compiler Bakc-End (Code Generation)

**Note that Milestone 3 folder contains all Milestone 1&2 files**

Read chapters
    9 (Instruction Selection),
   10 (Liveness Analysis),
   11 (Register Allocation)
   12 (Putting it All Together)
Complete program sections from chapter 9, 10, 11, 12. For register allocation, I didn't implement spill.

I provided a syslib.s file which contains an RiscV32 system library compatible
with the Gem5 simulator (https://groups.google.com/a/groups.riscv.org/forum/#!topic/sw-dev/se0TVeaA_JI)

Start with a register allocator that works if the function fits in caller-save registers
(7 registers + 8 arguments); then extend to fit into callee-save registers (another 12 registers)
where you save all of them; then extend to save only overwritten callee-save registers; then finally
spill registers for functiosn that don't fit at all.

**The main.sml structure accepts additional arguments:**

             -a generate and print assembly
                  temporaries will be printed as T###
             -r allocate registers
                  all temporaries should become registers
                  but if not, then they will still be T###
                  
**The canon.sml do cononicalization:**

				Canon.commute :: TREE.stm * TREE.exp -> bool
            // as explained on p.175, the existing implementation works; 
            // but fails to allow most optimizations

**For riscvframe.sml:**

These functions will use a number of other values in RiscVFrame, things like RiscVFrame.calldefs, RiscVFrame.wordSize, RiscVFrame.externalCall, ...

1. PEE1: 
           
           RiscVFrame.procEntryExit1 :: FRAME.frame * TREE.stm -> TREE.stm
           
	As explained on p.261, it needs to pick up whatever information your register allocator figures out for the frame, and uses it to: 
     
     * move the arguments into the temporaries where the function body expects them to be.
     * move the result of a tiger function from the temporary result to the return value temporary.
2. PEE2:       

 			   RiscVFrame.procEntryExit2 :: FRAME.frame * [ Assem.instr ] -> [ Assem.instr ]

  As explained on pp. 208--208, it needs to set up the liveness properties of values needed at the end of a tiger function:
  
   * things like the old FP, SP, and the needed RA
3. PEE3: 

            FRAME.frame * [ Assem.instr ] -> { prolog :: string
                                             , body :: [ Assem.instr ]
                                             , epilog :: string }
As described on p. 209, this emits the dummy strings identifying the start and end of the assembly-language instructions for a given tiger function ... as described on p. 261, this needs to compute assembly-language instructions that perform the view shift at the start and end of the assembly-language for a tiger function (this is the subject of most of chapter 6, specifically pp. 124--136.)

**For makegraph:** 

Makegraph module is empty because I implement that flow-graph constructor function inside the RegAlloc structure -- after all, it's only ever called from there.
    
            MakeGraph.instrs2graph :: [ Assem.instr ] -> (Flow.flowgraph, [ Graph.Node ])
            // documented on p. 224
            MakeGraph.show :: Stream * Flow.flowgraph -> unit
            // This is a utility function

 **For Color module and RegAlloc module:**
 
 They are described on pp. 253--254 of the textbook. I use the Liveness implementation to see how to build programs from equational work-flow algorithms. Register allocation is just those same algorithms, done using different equations explained on pp. 242--250.


## Simulator and helper files

In the /TIGER folder, I've placed a number of files.  Most notably,

  * assemble 
    - a shell script that takes a single *.s file and 
    - assembles it with the system library into a RISCV32 executable
    - using the gnu toolchain in /opt/riscv32

 * run
   - a shell script that takes a RISCV32 executable and runs it
   - using the riscv32-qemu simulator from /opt/riscv32

and the support files it relies on:

  * main.c -- a simple RISCV32 entry point for LINUX

  * syslib.s -- the complete TIGER syslib for RISCV32 LINUX
  	  - only three routines interact with the OS:
		 - print(string),
		 - getchar():string,
		 - exit(int)
	    - the other routines are entirely self-contained

An example TIGER program, and the three stages:
   
   * ret2.tig -- a TIGER program
   * ret2.s   -- the compiled version of the TIGER program
   * ret2     -- the assembled version of the TIGER program

   
For example, my TIGER implementation compiled `ret2.tig` into `ret2.s`.  Then "compile `ret2.s`" built `ret2'`, which "run `ret2`" executes correctly.  In order for this to work, some conventions need to be followed:
	
* TIGER must emit the code for the main expression as the `tigermain`'	procedure
* my compiler emits calls to external functions (system library and other support routines) as calls to a label with the function name prefixed by "_"; this means my `env.sml'` contains the usual names; I've supplied that file.
* the system library must interoperate with the TIGER compiler; I've supplied my `syslib.s'`
		
My system library has been tested, you can see most of the test suite in `testSyslib.c` which might also help you to understand RISCV32.  

If you tried
             
    /opt/riscv32/bin/riscv32-unknown-elf-gcc -S main.c
    /opt/riscv32/bin/riscv32-unknown-elf-gcc -S testSyslib.c

then you'd get a `main.s`, and `testSyslib.s` which contain the RISCV32 assembly code for the C code.

