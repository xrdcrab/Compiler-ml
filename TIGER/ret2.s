        .file "ret2.tig"
        .ident "CMPT 442 tiger compiler output"

        .text
        .option nopic
        .align 1
        .globl tigermain

#PROCEDURE tigermain
tigermain:
	addi sp, sp, -4
	sw ra, (sp)
	
        la a0, aString
        jal _print
        li a0, 2

	lw ra, (sp)
	addi sp, sp, 4
        ret
#END tigermain

        .data
        .align 2
aString:
        .string "HELLO WORLD"
aChar:
	.string "X"
