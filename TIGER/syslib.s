# syslib.s -- runtime library for MCI-SML on RISCV-32G
#
#  dutchyn@cs.usask.ca     2018/03/15

.file "syslib.s"
.ident  "CMPT 442 tiger compiler system library"

.text
.option nopic
.align 1

# all external calls (to functions in this library) do not expect a static link
# all routines expect to trash t*, and carefully manage a*
#        e.g. a7 is ra if not a tail-call

# strings will be .asciiz (null-terminated, word-aligned sequences of bytes)
# arrays are word-aligned blocks of data with a U32 length at the beginning

# STANDARD LIBRARY:    function print(s : string)
.globl _print
# print the string s -- simply call the `puts' function
# CALLS OUT TO OPERATING SYSTEM: assume it trashes t0--6, uses a0--6
_print:
        j puts

# STANDARD LIBRARY:    function flush()
.globl _flush
# flush output -- do nothing
_flush:
        ret

# EXTERNAL SUPPORT:        strComp(s1 : string, s2 : string) : bool
.globl _strComp
# compare two strings for deep equality
# trashes t0--1, uses a0--1
_strComp:
                beqz a0, _nilPtr
        	beqz a1, _nilPtr
		beq  a0, a1, __strEQ
__loop:         lb t0, (a0)
                lb t1, (a1)
                bne t0, t1, __strNE
                beqz t0, __strEQ
                addi a0, a0, 1
                addi a1, a1, 1
                j __loop
__strEQ:        li a0, 1
                ret
__strNE:        li a0, 0
                ret

# STANDARD LIBRARY:    function size(s : string) : int
.globl _size
# compute the length of the string s
# trashes: t0--1, uses a0
_size:
                beqz a0, _nilPtr
                li   t0, 0
__sizeLoop:     lb   t1, (a0)
                beqz t1, __sizeDone
                addi t0, t0, 1
                addi a0, a0, 1
                j __sizeLoop
__sizeDone:     mv  a0, t0
                ret


# STANDARD LIBRARY:        function concat(s1 : string, s2 : string) : string
.globl _concat
# concatenate two strings pointed to by $a0 and $a1
# trashes t0--t3 (allocWords), uses a0--a6, a7=ra
_concat:
                beqz a0, _nilPtr
                beqz a1, _nilPtr
                mv   a7, ra                # a7 == ra
                mv   a6, a0                # a6 == &s1
                jal _size
                mv   a5, a0                # a5 = |s1|
                mv   a4, a1                # a4 = &s2
                mv   a0, a1
                jal _size
                mv   a2, a0                # a2 = |s2|
                add  a0, a0, a5
                addi a0, a0, 1             # a0 = |s1| + |s2| + 1
                jal _allocBytes
                mv   a3, a0                # a0,a3 = &newString
__copyOne:      beqz a5, __copyTwo
                lb   t0, (a6)
                sb   t0, (a3)
                addi a6, a6, 1
                addi a3, a3, 1
                addi a5, a5, -1
                j __copyOne
__copyTwo:      beqz a2, __copyDone     # copies string@a4 of length a2 to a3 with ...
                lb   t0, (a4)
                beqz t0, __copyDone     # ... early exit if string@a4 isn't long enough
                sb   t0, (a3)
                addi a4, a4, 1
                addi a3, a3, 1
                addi a2, a2, -1
                j __copyTwo
__copyDone:     sb   zero, (a3)
                jr   a7                        # start of string is a0

# STANDARD LIBRARY:        function substring(s : string, first : int, n : int) : string
.globl _substring
# make a new string with first ... first+n-1 characters of s
# trashes t0--t3 (allocBytes), uses a0--a4, a7=ra
_substring:                                # a2 = |string to copy|
                beqz a0, _nilPtr
                bltz a1, __badIndex
                bltz a2, __badIndex
                mv   a7, ra                # prepare for __copyTwo to return
                add  a4, a0, a1                # a4 = & s[first]
                addi a0, a2, 1
                jal _allocBytes
                mv   a3, a0                # a0,a3 = & newString
                j __copyTwo                # which will do the return
__badIndex:     la a0, __badIndexMsg
                j __abort

##################################
# THE HEAP: 1MB of zeroed space
.bss
.align 2
__heapPtr:        .word  0
__heapSpace:      .space 1048576, 0
__heapTop:        .word  0

.text
##################################


# EXTERNAL SUPPORT:        allocBytes(b : int) : PTR
.globl _allocBytes
# allocate b bytes of memory (rounded up to multiple of 4) and return pointer
# trashes t0--3 (in malloc), uses a0
_allocBytes:
                addi a0, a0, 3
                srli a0, a0, 2
                # fall through

# EXTERNAL SUPPORT:        allocWords(w : int) : PTR
.globl _allocWords
# allocate w words of memory and return pointer
# trashes t0--3 (in malloc), uses a0
_allocWords:
                slli a0, a0, 2
                # fall through

# move the heap pointer by b bytes
# trashes t0--3, uses a0
__malloc:
                la   t0, __heapPtr        # t0 = &heapPtr
                lw   t1, (t0)
                bnez t1, __goodHeap
                la   t1, __heapSpace
                                        # t1 = old heapPtr
__goodHeap:     add  t2, t1, a0         # t2 = new heapPtr
                addi t2, t2, 3
                andi t2, t2, -3         # t2 = aligned new heapPtr
                la   t3, __heapTop
                bgt  t2, t3, __outOfSpace
                sw   t2, (t0)
                mv   a0, t1
                ret
__outOfSpace:   la   a0, __heapExhaustedMsg
                j    __abort



# EXTERNAL SUPPORT:        nilPtr()
.globl _nilPtr
# abort with error message regarding nil pointer
_nilPtr:
                la a0, __nilPointerMsg
                j __abort

# EXTERNAL SUPPORT:        indexOutOfBounds()
.globl _indexOutOfBounds
# abort with error message regarding array out of bounds
_indexOutOfBounds:
                la a0, __indexBoundsMsg
                j __abort


# STANDARD LIBRARY:    function not(b : boolean) : boolean
.globl _not
# indicate whether the argument is zero
# uses a0
_not:
                seqz a0, a0
                ret

# STANDARD LIBRARY:        function ord(s : string) : int
.globl _ord
# convert s[0] to ASCII (or -1 if |s| < 1)
# uses a0
_ord:
                beqz a0, _nilPtr         # sanity: is pointer valid?
                lb   a0, (a0)
                bnez a0, __goodChar
                li   a0, -1
__goodChar:     ret

# STANDARD LIBRARY:        function chr(i : int) : string
.globl _chr
# convert i into a 1-character string
# trashes t0--3, uses a0, a6, a7=ra
_chr:
                bltz a0, __badChar
                li   t0, 255
                bgt  a0, t0, __badChar
                mv   a7, ra
                mv   a6, a0
                li   a0, 2
                jal _allocBytes                # a0 = & newString
                sb   a6, (a0)
                sb   zero, 1(a0)
                jr   a7

__badChar:      la   a0, __badCharMsg
                j    __abort

# STANDARD LIBRARY:        function getchar() : string
.globl _getchar
# read one character from standard input
# CALLS OUT TO OPERATING SYSTEM: assume it trashes t0--6, uses a0--6
_getchar:
		addi sp, sp, -8
        	sw   ra, 4(sp)

		li   a0, 4              # only need 2 but we'll take 4
		jal  _allocBytes
		sw   a0, (sp)
		sw   zero, (a0)
		mv   a1, a0             # arg1 == buffer
                li   a0, 0              # arg0 == stdin (aka 0)
                li   a2, 1              # arg2 == 1 character
		jal  read               # a0 is 0 (EOF) or 1
		bltz a0, __readFail
		lw   a0, (sp)
		lw   ra, 4(sp)
		addi sp, sp, 8
		ret

__readFail:	la   a0, __readFailMsg
		# fall through

__abort:
                jal _print
        	li a0, 255
		# fall through

# STANDARD LIBRARY:        exit(i : int)
.globl _exit
# exit with error code integer in a0
# uses a0
__exit:
                j exit

		j    __abort

#########################
.data
.align 2
__heapExhaustedMsg:     .string "HEAP EXHAUSTED ... ABORTING\n"
__badIndexMsg:          .string "BAD CHAR INDEX ... ABORTING\n"
__nilPointerMsg:        .string "NIL POINTER ... ABORTING\n"
__indexBoundsMsg:       .string "ARRAY INDEX OUT OF BOUNDS ... ABORTING\n"
__badCharMsg:           .string "INVALID CHAR TO CHR ... ABORTING\n"
__readFailMsg:		.string "READ FAILED ... ABORTING\n"
#########################

