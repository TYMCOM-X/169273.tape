ASM - 8086 Assembler



Syntax:



		asm src_file [objfile [list_file]]



	where:

			src_file	is a file containing 8086 Assembler Source

						statements.

			obj_file	is the file where the resulting relocatable

						object file is placed.

			list_file	if specified, causes a assembler listing file

						to be created with this name



Examples:



			asm source.a object.o list.l

			asm test.a

			asm junk.a junk.o





Description

-----------



The 8086 Assembler converts assembler mnemonics into their machine

code representations and creates a relocatable object file.

The Assembler manages 3 independant relocatable segments where

code and/or data can be placed. Provisions are also made to allow

absolute memory locations to be used (if programs are to run in

ROM, for example). The relocatable segments can be linked with

the segments of other assembled or compiled programs, and then bound

to absolute memory locations by the linker.



Each line of the source file is assembled separately, and has

the following format:



	[label:] [prefix] opcode [arg1 [,arg2]]



Any characters following a semi-colon (;) are treated as comments.

Prefix may be any one of:



	lock

	rep

	repz

	repnz

	word

	w

	byte

	b

	abs



The recognized assembler statements are summarized in the following table:



mov		<r> , <r/m>				call		<expr>				aas

		<r/m> , <r>				jmp			.+<expr>			das

		<r/m> , #<expr>						.-<expr>			aam

		<r/m> , <sr>						@<r/m>				aad

		<sr> , <r/m>											cbw

								abs call	<expr>				cwd

push	<r/m>					abs jmp		@<r/m>				aas

pop		<sr>													daa

								je/jz		<expr>				xlat

xchg	<r> , <r/m>				jl/jnge		.+<expr>			lahf

		<r/m> , <r>				jle/jng		.-<expr>			sahf

								jb/jnae/jc						pushf

and		<r/m> , <r>				jbe/jna							popf

test	<r> , <r/m>				jp/jpe							movb

or		<r/m> , #<expr>			jo								cmpb

xor								js								scab

add								jne/jnz							lodb

adc								jnl/jge							stob

sub								jnle/jg							movw

sbb								jnb/jae/jnc						cmpw

cmp								jnbe/ja							scaw

								jnp/jpo							lodw

lea		<r> , <r/m>				jno								stow

lds								jns								clc

les																cmc

								loop			<expr>			stc

inc		<r/m>					loopz/loope		.+<expr>		cld

dec								loopnz/loopne	.-<expr>		std

neg								jcxz							cli

not																sti

esc								shl			<r/m>				hlt

mul								sal			<r/m> , cl			wait

imul							shr

div								sar								ret		<expr>

idiv							rol							abs ret

								ror

in		<expr>					rcl								into

out		[dx]					rcr								iret

																ret

int		<expr>												abs ret





where:



<r> ::=

	ax	al

	bx	bl

	cx	cl

	dx	dl

	si	ah

	di	bh

	bp	ch

	sp	dh



<sr> ::=

	es

	cs

	ss

	ds



							Default Segments

<r/m> ::=

	<r>						;ds

	<expr>					;ds

	[si]					;ds

	[di]					;ds

	[bx]					;ds

	[bp]					;ss

	<expr>[si]				;ds

	<expr>[di]				;ds

	<expr>[bp]				;ss

	<expr>[bx]				;ds

	[bx][si]				;ds

	[bx][di]				;ds

	[bp][si]				;ss

	[bp][di]				;ss

	<expr>[bx][si]			;ds

	<expr>[bx][di]			;ds

	<expr>[bp][si]			;ss

	<expr>[bp][di]			;ss



A segment override may also be accomplished by placing "<sr>:" in front

of the address (or index register). See the examples for the proper

syntax.



An expression consists of any arithmetic expression involving constants

or symbols. A symbol is a 1 to 10 character name consisting of

A-Z, a-z, "_", "$", "<", ">", or 0-9. The first character must not

be numeric.



The constructs "abs jmp", "abs call", and "abs ret" are used to

represent intersegment jumps and calls. This may differ from

the INTEL standard.



NOTE: that all mnemonics MUST be lower case since the assembler

distinguishes between upper and lower case characters. Thus

"TEMP", "Temp", and "temp" all represent different symbols.



The prefixes "byte" (or "b") and "word" (or "w") may be used

in the case of ambiguous memory references and I/O instructions

to force an 8 bit or 16 bit access. For example...



		shr	TEMP		;shift 16 bit word of memory

	b	shr	TEMP		;shift only 8 bits

	w	shr TEMP		;shift 16 bits



If no prefix is specified, then "word" is always assumed unless

an 8 bit register (such as "al") is referenced.



Constants

---------



Constants beginning with zero are assumed to be octal, unless

followed by the letter "h", in which case they are hex. All other

constants are decimal. For example...



	034		is octal

	34		is decimal

	034h	is hex

	34h		is also hex



A character enclosed in single quotes is a constant with

the value of that ASCII character.



Expressions

-----------



An expression may be any arithmetic or logical combination of symbols

and constants. Brackets may be used if desired, otherwise the normal

hierarchy of operations is assumed. Logical operations have lower

precedence than arithmetic operations.



Manifest Constants

------------------



Symbols may be predefined with constant values using the statement

"symbol = expression". Any further references to this symbol will

result in the symbol being replaced with a constant of the given

value. For example...



	CR	= 0dh		;define a carriage return

	LF	= 0ah		;define a line feed

	CRLF = CR*256 + LF	;newline

	;

	mov ax,#CRLF



Another method of defining constant symbols is with the "equ" statement.

"symbol equ expression" will define a constant symbol with the value of

the expression. All references to this symbol will assume that the symbol

is a defined label (absolute), so any expressions involving such symbols

will always result in 16 bit results and will never be shortened or

sign extended. For example...



	sub sp,#LOCALS

    ;

LOCALS equ 10



will always result in the long form of the "sub" statement being used, so

that the definition of "LOCALS" is allowed AFTER it has been used. The "="

statement must always be used BEFORE any references to the symbol.



Jumps and Calls

---------------



All jumps and calls are relative to the program counter (except for

indirect and itersegment transfers). The assembler automatically

calculates relative offsets when labels are used. The construct

".+<expr>" is dangerous unless one is sure about the lengths of

every instruction. A jump, call, or conditional jump will always

be turned into the shortest form by the assembler. Eight bit offsets

are used wherever possible, and conditional branches are converted

into a longer form if the destination label is out of range of an

eight bit offset. Thus the programmer need not know in advance how

far away a label is when using conditional branches.



In order to provide this flexibility in relative branches, the assembler

uses a two pass algorithm as well as lists of unresolved branches which

results in constant readjusting of instruction lengths as the first pass

is parsing the source lines. The result is object code which always

uses the shortest possible form of any branch statement.



Segments and Relocation

-----------------------



All memory references to locations in segments one, two, and three

are assumed to be relocatable. In segment zero, all references are

absolute and will not be relocated by the linker. If an "org"

directive is used,  then all further data is placed in segment

zero, and relocation is turned off. This allows an easy method

of creating programs which must run in fixed locations in memory

(such as ROM).



Segment one is normally used for code. Segment two is convenient

for placing tables, strings, and other constant data which must

still be referenced as data. Segment three can be used for

variables. When programs are loaded into memory, segments two

and three are concatenated into the same data segment by the

operating system. The first instruction in segment one (code

segment) is then executed. Initialized data in segment three

is converted into initialization code which is placed before

the first statement in the code segment, so that initialization

of variables in segment 3 is not actually performed until

run time. This allows many users to share the same code of a common

program, yet still be guaranteed that their version will start with

the same initialized data.



References to external symbols are defined when the object module

is linked to other modules by the linker. Any references to symbols

which have not been defined by the end of the program are assumed

to be external symbols. The "import" and "export" directives allow

the programmer to specify which symbols and labels are defined

within this program to be used by external programs, and which

symbols are to be found elsewhere.



Assembler Directives

--------------------



	seg n			Switch to data segment "n", where n = 0, 1, 2, 3.

					All further code and data will be placed in this

					segment

	org nn			Switch to segment zero and reset the location

					pointer to "nn". All further code and data will

					be placed in memory starting at this physical

					address (within a 64K address space).

	import symbol	Informs the assembler that this symbol is

					externally defined.

	export symbol	Informs the assembler that this symbol is

					defined within this program, but may be

					used by external programs as well.

	rmb n			reserve "n" bytes of memory (set to zero)

	end				This should be the last line in the program



Data Initialization

-------------------



The assembler also allows data bytes and words to be defined with

the "data" statement. The syntax is of the form:



	data	value [ ,value [ ,value [...] ] ]



A "byte" ("b") or "word" ("w") prefix may be used to specify the

size of the given values. A value can be a constant, symbol, expression,

or a string constant. String constants are ASCII characters enclosed

in double-quotes ("). String values are packed into word values

and are padded to an even word boundary if necessary by adding an

extra zero byte.



Examples

--------



	mov ax,3				;load ax with contents of memory location 3

	mov	ax,ss:3				;load ax with memory location 3 in the

							 stack segment

	mov ax,3[si]			;ax <-- ((si) + 3)

	mov	ax,-7[bx][si]		;ax <-- ((bx) + (si) - 7)

	mov ax,5[ss:bx][di]		;ax <-- ((bx) + (di) + 5) in stack segment

	mov ax,bx				;load ax with contents of register bx

	mov	ax,#5				;load ax with constant 5

	mov ax,es				;load ax with the extra-segment register

	mov	ax,TEMP				;load ax with contents of memory location TEMP

	mov	ax,#TEMP			;load ax with the value of TEMP



Example Program

--------------



			CONST	= 7

			loc 3			;switch to segment 3

A:		b	data 3			;reserve a byte with the value 3 labelled "A"

B:		w	data 4

TABLE:		rmb 20			;reserve 20 bytes of memory labelled "TABLE"



			loc 1

test:		mov ax,A

			cbw

			add ax,#CONST

			add 2[bx],ax

			inc bx

			ret

			end







  