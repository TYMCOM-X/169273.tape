			AN ANNOTATED OUTLINE OF TECO COMMANDS
			*************************************


META-NOTATION
=============
^x		Denotes the single character, control-x.
|		Used alone to denote alternation.
m | n | arg	Integer arguments.
string		String argument.
cmd		A command string.
$		Denotes the character altmode unless mentioned otherwise.
k		Denotes either "m,n", or "n"; a text range of characters m through
		n, or n lines.
file		Denotes a file name pair, e.g., "FOO BAR".
dir		Denotes a device and directory specification, e.g., "DSK: USERS;".


SPECIAL CHARACTERS
==================
^G		Quit to TECO top-level; cancels partial command line.
^Z		Interrupt to DDT.
altmode		Terminates text argument; two successive altmodes terminate
		command string.
rubout		Deletes last character typed in.

	QUOTE
	-----
^Q		Quote next character.

	MODIFIERS
	---------
: | @		Prefix to certain commands, modifies them.

	LOGICAL CONNECTIVES
	-------------------
&		Logical and, an arithmetic operator.
#		Inclusive or, an arithmetic operator.
^A		Exclusive or, an arithmetic operator.

	ARITHMETIC
	----------
0-9		Digits:  xxxx is interpreted in base FS IBASE$ (10)
		         xxxx. is interpreted in base FS I.BASE$ (8)
+		Addition, an arithmetic operator.
space		Same as "+", except that space, by itself, is not a non-null arg.
-		Subtraction, an arithmetic operator.
*		Multiplication, an arithmetic operator(with no operator precedence).
/		Division, an arithmetic operator(with no operator precedence).
( | )		Parentheses, grouping in arithmetic expressions.

	ARGUMENT SEPARATOR
	------------------
,		Separates numerical arguments.


THE BUFFER
==========
.		Value is number of char to left of pointer.
Z		Value is number of char in buffer*.
H		Equivalent to "B,Z", i.e., specifies whole buffer*.
B		Value is 0, i.e., beginning of buffer*.
		*Meaning is modified by use of virtual boundaries.

	INSERTING
	---------
Istring$	Insert string in buffer to left of pointer.
@Ixstringx	Insert string delimited by char x to left of pointer.
nI		Insert the char with ASCII code n.
m,nI		Insert m copies of the char with code n.
tab		Horizontal tab, self-inserting character.
backspace	Backspace, self-inserting character.
n\		Insert printed representation of n (in base ..E).
^^		Value = ASCII code of next char in cmd.
FI		Read one char from TTY and return its ASCII code as value.

	ASCII
	-----
mA		Value = ASCII code of char m positions to right of ptr.
		0A => char to immediate left of ptr.

	UPPER-CASE/LOWER-CASE
	---------------------
F$		Controls case conversion on input/output ($ => dollar).
kFC		Convert text range k to lower-case.

	MOVING AROUND
	-------------

		ABSOLUTE POSITION
		-----------------
mJ		Position ptr to after m-th char in buffer.

		RELATIVE CHARACTER POSITION
		---------------------------
mR		Move ptr left m char; R => 1R, -mR => mC.
m:R		Same as mR but returns -1(move OK) or 0(if move would cause an error).
mC		Move ptr right m char; C => 1C.
m:C		Returns value like m:R.
\		Move ptr right over number, return number's value.

		LINE POSITION
		-------------
mL		Move ptr to beginning of m-th line after current position;
		0L => beginning of current line.
m,nL		Same as m+n-.J, used by FW, etc.
m:L		Do mL, then backup over CR-LF; :l => end of current line.

		WORD POSITION
		-------------
nFW		Returns ".,m", a buffer range where m is the position immediately after
		the n-th word(contiguous string of non-delimiters; see ..D, the delimiter
		dispatch table).
-nFW		Returns "m,.", the range to the left.
n:FW		Same as FW, but stops before the n-th word; i.e., n:FWL = nFWL-FWL.
n@FW		Same as FW, but finds LISP atoms, not words.
nFWL		Do an nFW, yielding ".,m", move to m (=m+.-.J).
nFWR		Same as nFWL.

		LIST POSITION
		-------------
nFL		Returns ".,m", where m is the position immediately after the
		n-th list to the right.
-nFL		Returns "m,.", the range to the left.
n@FL		Same as nFL, but finds S-expressions, not lists.
nFU		Returns ".,m", where m is the position immediately after the n-th
		level up in parentheses to the right.
-nFU		Same as nFU, but returns "m,.", and works to the left.
nFD		Returns ".,m", where m is the position immediately after the n-th
		level down in parentheses to the right.
-nFD		Same as nFD, but returns "m,.", and works to the left.
nFLL		Does nFL, yielding ".,m", moves to m.
nFLR		Same as nFLL.

	DELETING
	--------

		ABSOLUTE POSITION
		-----------------
m,nK		Kill chars in the range m,n. Move ptr there.

		CHARACTER
		---------
nD		Delete n chars to right of ptr.
-nD		Delete n chars to left of ptr.

		LINE
		----
nK		Kill chars from . to position nL would have moved to.
		K => 1K, kill to beginning of next line.
n:K		Kill chars from . to position n:L would have moved to.
		:K => 1:K, kill to end of current line.

		WORD
		----
nFWK		Do an nFW returning ".,m"; kill chars in this range.
nFWD		Same as nFWK.

		LIST
		----
nFLK		Do an nFL returning ".,m"; kill chars in this range.
nFLD		Same as nFLK.


TYPE-OUT
========
kT		Type out text in range k (n lines or m,n chars).
kV		Display text in range k on CRT.
FTstring$	Types its string argument.
FVstring$	Displays its string argument.
kVW		Does kV, then waits for TTY input of one char whose ASCII code
		is returned as value.
k=		Types out k ("n" or "m,n") in current output radix(in ..E).


SEARCH
======
nSstring$	Find n-th occurrence of string searching forward and position ptr after it.
-nSstring$	Same as nSstring$, but search backward and position ptr before n-th string.
n@Sxstringx	Same as nSstring, where string is delimited by any char x.
n:Sstring$	Same as nSstring$, but returns value = -n if successful, 0 if not.
	Special chars in string:
	^B	Matches any delimiter(see ..D).
	^Nx	Matches any char but x.
	^O	Divides string into alternate patterns.
	^Qx	Quotes x.
	^X	Matches any char.
nNstring$	Same as nSstring$, but does P if end of buffer is reached.
n_string$	Same as nSstring$, but does Y if end of buffer is reached.
kFBstring$	Same as Sstring$ in the search domain defined by k; for k = m,n where
		m>n, search backwards; ":" and "@" modifiers work.
FK		Returns value = - FS INSLEN$ = - length of last string inserted, or found
		by search or FW.  FK < 0 except for backward search or FW.


SORT
====
^Pcmd0$cmd1$cmd2$	ASCII sort command, with the following algorithm:
		Move ptr to start of buffer;
		..0 := cmd0;  ..1 := cmd1;  ..2 := cmd2;
		Iterate:
			Ptr is at start of next record;
			Cmd0 moves ptr to start of key;
			Cmd1 moves ptr to end of key;
			Cmd2 moves ptr to end of record;
		Do a sort of records based on keys;
		Notes:	Dollar signs in the cmdi are replaced by altmodes.
			If FS ^P CASE$ .NEQ. 0, then ^P ignores case.


FILES
=====

	DIRECTORIES
	-----------
ETfile$		Set default filenames to file.
EL		Type out listing of default directory.
EYdir$		Same as EL, but with specified device and sname.
EM		Insert listing of default directory in buffer.
EZdir$		Same as EM, but with specified device and sname.

	OPEN READ
	---------
ERfile$		Opens file for input.
EPfile$		Does ERfile$, then bigprints filename twice on device open for writing.

	OPEN WRITE
	----------
EI		Open a file "_TECO_ OUTPUT" for writing on default device and sname.
0EI		Same as EI, but sets default device to DSK:.
:EI		Same as EI, but uses current filename defaults rather than "_TECO_ OUTPUT".
EWdir$		Same as EI, but with specified device and sname.
:EW dir file$	Same as EW, but also with specified filenames.

	INPUT
	-----
Y		Kills the buffer; inserts one page from current input file into buffer.
@Y		Kills the buffer; yanks the entire input file into buffer.
A		Append next page of input file to buffer (no arg allowed).
n:A		Append n lines or up to page boundary of input file to buffer.
@A		Append all of input file, and close file.
nP		Output buffer and "^L", kill buffer, read one page from input file;
		all done n times.

	OUTPUT
	------
nP		See description above under input.
m,nP		Output specified range of buffer, but do not clear it or input.
nPW		Output buffer and "^L", but do not clear it or input; done n times.
EEfile$		Same as "infinityP EFfile$ EC".

	CLOSE
	-----
EFfile$		Close the output file and change its name to "file".
EC		Close the input file.

	DELETE
	------
EDfile$		Delete the file.

	COPY
	----
E_old$new$	Make a copy of file "old" and name it "new".

	LINK
	----
EQfrom$to$	Create a link named "from" pointing to the file "to".

	RENAME
	------
ENold$new$	Rename the file "old" to have name "new".


Q-REGISTERS
===========
:Iqstring$	Insert string into Q-reg q, replacing prior contents.
n:Iq		Insert char with ASCII code n into Q-reg q, replacing prior contents.
@:Iqxstringx	Same as :Iqstring$, where string is delimited by any char x.
m,n:Iq		Same as n:Iq, but inserts m copies of char.
nUq		Inserts number n in Q-reg q; returns no value.
m,nUq		Inserts number n in Q-reg q; returns m.
kXq		Inserts text range k into Q-reg q, replacing prior contents.
k@Xq		Same as kXq, but appends text to q's contents.
kFXq		Same as X and K combined, i.e., "kXq kK".
nFWXq		Combines nFW with Xq.
nFLXq		Combines nFL with Xq.
Gq		Inserts text (or decimal representation of number) from Q-reg q into buffer.
		FS INSLEN$ is set to length of inserted string.
%q		Increments numeric contents of Q-reg q and returns result.
[q		Push text or number from Q-reg q onto Q-reg push-down list.
]q		Pop text or number from Q-reg push-down list into Q-reg q.
FQq		Value = number of char in Q-reg q or -1 if a number.


MACROS
======

	EXECUTE
	-------
m,nMqstring$	Execute string or buffer contents of Q-reg q as a TECO command.  If q 
		contains a number, execute the corresponding ^R-mode character.

	ARGUMENTS
	---------
^X		Value is m, first arg to m,nMq.  Only used inside macros.
^Y		Value is n, second arg to m,nMq (or only arg to nMq).
F^X		Value is all args to kMq.
F^Y		Value is number of args to kMq.
^]^X		Reads and returns string arg which follows kMq.

	ITERATION
	---------
n< cmd >	Command is executed n times, or indefinitely if n is null.
n;		Does nothing if n<0, otherwise passes control to char after next ">",
		i.e., terminates iteration.  Null n => use value of last search.

	TEST AND BRANCH
	---------------
arg"x then-cmd '"# else-cmd '
		Conditional which checks arg according to condition x; discards arg;
		executes then-cmd if condition was true, else-cmd if false.
arg"x then-cmd '
		Conditional without else-cmd.
argF"x		Same as arg"x, but passes first arg to then-cmd or else-cmd.
	Note:  The conditions x are:
	B	Arg is ASCII code for a delimiter(see ..D).
	C	Arg is ASCII code for a non-delimiter.
	E	Arg = 0.
	G	Arg > 0.
	L	Arg < 0.
	N	Arg .NEQ. 0.

	TAG
	---
!label!		Defines label, or brackets comments.

	GOTO
	----
Olabel$		Sends command execution to character after !label!.


Compiled by:	Mike Wirth (MCW@MIT-MC)
		Oct. 29, 1976
                                                                                                                                                                                                                                                                                                                                              