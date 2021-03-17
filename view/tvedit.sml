							<PENTTI>TVGUID

 HINT:	The following commands allow you to read this manual using
	TV-Edit  ("$" stands for typing the 'ESC' or 'ALT MODE' before 
	typing the command character(s)):
	     $W    to read the next window,
	     $-W   to read the previous window,
	     $10G  to go to page 10, and
	     $$F   to finish (notice, TWO ESCs and an F).
	To speed printing, type $$4Y to shorten the window 
        (useful on slow terminals).

     TV-Edit is a page-oriented text editor.  It was originally writ-
ten for the IMSSS/Stanford AI Project PDP-1 computer and Philco dis-
plays by Brian Tolliver in 1965(?).  Four of its "descendants", all
written for the PDP-10, are currently in use at Stanford:  two at the 
AI Project use the DATA-DISC display system; one at IMSSS (by John 
Prebus, since 1970) uses the IMLAC PDS-1 display computer; and the 
present one at IMSSS and SUMEX (by Pentti Kanerva, since 1972) with 
versions for TEC Series 400 and DATAMEDIA Elite 2500 displays.

     As a rule, commands are entered "in the dark", i.e., the command 
characters themselves are not displayed as they are typed in.  How-
ever, once a command is completely entered, its effect on the file is 
immediately visible.  This instant feedback is invaluable in learning
to use the editor and in assuring the user that the right things are
happening to his file.

STARTING:
     WRITE MODE.  Option "W" implies  that  you  want  to  write, 
i.e., that the file will be modified.  However, if the file is not 
your own (not in connected directory), a file with the same name is 
created in your directory, leaving the original file intact.

     READ-ONLY MODE.  Option "R", for "Read-Only Mode", allows you to 
view the contents of the file, but protects against accidental modifi-
cation.  In addition to safety, read-only mode is considerably faster.  

        /R              Read only mode:  the default is read/write
        /E              The user has an "edit" key on his/her terminal
        /N              No edit key.
        /Pn             Go to page n.
        /Lm             Go to line m.
        /Yy             Set the number of lines displayed to y.
                        Can be reset by the editor Y command.
        /Xx             Set the number of columns displayed to x.
                        Can be reset by the editor X command.

		   I   insert mode, 
		   R   read-only mode,
		   L   lowercase input mode, and
		   U   uppercase input mode.  

     The general rule of TV-Edit is that straight typing goes to the
text file as such, unless you are in read-only mode, in which case 
straight typing is ignored.  To do anything else, you have to type 
EDITOR COMMANDS.  These commands allow you to move the cursor within 
the current window (pointing), view the file through successive win-
dows, search for characters and words, insert, delete, replace, and 
rearrange text (if in write mode), and, of course, finish the edit 
and save your work on a disk file.

It should be pointed out that
		$3G 	$3$G	[3G]	and	[3]G
are typed differently but mean the same thing in the editor.

     In general, TV-Edit commands can be thought of as one-letter
commands (more precisely, one-character commands) that can take  AR-
GUMENTS  (modifiers).  Arguments BEFORE the command letter usually 
give a (repeat)  COUNT;  arguments AFTER the command letter specify a 
TARGET.  If the count is omitted, 1 is assumed (exceptions:  "$G" and
"$W" commands).

     DOUBLE ESC.  Typing two ESCs before the command means one of two
things depending on the command:  Either

 (a) a huge count is to be used, or
 (b) a different function is to be performed.  

An example of the former is "$$G" or "[$G]" which takes to end of the
file; of the latter, "$$F" for finish ("$F" does nothing).  Commands 
for which the double ESC option signifies a different command are:

		     =, F, H, N, X, and Y.

These commands are explained later on. 

     If you start the command with two ESCs for a huge count, but 
continue with an honest count, the count given will be used.  Thus, 
"$$3G" goes to page 3.  Double ESC followed by count makes the count 
sticky for the following "again" command (explained later).

			   W  --  Window
     [W]      Display the next window.
     [-W]     Display the previous window.
     [3W]     Scroll 3 lines.

			     G  --  Go
     [G]      Go to (and display the beginning of) next page.
     [3G]     Go to page 3, line 1.
     [3.45G]  Go to page 3, line 45.
     [.G]     Go to current page, line 1.
     [.45G]   Go to current page, line 45.
     $$G      Go to end of file.

		X, Y  --  set window width, height
     $$11X    Make the window 11 columns wide.
     $$11Y    Make the window 11 lines high.

			    <  --  left
     [<]         Move cursor left 1 position.
     [22<]       Move left 22 positions.
     $$<         Move to left margin. also: [$<]

			    >  --  right
     [>]         Move cursor right 1 position.
     [22>]       Move right 22 positions.
     [$>]        Move to right margin.

			     ^  --  up
     [(n)^]      Move cursor up a line (n lines), to top line at most.

			   \, LF  --  down
     [(n)\]      Move down (n) line(s), to bottom line at most.
     'LF'        Move down a line.

			X, Y  --  absolute X, Y
     [55X]       Move cursor to column 55 on the current line.
     [11Y]       Move cursor to line 11 on the current column.

		   SPACE  --  right, by characters
     ['SPACE']   Point to the next character (the one on the right).
     [5'SPACE']  Point to the 5th character (on the right).
     [$'SPACE']  Point to end of line.

	       DEL, RUBOUT  --  left, by characters, or up, by lines
     'DEL'     Point to the previous character.
     ['DEL']   Same as 'DEL' alone, except up at beginning of line.
     [5'DEL']  Point to the 5th character on the left or 5th line above.
     [$'DEL']  Point to the beginning of current line or page.

		      )  --  right, by words
     [)]       Point to beginning of next word.
     [3)]      Point to beginning of 3rd word on the right.

		       (  --  left, by words
     [(]       Point to the beginning of previous word.
     [5(]      Point to the beginning of 5th word on the left.

		  CR  --  down (or up), by lines
     'CR'      Point to beginning of next line.  Start a new line 
	       if at end of file and in write mode.
     ['CR']    Same as 'CR' alone (no new line at end of file).
     [12'CR']  Point to beginning of 12th line down.	
     [0'CR']   Point to beginning of current line.
     [-'CR']   Point to beginning of previous line.
     [$'CR']   Point to page mark at end of current page.
     [$-'CR']  Point to page mark at beginning of current page.


     The commands in the first group ("<", ">", "^", "\") observe
the window boundaries but NOT end of line and page marks.  The
commands in the second group ('SPACE', "(", ")", 'DEL', 'CR') ob-
serve end of line and page marks (i.e., text) but NOT window bound-
aries.  Both respect the end of file ("X" and "Y" commands don't).

     OVERTYPING.  If you are pointing to a character and type, the
new character replaces the old.  Exceptions: 

 1.  TABs never replace other characters but are inserted in 
     front of them.

 2.  Regular characters never replace TABs (the last TAB).  The 
     TAB is pushed to the right.

 3.  Insert mode, which is explained later.

		     D 'CR'  --  Delete lines
     [D'CR']    Delete the current line.
     [5D'CR']   Delete 5 lines starting from the current line.
     [$D'CR']   Delete down to bottom of window or next page mark,
	        whichever comes first.
     [-3D'CR']  Delete current line and the 2 lines above.

		     D 'DEL'  --  Delete words
     [(n)D'DEL']  Delete the current word (and n-1 following words).
     [-5D'DEL']   Delete the current word and 4 preceding words.

	      D <letter>  --  Delete characters
	      K		  --  delete (Kill) characters
     [DD]	  Delete current character (the second "D" could be 
		  replaced by any letter).
     [K]          Same as "[DD]".  This is the preferred way to delete 
		  characters; you need to type only one letter.
     [11K]        Delete 11 characters.
     [$K]         Delete rest of current line.
     [-(n)K]      Delete (n) characters LEFT of the current character.

     The basic command for INSERTING is  $I.
     Page marks are insert-ed with the "P" command.
     To leave insert mode, type any pointing command.

     Things to notice about typing in insert mode:

     (a) DEL (RUBOUT key) without the EDIT key deletes the character 
to the left of the cursor (the one last inserted).

     (b) CR (without the EDIT key) inserts a blank line between the 
current line and the one below it.  An attempt to break a line into
two lines by inserting a 'CR' fails.  Use the "B" command for that 
(explained later).


		      I 'CR'  --  Insert lines
     [I'CR']      Insert a blank line above the current line.
     [5I'CR']     Push current line down 5 lines and insert 5 blank ones.
     [$I'CR']     Insert as many blank lines as there are lines from 
		  current line to bottom of screen.
     [-3I'CR']    Push current line up 3 lines and insert 3 blank lines.

		     I 'DEL'  --  Insert words
     [I'DEL']   (Prepare to) insert text in front of the current word.

		     P  --  insert a Page mark
     [P]        Insert a page mark above the current line.
     [D'CR']	Delete page mark.

     (b) The page marks limit the extent of the text-sensitive com-
mands 'CR', 'DEL', D'CR', S'CR', Z'CR', and 'quote'.  They can some-
times be utilized for terminating string execution.  ("S", "Z", and 
'quote' commands and string execution are explained later).

			^L  --  Leap, abort
     (CTRL)-L  Abort the command (or command string) currently 
	       being input or executed.  Flush TTY input buffer.

     NOTE:  Typing "^C" followed by "REE 'CR'" has a similar
     effect, but can also have undesirable side effects.  It
     is recommended only in an emergency, when the  ^L  fails.

		  ^O  --  suppress Output (shut up)
Every other  (CTRL)-O  stops window generation, every other starts it
up again.  Editing with self-calling strings can at times be speeded 
up considerably by letting the editor run "in the dark".

			    A -- Again
     [A]   Repeat the last command but use count 1.
     [5A]  Repeat the last command using count 5.
     $$A   Repeat the last command the way it was originally given
	   (true for most commands).

	     =  --  what's the character, where are we
     [=]   Print on message line the following (an example):
	     %A='141...X=59...P.L=20.38...17.45.. FILE1.;1

	   i.e., 
	   (a) current character is lowercase "A" ("%A" for lower-
	       case A, "^A" for (CTRL)-A), and its octal value is
	       141;
	   (b) cursor is at column 59 of page 20 line 38;
	   (c) page 17 line 45 is the first line in the temporary 
	       memory storage (that's how far you can back up with-
	       out having to wait); and 
	   (d) the name of the file is "FILE1.;1".

     $$=    Indicate, above the current line, the exact contents (upper-
	    case, lowercase, control characters) of the current line.  
	    The following symbols are used on the indicator line:

	    Indicator
	    character	      Real character	

	        .	space or regular punctuation (40-100,133-137)
	        ^	(TECs only) uppercase letter (101-132) 
	        _	(TECs only) lowercase letter (141-172) 
	        !	(TECs only) lowercase punctuation (140,173-176)
	    BOX or *	DEL, RUBOUT (177)
	        :	TAB (11)
	   left-L or \	end of line (12,15)
	        @	NULL character, CTRL-@ (0)
	      A...Z	CTRL-A,...,CTRL-Z (1-32)
	      [..._	CTRL-[,...,CTRL-_ (33-37)
			 (ESC is CTRL-[, TENEX EOL is CTRL-_)

     "$-N" lets you view the messed-up line again.
     "[A]" after an "=" command indicates the NEXT character or line.

		N  --  refresh screen ("N" for no-op)
     [4N]   Refresh (reprint) 4 lines starting from current line.
     [-4N]  Refresh 4 lines above the current line.
     [0N]   Refresh line 0 (the message line).
     [$N]   Refresh the entire screen.

			    H  --  shift
     [H]      Shift (from "L" to "U" or from "U" to "L") for the next 
	      input character.  
	      NOTE:  On TECs the preferred way to shift for one 
	      character is to type the lower-left key of the 5 x 3 
	      pad by itself.  On DATAMEDIAs use the SHIFT key.
     [8H]     Shift next 8 input characters (or until end of line).
     $$H      Shift lock:  Permanently shift the case.
	      NOTE:  On TECs the preferred way to shift-lock is to 
	      type the lower-left key of the 5 x 3 pad while the EDIT
	      key is depressed.  On DATAMEDIAs use the LOCK key.

		U, L  --  enter in Upper/Lower case
     [(n)U]   Enter next (n) input(s) in upper case.
     [(n)L]   Enter next (n) input(s) in lower case.
	      .U, .L  --  convert to Upper/Lower case
     [(n).U]  Convert (next n) character(s) to upper case.
     [(n).L]  Convert (next n) character(s) to lower case.

		    '  --  ASCII value in octal
     [']33'CR'     Enter the ASCII character whose value is 33 (octal)
		   in the file.  See "T" command.

		       S  --  character Search
     [Sa]          Point to the next "A" on the current line.
     [3Sa]         Point to the 3rd "A" (on the right).
     [-3Sa]        Point to the 3rd "A" on the left of the current 
		   character.

		      S 'DEL'  --  word Search
     [S(n)'DEL'A]  Point to the first (nth) word that is to the right 
		   of the current word and starts with an "A".
     [-S2'DEL'A]   Point to the 2nd word starting with "A" that is to 
		   the left of the current word.

		      S 'CR'  --  line Search
     [S'CR'A]       Point to the first line starting with "A" that is 
		    below the current line.  A line starts with "A" 
		    if the first visible character is "A".  SPACE and
		    TAB are invisible characters.
     [-3S'CR'A]     Point to the 3rd line starting with "A" that is 
		    above the current line.

		   T <string>  --  string search
     [100T]THE'CR'  Scan up to 100 LINES starting from current cursor 
		    position (+1) and point to the first occurrence of 
		    the word "THE".
     $$TEND'CR'     Scan for the first "END" that is to the right of 
		    or below the current character.

     NOTE:  The count refers to the maximum number of LINES that 
     are to be scanned.  The command is accepted in a standard 
     fashion up to, and including, the command letter "T".  The 
     string argument is accepted on the message line, where 'DEL' 
     can be used for editing the argument string (it deletes from 
     the rear).  The 'CR' that terminates the string argument is 
     displayed as a left-L or "\".  String search command that 
     is given without a target (with empty target string) uses 
     the most recent string target:

     $$T'CR'        Search for the next occurrence of the string that 
		    was last searched for.

		   Z  --  delete (Zap) characters
     [ZA]           Delete characters on the right up to the 3rd "A".
     [-3ZA]         Delete characters on the left up to and including 
		    the 3rd "A".

		  Z 'DEL'  --  delete (Zap) words
     [Z'DEL'A]      Delete words (on the right) up to the first start-
		    ing with "A".
     [-3Z'DEL'A]    Delete words (on the left) up to and including the 
		    3rd one starting with "A".

		   Z 'CR'  --  delete (Zap) lines
     [Z'CR'A]       Delete lines (downwards) up to the first starting 
		    with "A".
     [-3Z'CR'A]     Delete lines (upwards) up to and including the 3rd 
		    starting with "A".

			    O  --  Oops
     [O]          Undo the last command provided it was a delete.  If 
		  it was not a delete but you have been editing a line, 
		  restore the line to what it was when you started 
		  editing it.

		     R 'DEL'  --  Recover words
     [(n)R'DEL']  Recover (n) most recently deleted word(s) and insert
		  on the current line.
     $$R'DEL'     Recover the words deleted with the last word-delete 
		  command.  Observes word-delete count and direction.

		     R 'CR'  --  Recover lines
     [(n)R'CR']   Recover (n) most recently deleted line(s) and insert
		  above the current line.  Will undo "[(n)D'CR']".
     [-3R'CR']    Recover three most recently deleted lines and insert 
		  them below the current line.  Will undo "[-3D'CR']".
     $$R'CR'      Recovers the lines deleted by the last line-delete 
		  command the best it can.  Observes last line-delete 
		  count and direction (up or down).	

     NOTE:  The maximum number of lines recovered by one "R"
     command is restricted to the number of lines between the 
     current line and the top or bottom of window, depending 
     on the direction of the command.  It is therefore at 
     times necessary to execute the command more than once to
     get all the saved lines (maximum of 24) back in the file.  

     The editor "remembers" the number of items (words or lines) last
deleted, and the direction of deleting.  The double ESC version of 
the "R" command uses this information to limit the extent of the re-
covered material.  You can override the internal settings by giving 
your own sign and count with the command.

     The word and line recovery mechanisms are independent of each 
other.  No corresponding character recovery mechanism exists.

			B  --  Break a line
     [B]         Cut current line into two at where the cursor is.

		       J  --  Join two lines
     [J]         Join current line to the one below.
     [-J]        Join current line to the one above.

	     M 'CR', M 'DEL'  --  Move a line, Move a word
     [(n)M'CR']  Move the current line down over one line (n lines).
	         This command never moves past a page mark, or bottom
	         (or top) of window.
     [-2M'DEL']  Move current word left over two words.

	      C 'CR', C 'DEL'  --  Copy lines, Copy words
[5C'CR']    Duplicate this and next 4 lines.
	         The number of lines copied is restricted by a page 
	         mark or bottom (or top if "-C") of window.
     [C'DEL']    Duplicate the current word.

	     .D 'CR', .D 'DEL'  --  "remember" lines/words
     [.D'CR']    Save a copy of the current line for later recovering.
     [-3.D'CR']  Save copies of this and two preceding lines.
     [$.D'DEL']  Save copies of words from here to end of line.

     NOTE:  The ".D" command uses the same mechanisms as the
     "D" command for saving lines or words.  They are recovered 
     by the appropriate "R" commands.  In addition to leaving 
     the file intact (only the cursor moves), the ".D" command 
     causes internal counting whose function is to hang onto all 
     the lines (words) that have been stored with the corre-
     sponding ".D" command or subsequent delete commands.  When 
     the internal storage is full, a message asking you to re-
     trieve deleted lines (words) will appear.  A line (word) 
     once stored using the ".D" command has to be retrieved 
     sooner or later to get rid of that message and allow un-
     restricted deleting.

     A string in TV-Edit consists of the string ID character, also 
called the name of the string or the string call character, fol-
lowed by the effective string (the part that gets executed).  Most 
control characters are valid string ID characters.  The following
ones are not:  ^@,  ^C,  ^I,  ^J,  ^L,  ^M,  ^O,  ^T,  and  ^[.  The 
effective string may contain string calls (names of other strings), 
including calls of itself.  

     Some commands are restricted so as to work only if entered from 
the keyboard.  An attempt to execute such commands under string con-
trol results in (orderly) string execution abort, and a message to 
that effect will be displayed.  This is the editor program's way of
preventing excessive damage by runaway strings.

     In addition to illegal commands, string execution abort can be 
caused by other internal conditions, for example:

     (a) Execution has been under string control for the last 1000 
"input" characters.  This is a precaution against "looping" strings.

     (b) Search fails.  This makes automatic search-substitute fea-
sible.  It is safe to define a string "^Q[SA]B'DEL'^Q", where ^Q is 
the string ID) and use it to replace all "A"s by "B"s on a line.  
String execution terminates when the cursor reaches end of line and 
fails to find more "A"s.

     (c) String "attempts" to delete more lines (words) than can be
recovered.  It is always possible to undelete the lines (words) de-
leted by the string just executed.  No such precaution against char-
acter deletion exists.

     (d) String "attempts" to delete a page mark or write on a page 
mark.  A useful device for stopping self-calling strings.


		/  --  begin or end string definition

     [/]^QABC[/]  Define the string "^QABC", (CTRL)-Q is the string
		  ID, the effective string is "ABC".  This is the 
		  standard way of defining strings.
     [/]^WXYZ[A]  Define the string "^WXYZ^W".  The first (CTRL)-W 
		  is the string ID, the second (last) is a self-call
		  due to terminating the string definition with an 
		  "[A]" instead of "[/]".  The effective string is 
		  "XYZ^W".

	       R <string ID>  --  Retrieve a string
     [R]^Q  Writes (inserts) the string for  ^Q  in your file.

		 "  --  define a string from file
     ["]    Define a string:  Take the current character as string ID,
	    the immediately following text, up to "$/" or page mark, 
	    as the effective string.  

     The "R<ID>" command saves strings on the file the way 'quote' 
command wants to see them.

     MOVING TEXT.  The strings provide one more method of copying and
moving text.  You can use the 'double quote' command to store up to
a windowfull of text "under" a control character, and retrieve it 
with the "R" command (or by entering insert mode and typing the con-
trol character--risky if the text contains control characters which 
are string calls).  After retrieving, some clean-up might be neces-
sary.  To avoid overflowing the SAIL string space (and blowing up the
editor), store only few pages of text under control characters at 
once, and "undefine" the control characters as soon as you have re-
trieved the text you want.



Saving and Loading of Sets of Strings
------ --- ------- -- ---- -- -------

     Currently defined strings form a set.  File 'TVSTRI.NGS'
in your directory is for storing sets of strings.  Sets in the string
file are identified by the letters A,...,Z.  

     The set in memory at the time of finish is referred to as the 
MOST RECENT SET, and is automatically stored on page 0 of the string
file when finishing, and loaded when starting the editor.  This pro-
vides continuity from one editing session to the next.



     A text file can be permanently assigned to a set of strings, 
and commands exist for saving and loading the currently assigned set.
The assignment is made when the editor is started, as a response to 
the "(Strings)" request.


		.D <letter>  --  save (Dump) strings

     $$.Dx   Save current strings on page  x  of TVSTRI.NGS.
     $$.D.   Save current strings on page assigned to this file.

     NOTE:  The set in memory REPLACES the designated set in
     the string file.  If the set in memory is empty (as a
     result of "$$.R0", see below), the string file page in
     question is deleted.


		.R <letter>  --  load (Read) strings

     $$.Rx   Load (merge) strings from page  x  of the string file.
     $$.R.   Load strings assigned to the text file.
     $$.R0   Clear strings from memory.

     NOTE:  The set in the file is MERGED to the set in memory.
     Thus a string in memory is redefined only if it is defined
     in the set being loaded from file.


     If  x  is not a letter (non-0 digit, for instance), the most
recent set is assumed.

     

     




	    5. WHAT HAPPENS TO YOUR DIRECTORY AND FILES
	       ==== ======= == ==== ========= === =====



     In its current form TV-Edit uses two files for write-mode edit-
ing, OLD and NEW.  The old file is the one you specify at the be-
ginning of edit.  Let it be "FILE1.A".  The editor creates a new file
and gives it the name "TVSTRI.NGS".  When the message ".YOU.WILL
.HAVE.TO.WAIT..." appears, file "TVSTRI.NGS" is closed and holds
the updated file.  It is renamed to "FILE1.A", and the old "FILE1.A" 
vanishes.  Failure to rename "TVSTRI.NGS" to "FILE1.A" does not
mean that you have lost your work.  It is there, but under a new name.

     If an editing session is aborted either by  ^C  system call or
by a system crash, the "TVSTRI.NGS" file contains the beginning of 
the new file.  Consult with a knowledgeable individual to help recover 
the work from the aborted session.

     WARNING:  If you switch between SOS and TV-Edit, the SOS line
number are not displayed.  If you specify read-only mode, they of
course stay intact in your file, but they vanish if you edit in write
mode.  Also, BASIC line numbers are of the disappearing variety.






		     6. IN CASE OF TROUBLE
		        == ==== == =======


     1.  MESSED-UP SCREEN.  Type   $$N   to refresh.


     2.  RUNAWAY CURSOR.  Cursor is off the main screen and every com-
mand seems to just ring the bell.  Type

				[1X1Y]		

to get cursor to beginning of line 1.


     3.  RUNAWAY PROGRAM.  Try   ^L   first.  If that fails, type

	   ^C          to stop the program.  Once stopped, type
	   REE 'CR'    for REENTER, to get back to the editor,
	   $$N         to refresh the screen.
|
If that fails, type  ^C  again and give up.  You lose.


     4.  FILE "name.ext" LOST.  Check your directory for the file

			    TVSTRI.NGS

It possibly is the updated file. Check what is in the file (use Read- 
only mode).  Then if the file looks right rename "TVSTRI.NGS" to
 "name.ext".


     5.  You are typing to the EXEC instead of the editor.  You (acci-
dentally) typed  ^C.  If you want to save your editing, type

		    CONT 'CR'	to continue,
		    $$N		to refresh the screen.


     6.  MYSTERIOUS MESSAGES.  Some messages are there to help you, 
others are for debugging the editor program.  The latter are of the 
form:

		   cryptic name: cryptic story

where the name is a name of a procedure, and the story a description 
of the error condition.  These debugging messages are accompanied by 
long ringing of the bell.  Two are known to surface from time to time:  
"SETXY: EMPTY W[1]" when starting to edit an old file that is of zero 
length, and "LOPTAIL: TS > MAXS INITIALLY" when the old file contains 
lines longer than 132 characters.  Other debugging messages should be 
brought to the attention of the author.  It ought to be safe to con-
tinue after such messages.



APPENDIX I.  NOTATION USED IN THIS MANUAL
========     -------- ---- -- ---- ------


    $	     ESC key, also called  ALT MODE  or  ENTER.

  'ESC'	     ESC key.

  'CR'	     (Carriage) Return key.

  'LF'	     Line Feed key.

  'DEL'	     DELETE or RUBOUT key.

 'SPACE'     Space bar.

 (CTRL)-A    Control characters are typed with the aid of the CONTROL 
  CTRL-A       key.  Shown here are three "standard" ways of indicat-
   ^A	       ing CTRL-A.

 (EDIT)-A    Edit characters are typed with the aid of the EDIT key.

  [...]	     Brackets indicate typing that is done while holding
	       down the EDIT key.  (EDIT)-A  and  [A]  mean the same 
	       thing.

  <...>	     Brokets are sometimes used to enclose a description of	
	       required arguments.

  (...)	     Parentheses are sometimes used to enclose optional
	       arguments or information.

  "..."	     Double quotes are used inside text to delimit a command 
	       or a string.




APPENDIX II.  TEC/DATAMEDIA TV-EDIT COMMANDS
========      --- --------- -- ---- --------

NOTATION:

   $	 ESC, ALT MODE.  Instead of typing the ESC and then the 
	   command, you can hold down the EDIT key (orange on TECs) 
	   as you type the command.  They mean the same thing.
   ^	 Control character indicator
  CR	 (Carriage) Return key
  LF	 Line Feed key
 SPACE	 Space bar
  DEL	 DELETE or RUBOUT key 
   -	 minus sign before command letter is optional (its effect
	   is to do the command backwards)
   n	 count before command letter is optional (its effect is to 
	   "repeat" the command  n  times)
   w	 "word mode" option available (type 'DEL' after command 
	   letter to delete (insert, etc.) words)
   l	 "line mode" option available (type 'CR' after command
	   letter to delete (insert, etc.) lines)
   c	 command requires a target character 
   s	 command requires a (target) string argument
numbers	 page numbers in this manual


Pseudo-commands (not seen by the command interpreter):
------ --------

      ^A	Again.  Repeat last command.  (Predefined string 
		  call.)  20
      ^L	Leap.  Abort a command.  19
      ^O	Suppress output.  19

    SHIFT/	Invert keyboard input case.  Generated by lower-left-
  SHIFT LOCK	  corner key of the 5 x 3 pad (TECs only).  22


Genuine commands (seen by the command interpreter):
------- --------

 $n    LF	Cursor down.  14
 $-n   CR 	Cursor down, by lines.  15
 $-n  SPACE	Cursor right, by characters.  14

 $-n   DEL	Cursor left, by characters, or up, by lines.  15
 $	"	Define a string to be the text on a file.  28
 $	'  s	Enter a character in octal.  s  must be octal string.  23

 $n	(	Cursor left, by words.  15
 $n	)	Cursor right, by words.  15
 $	/	Begin/end of string definition.  27

 $n	<	Cursor left by columns.  14
 $	=	Where are we, what's the character?  20
$$	=	Indicate uppercase/lowercase/control characters of 
		  a line.  21



 $n	>	Cursor right, by columns.  14
 $n	\	Cursor down.  14
 $n	^	Cursor up.  14

 $-n	A	Again.  Repeat last command.  20
 $	B	Break a line.  26
 $-n	C  wl	Copy words or lines.  26

 $-n	D  wl	Delete words or lines.  16, 17
 $-n   .D  wl	Hide (store) copies of words or lines.  26
$$     .D    c	Dump strings in TVSTRI.NGS file.  29

$$     .D.	Dump strings in currently assigned set.  29
$$	F	Finish.  12
$$     .F	Save-and-Continue.  12

 $p.l	G	Go to Page  p  Line  l.  13
 $     +G	Go to next place according to Place List.  5, 6, 13
 $n	H	Shift next  n  input characters (for TECs).  22

$$	H	Shift lock (for TECs).  22
 $n	I  wl	Enter insert mode, insert.  17, 18
 $-	J	Join two lines.  26

 $-n	K	Delete characters.  17
 $n	L	Shift to lower case (for TECs).  22
 $n    .L	Convert text to lower case.  22

 $-n	M  wl	Move a word or a line.  26
 $	N	Refresh current line.  21
 $     0N	Refresh message line.  21

 $-n	N	Refresh  n  lines.  21
$$	N	Refresh entire screen.  21
 $	O	Oops.  Undo the delete.  25

 $-	P	Insert a page mark.  18
 $-n	R  wl	Recover words or lines.  25
 $	R    c	Retrieve a string, enter it in file.  28

$$     .R    c	Read strings from TVSTRI.NGS file.  29
$$     .R.	Read currently assigned set of strings.  29
$$     .R0	Clear strings.  29

 $-n	S  wlc	Character search.  23, 24
 $n	T  s	String search.  24
 $n	U	Shift to upper case (for TECs).  22

 $n    .U	Convert text to upper case.  22
 $-n	W	Window.  13
 $n	X	Cursor to column  n  of window.  14

$$n	X	Set window width to  n  columns.  11
 $n	Y	Cursor to line  n  of window.  14
$$n	Y	Set window height to  n  lines.  13

 $-n	Z  wlc	"Zap", delete to a target character.  24



  _|]‰