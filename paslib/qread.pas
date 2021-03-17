$PAGE QREAD -- QED Terminal Input Interface
(*   +--------------------------------------------------------------+
     |                                                              |
     |                         Q R E A D                            |
     |                         - - - - -                            |
     |                                                              |
     +--------------------------------------------------------------+

     MDSI, COMPANY CONFIDENTIAL

     STARTED: 29-Jul-77

     PURPOSE: Reads an input line, including control characters.

     USAGE:
	external function qread : qstring;
	var newline: qstring;

	newline := qread ;

     INPUT: 

	None.

     OUTPUT:

	newline is the line from TTY input.

     REQUIREMENTS: The files TTY and  TTYOUTPUT  must  be  open  when
	QREAD is called.

     NOTES: If  the  input  line  is  too  long,  an  end-of-line  is
	simulated  and  TTY  is  left  in the middle of a line - i.e.
	EOLN  (TTY)  is  false.  A  subsequent  call  picks  up   the
	remainder as a separate line.  Only CR and ESC will be treated
	as end of line characters.

     RESPONSIBLE: Software Tools

     CHANGES: 7/2/79 P. LEE - Changed EDITREAD to QREAD and deleted
	      intra-line editing. This header also changed.
	3/23/81 QQSV - A substantial rewrite of this code to eliminate
		special EOF code and to make line wraparound and long
		lines work properly.  Header updated to reflect change.
	3/31/81	QQSV - Added JUNKLINE logic to prevent lines of QSTRINGLEN
		characters from generating a null line if first line ends
		with CR-LF.  This is a hack; it can be eliminated if the
		run time code ever recognizes CR-LF as a line end.

        9/24/81 djm  - Removed duplicate const declarations of lf, cr, and esc,
                       which are now declared in QED.INC.

       10/05/81 djm  - Added VAX code to read a line from a text file that
                       contains control characters.

        5/07/82 djm  - Changed $IF VAX to $IFANY (VAX, M68).

     ---------------------------------------------------------------- *)

MODULE QREAD;

PUBLIC FUNCTION QREAD: QSTRING;

VAR	NEWLINE: QSTRING;			(* A convenient input buffer	*)
	JUNKLINE: BOOLEAN;			(* If line should be ditched	*)

BEGIN
$IFANY (VAX, M68)
  IF EOLN(TTY) THEN
    READLN(TTY);
  READ(TTY,QREAD);
  RETURN;
$END
$IF P10
QREAD := '';					(* Nothing yet	*)
  REPEAT					(* Until line legitimately complete	*)
  JUNKLINE := FALSE;				(* Looks good so far	*)
  IF EOLN (TTY)
  THEN
    BEGIN					(* Get more input, if necessary	*)
    IF (TTY^ <> CR) AND (TTY^ <> ESC)		(* Include prior EOLN character if not CR or ESC *)
    THEN QREAD := QREAD || TTY^;
    READLN (TTY)
    END
  ELSE JUNKLINE := TRUE;			(* May be phony wraparound	*)
  READ (TTY, NEWLINE: QSTRINGLEN - LENGTH (QREAD));
  IF LENGTH (NEWLINE) > 0
  THEN						(* Concatenate new stuff onto end	*)
    BEGIN
    IF EOLN (TTY) AND (TTY^ = LF) AND (NEWLINE[LENGTH (NEWLINE)] = CR)
    THEN
      BEGIN					(* CRLF ==> CR. Fake it for line end	*)
      TTY^ := CR;
      IF LENGTH (NEWLINE) > 1
      THEN					(* Definitely a real line	*)
	BEGIN
	QREAD := QREAD || SUBSTR (NEWLINE, 1, LENGTH (NEWLINE) - 1);
	JUNKLINE := FALSE			(* So make sure we keep it	*)
	END
      END
    ELSE
      BEGIN
      QREAD := QREAD || NEWLINE;
      JUNKLINE := FALSE				(* Keep this line	*)
      END
    END
  UNTIL (EOLN (TTY) AND ((TTY^ = CR) OR (TTY^ = ESC)) AND NOT JUNKLINE)
	OR (LENGTH (QREAD) = QSTRINGLEN)
$END
END.						(* We got enough for now	*)
