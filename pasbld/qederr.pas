$page QEDERROR -- Some Observations
(*   +--------------------------------------------------------------+
     |                                                              |
     |                      Q E D E R R O R                         |
     |                      - - - - - - - -                         |
     |                                                              |
     +--------------------------------------------------------------+


     STARTED: 17-Aug-77

     PURPOSE: This prints messages assocaited with a QED error code.

     USAGE:
	QEDERROR (F, CODE, LEVEL);

     INPUT: 

	F	   is the file to write the message is to be written.

	CODE       is  the QED error code for which the message is to
		   be printed.

	LEVEL      is the level of the message desired.  Level 1 is a
		   brief   message;   subsequent   levels  give  more
		   information.

     REQUIREMENTS: It is assumed that the  file  TTYOUTPUT  has  been
	opened before this program is called.

     ALGORITHM: If the level specified is 1 and the code appears in a
	special list (stored locally) then  '?'  is  printed  as  the
	message.   Otherwise,  a  file  is  searched  for  the  first
	message with matching code and level numbers.  A  code  of  *
	matches  any  code.  A  level  is  matched by any level value
	greater or equal to the value  specified.  So,  the  messages
	associated with the same code should be ordered by increasing
	level number.  The format of the file is this:  a  series  of
	messages  separated  by blank line(s).  The last message must
	be followed by a blank line.  A single  message  starts  with
	<code>,<level>.  The  rest is text which is parsed into words
	and output in fill mode to fit the width of the terminal.

     RESPONSIBLE: Software Tools

     CHANGES: 09/25/81 - djm - Added VAX code for opening QEDERR.MSG file.
              05/04/82 - djm - Added M68000 code for opening QEDERR.MSG file.

     ---------------------------------------------------------------- *)


MODULE QEDERRORppas
  OPTIONS SPECIAL;
$SYSTEM filutl.inc
$SYSTEM infpac.inc
$SYSTEM wio.inc
(*
$IF VAX
$SYSTEM imgnam.inc
$END
*)
$SYSTEM cmdutl.typ
$SYSTEM cmdutl.inc
$SYSTEM query.inc
$SYSTEM wio.typ
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM qedtyp.typ
$SYSTEM qspat.inc
$SYSTEM qspred.inc
$SYSTEM qld.inc
$SYSTEM qread.inc
$SYSTEM qedln.inc
$SYSTEM qmark.inc
$SYSTEM qprint.inc
$SYSTEM qsubst.inc
$SYSTEM qjoin.inc
$SYSTEM qsplit.inc
$SYSTEM qopen.inc
$SYSTEM qed.inc
$SYSTEM qlabel.inc

$PAGE QEDERROR -- The Code
TYPE QEDERRLEVEL = 1..10;

VAR ERRFILE: TEXT;

PUBLIC PROCEDURE QEDERROR (VAR F: TEXT; CODE: QERRCODE; LEVEL: QEDERRLEVEL);

 VAR MSGCODE: 0..255;				(* code read from file *)
     MSGLEVEL: QEDERRLEVEL;			(* level readfrom files *)
     WORD: STRING[32];				(* assembled word from message *)
     COLUMN: 0..255;				(* output column *)
     ERRFILE_NAME: FILE_NAME;                   (* complete error file name *)
$IF P10 JOBSTUFF: JOBREC;                       (* for error file PPN *)
$IF VAX IMGERR: IMAGE_ERROR;                    (* image_file_name error *)


 (* writes text to TTY, filling to width of terminal *)

 PROCEDURE OUT;
  BEGIN
   IF (COLUMN + LENGTH (WORD)) > 72 (* typical terminal width *) THEN BEGIN
     WRITELN (F);
     COLUMN := 0;
     IF WORD  = '' THEN RETURN
   END;
   COLUMN := COLUMN + LENGTH (WORD);
   WRITE (F, WORD)
  END;


 BEGIN
  COLUMN := 0;

  IF (LEVEL = 1) AND (CODE > QFATAL) THEN BEGIN	(* brief message *)
    WRITELN (F, '?'); BREAK;
    RETURN
  END;

$IF P10
  JOBINFO (JOBSTUFF);
  ERRFILE_NAME := 'QEDERR.MSG' || JOBSTUFF.PROGDIR;
$END
$IF VAX
  IMAGE_FILE_NAME (ERRFILE_NAME, IMGERR);
  ERRFILE_NAME := SUBSTR (ERRFILE_NAME, 1, SEARCH (ERRFILE_NAME, [']'] )) ||
                  'QEDERR.MSG';
$END
$IF M68 ERRFILE_NAME := '1000..QEDERR.MS';
  OPEN (ERRFILE, ERRFILE_NAME );   (* get file *)
  IF EOF (ERRFILE) THEN BEGIN			(* fatal situation *)
    WRITELN (F, 'QED error file missing.'); BREAK;
    RETURN
  END;

  LOOP						(* search for message in file *)
    REPEAT READLN (ERRFILE) UNTIL NOT EOLN (ERRFILE) OR EOF(ERRFILE);	(* skip blanks before start of msg *)
    IF EOF (ERRFILE) THEN BEGIN			(* code + level not found *)
      WRITELN (F, 'Error not found.', ORD (CODE):5); BREAK;
      RETURN
    END;
    IF ERRFILE^ = '*'				(* get errcode from file *)
      THEN BEGIN				(* '*' matches any code *)
	MSGCODE := ORD (CODE);			(* force match *)
	GET (ERRFILE);				(* eat comma following asterisk *)
	GET (ERRFILE)				(* and get first digit of following number *)
      END
      ELSE READ (ERRFILE, MSGCODE);
    READ (ERRFILE, MSGLEVEL);			(* get level from file *)
  EXIT IF (MSGCODE = ORD (CODE)) AND (MSGLEVEL >= LEVEL);
    REPEAT READLN (ERRFILE) UNTIL EOLN (ERRFILE);   (* skip til blank line following msg *)
  END;

  REPEAT					(* output msg *)
    WHILE ERRFILE^ <= ' ' DO GET (ERRFILE);	(* skip control chars at start of msg *)
    WHILE NOT EOLN (ERRFILE) DO BEGIN		(* extract words from line *)
      IF ERRFILE^ > ' ' THEN BEGIN
	WORD := '';
	REPEAT
	  WORD := WORD || ERRFILE^;
	  GET (ERRFILE)
	UNTIL ERRFILE^ <= ' ';
	OUT; WORD := ' '; OUT;			(* write word followed by blank *)
      END
      ELSE GET (ERRFILE);			(* ignore control chars, i.e. white space *)
    END;
    READLN (ERRFILE);				(* go to next line *)
  UNTIL EOLN (ERRFILE);				(* message terminated by blank line *)
  WRITELN (F); BREAK;				(* terminate msg *)
  CLOSE (ERRFILE);				(* clean up our dirty laundry *)
 END.
