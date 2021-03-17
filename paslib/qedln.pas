$PAGE QEDLN -- Text Buffer Manager for QED
MODULE QEDLN OPTIONS SPECIAL;
(*   +--------------------------------------------------------------+
     |                                                              |
     |                  	q e d l n                           |
     |                  	- - - - -                           |
     |                                                              |
     +--------------------------------------------------------------+

     mdsi, company confidential

     started: 21-apr-77

     purpose: this package contains the basic routines which manage a
	qed text buffer.

     usage:
	entry points...
	  qinitbuf.....initialize a text buffer.
		      this routine should only be called once with a
		      buffer, prior to its first use.
	  qdelbuf......delete (release) the contents of a text buffer.
		      whenever a buffer is to be reused, this routine
		      should be called, rather than qinitbuf.
	  qgetline.....return the text of a line.
	  qmodline.....change the text of a line.
	  qaddline.....add a new line to the buffer.
	  qdellines....delete line(s) from the buffer.
	  movelines...move line(s) in the buffer.
	  copylines...copy (duplicate) line(s) in the buffer.

     requirements: this  package  uses  the qed string routines,  and
	the text returned by these routines  is  in  the  qed  string
	form.

     algorithm: line  descriptors  are  maintained in a doubly linked
	list with header information in the passed buffer descriptor.
	the line descriptors in turn contain qed strings.

     notes: since  any  of  the qed line operations may be broken out
	of,  care is taken in  the  management  of  the  line  chain.
	while  storage  may  be  lost  as  the result of breaks,  the
	following strategy insures that  these  routines  will  never
	leave  the line chain in a confused state,  i.e.,  will never
	partially complete operations.

	1.  lastlineno is always correct.

	2.  getlinep and lastlinep may  not  be  defined  (are  nil),
	even  if  the  buffer  contains  text.  whenever an operation
	changes getlineno or lastlineno,  the corresponding  xxxlinep
	is niled first,  then updated after the xxxlineno is changed.

	3.  the back line chain may be broken,  i.e.,  a qline  other
	than  the  first  one  may have prevlinep=nil.  however,  the
	forward chain is always complete.

	4.  the definitive test of an empty buffer  is  lastlineno=0,
	in  which  case,  the other contents of the buffer are assumed
	to be arbitrary.

     responsible: a. kortesoja

     changes: 
	12/11/78 smr changed qfilewrite to check for write errors.
	12/11/78 smr added parameter to qfilewrite which indicates
		     whether new/old file prompting is desired.

	7/30/79 P.Lee  Changed QSETBOUNDS & MAP to use an offset in
			the buffer for bounded line addressing. Also
			the added option of using the entire buffer
			or the bounded buffer in bounding .

        9/17/81 djm    Replaced old MASK/UNMASK/PUSHESCAPE/ESCPOP/FIRESCAPE
                       attention handling with the new Exception Handling
                       constructs available in Pascal.

        9/21/81 djm    Removed $IF ANC code in procedure makeline, and removed
                       procedures QTAG, QTAGSET, and QTAGCLEAR.  This code is
                       still present in the ANC version.

        9/24/81 djm    Removed duplicate const declarations of lf and cr, 
                       which are now declared in QED.INC.

        9/25/81 djm    Added VAX code to QFILEAPPEND to read in lines from
                       a text file that contains control characters.

        9/28/81 djm    Added $IF P10 switches around certain system dependent
                       portions of the 940 file handling code.

       10/01/81 djm    Added more $IF P10 switches around more system dependent
                       portions of the 940 file handling code.

        5/17/82 djm    Added initialization of S940 flag in qfilewrite for
                       non-P10 code.

     ---------------------------------------------------------------- *)



$PAGE move
(* MOVE is the ultimate procedure for all line manipulations, including
   additions, deletions, and real moves.  It handles all the bookkeeping
   for updating the special line numbers and pointers.  It is assumed that
   all line numbers and pointers passed to this routine are reasonable. *)


PROCEDURE MOVE
     (	VAR BUF: QBUFFER;			(* buffer to manipulate *)
	FLN: QLINENO;				(* addr of first line of section to be moved, if
						   zero, lines are new additions from garblist *)
	FLP: QLINEP;				(* ptr to above line *)
	LLN: QLINENO;				(* addr of last line to be moved, if fln = 0 then
						   this is #lines - 1 to yield proper count *)
	LLP: QLINEP;				(* ptr to above line *)
	TLN: QLINENO;				(* addr of line after which text is to be moved *)
	TLP: QLINEP   );			(* ptr to above line, if nil, lines are added to
						   the list of lines to be discarded *)

VAR CNT: QLINENO;
    TLNO: QLINENO;				(* tln adjusted for movements *)
BEGIN
 WITH BUF DO BEGIN
  CNT := LLN - FLN + 1;				(* count of lines to move *)
  TLNO := TLN;
  MASK(ATTENTION);


  (* slice out source lines from buffer if move or delete *)

  IF FLN <> 0 THEN BEGIN			(* check for lines in buffer *)
    FLP^.PREVLINEP^.NEXTLINEP := LLP^.NEXTLINEP;    (* take off chain *)
    IF LLP^.NEXTLINEP <> NIL THEN 
      LLP^.NEXTLINEP^.PREVLINEP := FLP^.PREVLINEP;

    IF FLN <= LBOUND THEN			(* adjust special line numbers *)
      IF LBOUND > LLN THEN LBOUND := LBOUND - CNT
	ELSE BEGIN				(* lbound in lines moved, new lbound follows *)
	  LBOUND := FLN;
	  LBOUNDP := LLP^.NEXTLINEP
	END;

    IF FLN <= GETLINENO THEN
      IF GETLINENO > LLN THEN GETLINENO := GETLINENO - CNT
	ELSE BEGIN				(* getlineno in lines move, new before them *)
	  GETLINENO := FLN - 1;
	  GETLINEP := FLP^.PREVLINEP
	END;

    IF FLN <= HBOUND THEN
      IF HBOUND > LLN THEN HBOUND := HBOUND - CNT
	ELSE BEGIN				(* hbound in lines moved, new before them *)
	  HBOUND := FLN - 1;
	  HBOUNDP := FLP^.PREVLINEP
	END;

    IF FLN <= LASTLINENO THEN
      IF LASTLINENO > LLN THEN LASTLINENO := LASTLINENO - CNT
	ELSE BEGIN				(* last line in lines moved, new at new end of buffer *)
	  LASTLINENO := FLN - 1;
	  LASTLINEP := FLP^.PREVLINEP
	END;

    IF FLN < TLNO THEN TLNO := TLNO - CNT	(* addr of target may be affected too *)
  END


  (* if appending new lines, remove from garb list *)

  ELSE IF LLP = GARBLINEP			(* quick check to see that line is on list *)
    THEN GARBLINEP := FLP^.PREVLINEP;


  (* if deleting, add to list to be discarded *)

  IF TLP = NIL THEN BEGIN
    FLP^.PREVLINEP := GARBLINEP;		(* garb chain is backwards *)
    GARBLINEP := LLP
  END


  (* if moving or appending, add after target line *)

  ELSE BEGIN
    LLP^.NEXTLINEP := TLP^.NEXTLINEP;		(* thread source to target *)
    FLP^.PREVLINEP := TLP;

    IF TLP^.NEXTLINEP <> NIL THEN		(* thread target to source *)
      TLP^.NEXTLINEP^.PREVLINEP := LLP;
    TLP^.NEXTLINEP := FLP;

    IF LASTLINENO = TLNO THEN LASTLINEP := LLP;	(* adjust special hooks *)
    LASTLINENO := LASTLINENO + CNT;
    IF TLNO <= HBOUND THEN BEGIN
      IF TLNO = HBOUND THEN HBOUNDP := LLP;
      HBOUND := HBOUND + CNT;
      IF TLNO = LBOUND - 1 THEN LBOUNDP := FLP
	ELSE IF TLNO < LBOUND THEN LBOUND := LBOUND + CNT
    END
  END;

  CHANGES := TRUE;
  UNMASK(ATTENTION);
 END
END;
$PAGE cleangarb
(* CLEANGARB removes deleted (or unused) lines from the so-called garb list.
   The list is scanned backwards and one line at a time is deleted. This
   code runs unmasked; if interrupted, at most one line will be lost (i.e.
   unchained, but not disposed.) *)

PROCEDURE CLEANGARB ( VAR BUFFER: QBUFFER );
 VAR LP: QLINEP;
 BEGIN
  WITH BUFFER DO BEGIN
   WHILE GARBLINEP <> NIL DO BEGIN		(* scan list and delete one at a time *)
     LP := GARBLINEP;				(* save current ptr in temp *)
     GARBLINEP := GARBLINEP^.PREVLINEP;
     DISPOSE (LP)				(* now delete, after stepping over it in chain *)
   END
  END
 END;
$PAGE findlinep
(*    internal procedure to find the pointer to a passed lineno    *)

PROCEDURE FINDLINEP(VAR BUF: QBUFFER; LNO: QLINENO; VAR LP: QLINEP);

(* assumes that l is a good number *)


  PROCEDURE SETLP(TP: QLINEP);			(* sets findlinep return value *)
  BEGIN
    WITH BUF DO BEGIN				(*update buf info first*)
      MASK(ATTENTION);
      GETLINENO := LNO;
      GETLINEP := TP;
      UNMASK(ATTENTION)
    END (*with*);
    LP:= TP					(*now return pointer*)
  END (*setlp*);


  PROCEDURE SEARCH(BEGLINENO: QLINENO; BEGLINEP: QLINEP;
		   ENDLINENO: QLINENO; ENDLINEP: QLINEP);
    VAR TP: QLINEP; I: QLINENO;			(*used in line search*)
  BEGIN
    (*determine search direction*)
    IF ((ENDLINENO - LNO) <= (LNO - BEGLINENO)) AND (ENDLINEP <> NIL)
      THEN BEGIN				(*search backward from endlineno to lno*)
	TP:= ENDLINEP;
	FOR I:= ENDLINENO-1 DOWNTO LNO DO
	  TP:= TP^.PREVLINEP
      END
      ELSE BEGIN				(*search forward from beglineno to lno*)
	TP:= BEGLINEP;
	FOR I:= BEGLINENO+1 TO LNO DO
	  TP:= TP^.NEXTLINEP
      END;
    SETLP(TP)					(*update getline information*)
  END (*search*);


BEGIN						(*findline*)
  WITH BUF DO
    IF LNO < GETLINENO
      THEN IF LNO < LBOUND
	THEN SEARCH (0, FIRSTLINEP, GETLINENO, GETLINEP)
	ELSE IF LNO < HBOUND
	  THEN SEARCH (LBOUND, LBOUNDP, GETLINENO, GETLINEP)
	  ELSE SEARCH (HBOUND, HBOUNDP, GETLINENO, GETLINEP)
      ELSE IF LNO < LBOUND
	THEN SEARCH (GETLINENO, GETLINEP, LBOUND, LBOUNDP)
	ELSE IF LNO <= HBOUND
	  THEN SEARCH (GETLINENO, GETLINEP, HBOUND, HBOUNDP)
	  ELSE SEARCH (GETLINENO, GETLINEP, LASTLINENO, LASTLINEP)
END (*findline*);
$PAGE utilities
(*********** text buffer manager utility routines ***********)

(* function to transform bounded linenos into absolute ones *)

FUNCTION MAP
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	LINE: QLINENO				(* line number to transform *)
		): QLINENO;			(* mapped result *)

BEGIN
  MAP := LINE + BUFFER.LBOUND - BUFFER.OFFSET
END;						(* map *)

(*    procedure to check a line number    *)

FUNCTION CHKLINE(VAR BUF: QBUFFER; L: QLINENO; VAR ERR: QERRCODE): BOOLEAN;
BEGIN
  ERR:= QOK;
  IF (L < BUF.LBOUND) OR (L > BUF.HBOUND) THEN ERR:= QBADLN;
  CHKLINE:= (ERR=QOK)
END (*chkline*);


(*    procedure to check a line range    *)

FUNCTION CHKRANGE(VAR BUF: QBUFFER; F,L: QLINENO; VAR ERR: QERRCODE): BOOLEAN;
BEGIN
  ERR:= QOK;
  IF F > L THEN ERR:= QBADRN
  ELSE IF L > BUF.HBOUND THEN ERR:= QBADUB
  ELSE IF F < BUF.LBOUND THEN ERR:= QBADLB;
  CHKRANGE:= (ERR=QOK)
END (*chkrange*);
$PAGE makeline
(* procedure to create a qed line record, does not chain it in *)

FUNCTION MAKELINE (VAR BUF: QBUFFER; LINE: QSTRING): QLINEP;
 TYPE
   SYNLINEP = ^SYNLINE;				(* synthetic line *)
   SYNLINE =
     PACKED RECORD
       PREVLINEP, NEXTLINEP: QLINEP;
       STRNG: PACKED ARRAY[1..*] OF CHAR
     END;
 VAR SYNP: SYNLINEP;
     NP: QLINEP;
 BEGIN
  NEW (SYNP, LENGTH (LINE));			(* alloc line of appropriate length *)
  SYNP^.STRNG[1:LENGTH(LINE)]:=LINE;		(* copy only to length allocated *)
  NP := ADDRESS (SYNP^);			(* coerce the pointer *)
  WITH NP^ DO BEGIN
    NEXTLINEP := NIL;
    PREVLINEP := BUF.GARBLINEP;			(* add to dispose list, in case we lose it *)
    IF BUF.GARBLINEP <> NIL THEN
      BUF.GARBLINEP^.NEXTLINEP := NP;
    BUF.GARBLINEP := NP
  END;
  MAKELINE := NP
 END;						(* makeline *)
$PAGE qdelbuf

(*    procedure to delete a buffer    *)

PUBLIC PROCEDURE QDELBUF(VAR BUF: QBUFFER);
  VAR ERR: QERRCODE;				(*we need it but we ignore them*)
BEGIN						(*qdelbuf*)
  WITH BUF DO BEGIN
    IF LASTLINENO>0 THEN BEGIN			(*something to release*)
      MOVE (BUF, 1, FIRSTLINEP^.NEXTLINEP, LASTLINENO, LASTLINEP, 0, NIL);  (* move lines to garb list *)
      CLEANGARB (BUF);				(* dispose the lines *)
    END;
    DISPOSE (FIRSTLINEP);			(* get rid of zeroth line *)
    SPREDDISPOSE (MARK);			(* dispose mark predicate *)
    MARK := NIL					(* for good measure *)
  END						(*with*)
END (*qdelbuf*);

$PAGE qinitbuf
(*    procedure to initialize buffer for first time    *)

PUBLIC PROCEDURE QINITBUF(VAR BUF: QBUFFER);
BEGIN
  WITH BUF DO
  BEGIN
    LASTLINENO := 0;
    GETLINENO := 0;
    LBOUND := 1;
    OFFSET := 1;
    OLDOFFSET := 1;
    HBOUND := 0;
    CURLINENO := 0;
    NEW (FIRSTLINEP);				(* dummy zeroth line to make things easier *)
    WITH FIRSTLINEP^ DO BEGIN
      PREVLINEP := NIL;
      NEXTLINEP := NIL
    END;
    LASTLINEP := FIRSTLINEP;
    GETLINEP := FIRSTLINEP;
    LBOUNDP := NIL;
    HBOUNDP := FIRSTLINEP;
    GARBLINEP := NIL;
    CURFILE := '';
    CURFILEOK := FALSE;
    CHANGES := FALSE;
    MARK := NIL;
  END
END;						(* qinitbuf *)
$PAGE qgetline
(*    function to return text of line    *)

PUBLIC FUNCTION QGETLINE(VAR BUF: QBUFFER; L: QLINENO; VAR ERR: QERRCODE): QSTRING;

VAR
  LP: QLINEP;
  LNO: QLINENO;

BEGIN
  LNO := MAP (BUF, L);
  IF NOT CHKLINE(BUF, LNO, ERR) THEN QGETLINE:= ''
  ELSE BEGIN
    FINDLINEP(BUF, LNO, LP);
    QGETLINE := SUBSTR (LP^.SOURCE, 1, LENGTH (LP^.SOURCE))
  END
END (*qgetline*);

$PAGE qmodline

PUBLIC PROCEDURE QMODLINE(VAR BUF: QBUFFER; L: QLINENO; NEWTEXT: QSTRING;
  VAR ERR: QERRCODE);

VAR
  LP,NP: QLINEP;
  LNO: QLINENO;

BEGIN
  LNO := MAP (BUF, L);
  IF CHKLINE(BUF, LNO, ERR) THEN BEGIN
    FINDLINEP(BUF, LNO, LP);
    NP := MAKELINE (BUF, NEWTEXT);
    MASK(ATTENTION);
    WITH NP^ DO BEGIN
      BUF.GARBLINEP := PREVLINEP;		(* remove new from garb list *)
      PREVLINEP := LP^.PREVLINEP;		(* chain new line to neighbors of old line *)
      NEXTLINEP := LP^.NEXTLINEP
    END;
    WITH LP^ DO BEGIN				(* chain neighbors to new line *)
      PREVLINEP^.NEXTLINEP := NP;		(* make forward chain *)
      IF NEXTLINEP <> NIL			(* build backward chain *)
	THEN NEXTLINEP^.PREVLINEP := NP
	ELSE BUF.LASTLINEP := NP;
      IF LNO = BUF.LBOUND THEN BUF.LBOUNDP := NP;   (* if this was special line, reset ptr *)
      IF LNO = BUF.HBOUND THEN BUF.HBOUNDP := NP;
      IF LNO = BUF.GETLINENO THEN BUF.GETLINEP := NP;
    END;
    LP^.PREVLINEP := BUF.GARBLINEP;		(* put old on garb list to dispose *)
    BUF.GARBLINEP := LP;
    BUF.CHANGES := TRUE;
    UNMASK(ATTENTION);
    CLEANGARB (BUF)				(* dispose old line *)
  END
END (*qmodline*);
$PAGE qaddline
(*    procedure to add a line to a buffer    *)

PUBLIC PROCEDURE QADDLINE(VAR BUF: QBUFFER; L: QLINENO; TEXT:QSTRING;
  VAR ERR: QERRCODE);

VAR
  LNO: QLINENO;
  NP, LP: QLINEP;

BEGIN
  ERR := QOK;					(* assume success *)
  LNO := MAP (BUF, L);
  IF LNO > BUF.HBOUND THEN ERR := QBADLN
  ELSE BEGIN
    FINDLINEP (BUF, LNO, LP);			(* find line to append to *)
    NP := MAKELINE (BUF, TEXT);			(* create a line with text *)
    MOVE (BUF, 0, NP, 0, NP, LNO, LP);		(* move from garblist to buffer *)
  END
END (*qaddline*);
$PAGE qdellines
(*    procedure to delete line(s) from buffer    *)

PUBLIC PROCEDURE QDELLINES (VAR BUF: QBUFFER; F,L: QLINENO; VAR ERR: QERRCODE);

VAR
  FP,
  LP: QLINEP;
  FNO,
  LNO: QLINENO;
BEGIN
  FNO := MAP (BUF, F);
  LNO := MAP (BUF, L);
  IF CHKRANGE (BUF, FNO, LNO, ERR) THEN BEGIN
    FINDLINEP (BUF, FNO, FP);			(* find addressed lines *)
    FINDLINEP (BUF, LNO, LP);
    MOVE (BUF, FNO, FP, LNO, LP, 0, NIL);	(* move to garb list *)
    CLEANGARB (BUF);				(* and dispose *)
    ERR := QOK
  END
END (*qdellines*);
$PAGE qbuflength
PUBLIC FUNCTION QBUFLENGTH ( VAR BUF: QBUFFER ): QLINENO;
 BEGIN
   WITH BUF DO BEGIN
     QBUFLENGTH := HBOUND - LBOUND + 1
   END
 END;

PUBLIC FUNCTION QDOLLAR_VAL ( VAR BUF: QBUFFER ): QLINENO;
  BEGIN
    QDOLLAR_VAL := QBUFLENGTH (BUF) + BUF.OFFSET - 1
  END;						(* qdollar_val *)

PUBLIC FUNCTION QFIRST_VAL ( VAR BUF: QBUFFER ): QLINENO;
  BEGIN
    QFIRST_VAL := BUF.OFFSET
  END;						(* qfirst_val *)
$PAGE qmovelines
PUBLIC PROCEDURE QMOVELINES
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	FIRST, LAST: QLINENO;			(* range of lines to be moved *)
	DEST: QLINENO;				(* where to move them to *)
	VAR ERR: QERRCODE);			(* error report *)

VAR
  FNO,
  LNO,
  DNO:		QLINENO;			(* for line number mapping *)
  FIRSTP,
  LASTP:	QLINEP;				(* temporary pointers *)
  DESTP:	QLINEP;				(* where to re-attach lines *)

BEGIN
  FNO := MAP (BUFFER, FIRST);
  LNO := MAP (BUFFER, LAST);
  DNO := MAP (BUFFER, DEST);
  IF NOT CHKRANGE (BUFFER, FNO, LNO, ERR) THEN RETURN;
  IF NOT ((DNO = BUFFER.LBOUND - 1) ORIF (CHKLINE (BUFFER, DNO, ERR))) THEN RETURN;
  IF (FNO <= DNO) AND (DNO <= LNO) THEN BEGIN	(* target within lines to be moved *)
    ERR := QBADMOVELA;
    RETURN
  END;
  FINDLINEP (BUFFER, FNO, FIRSTP);
  FINDLINEP (BUFFER, LNO, LASTP);
  FINDLINEP (BUFFER, DNO, DESTP);
  MOVE (BUFFER, FNO, FIRSTP, LNO, LASTP, DNO, DESTP);	(* do it *)
END;
$PAGE qcopylines
PUBLIC PROCEDURE QCOPYLINES
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	FIRST, LAST: QLINENO;			(* range of lines to copy *)
	DEST: QLINENO;				(* where to copy them to *)
	VAR ERR: QERRCODE);			(* error report *)

VAR
  SOURCE:	QSTRING;			(* to hold text of lines to be copied *)
  IDX:		QLINENO;			(* counter for lines *)
  FNO,
  LNO,
  DNO:		QLINENO;			(* for line number mapping *)
  FIRSTP,
  LASTP,
  DESTP:	QLINEP;				(* working pointers *)

BEGIN
  FNO := MAP (BUFFER, FIRST);
  LNO := MAP (BUFFER, LAST);
  DNO := MAP (BUFFER, DEST);
  IF NOT CHKRANGE (BUFFER, FNO, LNO, ERR) THEN RETURN;
  IF NOT ((DNO = BUFFER.LBOUND - 1) ORIF (CHKLINE (BUFFER, DNO, ERR))) THEN RETURN;

  (* construct copy of lines to move on garb list *)

  CLEANGARB (BUFFER);				(* not really necessary, but good form *)
  FIRSTP := NIL;				(* to check if first line copied *)
  FOR IDX := FIRST TO LAST DO BEGIN		(* copy lines, use relative #s with qgetline *)
    SOURCE := QGETLINE (BUFFER, IDX, ERR);	(* get text of line *)
    IF ERR <> QOK THEN RETURN;
    LASTP := MAKELINE ER, SOURCE);		(* append copy to garb list *)
    IF FIRSTP = NIL THEN FIRSTP := LASTP	(* remember start *)
  END;

  (* move copy of lines into buffer *)

  FINDLINEP (BUFFER, DNO, DESTP);
  MOVE (BUFFER, 0, FIRSTP, LNO-FNO, LASTP, DNO, DESTP)

END;						(* qcopylines *)
$PAGE bounding utilities
(* routine to set the buffer offset for addressing bounded lines *)
PUBLIC PROCEDURE QSETOFFSET (NEWOFFSET: QLINENO; VAR BUFFER: QBUFFER);
BEGIN
  BUFFER.OLDOFFSET := BUFFER.OFFSET;
  BUFFER.OFFSET := NEWOFFSET
END;

PUBLIC PROCEDURE QSETBOUNDS (VAR BUFFER: QBUFFER; LOW, HIGH: QLINENO;
	ABSOLUTE: BOOLEAN; VAR ERR: QERRCODE);

VAR
  TEMPOFFSET: QLINENO;
  TEMPP: QLINEP;				(* temporary storage *)
  FNO,
  LNO: QLINENO;					(* for bound conversion *)

BEGIN
  TEMPOFFSET := BUFFER.OFFSET;
  IF ABSOLUTE THEN BUFFER.OFFSET := BUFFER.LBOUND
  ELSE BUFFER.OFFSET := BUFFER.OLDOFFSET;
  FNO := MAP (BUFFER, LOW);
  LNO := MAP (BUFFER, HIGH);
  IF CHKRANGE (BUFFER, FNO, LNO, ERR) THEN
  WITH BUFFER DO
  BEGIN
    MASK(ATTENTION);
    FINDLINEP (BUFFER, FNO, TEMPP);
    FINDLINEP (BUFFER, LNO, HBOUNDP);
    LBOUNDP := TEMPP;
    LBOUND := FNO - OFFSET + 1;
    HBOUND := LNO - OFFSET + 1;
    UNMASK(ATTENTION)
  END;
  BUFFER.OFFSET := TEMPOFFSET;
  BUFFER.CURLINENO := QFIRST_VAL (BUFFER)
END (* qsetbounds *);



PUBLIC PROCEDURE QUNBOUND (VAR BUFFER: QBUFFER; VAR ERR: QERRCODE);
BEGIN
  ERR := QOK;
  MASK(ATTENTION);
  WITH BUFFER DO
  BEGIN
    LBOUND := 1;
    LBOUNDP := FIRSTLINEP^.NEXTLINEP;
    HBOUND := LASTLINENO;
    OFFSET := 1;
    HBOUNDP := LASTLINEP
  END;
  UNMASK(ATTENTION)
END (* qunbound *);
$PAGE QFILEAPPEND -- Read Buffer Text from a File

VAR
  F: TEXT;					(* kludge around brain-damage *)

PUBLIC PROCEDURE QFILEAPPEND
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	S940ID: FILE_ID;			(* file to read text from	*)
	WMOD: WMODIFIER;			(* 940 file modifier	*)
	WHERE: QLINENO;				(* where to append text *)
        VAR CNT: QLINENO;                       (* number of lines appended *)
	VAR ERR: QERRCODE);			(* error report *)

VAR
  IDX: QSTRINGIDX;
  CH: CHAR;
  WHERENO: QLINENO;				(* mapped address *)
  TLINE, RLINE: QSTRING;
  FIRST, LAST, WHEREP: QLINEP;
	PDP10ID: FILE_ID;			(* Converted name of file	*)
	RFLAG:	BOOLEAN;			(* Kludgy documentation revision switch	*)
	WERR:	WCODES;				(* Return codes from 940 I/O routines	*)
	S940:	BOOLEAN;			(* True if file is 940 file	*)
	WCHAN:	WCHANNEL;			(* Channel number from which to read	*)
	JUNKLINE: BOOLEAN;			(* True if line should be ditched *)

BEGIN
ERR := QOK;					(* Start with a clean slate	*)
WHERENO := MAP (BUFFER, WHERE);
IF WHERENO > BUFFER.HBOUND
THEN
  BEGIN						(* No reading outside file limits!	*)
  ERR := QBADLN;
  RETURN
  END;
CNT := 0;					(* No lines read yet	*)
S940 := FALSE;
PDP10ID := S940ID;
$IF P10
(*
*	Convert the file name and figure out what kind of a file we're
*	dealing with.  If it is a 940 file, use the special 940 file
*	opening routine; otherwise, just use standard I/O.
*)
MASK(ATTENTION);
WFILECONVERT (S940ID, WMOD, PDP10ID, WERR, RFLAG);
UNMASK(ATTENTION);
IF WERR = WBADNAME
THEN ERR := QNOFILE				(* Stop obvious garbage early	*)
ELSE IF WERR = WOK
THEN
  BEGIN						(* File is 940 file	*)
  WCHAN := GETCHANNEL;
  WOPEN (WCHAN, WERR, WINPUT, PDP10ID);
  IF WERR <> WOK
  THEN
    BEGIN
    FREECHANNEL (WCHAN);			(* Open failed. Return channel to pool	*)
    ERR := QNOFILE
    END
  ELSE S940 := TRUE;				(* Open succeeded. Leave the word	*)
  END
ELSE
$END
  QOPENFILE (F, PDP10ID, '', QINPUT_MODE, [QIO_ASCII], ERR);
IF ERR = QOK
THEN
  BEGIN						(* So far, so good	*)
  FIRST := NIL;                                 (* Initialize new chain *)
  WERR := WOK;
  IF NOT S940
  THEN F^ := CR;				(* Initialize normal file buffer	*)
    LOOP					(* The main read loop starts here	*)
    MASK(ATTENTION);
$IF P10
    IF S940					(* 940 files have special read routine *)
    THEN WINLINE (WCHAN, WERR, TLINE)
    ELSE
      BEGIN					(* Normal files are a bit more arduous	*)
      TLINE := '';				(* Start out fresh	*)
(*
*	Although there are a number of characters which will end lines
*	as far as Pascal is concerned, the only acceptable line end
*	characters for us are a CR-LF pair and CR by itself (not
*	recognized by PDP-10 I/O as a line end).  Accordingly, we
*	watch out for all other cases and force them to wrap around
*	for up to QSTRINGLEN characters.  EOF forces a halt, regardless
*	of the character in the buffer.
*
*	Note that a line of QSTRINGLEN characters should not generate a
*	zero length line following it.  Thus, if the character in F^ is a
*	CR, the line must be thrown out if the previous line had QSTRINGLEN
*	characters and the current line has only one (the CR).  This is
*	accomplished with a hack.  The hack can be removed if the run time
*	code ever treats CR-LF as a line end.
*)
	REPEAT					(* Until we fill a QED line	*)
	JUNKLINE := FALSE;			(* Line looks good so far	*)
	IF EOLN (F)				(* Fetch a line, if needed	*)
	THEN
	  BEGIN
	  IF F^ <> CR				(* Force nonstandard lines to wrap *)
	  THEN TLINE := TLINE || F^;
	  READLN (F)
	  END
	ELSE JUNKLINE := TRUE;			(* This might be a phony null line *)
	READ (F, RLINE: QSTRINGLEN - LENGTH (TLINE));	(* Don't get too much	*)
	IF EOF (F)
	THEN F^ := CR;				(* Always stop on EOF	*)
	IF LENGTH (RLINE) > 0
	THEN					(* We got something	*)
	  BEGIN
	  IF EOLN (F) AND (F^ = LF) AND (RLINE[LENGTH (RLINE)] = CR)
	  THEN
	    BEGIN				(* Chop trailing junk CR, if present *)
	    F^ := CR;				(* and set CR as fake line end	*)
	    IF LENGTH (RLINE) > 1
	    THEN				(* Must be a legitimate line	*)
	      BEGIN
	      TLINE := TLINE || SUBSTR (RLINE, 1, LENGTH (RLINE) - 1);
	      JUNKLINE := FALSE
	      END
	    END
	  ELSE
	    BEGIN
	    TLINE := TLINE || RLINE;		(* Bad line end. Will wrap	*)
	    JUNKLINE := FALSE			(* Don't eat the line	*)
	    END
	  END
	ELSE IF (LENGTH (TLINE) = 0) AND EOF (F)
	THEN WERR := WEOF			(* Absolutely all done	*)
	UNTIL (EOLN (F) AND (F^ = CR) AND NOT JUNKLINE) OR (LENGTH (TLINE) = QSTRINGLEN)
      END;
$END
$IFANY (VAX, M68)
    READLN(F);
    IF EOF(F) THEN
      WERR := WEOF
    ELSE
      READ(F,TLINE);
$END
    UNMASK(ATTENTION);
    EXIT IF WERR <> WOK;			(* Either EOF or error	*)
    MASK(ATTENTION);
    LAST := MAKELINE (BUFFER, TLINE);		(* Chain line into list	*)
    IF FIRST = NIL
    THEN FIRST := LAST;				(* Remember first line	*)
    UNMASK(ATTENTION);
    CNT := CNT + 1
    END;					(* Main loop	*)
(*
*	The file has now been completely read in.  If there was anything
*	there, chain the new blob of lines into the proper slot.  Indicate
*	that the buffer is unchanged if it was initially fresh.  Close
*	the file, reenable attention interrupts, and leave.
*)
  IF CNT > 0
  THEN
    BEGIN					(* Got something. Chain it in	*)
    FINDLINEP (BUFFER, WHERENO, WHEREP);
    MOVE (BUFFER, 0, FIRST, CNT-1, LAST, WHERENO, WHEREP)
    END;
  IF (BUFFER.LASTLINENO = CNT) AND (ERR = QOK)
  THEN
    BEGIN					(* Buffer was empty. Save file name *)
    BUFFER.CHANGES := FALSE;			(* Buffer starts out clean	*)
    IF S940
    THEN
      BEGIN					(* 940 name may be revised for DOC kludge *)
$IF P10
      WFILENAME (WCHAN, BUFFER.CURFILE, RFLAG);
      BUFFER.S940 := TRUE			(* This is now a 940 buffer	*)
$END
      END
    ELSE
      BEGIN					(* Just use normal name for PDP-10 file	*)
      BUFFER.CURFILE := FILENAME (F);
      BUFFER.S940 := FALSE
      END;
    BUFFER.CURFILEOK := TRUE
    END;
  IF S940
  THEN
    BEGIN					(* Special close logic for 940 files	*)
$IF P10
    WCLOSE (WCHAN, WERR);
    FREECHANNEL (WCHAN)
$END
    END
  ELSE CLOSE (F);				(* Normal close for others	*)
  END
  EXCEPTION
    OTHERS: BEGIN
              MASK(ATTENTION);
	      IF S940
	      THEN
		BEGIN					(* Special close for 940 file	*)
$IF P10
		WCLOSE (WCHAN, WERR);
		FREECHANNEL (WCHAN)
$END
		END
	      ELSE CLOSE (F);				(* Standard close for most things	*)
	      CLEANGARB (BUFFER);				(* Flush unfinished stuff	*)
              UNMASK(ATTENTION);
              SIGNAL();
            END;
END;						(* qfileappend *)
$PAGE qttyappend
PUBLIC PROCEDURE QTTYAPPEND
(	VAR BUFFER: QBUFFER;			(* working buffer *)
	WHERE: QLINENO;				(* where to append text *)
	VAR CNT: QLINENO;			(* number of lines appended *)
	VAR ERR: QERRCODE);			(* error report *)

VAR
  LINE: QSTRING;
  CH: CHAR;
  LINENUM: QLINENO;
  DONE: BOOLEAN;

BEGIN
  BREAK;
  LINENUM := WHERE;
  ERR := QOK;
(*if where = 0 then line := ''			(* get text of previous line to edit *)
  else line := qgetline (buffer, where, err);
  if err <> qok then return;    previous line editing deleted! *)
  DONE := FALSE;
  WHILE (ERR = QOK) AND (NOT DONE) DO
  BEGIN
    LINE := QREAD ;
    IF (LENGTH (LINE) = 1) ANDIF (LINE [1] = '.') THEN DONE := TRUE
    ELSE
    BEGIN
      QADDLINE (BUFFER, LINENUM, LINE, ERR);
      LINENUM := LINENUM + 1
    END;
  END;
  IF LINENUM > WHERE THEN BUFFER.CHANGES := TRUE;
  CNT := LINENUM - WHERE
END (* qttyappend *);
$PAGE qfilewrite

PUBLIC PROCEDURE QFILEWRITE			(* write text to file *)
(       VAR BUFFER: QBUFFER;			(* buffer to write from *)
	S940ID: FILE_ID;			(* file to write to	*)
	WMOD: WMODIFIER;			(* 940 file modifier	*)
	FN, LN: QLINENO;			(* range to write *)
	CONFIRM: BOOLEAN;			(* new/old file prompting? *)
	VAR ERR: QERRCODE);			(* error report *)

VAR
  FNO, LNO: QLINENO;
  FLP, LLP: QLINEP;
  LINENO: QLINENO;
  LINE: QSTRING;
  OPTIONS_SET: QIOOPTION_SET;
	PDP10ID: FILE_ID;
	S940, RFLAG: BOOLEAN;
	WERR: WCODES;
	WCHAN: WCHANNEL;

BEGIN
FNO := MAP (BUFFER, FN);
LNO := MAP (BUFFER, LN);
IF NOT CHKRANGE (BUFFER, FNO, LNO, ERR)
THEN RETURN;					(* Can't write outside legal range	*)
IF CONFIRM
THEN OPTIONS_SET := [QIO_CONFIRM]
ELSE OPTIONS_SET := [];
$IF P10
(*
*	If we have a saved file name, use it.  Otherwise, convert a possible
*	940 file to a PDP-10 name.  In either case, open the file for
*	output, prompting for old/new file as required.  If the user confirms
*	the prompt, back off the open and reopen it using the special 940
*	I/O if it's a 940 file.
*)
MASK(ATTENTION);
IF WMOD = '*'					(* If 940 name was already converted *)
THEN
  BEGIN
  PDP10ID := S940ID;				(* Don't bother to reconvert	*)
  WERR := WOK					(* But remember it was 940	*)
  END
ELSE WFILECONVERT (S940ID, WMOD, PDP10ID, WERR, RFLAG);
IF WERR = WBADNAME
THEN ERR := QNOFILE
ELSE QOPENFILE (F, PDP10ID, '', QOUTPUT_MODE, OPTIONS_SET, ERR);
      UNMASK(ATTENTION);
$END
$IFANY (VAX, M68)
  QOPENFILE (F, S940ID, '', QOUTPUT_MODE, OPTIONS_SET, ERR);
  S940 := FALSE;
$END
IF ERR = QOK
THEN
  BEGIN						(* File looks legitimate	*)
$IF P10
  IF WERR = WOK
  THEN
    BEGIN					(* Got a 940 file. Special open	*)
    CLOSE (F);
    WCHAN := GETCHANNEL;
    WOPEN (WCHAN, WERR, WOUTPUT, PDP10ID);
    IF WERR <> WOK
    THEN
      BEGIN					(* No can do. Release everything	*)
      FREECHANNEL (WCHAN);
      ERR := QNOFILE;
      RETURN;
      END;
    S940 := TRUE     				(* Success. Remember it	*)
    END
  ELSE S940 := FALSE;				(* Normal file signal	*)
$END
(*
*	The file has now been properly opened.  Figure out where the first
*	and last lines of the output block are; then write them one at a time.
*)
  FINDLINEP (BUFFER, FNO, FLP);
  FINDLINEP (BUFFER, LNO, LLP);
    REPEAT					(* Here comes the main output loop	*)
    MASK(ATTENTION);
    IF S940
    THEN
      BEGIN					(* Special output for 940 files	*)
$IF P10
      WOUTLINE (WCHAN, WERR, FLP^.SOURCE);
      IF WERR <> WOK
      THEN ERR := QWRTERR			(* Acknowledge write error	*)
$END
      END
    ELSE
      BEGIN					(* Standard output for normal files	*)
      WRITELN (F, FLP^.SOURCE);
      IF NOT EOF (F)
      THEN ERR := QWRTERR			(* Should be positioned at file end	*)
      END;
    UNMASK(ATTENTION);
    EXIT IF FLP = LLP;				(* No pointer shuffle after we're done	*)
    FLP := FLP^.NEXTLINEP			(* Advance to next line	*)
    UNTIL ERR <> QOK;				(* End of main loop	*)
(*
*	The file (or a hunk of it) has now been completely written.
*	if it was, in fact, the whole file, save the file name and
*	type for later defaulting.  Then close the file.
*)
  IF (FNO = 1) AND (LNO = BUFFER.LASTLINENO) AND (ERR = QOK)
  THEN
    BEGIN					(* Whole file written. Remember where	*)
    BUFFER.CHANGES := FALSE;			(* Buffer is now clean	*)
    IF S940
    THEN
      BEGIN					(* Get 940 file name from special code,	*)
$IF P10
      RFLAG := FALSE;				(* avoiding DOC revision kludge	*)
      WFILENAME (WCHAN, BUFFER.CURFILE, RFLAG);
      BUFFER.S940 := TRUE			(* All hail, XDS	*)
$END
      END
    ELSE
      BEGIN					(* Use standard code for normal file *)
      BUFFER.CURFILE := FILENAME (F);
      BUFFER.S940 := FALSE
      END;
    BUFFER.CURFILEOK := TRUE			(* We now have a saved name	*)
    END;
  IF S940
  THEN
    BEGIN
$IF P10
    WCLOSE (WCHAN, WERR);			(* The usual special 940 close	*)
    FREECHANNEL (WCHAN)
$END
    END
  ELSE CLOSE (F);				(* The usual stuff for normal files	*)
  END
ELSE IF ERR = QNOFILE				(* Don't treat confirm failure as error *)
THEN ERR := QOK;
EXCEPTION
  OTHERS: BEGIN
            MASK(ATTENTION);
	    IF S940
	    THEN
	      BEGIN					(* File is 940 file. Special close	*)
$IF P10
	      WCLOSE (WCHAN, WERR);
	      FREECHANNEL (WCHAN)
$END
	      END
	    ELSE CLOSE (F);				(* Standard close for normal file	*)
	    WRITELN (TTY, 'Warning--output file write incomplete.');
	    BREAK;
            UNMASK(ATTENTION);
            SIGNAL();
          END;
END.						(* QFILEWRITE	*)
  7 Kl