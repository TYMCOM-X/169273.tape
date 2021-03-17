$PAGE qlabelfind function
(* QLABEL.PAS - modified 9/24/81 by djm to change CHR(11B) to TAB *)

PUBLIC FUNCTION QLABELFIND
(	VAR BUFFER:	QBUFFER;	  
	START:		QLINENO;		(* where to start search *)
	TOP:		QLINENO;		(* upper search limit *)
	VAR QLABEL:	QSTRING;		(* label name, if found *)
	VAR DISPL:	QLINENO;		(* disp. of label from start *)
	VAR ERR:	QERRCODE):		(* error code *)
			BOOLEAN;		(* true if label found, else false *)

(* QLABELFIND searches the buffer from the start line backwards to the
   TOP line for a line with a label. If one is found, the label's name
   and its displacement from the START line are returned, with the value
   TRUE. Otherwise, if no label is found, FALSE is returned. 
   A label begins in column one with a character in the set
   ['A'..'Z','a'..'z','0'..'9','$'] and ends with the character
   preceding the next tab, blank, or end of line.  *)

TYPE
  CHARSET = SET OF CHAR;

CONST
  LABEL_HEADS : CHARSET := [ 'A'..'Z', '0'..'9', '$' ];

VAR
  LINENO: QLINENO;
  LINE: QSTRING;
  IDX: QSTRINGIDX;

BEGIN
  LINENO := START;
  QLABELFIND := FALSE;				(* until we find a label *)

  WHILE (NOT QLABELFIND) AND (LINENO >= TOP) DO BEGIN

    LINE := QGETLINE (BUFFER, LINENO, ERR);
    IDX := 1;

  EXIT IF ERR <> QOK;

    IF (IDX <= LENGTH(LINE)) AND
       (UPPERCASE (LINE[1]) IN LABEL_HEADS) THEN BEGIN	(* we have a label *)
      QLABELFIND := TRUE;
      IDX := 2;
      WHILE (IDX <= LENGTH(LINE)) ANDIF
	    (LINE[IDX] <> ' ')    ANDIF
	    (LINE[IDX] <> TAB) DO IDX := IDX + 1;
      QLABEL := SUBSTR (LINE, 1, IDX-1);	(* pull of the label name *)
      DISPL := START - LINENO			(* calculate displacement of label *)
      END
    ELSE LINENO := LINENO - 1

  END						(* while *)
END (* qlabelfind *).
