TYPE
  NUMWHERE = (NUMTOP,NUMBOT,NONUM);
  NUMDESC = RECORD
             WHERE: NUMWHERE;
             JUST: JUSTYPE;
             NUM:   INTEGER;
             ALTRNT: BOOLEAN
            END;
  TABARRAY = ARRAY[1..MAXTABS] OF LINEPTR;
  RANGE = ^RANGETYPE;
  RANGETYPE = RECORD
    FIRST, LAST: INTEGER;
    NEXT: RANGE
  END;
 
PUBLIC VAR
  CMDLN: LINE;		(* COMMAND LINE *)
  CMDLEN,CMDPTR: LINEPTR;	(* COMMAND LENGTH/INDEX *)
  repeat_cnt: lineptr;		(* /R:n switch parameter *)
  dodecap,			(* flags $decap on in effect *)
  dounder,			(* $flags $underline on in effect *)
  nodecap,			(* on if currently within at_sign string
				   and $decap is on *)
  docontrol: boolean;		(* flags $control on in effect *)

  under_state,			(* retains state of underlining duw
				   to & -- if within ampersand string,
				   set to [underline], otherwise null *)
  current_state: attribute_set; (* retains state of other
				  character commands between lines *)

VAR
 
  CURTABS: TABARRAY; (*CURRENT TAB SETTINGS*)
  CURTABCNT: 0..MAXTABS;  (* # TAB SETTINGS*)
  PINDENT,           (*PARAGRAPH INDENTATION*)
  CRINDENT,CLINDENT, (*RIGHT/LEFT INDENTATIONS FROM MARGIN*)
  CSPACING,          (*INTER-LINE SPACING*)
  CWIDTH,NWIDTH,     (*PAGE WIDTH (CURRENT AND NEW ONE FOR NEXT PAGE)*)
  CMARGIN,NMARGIN: LINEPTR;  (*MARGIN*)
  LINECNT,           (*COUNT OF LINES IN CURRENT PAGE*)
  PAGECNT,           (*COUNT OF PAGES PRINTED*)
  CTOP,NTOP,         (*BLANK LINES AT TOP OF PAGE*)
  CBOTTOM,NBOTTOM,   (*BLANK LINES AT BOTTOM OF PAGE*)
  CLENGTH,NLENGTH: INTEGER;   (*PAGE LENGTH*)
  CTITLE,NTITLE:PSTRGDESC;    (*TITLE*)
  CFOOT,NFOOT: PSTRGDESC;     (*FOOTNOTES*)
  CENTRY,NENTRY: PSTRGDESC;     (* table of contents line *)
  CNUMBER,NNUMBER: NUMDESC;   (*PAGE NUMBERING INFORMATION*)
  PAGERANGE, NPR, LPR: RANGE;	(* LIST OF PAGES TO OUTPUT *)
 
  OUTLINE: LINE;      (*TEXT LINES*)
  OUTBUF: LINE;               (*CURRENT OUTPUT LINE*)
  OUTLEN: LINEPTR;
  CHCNT: LINEPTR;             (*CHAR COUNT IN CURRENT OUTPUT LINE*)
  F_NAME: STRING[75];		(* $INCLUDE FILENAME *)
 
  CURSTATE: STATE;            (*INPUT PROCESSING MODE*)
  ATTOP,                      (*TOP OF PAGE FLAG*)
  page_top,     (* flags reason for being at top *)
  DONE: BOOLEAN;              (*INPUT COMPLETE FLAG*)
 
  CH: chtype;
  PAUSE,ADVANCE,CC,PRINTLNUM,
  DASHES,BACKSPACE,DO_UNDER: BOOLEAN;      (*OPTION FLAGS*)
 
  LNUM: LINENUM; LCNT: INTEGER;  (*FOR L OPTION*)
  I: INTEGER;   (*HANDY LOOP INDEX*)
  map: trans_table;  (* translation table *)
  njust: justype;   (* alternating just *)
  terminal: terminal_type;   (* type of terminal *)

  S_VAL: LINE;
  S_LEN: LINEPTR;

const
  dash: chtype := ('-', []);
 
PROCEDURE INITSCRIBE;	(* DEFAULT SETTINGS *)
BEGIN
  CRINDENT:= 0; CLINDENT:= 0; CURTABCNT:= 0;
  CSPACING:= 0;
  CWIDTH:= 75; NWIDTH:= 75;
  CMARGIN:= 5; NMARGIN:= 5;
  CTOP:= 6;  NTOP:= 6;
  CBOTTOM:= 6; NBOTTOM:= 6;
  CLENGTH:= 66; NLENGTH:= 66;
  CTITLE:= NIL; NTITLE:= NIL; CFOOT:= NIL; NFOOT:= NIL;
  CNUMBER.WHERE:= NUMBOT; NNUMBER.WHERE:= NUMBOT;
  CNUMBER.JUST:= JUSCENTER; NNUMBER.JUST:= JUSCENTER;
  CNUMBER.NUM:= 1; NNUMBER.NUM:= 1;
  PINDENT:= 0;
 
  PAGECNT := 0; PAGERANGE := NIL;
 
  CURSTATE:= JUSTIFYING; DONE:= FALSE; ATTOP:= TRUE;
 
  repeat_cnt:= 1;
  DASHES:= FALSE; BACKSPACE:= FALSE; DO_UNDER:= TRUE;
  PAUSE:= FALSE; ADVANCE:= FALSE; CC:= FALSE; PRINTLNUM:= FALSE;
  page_top := true;
  terminal := nospecial;
 
  CHCNT:= 0;

  (* INITIALIZE STRUCTURES PERTAINING TO SECTION NUMBERING *)

  CLEVEL:= 1;
  FOR I:=1 TO MAXLEVELS DO CURPOSN[I]:= 1;
  CURLEVNUM[1]:= 0;
END;   (*INITPROCEDURE*)
 
                    (* EXTERNAL PROCEDURES *)
 
EXTERNAL FUNCTION GETLINE(VAR F:TEXT; LENGTH:LINEPTR;      (*LINE READER*)
  CURSTATE: STATE; VAR RDLINE:LINE; VAR RDLEN:LINEPTR;
  VAR CMDLINE:LINE; VAR CMDLEN:LINEPTR): BOOLEAN;
EXTERNAL PROCEDURE INITREADER;
EXTERNAL PROCEDURE INITJUSTIFY;	(* JUSTIFICATION MODULE INITIALIZATION *)
EXTERNAL PROCEDURE GETLNR(VAR LNUM:LINENUM; VAR LCNT:INTEGER);   (*RETURN CUR. LNUM*)

EXTERNAL PROCEDURE WRTLINE(VAR F:TEXT; VAR ALINE:line;           (*LINE WRITER*)
  ITSLENGTH:LINEPTR; DO_UNDER:BOOLEAN; BACKSPACE:BOOLEAN; map: trans_table;
  terminal: terminal_type);


EXTERNAL PROCEDURE SCAN(VAR CURTOK:TOKENDESC);
