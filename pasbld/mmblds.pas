module mmblds options special (word);

(* BUILD command support routines for ODMS system. *)

$Include MMSSYM.TYP
$Include MMSRL.TYP

var
  f: text;				(* file for ".SYM" stuff *)
  fsrl : file of pdp10word;		(* File for ".SRL" *)
  hsloc, lsloc: integer;		(* relocation counters *)
  curr_high: boolean;

  SYMBOLS : SYM_ANCHOR;			(* For the .SRL symbols *)
  REC     : DATA_ARY;			(* For 18 word block for the 10 *)
  REC_IDX : DATA_ARY_IDX;		(* WHich word of the avail 18 *)

(* Forward procedure declarations *)

Procedure REL_XWD ( LH:Integer; LREL:Boolean; RH:Integer; RREL:Boolean);Forward;
$PAGE ASSM_WRITE
Procedure ASSM_WRITE ( S : String [ 256 ] );

(* This procedure controls the actual output of information to the .SYM file.
   If the file is not being used the no write is to be performed *)

Begin
  If F <> Nilf
    Then Writeln ( F , S )
End;
$PAGE THE_CURRENT_LOC
Function THE_CURRENT_LOC : Integer;

(* This function simply returns the address of the next available memory
   location. It uses the static flag, CURR_HIGH, to determine if the address
   to be returned is an address in the high segement or in the low segment.  *)

Begin
  If CURR_HIGH
    Then THE_CURRENT_LOC := HSLOC
  Else THE_CURRENT_LOC := LSLOC
End;
$PAGE DROP_CUR_REC
Procedure DROP_CUR_REC;

(* This procedure writes the contents of the current PDP10 output record
   to the .SRL file. Since the record may not be completly full due to the 
   nature of the LINK-10 data record the index, REC_IDX, to the next available
   slot in the PDP10 output record is used to limit the amount of the
   record to be output *)

Var I : DATA_ARY_IDX;

Begin
  For I := 1 to REC_IDX-1 Do	(* Only the filled-in fields *)
    Begin
      FSRL^ := REC [ I ];
      Put ( FSRL )
    End
End;
$PAGE INIT_REC
Procedure INIT_REC ( LINK_ITEM : ITEM_TYPE );

(* Initialize the PDP10 data record. If the record currently contains
   anything more than initialization data, then dump the record to the .SRL
   file. Initialize the record to the type specified by the input parameter. *)

Begin
  REC [ 1 ].RH := REC_IDX - 3;

  (* If the record contains anything more than initialization stuff
     then dump it. Code records have 3 words init, so rec_idx points to
     4 if empty. Other records contain 2 words init, but require two
     words of data. *)

  If REC_IDX > 4
    Then DROP_CUR_REC;
  REC [ 1 ].LH := LINK_ITEM;
  REC [ 1 ].RH := 0b;
  REC [ 2 ].VALUE := 0b;
  REC_IDX := 3;

  If LINK_ITEM = CODE_REC		(* Set start addr of block *)
    Then REL_XWD ( 0b , False , THE_CURRENT_LOC , True )
End;
$PAGE REL_WORD
Procedure REL_WORD ( WORD : PDP10WORD;
		     LREL : Boolean;
		     RREL : Boolean );

(* Put a fullword into the data record and set the proper relocation bits.
   If the record becomes full after adding the word then initialize a new
   record of the same type. This is the routine which will put all data,
   except for initialization, into the data record. This is done by
   the extensive use of the undiscriminated type, 'PDP10WORD'. *)

Begin
  REC [ REC_IDX ] := WORD;
  REC_IDX := REC_IDX + 1;

  (* Set the relocation bits in the relocation word *)

  REC [ 2 ].REL_BYTE [ REC_IDX-3 ] := 2 * Ord(LREL) + Ord ( RREL );

  (* If the record is now full then dump it *)

  If REC_IDX >= 21
    Then INIT_REC ( REC [ 1 ].LH )	(* Start a new record of the same type*)
End;
$PAGE REL_XWD
Procedure REL_XWD ( LH : Integer;  LREL : Boolean;
		    RH : Integer;  RREL : Boolean );

(* Create the next word in the data record from the two halfwords
   passed the this routine. Call REL_WORD with the fullword created
   from the two halfwords because it already does the relocation stuff *)

Var WORD : PDP10WORD;
    TEMP : PDP10WORD;

Begin
  (* The values coming in may be fullword integers, we only want the second
     half word, so coerce it out of them *)

  TEMP.VALUE := LH;
  WORD.LH := TEMP.RH;
  TEMP.VALUE := RH;
  WORD.RH := TEMP.RH;
  REL_WORD ( WORD , LREL , RREL )
End;

(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)


Procedure REL_INT ( X : Machine_word;
		    REL : Boolean );

(* Add an integer value as the next entry in the data record. The relocation
   information for the right halfword will be passed as a parameter. The
   left half will never be relocated. Call REL_WORD to get the real stuff
   done. *)

Var WORD : PDP10WORD;

Begin
  WORD.VALUE := X;
  REL_WORD ( WORD , False , REL )
End;
$PAGE radix50

(*  Radix50 takes a four-bit code and a six-character symbol, and constructs
    a PDP 10 word out of them, translating the symbol into 32-bit radix-50
    format.  *)


Public Function RADIX50 ( CODE : Integer;
			  SYM : Packed Array [ 1..6 ] of Char ) : PDP10WORD;

type
    rep_array = packed array [' ' .. '_'] of 0..50b;

const
    rep_50: rep_array =
     (    0,   0,   0,   0,   46b, 47b,   0,   0,     0,   0,   0,   0,     0,   0, 45b,   0,
         1b,  2b,  3b,  4b,    5b,  6b,  7b, 10b,   11b, 12b,   0,   0,     0,   0,   0,   0,
          0, 13b, 14b, 15b,   16b, 17b, 20b, 21b,   22b, 23b, 24b, 25b,   26b, 27b, 30b, 31b,
        32b, 33b, 34b, 35b,   36b, 37b, 40b, 41b,   42b, 43b, 44b,   0,     0,   0,   0, 47b  );

var
    i: 1 .. 6;
    SYMVAL : Machine_word;

begin
  symval := 0;
  for i := 1 to 6 do
    if sym [i] <> ' ' then
      symval := symval * 50b + rep_50 [sym [i]];
  radix50.code50 := code;
  radix50.sym50 := symval;
end (* radix_50 *);
$PAGE POLISH_SYMBOL
Procedure POLISH_SYMBOL ( SYM : ^SYM_ENTRY );

(* Write out a LINK-10 record for a polish symbol. Remember that polish
   symbols may contain a maximum of six characters. *)

Begin
  INIT_REC ( POLISH_REC );

  REC [ 2 ].LH := 020000b;
  REC [ 2 ].RH := 0b;

  If Length ( SYM^.SYMBOL ) > 6
    Then REC [ 4 ] := RADIX50 ( 2 , Substr ( SYM^.SYMBOL , 1 , 6 ) )
  Else REC [ 4 ] := RADIX50 ( 2 , SYM^.SYMBOL );
  REC [ 3 ].LH := 2b;
  REC [ 3 ].RH := REC [ 4 ].LH;
  REC [ 4 ].LH := REC [ 4 ].RH;
  REC [ 4 ].RH := 777775b;

  REC [ 5 ].LH := SYM^.ADDR;
  REC [ 5 ].RH := 0;
  REC_IDX := 6;

  INIT_REC ( SYMBOL_REC )
End;
$PAGE REG_SYMBOL
Procedure REG_SYMBOL ( SYM : ^SYM_ENTRY;
			COD: 0..17B );

(* Add a symbol to the current record being output. Remember only six
   characters are allowed. The value passed in parameter COD is a value
   indicating which type of symbol is being output. The value is the
   actual LINK-10 identifier. 1b indicates a public definition symbol.
   14b indicates an external symbol linkage. Call the general wrapper REL_WORD
   to have the actual word done *)

Var STR : String [ 6 ];

Begin
  If Length ( SYM^.SYMBOL ) > 6
    Then STR := Substr ( SYM^.SYMBOL , 1 , 6 )
  Else STR := SYM^.SYMBOL;

  REL_WORD ( RADIX50 ( COD , STR ) , False , False );
  REL_XWD ( 0b , False , SYM^.ADDR , Not SYM^.FIRST )
End;
$PAGE DROP_SYMBOLS
Procedure DROP_SYMBOLS;

(* Walk through the symbol table calling the proper routine to construct
   the correct link record, and disposing the table as we go. Remember
   that the values are static variables so re-initialize them incase the
   user wants to create another .SRL during this execution of ODMS *)

Var WALK : ^SYM_ENTRY;
    HOLD : ^SYM_ENTRY;

Begin
  INIT_REC ( symbol_rec );
  WALK := SYMBOLS.FIRST;
  While WALK <> Nil Do
    Begin
      Case WALK^.KIND of
	POLISH : POLISH_SYMBOL ( WALK );
	LOCAL  : ;	(* Do nothing *)
	EXT_DEF: REG_SYMBOL ( WALK , 14b );
	DEFINED ,
	PUBLIC_DEFINITION : REG_SYMBOL ( WALK , 1B )
      End;
      HOLD := WALK;
      WALK := WALK^.NEXT;
      Dispose ( HOLD )
    End;
  SYMBOLS.FIRST := Nil;
  SYMBOLS.LAST := Nil
End;
$PAGE FINDSYM
Function FINDSYM ( SYM : SYM_STRING ) : ^SYM_ENTRY;

(* This routine searches through the symbol tabel for the occurance of
   the symbol SYM. If the symbol is found then a pointer to the symbol
   table entry is returned. If the symbol were not found then add the
   symbol to the tabel and return a pointer to the node added to the
   symbol table. The type of the symbol table entry will be set to
   the type for an externally referenced symbol. If the symbol ends up not
   being of that type then the routines ENTRY, DEFINE, ARGNAM, or
   DO_LABEL will reset the type to the desired type. *)

Begin
  FINDSYM := SYMBOLS.FIRST;
  While FINDSYM <> Nil Do
    Begin
      Exit If FINDSYM^.SYMBOL = SYM;
      FINDSYM := FINDSYM^.NEXT
    End;

  If FINDSYM = Nil
    Then Begin
      New ( FINDSYM );
      If SYMBOLS.FIRST = Nil
	Then Begin
	  SYMBOLS.FIRST := FINDSYM;
	  SYMBOLS.LAST  := FINDSYM
	End
      Else Begin
	SYMBOLS.LAST^.NEXT := FINDSYM;
	SYMBOLS.LAST := FINDSYM
      End;
      FINDSYM^.SYMBOL := SYM;
      FINDSYM^.ADDR := 0;
      FINDSYM^.LEFT_CHAIN := False;
      FINDSYM^.FIRST := True;
      FINDSYM^.NEXT := Nil;
      FINDSYM^.KIND := EXT_DEF
    End
End;
$PAGE SET_ADR
Procedure SET_ADR ( S : SYM_STRING;
		Var SYM : ^SYM_ENTRY;
		Var ADR : Integer;
		Var REL : Boolean );

(* This is a general purpose routine used to look up a symbol, return
   a backchain address for the symbol, determine the "new" address for the
   symbol (or backchained symbol), and returns an indication if the
   symbol may be relocated. *)

Begin
  SYM := FINDSYM ( S );
  ADR := SYM^.ADDR;

  (* If this is not the first symbol then it may be relocated. THis
     is used in backchaining where the anchor of the chain must be
     a non-relocatable zero. *)

  REL := Not SYM^.FIRST;
  SYM^.FIRST := False;
  If not ( SYM^.KIND in [ PUBLIC_DEFINITION , LOCAL ] )
    Then SYM^.ADDR := THE_CURRENT_LOC
End;
$PAGE setupcode
Public Function SETUPCODE ( SRLFN : File_name;
				SYM_FN : File_name ): Boolean;

(* SETUPCODE first rewrites the ".SYM" and ".SRL" files; if it can,
   it returns true, else false. The routine emits the first few lines of 
   each file and sets up the .SRL files data record. If either file fails 
   to open then insure that the other one gets closed. *)

begin
  FSRL := Nilf;
  F := Nilf;
  If SYM_FN <> ''
    Then Rewrite ( F , '[,].SYM ' || SYM_FN );
  Rewrite ( FSRL , SRLFN || ' [,].SRL' );
  SETUPCODE := Eof ( FSRL ) And ( (F=Nilf) Orif Eof ( F ) );
  if setupcode then begin		(* we're cool *)
    hsloc := 400000b;			(* Where the highseg starts *)
    lsloc := 0b;			(* Where the losegment starts *)
    curr_high := true;			(* according to RELOC below *)

    (* Write out the initialization stuff for the .SYM file *)

    ASSM_WRITE ( '       twoseg' );
    ASSM_WRITE ( '       reloc   400000' );
    ASSM_WRITE ( '       title   PASTV.' );

    (* Now write out the initial stuff for the .SRL file *)

    SYMBOLS.FIRST := Nil;		(* Insure the symbol table empty *)
    SYMBOLS.LAST  := Nil;

  (* Indicates a .rel file to the linkage loader. *)

    FSRL^.LH := 4b;
    FSRL^.RH := 0b;
    Put ( FSRL );

    (* Filler needed by link *)

    FSRL^.VALUE := 0b;
    Put ( FSRL );

    (* Indicates upcomming name of the module *)

    FSRL^.LH := 6b;
    FSRL^.RH := 2b;
    Put ( FSRL );

    (* Filler because name is only allowed 6 chars, but set up for more *)

    FSRL^.VALUE := 0b;
    Put ( FSRL );

    (* The name of the module "PASTV." *)

    FSRL^.LH := 024036b;
    FSRL^.RH := 741245b;
    Put ( FSRL );

    (* Indicate that the upcoming information specifies where the high segment
       and the low segment start *)

    FSRL^.LH := 11b;
    FSRL^.RH := 0b;
    Put ( FSRL );

    (* Relocation bits for the high and low segment starts *)

    FSRL^.LH := 3b;
    FSRL^.RH := 1b;
    Put ( FSRL );

    (* Maximum loseg address, some big number 200000, but starts at 0 *)

    FSRL^.LH := 200000b;
    FSRL^.RH := 0b;
    Put ( FSRL );

    (* Highseg starts at 400000, but since we dont know where it ends fill 
       in the same value and LINK will go back and fix it up for us *)

    FSRL^.LH := 400000b;
    FSRL^.RH := 400000b;
    Put ( FSRL );

    (* Initialize the code record to be used throughout *)

    REC_IDX := 4;
    INIT_REC ( CODE_REC )
  End
  Else Begin
    If F <> Nilf Then Close ( F );
    If FSRL <> Nilf Then Close ( FSRL )	(* In case only 1 opened *)
  End
end (* function setupcode *);
$PAGE finishcode
public procedure finishcode;

(* FINISHCODE calls a routine to dump the symbol table. Then dump the
   END to the .SYM file and the end record to the .SRL file. Finally,
   close the files. *)

begin
  (* Dump all the LINKage symbols *)
  DROP_SYMBOLS;

  ASSM_WRITE ( ' end' );

  (* Dump the current rec and output an end rec *)
  INIT_REC ( end_rec );
  REC [ 2 ].LH := 240000b;
  REC [ 2 ].RH := 0b;
  REC_IDX := 3;
  REL_XWD ( 0b , False , HSLOC , True );
  REL_XWD ( 0b , False , LSLOC , True );
  REC [ 1 ].RH := 2b;

  (* Drop the end record and we are finished *)

  DROP_CUR_REC;

  (* Now close the files *)

  If F <> Nilf
    Then Close ( F );
  Close ( FSRL )
end (* procedure finishcode *);
$PAGE drop_line and comment  for emitting lines
procedure drop_line (s: string[60]);

(* DROP_LINE emits the passed string, and then increments the appropriate
   relocation counter by one.  *)

begin
  ASSM_WRITE ( S );
  if curr_high
    then hsloc := hsloc + 1
  else lsloc := lsloc + 1
end (* procedure drop_line *);



(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)


public procedure comment (s: string[60]);

(* COMMENT drops its line as a comment, with a leading semicolon. *)

begin
  ASSM_WRITE ( ';        ' || S )
end (* procedure comment *);
$PAGE hiseg and loseg  relocation routines
public procedure hiseg;

(* HISEG sets us up to emit code into the high segment, assuming we're
   not there already. *)

Var TEMP : String [ 40 ];

begin
  if not curr_high then begin
    Putstring ( TEMP , '       reloc   ' , HSLOC:6:O );
    ASSM_WRITE ( TEMP );
    curr_high := true;
    INIT_REC ( code_rec )
  end
end (* procedure hiseg *);


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)


public procedure loseg;

(* LOSEG does essentially the same thing. *)

Var TEMP : String [ 40 ];

begin
  if curr_high then begin
    Putstring ( TEMP , '       reloc   ' , LSLOC:6:O );
    ASSM_WRITE ( TEMP );
    curr_high := false;
    INIT_REC ( code_rec )
  end
end (* procedure loseg *);
$PAGE fword and hwords   for emitting single words
public procedure fword (i: machine_word);

(* FWORD simply emits its parameter as a word for MACRO to eat. *)

var
  s: string[60];

begin
  If I < 0
    Then Putstring ( S , '-' , ( -I ):12:O )
  else putstring (s, i:12:o);

  drop_line ('	' || s);

  (* Drop the word to the '.SRL' file too *)
  REL_INT ( I , False )
end (* procedure fword *);

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

public procedure hwords (l, r: integer);

(* HWORDS takes the left and right halves of a word respectively. *)

var
  s: string[60];

begin
  putstring (s, '	', l:6:o, ',,', r:6:o);

  drop_line (s);

  REL_XWD ( L , False , R , False )
end (* procedure hwords *);
$PAGE EMIT_INTERNAL_REQUEST
Procedure EMIT_INTERNAL_REQUEST ( ADR : Integer;
				  LEFT: Boolean );

(* This routine emits the special "backchain" internal request record.
   This is called when a backchained symbol is finally defined. This
   emits the backchain walk node and is smart and sets the rest of the
   references made in the .SRL to the address of the symbol *)

Begin
  INIT_REC ( int_request_rec );
  If LEFT			(* Special word emitted for left-backchainig *)
    Then REL_INT ( -1 , False );
  REL_XWD ( ADR , True , THE_CURRENT_LOC , True );
  REC [ 1 ].RH := REC_IDX - 3;
  DROP_CUR_REC;			(* Make sure it gets put out now *)
  REC_IDX := 4;		(* So that we only new a record *)
  INIT_REC ( code_rec )
End;
$PAGE entry and extern   symbol definition routines
public procedure entry (m: mdlid);

(* ENTRY causes a symbol to become an entry. *)

Var SYM : ^SYM_ENTRY;

begin
  SYM := FINDSYM ( M );
  If Not SYM^.FIRST	(* Emit the backchaining if needed *)
    Then EMIT_INTERNAL_REQUEST ( SYM^.ADDR , SYM^.LEFT_CHAIN );
  SYM^.ADDR := THE_CURRENT_LOC;
  SYM^.FIRST := False;
  SYM^.KIND := PUBLIC_DEFINITION;	(* Set the type of the symbol node *)

  (* Write the symbol to the .SYM file for macro to do the same thing *)

  ASSM_WRITE ( M || '::' )
end (* procedure entry *);


(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)


public procedure extern (m: mdlid);

(* EXTERN emits a request for an external symbol. *)

Var SYM : ^SYM_ENTRY;

begin
  SYM := FINDSYM ( M );
  SYM^.KIND := EXT_DEF;

  (* Emit the external symbol the the .SYM file *)

  ASSM_WRITE ( ' extern  ' || M )
end (* procedure extern *);
$PAGE DROP_ADDR PUSHJ17 and JSP1    References to transfer vector
Public Procedure JSP1 ( s : string [ 10 ] );

(* Emit a jump within the transfer vector to both the .SRL and .SYM *)

Var SYM : ^SYM_ENTRY;
    RELOC : Boolean;
    ADR : Integer;

Begin
  SET_ADR ( S , SYM , ADR , RELOC );
  DROP_LINE ( '    jsp 1,' || s );
  REL_XWD ( 265040b , False , ADR , RELOC )
End;

(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

Public Procedure PUSHJ17 ( s : string [ 10 ] );

(* Emit a pushjump the the specified symbol *)

Var SYM : ^SYM_ENTRY;
    ADR : Integer;
    RELOC : Boolean;

Begin
  SET_ADR ( S , SYM , ADR , RELOC );
  DROP_LINE ( '    PUSHJ 17,' || s );
  REL_XWD ( 260740b , False , ADR , ADR<>0 )
End;

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

Public Procedure DROP_ADDR ( s : String [ 10 ] );

(* Emit the address of a known symbol into the right halfword *)

Var SYM : ^SYM_ENTRY;
    ADR : Integer;
    RELOC : Boolean;

Begin
  SET_ADR ( S , SYM , ADR , RELOC );
  DROP_LINE ( '     000000,,' || s );
  REL_XWD ( 0b , False , ADR , RELOC )
End;
$PAGE jrst
public procedure jrst (id: mdlid);

(* EMitST to a symbol name *)

Var SYM : ^SYM_ENTRY;
    ADR : Integer;
    RELOC : Boolean;

begin
  SET_ADR ( ID , SYM , ADR , RELOC );
  drop_line ('	jrst		' || id);
  REL_XWD ( 254000b , False , ADR , False )
end (* procedure jrst *);

(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

Public Procedure JRSTADDR ( I : Integer );

(* Emit a JRST to a known address *)

Var STR : String [ 60 ];

Begin
  Putstring ( STR , '  jrst           ' , I:6:O );
  drop_line ( STR );
  REL_XWD ( 254000b , False , I , False )
End;

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

Procedure JRST1 ( id : mdlid );

(* Emit a JRST 1, to a symbol name *)

Var SYM : ^SYM_ENTRY;
    ADR : Integer;
    RELOC : Boolean;

Begin
  SET_ADR ( ID , SYM , ADR , RELOC );
  drop_line ('        jrst    1,      ' || id );
  SYM := FINDSYM ( ID );
  REL_XWD ( 254040b , False , ADR , RELOC )
End;
$PAGE PORTAL
Public Procedure PORTAL ( S : SYMPTR );

(* PORTAL drops a portal for high seg LTVs, checking for resident fillers. *)

Begin
  If S^.NAME = Nil
    Then JSP1 ( 'MOE.TV' )
  Else Begin
    EXTERN ( S^.NAME^.TEXT );
    JRST1 ( S^.NAME^.TEXT )
  End
End;  (* Procedure PORTAL *)
$PAGE define
public procedure define (id: mdlid; v: integer);

(* DEFINE creates a definition for a symbol external to the module. *)

Var SYM : ^SYM_ENTRY;
    TEMP: String [ 40 ];

begin
  SYM := FINDSYM ( ID );
  SYM^.ADDR := V;
  SYM^.FIRST := True;
  SYM^.KIND := DEFINED;	(* Remember to set the type of the symbol *)

  (* Drop the definition to the .SYM file *)

  Putstring ( TEMP , ID , '=:' , V:6:O );
  ASSM_WRITE ( TEMP )
end (* procedure define *);
$PAGE argname
public procedure argnam (id: mdlid);

(* ARGNAM emits an ARG pointer to a name. *)

Var SYM : ^SYM_ENTRY;

begin
  (* ARGNAME must set "Polish" attribute *)
  SYM := FINDSYM ( ID );
  SYM^.KIND := POLISH;		(* Remember to set the type of the symbol node *)
  SYM^.ADDR := THE_CURRENT_LOC;
  SYM^.FIRST := False;

  drop_line ('  exp	' || id || '		; arg pointer');

  REL_XWD ( 0b, False , 0b , False )
end (* procedure argnam *);


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)


public procedure do_label (s: mdlid);

(* LABEL emits a label without making it public *)

Var SYM : ^SYM_ENTRY;

begin
  ASSM_WRITE ( S || ':' );

  SYM := FINDSYM ( S );
  SYM^.KIND := LOCAL;		(* Remember to set the type of the symbol *)
  If Not SYM^.FIRST
    Then EMIT_INTERNAL_REQUEST ( SYM^.ADDR , SYM^.LEFT_CHAIN );
  SYM^.FIRST := False;
  SYM^.ADDR := THE_CURRENT_LOC
end (* procedure do_label *);
$PAGE mtvent
public procedure mtvent (id: mdlid; m: integer; ltv: integer);

(* MTVENT drops a master transfer vector entry. *)

begin
  PUSHJ17 ( 'OVL.TV' );		(* one word *)
  comment ('Overlaid symbol ' || id);
  hwords (m, ltv)
end (* procedure mtvent *);
$PAGE alloc and padout
public procedure alloc (n: integer);

(* ALLOC allocates a block of storage by adjusting '.'  . *)

Var TEMP : String [ 40 ];

begin
  assert (n >= 0);
  If curr_high
    Then hsloc := hsloc + n
  else lsloc := lsloc + n;
  Putstring ( TEMP , ' block   ' , N:6:O );
  ASSM_WRITE ( TEMP );

  (* Must also dump the current block being created ! *)
  INIT_REC ( code_rec )
end (* procedure alloc *);


(* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

public procedure padout;

(* PADOUT extends the current relocation counter to the next page boundary. *)

var p: integer;

begin
  if curr_high
    Then P := ( HSLOC + 10b ) Mod 1000b	(* Reserved stuff *)
  Else P := ( LSLOC + 140b ) Mod 1000b;	(* The loseg reserved stuff *)
  P := 1000b - P;
  If P < 1000b
    Then ALLOC ( P )
end (* procedure padout *);
$PAGE DROP_ASCII
Procedure DROP_ASCII ( S : String [ 60 ] );

(* Drop an ascii string to the .SRL file. This is done by first determining
   the length of the string to be dropped. If the string won't fit into the 
   current code record then dump the code record. Then drop the ascii
   string by parcelling the string into 5 character untis and dumping them one
   at a time to the output record. This is done by using the .STR field ot
   the undiscriminated record, PDP10WORD *)

Var I : Integer;
    TEMP : String [ 60 ];
    WORD: PDP10WORD;

Begin
  If ( Length ( S ) Div 5 ) + REC_IDX > 21
    Then INIT_REC ( code_rec );

  TEMP := S;
  While Length ( TEMP ) >= 5 do
    Begin
      WORD.VALUE := 0;
      WORD.STR := Substr ( TEMP , 1 , 5 );
      If Length ( TEMP ) = 5
	Then TEMP := ''
      Else TEMP := Substr ( TEMP , 6 );
      REL_WORD ( WORD , False , False );
    end;

  WORD.VALUE := 0;
  If Length ( TEMP ) > 0
    Then Begin
      For I := 1 to 5 Do
	Begin
	  If Length ( TEMP ) > 0 Then Begin
	    WORD.STR [ I ] := TEMP [ 1 ];
	    If Length ( TEMP ) = 1
	      Then TEMP := ''
	    Else TEMP := Substr ( TEMP , 2 )
	  End
	End
    End;
  REL_WORD ( WORD , False , False )
End;
$PAGE nhword
public procedure nhword (
  lh, rh: mdlid);

(* NHWORD emits the halves of a fullword as symbolic names. *)

Var SYM1 , SYM2 : ^SYM_ENTRY;
    ADR1 , ADR2 : Integer;
    RELOC1 , RELOC2 : Boolean;

begin
  SET_ADR ( LH , SYM1 , ADR1 , RELOC1 );
  SYM1^.LEFT_CHAIN := True;
  SET_ADR ( RH , SYM2 , ADR2 , RELOC2 );

  drop_line ( lh || ',,' || rh );

  REL_XWD ( ADR1 , RELOC1 , ADR2 , RELOC2 )
end (* procedure nhword *);


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)


Public Procedure SHWORD ( I : Integer;
			  S : MDLID );

(* Emit a full word consisting of a known address followed by a symbol *)

Var STR : String [ 60 ];
    SYM : ^SYM_ENTRY;
    RELOC : Boolean;
    ADR : Integer;

Begin
  Putstring ( STR , I:6:O );
  SET_ADR ( S , SYM , ADR , RELOC );
  
  DROP_LINE ( STR || ',,' || S );

  REL_XWD ( I , False , ADR , RELOC )
End;	(* SHWORD *)
$PAGE DROP_TEXT
public procedure drop_text (
  wds: integer;				(* total size of text block *)
  s: string[60] );			(* the string to use *)

(* TEXT emits a Pascal string into a block of code. *)

var
  i: integer;

begin
  DROP_ASCII ( S );

  ASSM_WRITE ( 'asciz/' || S || '/' );	(* drop the string itself *)
  i := ( length (s) div 5 ) + 1;	(* # of words used by text *)
  If CURR_HIGH
    Then HSLOC := HSLOC + 1
  else lsloc := lsloc + i;		(* correct relocation counters *)
  If I < WDS
    Then ALLOC ( WDS - I )
end (* procedure drop_text *).
P Aå