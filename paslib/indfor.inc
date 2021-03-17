(* INDFOR.INC Selected public vars for INDFOR module *)

external var
  THIS_TOKEN,
  NEXT_TOKEN: TOKENTYPE;
  LINE: TEXTLINE;
  OPTIONS: OPTIONSET;
  CURSTART,
  CUREND: LINE_IDX;			(* start and end of current token text *)
  EOL_CT: LINE_IDX;			(* number of endoflines following *)
  ORIG_IND,				(* original ind'n of current line *)
  TOK_IND,				(* ind. of current tok rel. to start *)
  CUR_IND,				(* current minimum indentation *)
  COM_IND,				(* indent. for stand-alone comments *)
  OUT_IND: LINE_IDX;			(* final ind. to WRITER *)
  LIN_INDENT,				(* output ind. of current line *)
  QUANTUM: LINE_IDX;			(* ind. quantum, 2 default *)

  READER,
  WRITER: ENVIRONMENT;			(* who we resume *)

  external procedure STK_INIT;		(* procedures from STK_INIT *)
  external procedure MARK (STK_FLAG);	(* stack without changing CUR_IND *)
  external procedure PUSH (STK_FLAG);	(* stack, tick CUR_IND a quantum *)
  external procedure NEW_LEVEL (STK_FLAG); (* stack, line up to THIS_TOKEN *)
  external procedure POP_UNTIL (FLAG_SET); (* pop until one of set is popped *)
  external procedure POP_WHILE (FLAG_SET); (* pop until one not of set *)

(* end of INDFOR.INC *)
