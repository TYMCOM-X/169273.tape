(* ****************************************************************************

	        Standard LL(1) Parse Machine Interpreter


   LL(1) analysis (top-down, no-backup) package.  Assumes the existence of two
   external routines, "scan", the lexical analyzer, and "generate", the 
   semantic processor.  The LL(1) compiler generator creates three include
   files: "grammar.sym" which defines a scalar type naming all terminal symbols
   and designated nonterminal symbols; "grammar.sem" which defines a scalar
   type giving the semantic operation codes that are passed to generate; and
   "grammar.tab" which contains the parsing machine tables an its associated
   definitions.

   Format of parsing machine tables:

   sym => defines the symbol to be recognized.  If it is a terminal symbol
	  (i.e. sym < nonterminal), sym is matched against the symbol class
	  of the current token from scan.  If it is a nonterminal symbol, the
	  pm "subroutine" defining the nonterminal is called by stacking the
	  address of the current pm instruction and continuing execution at
	  ntaddr.

   at =>  gives the address at which to continue the parse if the recognition
	  operation is successful.  Address less than minpmaddr indicate
	  a "return" from a pm subroutine.

   af =>  gives the continuation address if the recoginition operation fails.
	  The case af = err_rt causes immediate parse termination unless the
	  semantic operation takes error recovery action.

   sg =>  gives the semantic operation to perform if recognition is successful.
	  The value "nosemop" implies no operation.

   fg =>  gives the semantic operation to perform if recognition fails. The
	  operation may take recovery action on errors via the error routines
	  defined below, which alter the flow of pm interpretation so that
	  the err_rt action is not taken, or which skip or insert terminal
	  symbols.


   **************************************************************************** *)
$PAGE declarations
(* ====>>>>  include standard include files here  <<<<==== *)
$include anal.sym
$include anal.sem
$include anal.tab

var    (* parser state information *)
    ic,						(* address of current pminst *)
    nextic: pmaddr;				(* address of next pminst, changed by recovery routines*)
    needtok: boolean;				(*flags scan required on next terminal recognition*)
    tokensym: symbols;				(* token read by scanner *)

const maxpmstack = 100;				(* parse stack *)
var   pmstack: array[1..maxpmstack] of pmaddr;
      pmstackptr: 0..maxpmstack;

(*     assumed external procedures     *)

external function scan: symbols;
external procedure generate ( code: pmsemop );
$PAGE initparse
(*    procedure to initialize syntax analyzer    *)

public procedure initparse;
begin
  pmstackptr:= 1;
  needtok:= true;
  ic:= minpmaddr;
end (*initparse*);

$PAGE parse
(*    the syntactical analyzer    *)

type parseret = (parsecomplete,parsestackoflo,parseerror);

public procedure parse( var code: parseret);
  label 100,200;
begin

  (*  decode current pm instruction op code  *)

100:
  with pmtable[ic] do begin
    if sym < nonterminal then begin
      if needtok then begin			(*must scan new symbol*)
	tokensym := scan;
	needtok:= false
      end;
      if tokensym = sym then begin		(*recognition*)
	nextic:= at;
	if sg<>nosemop then generate(sg);
	needtok:= true				(*symbol has been recognized*)
      end
      else begin				(*no recognition*)
	nextic:= af;
	if fg<>nosemop then generate(fg)	(*may change nextic*)
      end;
      goto 200					(* go see what to do next *)
    end
    else begin					(* non-terminal symbol -- perform pm subroutine call *)
      if pmstackptr < maxpmstack then
	pmstackptr:= pmstackptr+1
      else begin				(* stack overflow *)
	code:= parsestackoflo;
	return
      end;
      pmstack[pmstackptr]:= ic;			(* make the call *)
      ic:= ntaddr;
      goto 100					(* go decode opcode *)
    end
  end;						(* decoding *)

  (* continue with indicated next pm instruction *)

200:
  if nextic >= minpmaddr then begin		(* pm goto *)
    ic:= nextic;
    goto 100					(* back to decode *)
  end;

  (* some kind of pm subroutine return *)

  ic:= pmstack[pmstackptr];
  pmstackptr:= pmstackptr-1;			(* back to call instruction *)

  (* take at or af of this (calling) instruction according to return *)

  if pmstackptr <= 0 then begin			(* back to head of parse *)
    if nextic = err_rt then begin		(* error not recovered from *)
      code:= parseerror;
      return					(* stop the parse *)
    end
    else begin					(* return normally *)
      code:= parsecomplete;
      return
    end
  end
  else with pmtable[ic] do begin		(* some kind of return *)
    if nextic = succrt then begin
      nextic:= at;
      if sg<>nosemop then generate(sg)
    end
    else if nextic = failrt then begin
      nextic:= af;
      if fg<>nosemop then generate(fg)
    end
    else begin
      nextic:= af;
      if fg<>nosemop then generate(fg);		(*may change nextic*)
      if nextic = err_rt then begin		(* no recovery made *)
	code:= parseerror;
	return
      end
    end;
    goto 200					(* decide where to go next *)
  end						(* some kind of return *)
end (*parse*);
$PAGE error recovery routines
(*    error recovery routines    *)

public procedure forcenext;			(* force at branch of current symbol *)
begin
  nextic:= pmtable[ic].at;
  needtok:= true
end (*forcenext*);


public procedure forceloop;			(* force re-parse of current symbol *)
begin
  nextic:= ic;
  needtok:= true
end

(*the*) .					(*end*)
  