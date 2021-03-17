$TITLE scnnam - SCANNR Name Table Utilities
$LENGTH 43

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         S C N N A M                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     MDSI, Company Confidential
     
     SYSTEM:  SCANNR Lexical Scanner Builder
     
     STARTED:  8 June 1978
     
     PURPOSE:  This module manages the scanner name list.  This is  a
        simple linked list of (name,value) pairs, where the name is a
        string, and the value is a pointer to a regular expression.
     
     ENTRY POINTS:
     
        INITNAMES   initializes the name list  to  contain  a  single
                    symbol  ANY,  which  is  defined  as  the regular
                    expression "minimum symbol ..  maximum symbol".
     
        ENTERNAME   is  called  with  a  name  token  and  a  regular
                    expression.  It  searches  the  name list for the
                    name.  If it is already in the  list,  it  is  an
                    error.  Otherwise,  the name is added to the list
                    with the regular expression as its value.
     
        ENTERSYMBOL is called with a name token which has occurred in
                    an  "ALPHABET  IS name, name, ... name" statement
                    and its index in the name  list.  It  enters  the
                    name  in  the  name  list  as  a literal with the
                    specified value, just as Entername would,  except
                    that it also marks the name as symbolic.
     
        SET_MODE    sets  the  input  mode for this scanner to Ascii,
                    Numeric or Symbolic.  If the  mode  is  symbolic,
                    the  symbolic names from the name list are put in
                    a symbolic names array.  This routine also checks
                    for duplicate alphabet definitions.
     
        LOOKUPNAME  is  called  with  a  name token.  It searches the
                    name list for that  name.  If  it  is  found,  it
                    returns   the   associated   regular  expression.
                    Otherwise, it is an error.
     
        CLEARNAMES  removes all entries from the name  list,  freeing
                    the associated regular expressions.
     
        PRT_SYMBOL  prints   on   the   listing   file  the  symbolic
                    representation  of  a  specified  numeric   value
                    corresponding  to  the current input mode, with a
                    string concatenated on the end.
     
     ---------------------------------------------------------------- *)
$PAGE declarations

$INCLUDE scannr.typ
$INCLUDE scnlit.typ
$INCLUDE scntok.typ

$INCLUDE scnrea
$INCLUDE scnreu
$INCLUDE scnerr
$INCLUDE scnlst
$INCLUDE fio

type
    name = ^ name_block;

    name_block = packed record
	next: name; (* The next entry in the name list. *)
	value: reg_exp; (* The value of the name. *)
	symbolic: boolean;
	name_str: packed array [1..*] of char; (* The name itself. *)
    end;


var name_list: name;

type
    input_modes = ( ascii_mode, numeric_mode, symbolic_mode, undefined_mode );

public var input_mode: input_modes;
	   min_symbol, max_symbol: number;

var nsymbols: number;
    symbols: ^ packed array [0..*] of name;
$PAGE initnames

(*  INITNAMES initializes the name list to contain the single symbol ANY,
    defined as a literal range which includes all possible literals.  *)

public procedure initnames;

begin
  new (name_list, 3);
  with name_list^ do begin
    next := nil;
    value := use_re ( lit_range ( minimum(number), maximum(number) ) );
    name_str := 'ANY';
    symbolic := false;
  end;
  input_mode := undefined_mode;
  nsymbols := 0;
end (* initnames *);
$PAGE enter

(*  ENTER adds a (name,value) pair to the beginning of the name list.  If
    the name is already in the list, an error message is printed and the
    new definition supercedes the old one.  *)

procedure enter ( nam: sym_value; re: reg_exp; sym: boolean );

var p: name;

begin
  p := name_list; (* See if the name is already in the list. *)
  while (p <> nil) andif (p^.name_str <> nam.lit_val^) do
    p := p^.next;

  if p <> nil then begin
    err_token (nam, 'Doubly defined symbol ' || nam.lit_val^);
    free_re (p^.value); (* Free the old value. *)
    p^.symbolic := sym;
    p^.value := use_re (re); (* Use the new value in its place. *)
  end

  else (* p = nil *) begin
    new (p, length (nam.lit_val^)); (* Create the new name node. *)
    with p^ do begin (* Fill in its fields. *)
      next := name_list;
      value := use_re (re); (* Mark the regular expression as in use. *)
      name_str := nam.lit_val^;
      symbolic := sym;
    end;
    name_list := p; (* Attach it to the front of the name list. *)
  end;
end (* enter *);
$PAGE entername

(*  ENTERNAME calls Enter to add a non-symbolic name to the name list.  *)

public procedure entername ( nam: sym_value; re: reg_exp );

begin
  enter (nam, re, false);
end;
$PAGE entersymbol

(*  ENTERSYMBOL calls Enter to add a name to the name list with its
    value set to a specified literal.  *)

public procedure entersymbol ( nam: sym_value; value: number );

begin
  enter (nam, literal (value), true);
  nsymbols := max (nsymbols, value);
end;
$PAGE set_mode

(*  SET_MODE sets the input mode to Ascii, Numeric or Symbolic.  If the mode
    is Symbolic, it also sets up the symbolic name table.  *)

public procedure set_mode ( nam: sym_value; mode: input_modes );

var p: name;
    val: number;

begin
  if input_mode <> undefined_mode then begin
    if input_mode = symbolic_mode then
      dispose (symbols);
    err_token (nam, 'Alphabet already specified');
  end;
  input_mode := mode;
  if mode = symbolic_mode then begin
    new (symbols, nsymbols);
    p := name_list;
    while p <> nil do begin
      if p^.symbolic then begin
	assert (litval (p^.value, val));
	symbols^[val] := p;
	p^.symbolic := false;
      end;
      p := p^.next;
    end;
  end;
end (* set_mode *);
$PAGE lookupname

(*  LOOKUPNAME returns the regular expression associated with a specified
    name in the name list.  If the name is not in the name list, an error
    message is printed and the empty set (phi) is returned.  *)

public function lookupname ( nam: sym_value ): reg_exp;

var p: name;

begin
  p := name_list; (* Find the name in the list. *)
  while (p <> nil) andif (p^.name_str <> nam.lit_val^) do
    p := p^.next;

  if p = nil then begin
    err_token (nam, 'Undefined symbol ' || nam.lit_val^);
    entername ( nam, phi ); (* No more errors on this name. *)
    lookupname := phi; (* Return the empty set. *)
  end

  else
    lookupname := p^.value; (* Return the value of the name. *)
end (* lookupname *);
$PAGE clearnames

(*  CLEARNAMES will traverse the name list, disposing of all the entries in it,
    and freeing the associated regular expressions.  *)

public procedure clearnames;

var nt: name;

begin
  while name_list <> nil do begin
    free_re (name_list^.value);
    nt := name_list^.next;
    dispose (name_list);
    name_list := nt;
  end;
  if input_mode = symbolic_mode then
    dispose (symbols);
end (* clearnames *);
$PAGE prt_symbol

(*  PRT SYMBOL prints the n-th symbol.  The symbol may be printed as a number,
    as a quoted character, or as a symbolic name, depending on the input mode.  *)

public procedure prt_symbol ( n: number; text: parm_string );

var sym: string [5];
    n1: number;

begin
  n1 := min (max (n, min_symbol), max_symbol);
  case input_mode of

    ascii_mode:
      begin
	if n1 in [ord(' ')..ord('~')] then
	  fio_write (listfb, '"' || chr (n1) || '"' || text)
	else begin
	  putstring (sym, n1:0);
	  fio_write (listfb, '/' || sym || text);
	end;
      end;

    numeric_mode:
      begin
	putstring (sym, n1:0);
	fio_write (listfb, sym || text);
      end;

    symbolic_mode:
      fio_write (listfb, symbols^[n1]^.name_str || text);

    undefined_mode:
      assert (false);

  end (* case input_mode *);
end (* prt_symbol *).
    