$TITLE SCNPAT - SCANNR Scanner Block List Utilities

module scnpat;

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         S C N P A T                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     MDSI, Company Confidential
     
     SYSTEM:  SCANNR Lexical Scanner Builder
     
     STARTED:  27 September 1979
     
     PURPOSE:  The  scanner  pattern  list  is  an  ordered  list  of
        (regular expression, action) pairs.  A pattern indicates that
        when the regular expression is recognized, the action  is  to
        be  performed.  If  the  regular  expressions of two patterns
        match the same input, the action of the pattern which  occurs
        earlier in the list is executed.
     
     ENTRY POINTS:
     
        initpatterns
                    initializes the pattern module.
     
        clearpatterns
                    clears and reinitializes the pattern list.
     
        new_pattern ( re, act, prm )
                    adds  a  pattern  to the list.  Re is the regular
                    expression which characterizes the  pattern.  Act
                    and  Prm are string lists.  For a pattern "IGNORE
                    <reg exp>", Act and Prm will  both  be  nil,  and
                    this  regular  expression will be entered with an
                    ignore   action.   For   a   pattern   "<reg exp>
                    {action}",  Act  will  be non-nil and Prm will be
                    nil, and this regular expression will be  entered
                    with  a  simple  action.  For  a pattern "SYMBOLS
                    {action} <parm list> = <reg exp>", both  Act  and
                    Prm  will  be non-nil, and the regular expression
                    will be entered with a parameterized action.
     
        add_pattern ( re, prm )
                    also adds a  pattern  to  the  list.  Re  is  the
                    regular  expression, and Prm is a string list.  A
                    call to AddPattern provides a regular  expression
                    and  a  parameter  list  for  an additional entry
                    under the current SYMBOLS list, which  must  have
                    been  defined  by  the last NewPattern call (with
                    non-nil Act and Prm).
     
        endpatterns is called to indicate that all patterns have been
                    specified.
     
        npatterns   is the number of patterns that have been defined.
     
        pat_re ( n )
                    is  a  function  which may only be called after a
                    call to EndPattern.  N  must  be  between  1  and
                    Npatterns.  PatRe  returns the regular expression
                    component of the N-th pattern.
     
        pat_action ( n )
                    is a function which may only be  called  after  a
                    call  to  EndPattern.  N  must  be  between 1 and
                    Npatterns.  PatAction returns a string list which
                    is  the  action  for  the  N-th pattern, with all
                    parameter  substitutions  performed.   When   the
                    calling  routine  is  finished  with  the  action
                    string list, it should call PatFree with it.
     
        pat_free ( act )
                    should be called to release an action string list
                    which was returned by PatAct.
     
     ---------------------------------------------------------------- *)
$PAGE declarations

$INCLUDE scannr.typ
$INCLUDE scnlit.typ

$INCLUDE scnrea

type
    pattern = ^ pattern_node;

    pattern_kinds =
     (  ignore_pattern, (* IGNORE <reg exp> *)
	simple_pattern, (* <reg exp> {action} *)
	parameterized_pattern, (* SYMBOLS {action} *)
	sub_pattern  ); (* <parms> = <reg exp> (in a SYMBOLS list) *)

    pattern_node = packed record
	next: pattern; (* Chains the pattern list. *)
	re_val: reg_exp; (* The controlling regular expresion. *)
	case kind: pattern_kinds of
	    ignore_pattern:
	      ( );
	    simple_pattern:
	      ( action: str_list ); (* The simple action. *)
	    parameterized_pattern:
	      ( template: str_list; (* The parameterized action. *)
		sub_list: pattern ); (* The list of (re, parms) pairs. *)
	    sub_pattern:
	      ( params: str_list; (* The parameters for a sub-pattern. *)
		parent: pattern ); (* The parent parameterized pattern. *)
    end (* pattern_node *);

var
    pattern_list: pattern;
    last_pattern: pattern;
    last_sub_pattern: pattern;
    pattern_array: ^ array [1..*] of pattern;

public var
    npatterns: integer;
$PAGE initpatterns

(*  Initpatterns must be called at the start of execution to prepare the
    pattern list.  *)

public procedure initpatterns;

begin
  pattern_list := nil;
  last_pattern := nil;
  last_sub_pattern := nil;
  pattern_array := nil;
  npatterns := 0;
end;
$PAGE discard_strings

(*  DiscardStrings will dispose of all the individual strings and list nodes
    in a string list.  *)

procedure discard_strings ( s: str_list );

var ts, ts1: str_list;

begin
  ts := s;
  while ts <> nil do begin
    dispose (ts^.str);
    ts1 := ts^.next;
    dispose (ts);
    ts := ts1;
  end;
end (* discard_strings *);
$PAGE clearpatterns

(*  Clearpatterns will dispose of the pattern list data structures, returning
    the pattern list to its initial state, and will free all the regular
    expressions associated to patterns.  *)

public procedure clearpatterns;

var i: integer;

begin
  assert (pattern_array <> nil); (* Must have called Endpatterns before now. *)

  for i := 1 to npatterns do begin
    with pattern_array^[i]^ do begin
      if re_val <> nil then
	free_re (re_val);
      case kind of

	ignore_pattern:
	  ;

	simple_pattern: (* Discard the action strings. *)
	  discard_strings (action);

	parameterized_pattern:
	  discard_strings (template);

	sub_pattern:
	  discard_strings (params);

      end (* case kind *);
    end (* with pattern_array^[i]^ *);
    dispose (pattern_array^[i]);
  end (* for i *);

  dispose (pattern_array);
  initpatterns;
end (* clearpatterns *);
$PAGE new_pattern

(*  NewPattern is called with a regular expression, an action string list,
    and a parameter string list.  It creates a new pattern for them, and
    attaches it at the end of the pattern list.  The action string list may
    be empty for an IGNORE pattern.  The parameter string list will be non-
    empty only if the pattern is being defined with a SYMBOLS construct.  *)

public procedure new_pattern ( re: reg_exp; act, prm: str_list );

var pat: pattern;

begin
  assert (pattern_array = nil); (* Must not have called EndScanners yet. *)
  npatterns := npatterns + 1;

  if prm = nil then begin
    if act = nil then
      new (pat, ignore_pattern)
    else begin
      new (pat, simple_pattern);
      pat^.action := act;
    end;
    pat^.re_val := use_re (re);
    last_sub_pattern := nil;
  end
  else (* prm <> nil *) begin
    new (pat, parameterized_pattern);
    pat^.re_val := nil;
    new (last_sub_pattern, sub_pattern);
    with last_sub_pattern^ do begin
      next := nil;
      re_val := use_re (re);
      params := prm;
      parent := pat;
    end;
    pat^.template := act;
    pat^.sub_list := last_sub_pattern;
  end;

  if pattern_list = nil
    then pattern_list := pat
    else last_pattern^.next := pat;
  last_pattern := pat;
  pat^.next := nil;
end (* new_pattern *);
$PAGE add_pattern

(*  AddPattern is called with a regular expression and a parameter string
    list.  It creates a new sub-pattern for them, under the last pattern
    which was created with NewPattern (which must have been a parameterized
    pattern, of course).  *)

public procedure add_pattern ( re: reg_exp; prm: str_list );

var pat: pattern;

begin
  assert (pattern_array = nil); (* Must not have called Endpatterns yet. *)
  assert (last_sub_pattern <> nil); (* Current pattern must be parameterized. *)

  npatterns := npatterns + 1;
  new (pat, sub_pattern);
  with pat^ do begin
    next := nil;
    re_val := use_re (re);
    params := prm;
    parent := last_pattern;
  end;
  last_sub_pattern^.next := pat;
  last_sub_pattern := pat;
end (* add_pattern *);
$PAGE endpatterns

(*  Endpatterns is called to indicate that all the patterns have been created.
    It creates the pattern array from the pattern list.  *)

public procedure endpatterns;

var i: integer;
    pat, next_pat: pattern;

begin
  assert (pattern_array = nil);

  new (pattern_array, npatterns);
  pat := pattern_list;
  for i := 1 to npatterns do begin
    if pat^.kind = parameterized_pattern then begin
      next_pat := pat^.next;
      pat := pat^.sub_list;
    end;
    pattern_array^[i] := pat;
    pat := pat^.next;
    if pat = nil then
      pat := next_pat;
  end (* for i *);
end (* endpatterns *);
$PAGE pat_re

(*  PatRe returns the regular expression component of the N-th pattern.  *)

public function pat_re ( n: integer ): reg_exp;

begin
  pat_re := pattern_array^[n]^.re_val;
end;
$PAGE copy_action

(*  CopyAction creates a copy of an action string list for PatAction.  The
    action string list may or may not be parameterized.  If it is, then
    Params will be the non-nil parameter string list.  If it is not, then
    Params will be nil.  *)

function copy_action ( action, params: str_list ): str_list;

type
    parm_code = '1' .. '9';

var parm_table: array [parm_code] of record
	length: integer;
	str: str_ptr;
    end;
$PAGE build_parm_table - in copy_action

(*  BuildParmTable fills in the parameter table from the Params string list,
    saving the length and string pointer for each parameter, and null entries
    in the rest of the table.  *)

procedure build_parm_table;

var ptag: parm_code;
    parm: str_list;

begin
  parm := params;
  for ptag := minimum (parm_code) to maximum (parm_code) do begin
    if parm <> nil then begin
      parm_table[ptag] := (length (parm^.str^), parm^.str);
      parm := parm^.next;
    end
    else
      parm_table[ptag] := (0, nil);
  end;
end (* build_parm_table *);
$PAGE expand_params - in copy_action

(*  ExpandParams takes as its argument a string which may contain parameter
    references, and returns a pointer to a string which reflects the original
    string with all its parameter references expanded.  The parameter rules
    are:

	Replace &1, &2, ... &9 by the corresponding parameters.
	Replace &<any non-digit> by the non-digit.
	Leave an "&" at the end of the string alone.			*)

function expand_params ( str: packed array [1..*] of char ): str_ptr;

var ind, rslt_len, ind1, dest_ind: integer;
    ptag: char;
begin

  (*  Find the length of the result string.  *)

  ind := 0;
  rslt_len := length (str);
  while ind < length (str) do begin
    ind := ind + search (substr (str, ind+1), ['&'], length (str) - ind + 1);
    if ind < length (str) then begin
      ind := ind + 1;
      rslt_len := rslt_len - 1;
      ptag := str[ind];
      if ptag in [minimum (parm_code) .. maximum (parm_code)] then
	rslt_len := rslt_len - 1 + parm_table[ptag].length;
    end;
  end;

  (*  Create the result string.  *)

  new (expand_params, rslt_len);

  (*  Write the expanded string into the result string.  *)

  ind := 0;
  dest_ind := 1;
  while ind < length (str) do begin
    ind1 := search (substr (str, ind+1), ['&'], length (str) - ind + 1);
    substr (expand_params^, dest_ind, ind1-1) := substr (str, ind+1, ind1-1);
    ind := ind + ind1;
    dest_ind := dest_ind + ind1 - 1;
    if ind < length (str) then begin
      ind := ind + 1;
      ptag := str[ind];
      if ptag in [minimum (parm_code) .. maximum (parm_code)] then begin
	substr (expand_params^, dest_ind, parm_table[ptag].length) := parm_table[ptag].str^;
	dest_ind := dest_ind + parm_table[ptag].length;
      end
      else begin
	expand_params^[dest_ind] := str[ind];
	dest_ind := dest_ind + 1;
      end;
    end;
  end;

end (* expand_params *);
$PAGE copy_action - main routine

var copy, act: str_list;
    simple_action: boolean;

begin
  assert (action <> nil);

  simple_action := (params = nil);
  if not simple_action then
    build_parm_table;

  copy_action := nil;
  act := action;
  while act <> nil do begin
    if copy_action = nil then begin
      new (copy_action);
      copy := copy_action;
    end
    else begin
      new (copy^.next);
      copy := copy^.next;
    end;

    if simple_action then begin
      new (copy^.str, length (act^.str^));
      copy^.str^ := act^.str^;
    end
    else
      copy^.str := expand_params (act^.str^);
    act := act^.next;
  end;
  copy^.next := nil;
end (* copy_action *);
$PAGE pat_action

(*  PatAction returns the action string list for the N-th pattern.  If the
    N-th pattern is a parameterized sub-pattern, parameter substitution is
    performed first.  *)

public function pat_action ( n: integer ): str_list;

begin
  with pattern_array^[n]^ do begin
    case kind of

      ignore_pattern:
	pat_action := nil;

      simple_pattern:
	pat_action := copy_action (action, nil);

      sub_pattern:
	pat_action := copy_action (parent^.template, params);

    end (* case kind *);
  end (* with *);
end (* pat_action *);
$PAGE pat_free

(*  PatFree is called with a string list which was returned by PatAction.
    It discards the string list.  *)

public procedure pat_free ( act: str_list );

begin
  discard_strings (act);
end.
 