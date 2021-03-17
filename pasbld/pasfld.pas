$TITLE pasfld -- Pascal Constant Expression Folding
$LENGTH 43

$IF PASS1  module pa1fld;
  
$IF PASS2  module pa2fld;

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         P A S F L D                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     PURPOSE:  This module contains the constant  expression  folding
        module.  Actually,  two  distinct  modules  are compiled from
        PASFLD.  If PASS1 is enabled, then  PA1FLD  is  generated.  The
        version  of  FOLD  in this module is recursive, and will fold
        all the operands of an expression before attempting  to  fold
        the  expression  itself.  If  PASS2  is  enabled, the PA2FLD is
        generated.  This  version  of  FOLD  is   non-recursive.   It
        assumes  that the operands of an expression will already have
        been folded when FOLD is called.
     
     ENTRY POINTS:
     
        fold        takes an expression tree, applies any  applicable
                    constant  folding  operations  to it, and returns
                    the folded expression tree.  The pass  1  version
                    of  Fold  also  takes  the  parse  tree  for  the
                    expression, and the  pass  2  version  takes  the
                    source   id   of  the  statement  containing  the
                    expression.  If  an  error  is  detected  in   an
                    expression  being  folded,  a  warning message is
                    printed on the indicated parse node or line,  and
                    the  expression  is  not  folded.  Only a warning
                    message  is  printed   because   there   is   the
                    possibility  that  the  expression  will  not  be
                    evaluated at run time, so an actual error may not
                    occur.
     
     ---------------------------------------------------------------- *)
$PAGE includes
$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM pasifu.inc
$SYSTEM pasval.inc
$SYSTEM pasesu.inc
$SYSTEM paserr.inc
$PAGE fold

$IF PASS1  public function fold ( e: expr; loc: parse_node ): expr;
  
$IF PASS2  public function fold ( e: expr; loc: source_id ): expr;

(*  If an error is detected in Fold, the Fatal routine is called.  Fatal
    prints the error message and returns to label 999, which is the exit
    point from Fold.  Non-fatal error messages are printed with a call to
    Warning.  *)

label 999;


procedure warning ( code: err_codes );

begin

$IF PASS1 err_node (code, loc);
$IF PASS2 err_print (code, loc, '', 0);

end (* fatal *);




procedure fatal ( code: err_codes );

begin
  warning (code);
  fold := e;
  goto 999;
end (* warning *);
$PAGE element, setmin, setmax & empty_set
(*  Element takes a constant set expression and an index, and returns
    'true' iff the index corresponds to an element which is in the domain
    of the set constant and which is in the set.  *)

function element ( s: val; i: int_type ): boolean;
begin
  with s.valp^ do
    element := (i >= set_origin) andif
	       (i <= set_origin + upperbound (set_val)) andif
               set_val [i-set_origin];
end (* element *);


(*  SetMin returns the lower bound of a set constant.  *)

function setmin ( s: val ): int_type;
begin
  setmin := s.valp^.set_origin;
end (* setmin *);


(*  SetMax returns the upper bound of a set constant.  *)

function setmax ( s: val ): int_type;
begin
  with s.valp^ do
    setmax := set_origin + upperbound (set_val);
end (* setmax *);


$IF PASS2
(*  EmptySet returns true if a set is empty.  *)

function empty_set ( s: val ): boolean;
var ind: int_type;
begin
  empty_set := true;
  with s.valp^ do
    for ind := 0 to upperbound (set_val) do
      exit if set_val [ind] do
	empty_set := false;
end (* empty_set *);
$ENDIF
$PAGE scal_const, intgr_const, bool_const & real_const
(*  ScalConst will set the return value of Fold to a specified
    scalar constant value, with the same type as the current value of
    Fold.  *)

procedure scal_const ( s: int_type );
begin
  fold := cst_expr ( cst_scalar(s), fold^.desc.base );
end;



(*  IntgrConst will set the return value of Fold to a specified
    integer constant expression.  *)

procedure intgr_const ( i: int_type );
begin
  fold := cst_expr ( cst_scalar(i), fold^.desc.base );
end;



(*  BoolConst will set the return value of Fold to a specified
    boolean constant value.  *)

procedure bool_const ( p: boolean );
begin
  fold := cst_expr ( cst_scalar(ord(p)), fold^.desc.base );
end;



(*  RealConst sets the return value of Fold to a specified real constant.
    The value is rounded to the required precision.  *)

procedure real_const ( r: real_type );
begin
  fold := cst_expr ( mkreal (r, fold^.desc.precision), type_real);
end;
$PAGE make_range_set
(*  MakeRangeSet will create a new constant set expression, whose range is
    specified by the descriptor of 'fold'.  If 'rangemin' and 'rangemax'
    are nil, the set will be empty.  Otherwise, 'rangemin' and 'rangemax'
    are scalar constant expressions specifying the bounds of the set range.  *)


$IF PASS1
 (* Pass 1 only *)

procedure make_range_set ( rangemin, rangemax: expr );

var min_element, max_element, ind: bit_range;

begin
  with fold^.desc do
    if set_cst_lwb and set_cst_len
      then fold := cst_expr (makeset (set_lwb, set_lwb+set_length-1), base)
      else fold := cst_expr (makeset (rangemin^.cst_val.ival, rangemax^.cst_val.ival), base);
  with fold^.cst_val.valp^ do begin
    for ind := 0 to upperbound (set_val) do
      set_val [ind] := false;
    if rangemin <> nil then begin
      min_element := max (rangemin^.cst_val.ival - set_origin, 0);
      max_element := min (rangemax^.cst_val.ival - set_origin, upperbound (set_val));
      for ind := min_element to max_element do
	set_val [ind] := true;
      if min_element > max_element then
	warning (err_set_rng_empty);
    end;
  end;
end (* make_range_set *);


$ENDIF (* End Pass 1 only code *)
$PAGE fold_binop
(*  FoldBinop performs the folding operations when both the arguments of a
    binary operator are constant values.  *)


procedure fold_binop ( opcode: tuple_opcodes; leftval, rightval: val );
$PAGE str_rel - in fold_binop
  (*  StrRel will compare the 'leftop' and 'rightop' string constants,
      and will return a code indicating the relation between them.  *)

  type rel_code= ( lss, eql, gtr );

  function str_rel: rel_code;

  begin
    if leftval.valp^.str_val < rightval.valp^.str_val then
      str_rel := lss
    else if leftval.valp^.str_val > rightval.valp^.str_val then
      str_rel := gtr
    else
      str_rel := eql;
  end (* str_rel *);
$PAGE union_operation, inters_operation, & diff_operation - in fold_binop
  (*  The Combiner functions take two boolean values, and return their
      conjunction (Both), disjunction (Union), or difference (Diff).  *)

  type combiner = function ( l, r: boolean ): boolean;


  function union_operation ( l, r: boolean ): boolean;
  begin
    union_operation := l or r
  end;


  function inters_operation ( l, r: boolean ): boolean;
  begin
    inters_operation := l and r
  end;


  function diff_operation ( l, r: boolean ): boolean;
  begin
    diff_operation := l and not r
  end;
$PAGE set_compare - in fold_binop
  (*  SetCompare tests for a specified relation between the set constants
      indicated by 'leftval.valp' and 'rightval.valp'.  The relation, 'rel', is actually
      a set from the range 0 .. 2.  Note that the relations <=, >=, =, and
      <> hold between two sets iff they hold between the characteristic
      boolean functions of the two sets at each point in the domain.  The
      'rel' parameter represents the relation between the characteristic
      boolean functions that must hold at each point of the domain.  Let
      B1 and B2 be two boolean values.  Then the function:

            r(B1,B2) = ord(B1) - ord(B2) + 1

      will be 0 if B1 < B2, 1 if B1 = B2, and 2 if B1 > B2.  'Rel' is then
      the set of values of 'r' which are acceptable at each point of the
      domain, as follows:

            <= :  [0,1]
            >= :  [1,2]
            =  :  [1]
            <> :  [0,2]                                                 *)

  type
    rel_set = set of 0 .. 2;

  const
    set_lss = 0;
    set_eql = 1;
    set_gtr = 2;

  procedure set_compare ( rel: rel_set );
  var ind, max_range: int_type;
  begin
    ind := min ( setmin (leftval), setmin (rightval) );
    max_range := max ( setmax (leftval), setmax (rightval) );
    while (ind <= max_range) andif
      ( (ord(element(leftval,ind)) - ord(element(rightval,ind)) + 1) in rel ) do
        ind := ind + 1;
    bool_const (ind > max_range);
  end (* set_compare *);
$PAGE set_combine - in fold_binop
  (*  SetCombine will create a new constant set expression.  The elements
      of the new set will obtained by applying the specified Combiner function
      to the corresponding elements of the 'leftval.valp' and 'rightval.valp' sets.  The
      elements of the new set will only be generated between the 'min_range'
      and 'max_range' limits.  *)

  procedure set_combine ( f: combiner );
  var min_range, max_range, ind: int_type;
  begin
    min_range := min ( setmin (leftval), setmin (rightval) );
    max_range := max ( setmax (leftval), setmax (rightval) );
    fold := cst_expr (makeset(min_range,max_range),fold^.desc.base);
    with fold^.cst_val.valp^ do
      for ind := min_range to max_range do
        set_val [ind-min_range] := f(element(leftval,ind),element(rightval,ind));
  end (* set_combine *);
$PAGE fold_binop - main routine
begin
  case opcode of

    iadd_op:
        intgr_const (leftval.ival + rightval.ival);
    isub_op:
        intgr_const (leftval.ival - rightval.ival);
    imul_op:
        intgr_const (leftval.ival * rightval.ival);
    idiv_op:
        if rightval.ival = 0 then
          fatal (err_zero_divide)
        else
          intgr_const (leftval.ival div rightval.ival);
    imod_op:
        if rightval.ival = 0 then
          fatal (err_zero_divide)
        else
          intgr_const (leftval.ival mod rightval.ival);

    radd_op:
        real_const (leftval.valp^.real_val + rightval.valp^.real_val);
    rsub_op:
        real_const (leftval.valp^.real_val - rightval.valp^.real_val);
    rmul_op:
        real_const (leftval.valp^.real_val * rightval.valp^.real_val);
    rdiv_op:
        real_const (leftval.valp^.real_val / rightval.valp^.real_val);

    expii_op:
        if (rightval.ival < 0) or
          (leftval.ival = 0) and (rightval.ival = 0)
            then fatal (err_exp_undef)
            else intgr_const (leftval.ival ** rightval.ival);
    expri_op:
        if (leftval.valp^.real_val = 0) and (rightval.ival <= 0)
          or (leftval.valp^.real_val < 0) and (rightval.ival < 0)
            then fatal (err_exp_undef)
            else real_const (leftval.valp^.real_val ** rightval.ival);
    exprr_op:
        if (leftval.valp^.real_val < 0) or
          (leftval.valp^.real_val = 0) and
            (rightval.valp^.real_val <= 0)
              then fatal (err_exp_undef)
              else real_const (leftval.valp^.real_val **
                               rightval.valp^.real_val);

    ile_op:
        bool_const (leftval.ival <= rightval.ival);
    ilt_op:
        bool_const (leftval.ival <  rightval.ival);
    igt_op:
        bool_const (leftval.ival >  rightval.ival);
    ige_op:
        bool_const (leftval.ival >= rightval.ival);
    ieq_op:
        bool_const (leftval.ival =  rightval.ival);
    ine_op:
        bool_const (leftval.ival <> rightval.ival);

    rle_op:
        bool_const (leftval.valp^.real_val <= rightval.valp^.real_val);
    rlt_op:
        bool_const (leftval.valp^.real_val <  rightval.valp^.real_val);
    rgt_op:
        bool_const (leftval.valp^.real_val >  rightval.valp^.real_val);
    rge_op:
        bool_const (leftval.valp^.real_val >= rightval.valp^.real_val);
    req_op:
        bool_const (leftval.valp^.real_val =  rightval.valp^.real_val);
    rne_op:
        bool_const (leftval.valp^.real_val <> rightval.valp^.real_val);

    sle_op:
        bool_const (str_rel() <> gtr);
    slt_op:
        bool_const (str_rel() = lss);
    sgt_op:
        bool_const (str_rel() = gtr);
    sge_op:
        bool_const (str_rel() <> lss);
    seq_op:
        bool_const (str_rel() = eql);
    sne_op:
        bool_const (str_rel() <> eql);

    setle_op:
        set_compare ([set_lss, set_eql]);
    setge_op:
        set_compare ([set_eql, set_gtr]);
    seteq_op:
        set_compare ([set_eql]);
    setne_op:
        set_compare ([set_lss, set_gtr]);

    ptreq_op:
        bool_const (true);
    ptrne_op:
        bool_const (false);

    or_op,
    orif_op:
        bool_const (leftval.ival + rightval.ival >= 1);
    and_op,
    andif_op:
        bool_const (leftval.ival + rightval.ival = 2);

    in_op:
        bool_const (element(rightval,leftval.ival));

    union_op:
        set_combine (union_operation);
    both_op:
        set_combine (inters_operation);
    diff_op:
        set_combine (diff_operation);

    round_op:
        real_const ( (round (leftval.valp^.real_val / (10.0 ** rightval.ival) ))
                     * (10.0 ** rightval.ival) );

    arctan_op:
        real_const ( arctan (leftval.valp^.real_val, rightval.valp^.real_val) )

  end (* case opcode *);
end (* fold_binop *);
$PAGE negate  [Pass 2]
(*  Negate will set the return value of Fold to the negation of a specified
    integer expression.  *)


$IF PASS2
  (* Pass 2 only. *)


procedure negate ( e: expr );
begin
  new (fold, ineg_op, ineg_op, 1);
  initexpr (fold, type_int);
  with fold^ do begin
    operand [1] := e;
    desc := e^.desc;
    if not desc.signed then begin
      desc.signed := true;
      desc.int_prec := desc.int_prec + 1;
    end;
  end;
  emit (fold);
end (* negate *);
$PAGE fold_bin_1  [Pass 2]
(*  FoldBin1 determines whether any folding transformations are applicable when
    only the first operand of a binary operator is constant.  *)

procedure fold_bin_1 ( opcode: tuple_opcodes; leftval: val; rightop: expr );

begin
  case opcode of

    iadd_op:
      if leftval.ival = 0 then
        fold := rightop;
    isub_op:
      if leftval.ival = 0 then
        if rightop^.opcode = ineg_op
          then fold := rightop^.operand [1]
          else negate (rightop);
    imul_op:
      if leftval.ival = 1 then
        fold := rightop
      else if leftval.ival = 0 then
        intgr_const (0)
      else if leftval.ival = -1 then
        negate (rightop);
    idiv_op:
      if leftval.ival = 0 then
        intgr_const (0);
    imod_op:
      if leftval.ival = 0 then
        intgr_const (0);

    expii_op:
      if leftval.ival = 0 then
        intgr_const (0)
      else if leftval.ival = 1 then
        intgr_const (1);

    ile_op:
      if not rightop^.desc.signed andif (leftval.ival <= 0) then
        bool_const (true);
    ilt_op:
      if not rightop^.desc.signed andif (leftval.ival < 0) then
        bool_const (true);
    igt_op:
      if not rightop^.desc.signed andif (leftval.ival <= 0) then
        bool_const (false);
    ige_op:
      if not rightop^.desc.signed andif (leftval.ival < 0) then
        bool_const (false);
    ieq_op:
      if not rightop^.desc.signed andif (leftval.ival < 0) then
        bool_const (false);
    ine_op:
      if not rightop^.desc.signed andif (leftval.ival < 0) then
        bool_const (true);

    setle_op:
      if empty_set (leftval) then
        bool_const (true);

    or_op,
    orif_op:
      if leftval.ival = 1
        then bool_const (true)
        else fold := rightop;
    and_op,
    andif_op:
      if leftval.ival = 1
        then fold := rightop
        else bool_const (false);

    union_op:
      if empty_set (leftval) then
        fold := rightop;
    both_op,
    diff_op:
      if empty_set (leftval) then
        fold := fold^.operand[1]

  end (* case opcode *);
end (* fold_bin_1 *);
$PAGE fold_bin_2  [Pass 2]
(*  FoldBin2 determines whether any folding transformations are applicable when
    only the second operand of a binary operation is a constant.  *)

procedure fold_bin_2 ( opcode: tuple_opcodes; leftop: expr; rightval: val );

begin
  case opcode of

    iadd_op,
    isub_op:
      if rightval.ival = 0 then
        fold := leftop;
    imul_op:
      if rightval.ival = 1 then
        fold := leftop
      else if rightval.ival = 0 then
        intgr_const (0)
      else if rightval.ival = -1 then
        negate (leftop);
    idiv_op:
      if rightval.ival = 1 then
        fold := leftop
      else if rightval.ival = 0 then
        fatal (err_zero_divide)
      else if rightval.ival = -1 then
        negate (leftop);
    imod_op:
      if rightval.ival = 1 then
        intgr_const (0)
      else if rightval.ival = 0 then
        fatal (err_zero_divide);
    expii_op:
      if rightval.ival = 0 then
        intgr_const (1)
      else if rightval.ival = 1 then
        fold := leftop;

    ile_op:
      if not leftop^.desc.signed andif (rightval.ival < 0) then
        bool_const (false);
    ilt_op:
      if not leftop^.desc.signed andif (rightval.ival <= 0) then
        bool_const (false);
    igt_op:
      if not leftop^.desc.signed andif (rightval.ival < 0) then
        bool_const (true);
    ige_op:
      if not leftop^.desc.signed andif (rightval.ival <= 0) then
        bool_const (false);
    ieq_op:
      if not leftop^.desc.signed andightval.ival < 0) then
        bool_const (false);
    ine_op:
      if not leftop^.desc.signed andif (rightval.ival < 0) then
        bool_const (true);

    setge_op:
      if empty_set (rightval) then
        bool_const (true);

    or_op,
    orif_op:
      if rightval.ival = 1
        then bool_const (true)
        else fold := leftop;
    and_op,
    andif_op:
      if rightval.ival = 1
        then fold := leftop
        else bool_const (false);

    in_op:
      if empty_set (rightval) then
        bool_const (false);

    union_op,
    diff_op:
      if empty_set (rightval) then
        fold := leftop;
    both_op:
      if empty_set (rightval) then
        fold := fold^.operand[2]

  end (* case opcode *);
end (* fold_bin_2 *);


$ENDIF (* End Pass 2 only code. *)
$PAGE fold_unop
procedure fold_unop ( opcode: tuple_opcodes; operand: expr );
$PAGE convert_string - in fold_unop
(*  ConvertString performs string->string, char->string, and 
    string->char constant conversions to a specified type.  The
    specific conversion depends on the result type and constant
    type, as follows:

                          ||  If the result type is:
                          ||-----------------------------------
                          ||  string    |  string    |  char
      and the type of     ||  length m  |  length m  |  (m=1)
      the operand is:     ||  fixed     |  varying   |
    ----------------------||===================================
                          ||            |  ok        |
      string, length = m  ||  ok        |  set var   |  ->chr
                          ||            |            |
    ----------------------||-----------------------------------
                          ||            |  ok        |  blank
      string, length < m  ||  copy      |  set var   |  pad
                          ||            |            |
    ----------------------||-----------------------------------
                          ||  copy      |  copy      |  ->chr
      string, length > m  ||  trunc     |  trunc     |  trunc
                          ||            |  set var   |
    ----------------------||-----------------------------------
                          ||            |  ->strv    |       
      char                ||  ->strf    |  set var   |  never
                          ||            |            |

      ok:       Use the unconverted operand string.
      set var:  Set the varying bit in the string constant.
      ->chr:    Make a scalar constant out of the first
                character of the string operand.
      copy:     Make a copy of the string operand with length m.
      blank:    Use the scalar value of the character ' '.
      pad:      Print the "null string padded" warning.
      trunc:    Print the "string constant truncated" warning.
      ->strf:   Make a string of length m from the scalar operand.
      ->strv:   Make a string of length 1 from the scalar operand.
      never:    A strcvt operator is never used to convert a
                character to a character.    *)
$PAGE
procedure convert_string;

var
    rslt_length: char_range;
    varying_ref: boolean;

begin
  if fold^.desc.kind = strings then
    rslt_length := fold^.desc.str_length;

  if fold^.desc.kind = chars then begin
    with operand^.cst_val.valp^ do begin      (* Operand must be a string. *)
      if length (str_val) = 0 then begin
        warning (err_pad_char);
        scal_const (ord(' '));
      end
      else begin
        scal_const (ord(str_val[1]));
        if length (str_val) > 1 then
          warning (err_trunc_string);
      end;
    end;
  end

$IF PASS1

  else if (fold^.desc.str_kind = varying) or fold^.desc.str_flex then begin
    with operand^.cst_val do begin
      varying_ref := (fold^.desc.str_kind = varying);
      if kind = scalar_cst then begin
        fold := cst_expr (makestring(1),nil);
        fold^.cst_val.valp^.str_val := chr(ival);
      end
      else if length (valp^.str_val) <= rslt_length then
	fold := cst_expr (operand^.cst_val,nil)
      else begin
	warning (err_trunc_string);
	fold := cst_expr (makestring(rslt_length),nil);
	fold^.cst_val.valp^.str_val := valp^.str_val;
      end;
      fold^.cst_val.valp^.str_varying_ref := varying_ref;
    end;
  end

  else begin
    with operand^.cst_val do begin
      if kind = scalar_cst then begin
        fold := cst_expr (makestring(rslt_length),nil);
	fold^.cst_val.valp^.str_val := chr (ival);
      end
      else if length (valp^.str_val) = rslt_length then
	fold := cst_expr (operand^.cst_val,nil)
      else begin
	fold := cst_expr (makestring(rslt_length),nil);
	fold^.cst_val.valp^.str_val := valp^.str_val;
	if length (valp^.str_val) > rslt_length then
	  warning (err_trunc_string);
      end;
    end;
  end;

$ENDIF

end (* convert_string *);
$PAGE convert_const_set - in fold_unop
(*  ConvertConstSet will create a set constant expression node containing the
    set from the operand, but with the attributes of the base type of the
    set convert operator.  *)

procedure convert_const_set;

var ind: int_type;
    setp: val;
    warned: boolean;

begin
  setp := operand^.cst_val;
  warned := false;
  with fold^.desc.base^.set_element_type^ do begin
    for ind := setmin (setp) to minval - 1 do
      exit if element (setp, ind) do begin
        warning (err_set_trunc);
        warned := true;
      end;
    if not warned then
      for ind := maxval + 1 to setmax (setp) do
        exit if element (setp, ind) do
          warning (err_set_trunc);
    fold := cst_expr (makeset (minval, maxval), fold^.desc.base);
  end;
  with fold^.cst_val.valp^ do
    for ind := 0 to upperbound (set_val) do
      set_val [ind] := element (setp, set_origin+ind);
end (* convert_const_set *);
$PAGE fold_unop - main routine
  begin  with operand^.cst_val do
    case opcode of

      ineg_op:
          intgr_const (- ival);

      rneg_op:
          real_const (-valp^.real_val);

      iabs_op:
          intgr_const (abs(ival));

      rabs_op:
          real_const (abs(valp^.real_val));

      bnot_op:
          bool_const (ival = 0);

      sclcvt_op:
	  if operand^.desc.kind in [bools, ints, chars, scalars] then
	    scal_const (ival);

      strcvt_op:
          convert_string;

      setcvt_op:
          convert_const_set;

      float_op:
          if kind = scalar_cst
            then real_const (ival)      (* float (int) *)
            else real_const (valp^.real_val);   (* float (real of other precision) *)

      odd_op:
          bool_const (odd(ival));

      trunc_op:
          intgr_const (trunc(valp^.real_val));

      sqrt_op:
          real_const (sqrt(valp^.real_val));

      ln_op:
          real_const (ln(valp^.real_val));

      log_op:
          real_const (log(valp^.real_val));

      exp_op:
          real_const (exp(valp^.real_val));

      sin_op:
          real_const (sin(valp^.real_val));

      arcsin_op:
          real_const (arcsin(valp^.real_val));

      sinh_op:
          real_const (sinh(valp^.real_val));

      cos_op:
          real_const (cos(valp^.real_val));

      arccos_op:
          real_const (arccos(valp^.real_val));

      cosh_op:
          real_const (cosh(valp^.real_val));

      tan_op:
          with valp^ do
            real_const (sin(real_val) / cos(real_val));

      tanh_op:
          real_const (tanh(valp^.real_val));

      cotan_op:
          with valp^ do
            real_const (cos(real_val) / sin(real_val));

      length_op:
	  intgr_const (length (valp^.str_val));

      lwc_op:
          if kind = scalar_cst then
            scal_const (ord(lowercase(chr(ival))))
	  else begin
            fold := cst_expr (makestring (length (valp^.str_val)), nil);
	    fold^.cst_val.valp^.str_val := lowercase (valp^.str_val);
          end;

      upc_op:
          if kind = scalar_cst then
            scal_const (ord(uppercase(chr(ival))))
	  else begin
            fold := cst_expr (makestring (length (valp^.str_val)), nil);
	    fold^.cst_val.valp^.str_val := uppercase (valp^.str_val);
          end;

      round_op:
          intgr_const (round(valp^.real_val));

      arctan_op:
          real_const (arctan(valp^.real_val))

    end (* case opcode *);
  end (* fold_unop *);
$PAGE legal_variant
(*  LegalVariant returns true iff the 'actual_variant' is selected by the
    tag values in the 'record_value'.  *)


function legal_variant ( record_value: val; actual_variant: typ ): boolean;

var
    tag_val: int_type;                          (* Value of a variant tag. *)
    var_lbl: typ;                               (* Scans the variant list for the tag_val. *)

begin
  with actual_variant^ do

    (*  If the variant is the record itself, then the reference is to a symbol
        in the record fixed part, which is always legal.  *)

    if kind = records then
      legal_variant := true

    (*  If the variant is really a variant, then we want to make sure it is
        either a top-level variant, or a subvariant within a legal variant.  *)

    else if not legal_variant (record_value,tag^.tag_recvar) then
      legal_variant := false

    (*  Now we only need to verify that the correct variant is selected
        at this level.  *)

    else begin

      (*  Find the value of the tag field.  This is trickier than it sounds,
          because the tag may be for an undiscriminated union, in which case
          there is no tag symbol, and thus no direct route to the field number
          of the ta field.  We therefore find the first symbol on the field
          list for the 'actual_variant' (if there weren't any, this routine
          would never have been called in the first place) and subtract one
          from its field number.  *)

      tag_val := record_value.valp^.elem_vals[field_list^.fld_number-1].ival;

      (*  Some variant which has the same field list as this variant should
          match the tag value.  We assume that we have the first variant with
          this field list, so we will scan the variant list from here until
          we find a variant label which matches the tag value (all is well)
          or until we run out of variant label nodes with this field list
          (you can't win them all).  *)

      var_lbl := actual_variant;
      repeat
        with var_lbl^ do
          legal_variant := others_var orif
                             ((minlab <= tag_val) and (tag_val <= maxlab));
        var_lbl := var_lbl^.next_variant;
      until legal_variant orif
        (var_lbl = nil) orif
          (var_lbl^.field_list <> (*actual_variant^.*)field_list);
    end;
end (* legal_variant *);
$PAGE fold_agg
(*  FoldAgg will fold a record or array constant, a Min or Max call, or a
    string concatenation or set union operator with all constant operands.  *)

procedure fold_agg ( op: expr );

var
    i: int_type;
    composite: val;
    i_result: int_type;
    r_result: real_type;
    len: char_range;
    total_len: char_range;

begin
  with op^ do begin
    case opcode of

      agg_val:  begin
	if (fold^.desc.kind = records) andif (fold^.desc.base^.flexible) andif
	   (operand[upperbound (operand)]^.desc.kind in [strings, chars]) then
	  return; (* can't fold because we don't know true upper bound *)
        if fold^.desc.kind = arrays
          then composite := makearray (upperbound (operand))
          else composite := makerecord (upperbound (operand));
        with composite.valp^ do begin
          struc_type := fold^.desc.base;
          for i := 1 to upperbound (operand) do
            elem_vals[i] := operand[i]^.cst_val;
        end;
        fold := cst_expr (composite, fold^.desc.base);
      end;

      imin_op:  begin
        i_result := operand[1]^.cst_val.ival;
        for i := 2 to upperbound (operand) do
          i_result := min (i_result, operand[i]^.cst_val.ival);
        scal_const (i_result);
      end;

      imax_op:  begin
        i_result := operand[1]^.cst_val.ival;
        for i := 2 to upperbound (operand) do
          i_result := max (i_result, operand[i]^.cst_val.ival);
        scal_const (i_result);
      end;

      rmin_op:  begin
        r_result := operand[1]^.cst_val.valp^.real_val;
        for i := 2 to upperbound (operand) do
          r_result := min (r_result, operand[i]^.cst_val.valp^.real_val);
        real_const (r_result);
      end;

      rmax_op:  begin
        r_result := operand[1]^.cst_val.valp^.real_val;
        for i := 2 to upperbound (operand) do
          r_result := max (r_result, operand[i]^.cst_val.valp^.real_val);
        real_const (r_result);
      end;

      cat_op:  begin
        total_len := 0;
        for i := 1 to upperbound (operand) do begin
          if operand[i]^.desc.kind = chars
            then total_len := total_len + 1
            else total_len := total_len + length (operand[i]^.cst_val.valp^.str_val);
        end;
        fold := cst_expr (makestring (total_len), nil);
        total_len := 1;
        with fold^.cst_val.valp^ do
          for i := 1 to upperbound (operand) do begin
            if operand[i]^.desc.kind = chars then begin
              str_val [total_len] := chr (operand[i]^.cst_val.ival);
              total_len := total_len + 1;
            end
            else begin
              len := length (operand[i]^.cst_val.valp^.str_val);
	      substr (str_val, total_len, len) := operand[i]^.cst_val.valp^.str_val;
              total_len := total_len + len;
            end;
          end;
      end;

      union_op:  begin
        fold_binop (union_op, operand[1]^.cst_val, operand[2]^.cst_val);
        for i := 3 to upperbound (operand) do
          fold_binop (union_op, fold^.cst_val, operand[i]^.cst_val);
      end

    end (* case opcode *);
  end (* with op^ *);
end (* fold_agg *);
$PAGE fold_str
(*  FoldStr will fold an expression in the string operation format.  *)


procedure fold_str ( opcode: tuple_opcodes; str_val, sop2, sop3: expr );

var
    ind: int_type;
    len: int_type;

begin
  with str_val^.cst_val.valp^ do begin
    case opcode of

      substr_ref:  begin
        ind := sop2^.cst_val.ival;
        if sop3 = nil
          then len := length (str_val) - ind + 1
          else len := sop3^.cst_val.ival;
        if ind < 1 then begin
          warning (err_str_range);
          ind := 1;
        end;
        if ind > length (str_val) + 1 then begin
          warning (err_str_range);
          ind := length (str_val) + 1;
        end;
        if ind + len > length (str_val) + 1 then begin
          warning (err_str_range);
          len := length (str_val) - ind + 1;
        end;
        if fold^.desc.kind = chars then
          scal_const (ord (str_val[1]))
        else begin
          fold := cst_expr (makestring(len),nil);
	  fold^.cst_val.valp^.str_val := substr (str_val, ind, len);
        end;
      end;
$PAGE
      index_op:  begin
	ind := index (str_val, sop2^.cst_val.valp^.str_val);
        if ind <> 0 then
          intgr_const (ind)
        else if sop3 = nil then
          intgr_const (0)
        else
          fold := sop3;
      end;

      search_op:  begin
        ind := 1;
        while (ind <= length (str_val)) andif
	      not element (sop2^.cst_val,ord(str_val[ind])) do
          ind := ind + 1;
        if ind <= length (str_val) then
          intgr_const (ind)
        else if sop3 = nil then
          intgr_const (0)
        else
          fold := sop3;
      end;

      verify_op:  begin
        ind := 1;
        while (ind <= length (str_val)) andif
	      element (sop2^.cst_val,ord(str_val[ind])) do
          ind := ind + 1;
        if ind <= length (str_val) then
          intgr_const (ind)
        else if sop3 = nil then
          intgr_const (0)
        else
          fold := sop3;
      end

    end (* case opcode *);
  end (* with str_val^.cst_val.valp^ *);
end (* fold_str *);
$PAGE fold - main routine
var
    op_ind: int_type;
    all_cst: boolean;
    cst1, cst2: boolean;
    saved_cursor: tuple;

begin
  fold := e;
  if fold = nil then return;
  with fold^, desc do begin
    if kind = unknown_type then
      return;
$IF PASS1
  (* Pass 1 - Fold recursively *)
    saved_cursor := if_cursor;
    t_set (e^.prev);
    if (opcode >= nary_op) and (opcode <= last_nary_op) then
      for op_ind := 1 to upperbound (operand) do
        operand[op_ind] := fold (operand[op_ind], loc);
$ENDIF
    case opcode of


      (*  An IDENTIFIER REFERENCE can be reduced to a constant value if it
          is a non-external constant identifier.  *)

      ident_ref:
        with id_sym^ do
          if (kind = consts) and (dcl_class <> external_sc) then
            fold := cst_expr (init_value,type_desc);


      (*  A FIELD REFERENCE to a constant record value may be reduced to the
          value of the referenced field of the constant.  *)

      field_ref:
        begin
$IF PASS1
  (* Pass 1 - Fold recursively *)
          base_rec := fold (base_rec, loc);
$ENDIF
          if constp (base_rec) then
            if legal_variant (base_rec^.cst_val,field_sym^.fld_variant)
              then fold := cst_expr (
                    base_rec^.cst_val.valp^.elem_vals [field_sym^.fld_number],
                    field_sym^.type_desc )
              else fatal (err_cst_field);
        end;


      (*  A constant ARRAY REFERENCE with a constant subscript value may be
          reduced to the value of the subscripted array element.  The
          subscript can not be out of range, since the array semantics and
          copy semantics generate a sclcvt_op for any questionable array
          subscripts.  *)

      array_ref:
        begin
$IF PASS1
  (* Pass 1 - Fold recursively *)
          base_array := fold(base_array, loc);
          index_val := fold(index_val, loc);
$ENDIF
          if constp(base_array) and constp(index_val) then
            fold := cst_expr (
                  base_array^.cst_val.valp^.elem_vals [index_val^.cst_val.ival],
                  base_array^.desc.base^.element_type )
          else if (base_array^.opcode = agg_val) and constp(index_val) then
            fold := base_array^.operand [index_val^.cst_val.ival];
        end;


      (*  A SUBSTR REFERENCE may be reduced to a constant if all of the operands
          are constants.  *)

      substr_ref:
        if constp (base_string) and constp (substr_index) and
           ( (substr_length = nil) orif constp (substr_length) ) then
          fold_str (substr_ref, base_string, substr_index, substr_length);


      (*  A BINARY OPERATOR can be reduced to a constant value if both its
          operands are constant values.  *)

      first_nbinary_op .. last_nbinary_op:
        begin
          cst1 := constp (operand[1]);
          cst2 := constp (operand[2]);
          if cst1 and cst2 then
            fold_binop (opcode, operand[1]^.cst_val, operand[2]^.cst_val)
$IF PASS2
  (* Pass 2 - Apply algebraic identities *)
          else if cst1 then
            fold_bin_1 (opcode, operand[1]^.cst_val, operand[2])
          else if cst2 then
            fold_bin_2 (opcode, operand[1], operand[2]^.cst_val);
$ENDIF
        end;


      (*  A UNARY OPERATOR can be reduced to a constant value if its operand
          is a constant value.  *)

      first_nunary_op .. last_nunary_op:
        begin
          if constp (operand[1]) then
            fold_unop (opcode,operand[1]);
        end;


      (*  The ROUND and ARCTAN operators may be either binary or unary.
          They shoulded if their argument(s) are constant.  *)

      round_op, arctan_op:
        if constp (operand[1]) then
          if upperbound (operand) = 1 then
            fold_unop (opcode, operand[1])
          else if constp (operand[2]) then
            fold_binop (opcode, operand[1]^.cst_val, operand[2]^.cst_val);


      (*  A SET GENERATOR operator may have zero, one, or two arguments.
          In Pass 1, they should be folded if all the arguments are constant.  *)

$IF PASS1
      gen_set_op:
        if upperbound (operand) = 0 then
          make_range_set (nil, nil)
	else if constp (operand [1]) then begin
	  if upperbound (operand) = 1 then
	    make_range_set (operand[1], operand[1])
	  else if constp (operand [2]) then
	    make_range_set (operand[1], operand[2]);
	end;
$ENDIF


      (*  An AGGREGATE VALUE, a MAX or MIN function call, the concatenation of
          two or more strings, or the union of two or more sets, can all be
          reduced to constants if all of their operands are constants.  *)

      agg_val, imin_op, rmin_op, imax_op, rmax_op, cat_op, union_op:
        begin
          all_cst := true;
          for op_ind := 1 to upperbound (operand) do
            exit if not constp (operand[op_ind]) do
              all_cst := false;
          if all_cst then
            fold_agg (fold);
        end;


      (*  An INDEX, SEARCH, or VERIFY call may be reduced to a constant if
          the first two arguments are constants, and the third argument is
          either omitted or a constant.  *)

      index_op, search_op, verify_op:
        if constp (operand[1]) and constp (operand[2]) then
          if upperbound (operand) = 2 then
            fold_str (opcode, operand[1], operand[2], nil)
          else if constp (operand [3]) or (opcode <> substr_ref) then
            fold_str (opcode, operand[1], operand[2], operand[3]);

      others:

    end (* case opcode *);
  end (* with fold^, desc *);


999:

$IF PASS1
  (* Pass 1 - Fold recursively *)
  t_set (saved_cursor);
$ENDIF
end (* fold *).
  @Ü