$WIDTH=100
$LENGTH=55
$TITLE PASESU.PAS, last modified 1/18/84, zw
MODULE pasesu OPTIONS SPECIAL(WORD);
(*TYM-Pascal compiler -- expression semantics utilities*)

$HEADER pasesu.hdr

$PAGE system modules

$SYSTEM PASCAL.INC
$SYSTEM PASIST.INC
$SYSTEM PTMCON.INC
$SYSTEM PASPT.TYP
$SYSTEM PASIF.TYP
$SYSTEM PASVAL.INC
$SYSTEM PASEMU.INC
$SYSTEM PASIFU.INC
$SYSTEM UTLMTH.INC
$SYSTEM PASERR.INC

$PAGE ck_unknown_type

(*  CkUnknownType is called with a type node whose kind is 'unknown_type'.
    An error message is printed at the first occurrence of the type name,
    marking it as undefined, and the type node is changed from an unknown
    type to an indirect type, whose actual type is nil.  *)

public procedure ck_unknown_type ( node: typ );

begin
  with node^ do begin
    if (type_id <> nil) andif (type_id^.name <> nil) then
      with type_id^.name^ do begin
        if declaration.line_no <> 0
          then err_print ( err_type_undefined, declaration, text, 0 )
          else err_print ( err_envtype_undefined, cur_source, text, 0 );
      end;
    kind := indirect_type;
    actual_type := nil;
  end;
end (* ck_unknown_type *);
$PAGE initexpr

(*  InitExpr is called with an expression tree node and a type record, and
    sets up the type information in the node in accordance with the record.  *)

public procedure initexpr ( node: expr; node_type: typ );

var nt: typ;

begin
  nt := node_type; (* If this is an indirect type node, *)
  while (nt <> nil) andif (*   find the actual type. *)
    (nt^.kind = indirect_type) do
      nt := nt^.actual_type;
  with node^ do begin
    result := nil;
    ref_fre := 0;
    context := valx;
    blk_input_tuple := false;
    copy_tuple := false;
    killed_tuple := false;
    desc.base := nt;
    if nt = nil then
      desc.kind := unknown_type
    else with nt^ do begin
      desc.kind := kind;
      case kind of
        bools, ints, chars, scalars:
          begin
            desc.signed := minval < 0;
            desc.int_prec := base_size;
          end;
	files, pointers:
	  begin
	    desc.signed := false;
	    desc.int_prec := base_size;
	  end;
        reals:
          desc.precision := precision;
        strings:
          begin
            desc.str_kind := str_kind;
            desc.str_length := str_length;
            desc.str_flex := flexible;
          end;
        sets:
          begin
            if nt^.set_element_type = nil then
              desc.base := nil;
            desc.set_cst_lwb := true;
            desc.set_lwb := set_element_type^.minval;
            desc.set_cst_len := true;
            desc.set_length := set_element_type^.maxval - set_element_type^.minval + 1;
          end;
        unknown_type:
          ck_unknown_type (nt);
        OTHERS: (*do nothing*)
      end (* case kind *);
    end (* nt <> nil *);
  end (* with node^ *);
end (* initexpr *);
$PAGE initstr

(* InitStr is called with an expression node representing a string expression
   (substr or concatenation), a flag indicating a flexible or nonflexible result,
   and a maximum length.  It sets up the descriptor for the node.  The result is
   always nonvarying. *)


public procedure initstr ( node: expr; str_flex: boolean; str_length: char_range );

begin
  with node^ do begin
    result := nil;
    desc.base := nil;
    desc.kind := strings;
    desc.str_kind := nonvarying;
    desc.str_flex := str_flex;
    desc.str_length := str_length;
  end;
end (* initstr *);
$PAGE cst_expr

(*  CstExpr takes a value node and a type node, and returns a CST_REF
    expression tree node with that type and value.  *)


public function cst_expr ( cvalue: val; ctype: typ ): expr;

begin
  new (cst_expr,cst_ref,cst_ref);
  initexpr (cst_expr,ctype);
  with cst_expr^ do begin
    cst_val := cvalue;
    case cst_val.kind of

      scalar_cst:
        begin
          desc.signed := (cst_val.ival < 0);
          desc.int_prec := int_bits (cst_val.ival);
        end;

      string_cst:
        begin
          desc.kind := strings;
          desc.str_kind := nonvarying;
          desc.str_length := length (cst_val.valp^.str_val);
          desc.str_flex := false;
        end;

      set_cst:
        begin
          with cst_val.valp^ do begin
            if desc.base <> nil then
              desc.base := desc.base^.set_element_type^.base_type;
            desc.kind := sets;
            desc.set_cst_lwb := true;
            desc.set_lwb := set_origin;
            desc.set_cst_len := true;
            desc.set_length := dimension (set_val);
          end;
        end;

      real_cst:
        begin
          desc.kind := reals;
          desc.precision := cst_val.valp^.real_prec;
        end;
      OTHERS: (*do nothing*)
    end (* case *) ;
  end (* with cst_expr^ *);
  emit (cst_expr);
end (* cst_expr *);
$PAGE constp

(*  Constp determines whether or not its argument is a reference to a valid
    constant.  Errorneous cases are not considered as constants.        *)


public function constp ( cst_maybe: expr ): boolean;

begin
  if cst_maybe = nil then
    constp := false
  else
    with cst_maybe^ do
      constp := (opcode = cst_ref) andif (desc.kind <> unknown_type)
        andif (cst_val.kind <> subr_cst); (* No subr constants allowed. *)
end (* constp *);
$PAGE assignable
(*  Assignable is a test function which checks whether an expression tree
    represents a variable reference, which may occur on the left-hand side
    of an assignment or as a 'var' parameter.  *)


public function assignable ( ref: expr ): boolean;
begin
  with ref^ do

    if desc.kind = unknown_type then
      assignable := true

    else
      case opcode of

        ident_ref:
          assignable := (id_sym^.kind = vars);

        field_ref:
          assignable := assignable (base_rec);

        array_ref:
          assignable := assignable (base_array);

        substr_ref:
          assignable := assignable (base_string);

        ptr_ref:
          assignable := true;

        buffer_ref:
          assignable := true;

        others:
          assignable := false

      end (* case opcode *);
end (* assignable *).
 