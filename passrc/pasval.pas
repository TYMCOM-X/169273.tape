$WIDTH=100
$LENGTH=55
$TITLE PASVAL.PAS, last modified 1/16/84, zw
MODULE pasval OPTIONS SPECIAL(WORD);
(*TYM-Pascal compiler -- value node creation routines*)

$HEADER PASVAL.HDR

$PAGE system modules

$SYSTEM PASCAL.INC
$SYSTEM PASIST.INC

$PAGE makestring, makeset, makearray, makerecord, cst_scalar, mkreal

PUBLIC FUNCTION makestring(len: char_range): val;
(*returns a string constant table node big enough for the specified length*)
BEGIN
  makestring.kind := string_cst; NEW(makestring.valp,string_cst,len);
  WITH makestring.valp^ DO BEGIN def_addr := NIL; str_varying_ref := FALSE END
END;

PUBLIC FUNCTION makeset(minbit, maxbit: int_type): val;
(*returns a set constant table node big enough for [minbit .. maxbit]*)
VAR size: bit_range;
BEGIN
  IF maxbit >= minbit THEN size := maxbit - minbit + 1 ELSE size := 0;
  makeset.kind := set_cst; NEW(makeset.valp, set_cst, size - 1);
  WITH makeset.valp^ DO BEGIN def_addr := NIL; set_origin := minbit END
END;

PUBLIC FUNCTION makearray(n_comps: int_type): val;
(*returns an array constant table node which is big enough to hold n comps*)
BEGIN
  makearray.kind := array_cst;
  NEW(makearray.valp, array_cst, n_comps); makearray.valp^.def_addr := NIL
END;

PUBLIC FUNCTION makerecord(n_fields: int_type): val;
(*returns a record constant table node which is big enough to hold n fields*)
BEGIN
  makerecord.kind := record_cst;
  NEW(makerecord.valp, record_cst, n_fields); makerecord.valp^.def_addr := NIL
END;

PUBLIC FUNCTION cst_scalar(svalue: int_type): val;
(*returns a scalar constant table node containing the specified scalar value*)
BEGIN
  cst_scalar.kind := scalar_cst; cst_scalar.ival := svalue
END;

PUBLIC FUNCTION mkreal(rval: real_type; rprec: prec_type): val;
(*returns a real constant table node containing the specified real value*)
BEGIN
  mkreal.kind := real_cst; NEW(mkreal.valp, real_cst);
  WITH mkreal.valp^ DO BEGIN
    def_addr := NIL; real_val := rval; real_prec := rprec
  END
END.

