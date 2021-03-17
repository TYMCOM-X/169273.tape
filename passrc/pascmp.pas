$WIDTH=100
$LENGTH=55
$TITLE PASCMP.PAS, last modified 1/3/84, zw
MODULE pascmp OPTIONS SPECIAL(WORD);
(*type compatibility tests*)

$HEADER PASCMP.HDR

$PAGE system modules

$SYSTEM pascal.inc
$SYSTEM pasist.inc

$PAGE definitions

public function identtypes ( ta, tb: typ ): boolean;
forward;
$PAGE identparms:  compare two parameter lists
(*  IDENTPARMS will return true if the parameters of two subroutine types have
    identical types and matching kinds (value/variable).  *)

function identparms ( ta, tb: typ ): boolean;

var i: parm_range;

begin
  identparms := true;
  for i := 1 to upperbound (ta^.params) do
    exit if not (identtypes (ta^.params[i].parm_type, tb^.params[i].parm_type) and
                 (ta^.params[i].parm_kind = ta^.params[i].parm_kind)) do
      identparms := false;
end (* identparms *);
$PAGE identranges
(* IDENTRANGES tests if the set element type or array index types denote the
   same subrange.  This differs from identtypes in that the packing of the
   subranges need not be the same. *)

function identranges (typa, typb: typ): boolean;
begin
  identranges := (typa = typb) orif ((typa = nil) or (typb = nil));
  if identranges then return;           (* obvious or default match *)

  identranges := (typa^.base_type = typb^.base_type) and        (* compare ints, bools, chars, or scalars *)
                 (typa^.minval = typb^.minval) and
                 (typa^.maxval = typb^.maxval);
end;
$PAGE identtypes:  test if two types are identical
(*  identtypes will return true iff a value of type a is
    necessarily also a value of type b, and vice versa.  *)

public function identtypes;                     (* see forward declaration *)

var typa, typb: typ;

begin
  typa := ta;
  while (typa <> nil) andif (typa^.kind = indirect_type) do
    typa := typa^.actual_type;
  typb := tb;
  while (typb <> nil) andif (typb^.kind = indirect_type) do
    typb := typb^.actual_type;
  identtypes := (typa = typb) orif
                ((typa = nil) or (typb = nil));
  if identtypes then return;

  identtypes := (typa^.kind = typb^.kind);
  if not identtypes then return;

  identtypes := (typa^.kind = files) andif
                ( (typa^.file_kind = anyfile) or (typb^.file_kind = anyfile) );
  if identtypes then return;

  if typa^.packable or typb^.packable then begin                (* unpacked and packed types are not identical *)
    identtypes := (typa^.packable = typb^.packable)
                    and (typa^.base_size = typb^.base_size);      (* required for simple types,
                                                           redundant for arrays, records *)
    if not identtypes then return;
  end;

  with typa^ do
    case kind of

      ints,
      chars,
      bools,
      scalars:  identtypes :=
          (base_type = typb^.base_type) and
          (minval = typb^.minval) and
          (maxval = typb^.maxval);

      reals:  identtypes :=
          (rminval = typb^.rminval) and
          (rmaxval = typb^.rmaxval) and
          (precision = typb^.precision);

      sets:  identtypes :=
          identranges(set_element_type,typb^.set_element_type);

      pointers:  identtypes :=
          identtypes(target_type,typb^.target_type);

      arrays:  identtypes :=
          identtypes(element_type,typb^.element_type) and
          identranges(index_type,typb^.index_type) and
          (flexible = typb^.flexible) and (generic = typb^.generic);

      files:  identtypes :=
          (file_kind = typb^.file_kind) andif
          identtypes (component_type, typb^.component_type);

      strings:  identtypes :=
          (str_kind = typb^.str_kind) and
          (str_length = typb^.str_length) and
          (flexible = typb^.flexible);

      procs,
      funcs:  identtypes :=
        ( identtypes(return_type,typb^.return_type) and
          (fortran_call = typb^.fortran_call) and
          (upperbound (params) = upperbound (typb^.params)) ) andif
          identparms(typa,typb);

      others:  identtypes :=
          (typa = typb)

    end (* case kind *);
end (* identtypes *);
$PAGE equivtypes
(* EQUIVTYPES tests for the strict compatibility of two types. *)

public function equivtypes ( ta, tb: typ ): boolean;

var typa, typb: typ;

begin
  typa := ta;
  while (typa <> nil) andif (typa^.kind = indirect_type) do
    typa := typa^.actual_type;
  typb := tb;
  while (typb <> nil) andif (typb^.kind = indirect_type) do
    typb := typb^.actual_type;
  equivtypes := (typa = typb) orif ((typa = nil) or (typb = nil));
  if equivtypes then return;

  equivtypes := (typa^.kind = typb^.kind);
  if not equivtypes then return;

  equivtypes := (typa^.kind = files) andif
                ( (typa^.file_kind = anyfile) or (typb^.file_kind = anyfile) );
  if equivtypes then return;

  if typa^.packable or typb^.packable then begin
    equivtypes := (typa^.packable = typb^.packable);
    if typa^.kind in [ints, reals] then
      equivtypes := equivtypes and (typa^.base_size = typb^.base_size);
    if not equivtypes then return;
  end;

  with typa^ do
    case kind of

      scalars:  equivtypes :=
          (base_type = typb^.base_type);

      reals:  equivtypes :=
          (precision = typb^.precision);

      sets:  equivtypes :=
          identranges(set_element_type,typb^.set_element_type);

      pointers:  equivtypes :=
          identtypes(target_type,typb^.target_type);

      arrays:  equivtypes :=
          identtypes(element_type,typb^.element_type) and
          ( identranges(index_type,typb^.index_type) orif
            (generic or typb^.generic) orif
            ( (flexible or typb^.flexible) and
              (index_type^.base_type = typb^.index_type^.base_type) and
              (index_type^.minval = typb^.index_type^.minval)           ) );

      files:  equivtypes :=
          (file_kind = typb^.file_kind) andif
          identtypes (component_type, typb^.component_type);

      strings:  equivtypes :=
          (str_kind = typb^.str_kind) and
          ( (str_length = typb^.str_length) or (flexible or typb^.flexible) );

      records:  equivtypes :=
          (typa = typb);

      procs,
      funcs:  equivtypes :=
        ( identtypes(return_type,typb^.return_type) and
          (fortran_call = typb^.fortran_call) and
          (upperbound (params) = upperbound (typb^.params)) ) andif
          identparms(typa,typb);

      others:  equivtypes :=
          true

    end (* case kind *);
end (* equivtypes *).
