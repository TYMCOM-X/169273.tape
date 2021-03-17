(* EXE003 - general test of reference code.  *)

program exe003;
$disable generics

type
  neg_bound = -3..7;
  color = (red, blue, green);
  color_arr = packed array [1..20] of color;
  str3 = packed array [1..3] of char;
  str7 = packed array [1..7] of char;
  str = string[16];
  set8 = set of 1..8;
  set40 = set of 1..40;
  set8array = array [1..6] of set8;
  pset8array = packed array [1..6] of set8;
  set40array = array [1..3] of set40;
  pset40array = packed array [1..3] of set40;
  rec8 = record
    f1: char;
    f2: boolean
  end;
  prec8 = packed record
    pf1: char;
    pf2: boolean
  end;
  int9 = 0..#o777;
  bigrec = packed record
    f1: str3;
    f2: int9;
    f3: set8;
    f4: boolean
  end;
  intarr = packed array [1..5] of int9;
  sarr = array [1..5] of prec8;
  psarr = packed array [1..5] of prec8;
  barr = array [1..3] of bigrec;
  pbarr = packed array [1..3] of bigrec;
  arrarr = array [1..3] of intarr;
  parrarr = packed array [1..3] of intarr;
  bigbigrec = packed record
    f1: boolean;
    f2: bigrec;
    f3: prec8;
    f4: intarr
  end;
  therec = packed record
    f1: boolean;
    case blah: int9 of
      1,3..5: (f2: int9);
      2,6..7: (case boolean of
        true: (case int9 of
          1..3,6: (f3: prec8);
          4..5,7: (f4: set8));
        false: (f5: intarr))
  end;
  str3arr = packed array [1..3] of str3;
  srec = packed record
    f1: intarr;
    f2: boolean
  end;
  another_rec = packed record
    f1: integer;
    case f2: boolean of
      true: ();
      false: (f3: 0..#o777777)
  end;
  yet_another_rec = packed record
    case char of
      'a'..'c': (f1: set8);
      'w'..'z': (f2: str3);
      others:   (f3: color)
  end;
  neg_arr = array [-2..3] of neg_bound;
  pneg_arr = packed array [-2..3] of neg_bound;
  neg_rec = packed record
    f1: int9;
    f2: boolean;
    f3: neg_bound;
    f4: color
  end;
$IF GENERICS gen_arr = packed array [*] of int9;
$IFNOT GENERICS gen_arr = packed array [-3..*] of int9;
  half_neg = -#o400000..#o377777;
  half_neg_rec = packed record
    f1: 0..#o777777;
    f2: half_neg;
  end;
  flex_arr = packed array [-3..*] of int9;
  flex_rec = packed record
    f1: color;
    f2: packed array [1..*] of half_neg;
  end;

  scalar_type = (sc0, sc1, sc2, sc3, sc4, sc5 );
  scalar_rec = packed record
    i1 : 1..50;
    s1 : scalar_type;
    s2 : scalar_type;
    s3 : scalar_type;
    i2 : 1..4;
  end;

public var
  acolor: color := blue;
  an_int: integer := -3;
  ch: char := 'A';
  bool: boolean := true;
  x:real := -17.0;
  abc: str3 := 'ABC';
  efg: str7 := 'ABCDEFG';
  efgstr: str := '890123456';
  set81: set8 := [2,4,6,8];
  set401: set40 := [5,10,15,20,25,30,35,40];
  set8arr: set8array := ([1],[1,2],[1,2,3],[8],[7,8],[6,7,8]);
  pset8arr: pset8array := ([1],[1,2],[1,2,3],[8],[7,8],[6,7,8]);
  set40arr: set40array := ([40,39,38,37],[1,2,3],[10,20,30,40]);
  pset40arr: pset40array := ([40,39,38,37],[1,2,3],[10,20,30,40]);
  colors: color_arr := (red, blue, green, red, red, blue, blue, green, green,
      green, blue, red, green, green, blue, blue, red, red, blue, green);
  rec81: rec8 := ('Z',true);
  prec81: prec8 := ('A',true);
  recarr: sarr := (('A',true),('B',false),('C',true),('D',false),('E',false));
  precarr: psarr := (('A',true),('B',false),('C',true),('D',false),('E',false));
  brecarr: barr := (('ABC',#o777,[1,2,3],true),('xyz',0,[],false),('abc',1,[2,4,6],true));
  pbrecarr: pbarr := (('ABC',#o777,[1,2,3],true),('xyz',0,[],false),('abc',1,[2,4,6],true));
  a2d: arrarr := ((1,2,3,4,5),(5,4,3,2,1),(507,508,509,510,511));
  pa2d: parrarr := ((1,2,3,4,5),(5,4,3,2,1),(507,508,509,510,511));
  bbrec: bigbigrec := (true, ('lmn',3,[6,7,8],false),('z',true),(2,4,6,8,10));
  arec: therec := (true,7,true,5,[1,3,5,7]);
  other1: another_rec := (-64,true);
  other2: another_rec := (-32,false,#o400000);
  shortarr: parrarr := ((1,2,3,4,5),(2,4,6));
  shortrec: bigbigrec := (false,('abc',3),('z',true));
  srec1: srec := ((1),true);
  str3arr1: str3arr := ('ABC', 'DEF', 'GHI');
  arec2: therec := (false, 2, false, (8, 16, 24, 32, 40));
  yet1: yet_another_rec := ('a', [8,3..5]);
  yet2: yet_another_rec := ('y','oof');
  yet3: yet_another_rec := (chr(2),green);
  neg_arr1: neg_arr := (-3,7,-2,-1,6,0);
  pneg_arr1: pneg_arr := (-3,7,-2,-1,6,0);
  neg_rec1: neg_rec := (511, false, -2, blue);

var
  flx_arr_ptr: ^flex_arr;
  negb: neg_bound;
  i,j,k: integer;
  the_rec_ptr: ^therec;

public procedure error ( error_number: 1..999 );
  begin
    writeln ( ttyoutput, 'Error ', error_number );
    break ( ttyoutput );
  end;
$PAGE outer

public function outer ( hue: color; var gene: gen_arr; small_rec: prec8 ): neg_bound;

var
  dx,dy: minimum(real)..maximum(real) prec 14;
  hn_rec1: half_neg_rec;
  fa_ptr: ^flex_arr;
  fr_ptr: ^flex_rec;
  bbptr: ^bigbigrec;
  hn_rec: half_neg_rec;

  scal_rec : scalar_rec;
  int_1    : integer;
  scal_1   : scalar_type;

$PAGE scalar coercion procedures
procedure scalar_test_1 ( coerced_integer : scalar_type);
  var scalar : scalar_type;
  begin
    scalar := pred (coerced_integer);
    if scalar <> sc3
      then error( 231 );
  end;
procedure scalar_test_2 (var coerced_integer : scalar_type);
  var scalar : scalar_type;
  begin
    scalar := succ (coerced_integer);
    if scalar <> sc5
      then error ( 232 );
  end;
procedure scalar_test_3 (coerced_integer : scalar_type);
  var scalar : scalar_type;
  begin
    scalar := pred (coerced_integer);
    if scalar <> sc0
      then error ( 233 );
  end;
procedure scalar_test_4 (sc_rec : scalar_rec);
  begin
    if ord (sc_rec.s1) <> 1 
      then error ( 234 );
    if scalar_type (sc_rec.i2) <> sc2 
      then error ( 235 );
  end;
procedure scalar_test_5 ( var  sc_rec : scalar_rec );
  begin
    sc_rec.i1 := ord (sc_rec.s1);
    sc_rec.s2 := scalar_type (sc_rec.i2);
    sc_rec.s3 := scalar_type (sc_rec.i1 + 2);
  end;
$PAGE inner

function inner ( flx: flex_arr; var flx_rec: ^flex_rec; ch_set: set of ' '..'g';
		therecptr: ^therec; bbptr: ^bigbigrec; b: boolean ): half_neg_rec;

var
  i,j: int9;
  hue2: color;

begin
  i := dimension ( flx );			(* 5 *)
  if i <> 5 then error ( 301 );
  i := dimension ( gene );			(* 6 *)
  if i <> 6 then error ( 302 );
  inner.f1 := flx_rec^.f2[4] + flx[0] div 3;	(* 400001b *)
  if inner.f1 <> #o400001 then error ( 303 );
  inner.f2 := -gene[2];				(* -7 *)
  if inner.f2 <> -7 then error ( 304 );
end;

$PAGE outer - body
begin
  new ( fa_ptr, 1 );
  fa_ptr^[ an_int ] := shortarr[ 2, ord ( upperbound ( fa_ptr^ ) = 1 ) ];	(* 2 *)
  if fa_ptr^[ -3 ] <> 2 then error ( 201 );
  fa_ptr^[-2] := pa2d[2,upperbound(fa_ptr^)];		(* 5 *)
  if fa_ptr^[-2] <> 5 then error ( 202 );
  fa_ptr^[-1] := arec2.f5[fa_ptr^[-2]] div a2d[1,5];	(* 40 div 5 => 8 *)
  if fa_ptr^[-1] <> 8 then error ( 203 );
  fa_ptr^[0] := other1.f1 + 70;			(* 6 *)
  if fa_ptr^[0] <> 6 then error ( 204 );
  fa_ptr^[pbrecarr[3].f2] := (brecarr[fa_ptr^[-3] - 1].f2 + 1) div 8;	(* 64 *)
  if fa_ptr^[ 1 ] <> 64 then error ( 205 );
  
  new ( fr_ptr, 5 );
  with fr_ptr^ do begin
    f1 := hue;					(* green *)
    if fr_ptr^.f1 <> green then error ( 206 );
    f2[1] := an_int;				(* -3 *)
    if fr_ptr^.f2[1] <> -3 then error ( 207 );
    f2[2] := -1000;				(* -1000 *)
    if fr_ptr^.f2[2] <> -1000 then error ( 208 );
    f2[3] := -other2.f3;			(* -400000b *)
    if fr_ptr^.f2[3] <> -#o400000 then error ( 209 );
    f2[4] := other2.f3 - 1;			(* 377777b *)
    if fr_ptr^.f2[4] <> #o377777 then error ( 210 );
    f2[fa_ptr^[-2]] := other2.f1;		(* -32 *)
    if fr_ptr^.f2[5] <> -32 then error ( 211 );
  end;

  new ( the_rec_ptr, 7, false );
  the_rec_ptr^.f1 := true;			(* true *)
  if the_rec_ptr^.f1 <> true then error ( 212 );
  the_rec_ptr^.f5[1] := the_rec_ptr^.blah;	(* 7 *)
  if the_rec_ptr^.f5[ 1 ] <> 7 then error ( 213 );
  the_rec_ptr^.f5[2] := gene[0] * gene[-1];	(* 50 *)
  if the_rec_ptr^.f5[ 2 ] <> 50 then error ( 214 );
  the_rec_ptr^.f5[3] := gene[ fr_ptr^.f2[1] ];	(* 21 *)
  if the_rec_ptr^.f5[ 3 ] <> 21 then error ( 215 );

  new ( bbptr );
  bbptr^.f2.f2 := 511;				(* 511 *)
  if bbptr^.f2.f2 <> 511 then error ( 216 );
  bbptr^.f1 := true;				(* true *)
  if bbptr^.f1 <> true then error ( 217 );
  bbptr^.f4[3] := 120;				(* 120 *)
  if bbptr^.f4[ 3 ] <> 120 then error ( 218 );

  hn_rec := inner ( fa_ptr^, fr_ptr, ['A'..'D'], the_rec_ptr, bbptr,
		    false );			(* 400001b, -7 *)
  if hn_rec.f1 <> #o400001 then error ( 219 );	
  if hn_rec.f2 <> -7 then error ( 220 );	
  outer := -fa_ptr^[an_int];			(* -2 *)
  if outer <> -2 then error ( 221 );

  (* Test coerced scalars as parameters to procedures *)

  int_1 := 4;
  scal_1 := scalar_type ( int_1 );
  if scal_1 <> sc4 
    then error ( 230 );
  (* pass the coerced scalar as a value parameter *)
  scalar_test_1 (scal_1);
  (* Pass the coerced scalar as a var parameter *)
  scalar_test_2 (scal_1);
  
  with scal_rec
  do begin
    i1 := 1;
    i2 := 2;
    s1 := scalar_type (i1);
    s2 := scalar_type (i2);
    s3 := scalar_type (i2 + 1);
  end;
  (* Pass a scalar from the record *)
  scalar_test_3 (scal_rec.s1);
  (* now pass the entire record as a parameter *)
  scalar_test_4 (scal_rec);
  (* Pass the record as a var parameter *)
  scalar_test_5 (scal_rec);
  if scal_rec.i1 <> 1 
    then error( 236 );
  if scal_rec.i2 <> 2
    then error (237);
  if scal_rec.s1 <> sc1
    then error (238);
  if scal_rec.s2 <> sc2
    then error (239);
  if scal_rec.s3 <> sc3
    then error (240);
end  (* proc outer *) ;
$PAGE reftst - body
begin
  rewrite ( ttyoutput );
  writeln ( ttyoutput, 'Begin EXE003' );
  acolor := blue;
  if acolor <> blue then error ( 101 );
  an_int := -3;
  if an_int <> -3 then error ( 102 );
  ch := 'z';
  if ch <> 'z' then error ( 103 );
  x := -17.0;  
  if x <> -17.0 then error ( 104 );

  rec81.f1 := ch;				(* 'z' *)
  if rec81.f1 <> 'z' then error ( 105 );
  prec81.pf2 := true;				(* true *)
  if prec81.pf2 <> true then error ( 106 );
  recarr[ 4 ].pf2 := prec81.pf2;		(* true *)
  if recarr[ 4 ].pf2 <> true then error ( 107 );
  an_int := brecarr[ 3 ].f2;			(* 1 *)
  if an_int <> 1 then error ( 108 );
  an_int := -3;
  ch := precarr[ brecarr[3].f2 ].pf1;		(* 'A' *)
  if ch <> 'A' then error ( 109 );
  pbrecarr[ 2 ].f2 := a2d[3,4];			(* 510 *)
  if pbrecarr[ 2 ].f2 <> 510 then error ( 110 );
  pa2d[ 1, pa2d[2,4] ] := bbrec.f4[5];		(* 10 *)
  if pa2d[ 1, 2 ] <> 10 then error ( 111 );
  bbrec.f2.f4 := arec.f1;			(* true *)
  if bbrec.f2.f4 <> true then error ( 112 );
  other1.f2 := other2.f2;			(* false *)
  if other1.f2 <> false then error ( 113 );
  neg_arr1[neg_rec1.f3] := neg_arr1[ -1 ];	(* 7 *)
  if neg_arr1[ -2 ] <> 7 then error ( 114 );
  k := neg_arr1[ -2 ];				(* 7 *)
  if k <> 7 then error ( 115 );
  neg_arr1[ -1 ] := -2;				(* -2 *)
  if neg_arr1[ -1 ] <> -2 then error ( 116 );
  neg_arr1 [-an_int] := neg_arr1 [2]; (* 6 *)
  if neg_arr1 [3] <> 6 then error (117);
  neg_arr1 [-2] := neg_arr1 [neg_arr1 [1]]; (* -2 *)
  if neg_arr1 [-2] <> -2 then error (118);
  neg_arr1 [neg_arr1 [-2]] := neg_arr1 [-an_int]; (* 6 *)
  if neg_arr1 [-2] <> 6 then error (119);

  pneg_arr1[neg_rec1.f3] := pneg_arr1[ -1 ];	(* 7 *)
  if pneg_arr1[ -2 ] <> 7 then error ( 120 );
  k := pneg_arr1[ -2 ];				(* 7 *)
  if k <> 7 then error ( 121 );
  pneg_arr1[ -1 ] := -2;				(* -2 *)
  if pneg_arr1[ -1 ] <> -2 then error ( 122 );
  pneg_arr1 [-an_int] := pneg_arr1 [2]; (* 6 *)
  if pneg_arr1 [3] <> 6 then error (123);
  pneg_arr1 [-2] := pneg_arr1 [pneg_arr1 [1]]; (* -2 *)
  if pneg_arr1 [-2] <> -2 then error (124);
  pneg_arr1 [pneg_arr1 [-2]] := pneg_arr1 [-an_int]; (* 6 *)
  if pneg_arr1 [-2] <> 6 then error (125);
  
  new ( flx_arr_ptr, 2 );
  flx_arr_ptr^[-3] := 21;			(* 21 *)
  if flx_arr_ptr^[ -3 ] <> 21 then error ( 126 );
  flx_arr_ptr^[-2] := pa2d[3,2] mod 500;	(* 8 *)
  if flx_arr_ptr^[ -2 ] <> 8 then error ( 127 );
  flx_arr_ptr^[-1] := a2d[2,1];			(* 5 *)
  if flx_arr_ptr^[ -1 ] <> 5 then error ( 128 );
  flx_arr_ptr^[0]  := bbrec.f4[5];		(* 10 *)
  if flx_arr_ptr^[ 0 ] <> 10 then error ( 129 );
  flx_arr_ptr^[1] := 0;				(* 0 *)
  if flx_arr_ptr^[ 1 ] <> 0 then error ( 130 );
  flx_arr_ptr^[2] := arec.blah;			(* 7 *)
  if flx_arr_ptr^[ 2 ] <> 7 then error ( 131 );

  negb := outer ( green, flx_arr_ptr^, prec81 );	(* -2 *)
  if negb <> -2 then error ( 132 );
  writeln ( ttyoutput, 'End EXE003' );
end.
  