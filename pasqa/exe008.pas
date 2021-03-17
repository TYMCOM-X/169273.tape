Program EXE008 Options nocheck;

type
  three_byte_rec = packed record
    byte_1 : 0..255;
    byte_2 : 0..55;
    byte_3 : -128..127
  end;

  three_word_rec = packed record
    word_1 : 0..1024;
    word_2 : 0..32767;
    word_3 : -256..255
  end;

public var
  i,j,k : integer;
  i_byte, j_byte, k_byte : 0..255;
  i_word, j_word, k_word : 0..32767;
  x,y,z : real;
  xx,yy,zz : -1e10 .. 1e10 prec 10;
  flex_fstr : ^packed array [ 1..* ] of char;
  flex_vstr : ^string [ * ];
  fstr30 : packed array [ 1..30 ] of char;
  str30 : string [ 30 ];
  byte_rec : three_byte_rec;
  word_rec : three_word_rec;
  ch : char;
  bool, bool1, bool2, bool3, in_func4: boolean;

function func_add ( a: integer; b: integer ) : integer;
  begin
    func_add := a + b
  end;

procedure error ( num : integer );
  begin
    writeln ( ttyoutput , 'Error ' , num )
  end;
   
procedure r_eql (x, y: real; num: integer);
  begin
    if abs (x - y) >= 0.000001 then error (num)
  end;

function targ_test ( x : real ) : real;

begin
  targ_test := x
end;

function bool_func1 (b1, b2: boolean): boolean;
  begin
    if not b1 then error (403);
    if not b2 then error (404);
    bool_func1 := b1 andif b2
  end;
function bool_func2 (b1, b2: boolean): boolean;
  begin
    error (401) (* shouldn't ever be here *)
  end;
function bool_func3: boolean;
  begin
    error (402) (*    "       "   "   "   *)
  end;
function bool_func4 (b1: boolean): boolean;
  begin
    if not b1 then error (405);
    bool_func4 := b1;
    in_func4 := true
  end;

begin
  rewrite ( ttyoutput );
  writeln ( ttyoutput , 'Begin EXE008' );

  (* The following examples are not of targetting, but rather test some
     dynamic temps. The function call to "error" with a variable does
     show some targetting, though. *)

  new ( flex_fstr , 300 );
  putstring ( flex_fstr^[1:10] , '1234567890' );

  x := 3.1415926535;
  xx := 3.141592653578;

  for i := 1 to 7 do
    begin
      y := round ( x , index(uppercase(flex_fstr^), flex_fstr^[i] ));
      yy := round ( xx , index(uppercase(flex_fstr^),flex_fstr^[i] ));
      if y <> round ( x , i )
	then error ( 10+i );
      if yy <> round ( xx , i )
	then error ( 20+i );
      zz := round ( x , index(lowercase(flex_fstr^), flex_fstr^[i] ));
      z := round ( xx , index(lowercase(flex_fstr^), flex_fstr^[i] ));
      if zz <> round ( x , i )
	then error ( 30+i );
      if z <> round ( xx , i )
	then error ( 40+i )
    end;

  (* lets try some targeting examples *)

  i := 33;
  j := 66;
  k := i + j;
  if k <> 99 then error ( 101 );
  i := k + i + j;
  if i <> 198 then error ( 102 );
  j := j + j + j + j;
  if j <> 4*66 then error ( 103 );
  j := 66;
  i := 33;
  k := k + j div i;
  if k <> 101 then error ( 104 );
j := 123456;
  i := - j;
  if i <> -123456 then error ( 105 );

  (* Test MOD function *)
  i := maximum (integer);
  j := 3;
  k := i mod j;
  if k <> 1 then error ( 106 );
  k := j mod i;
  if k <> 3 then error ( 107 );
  j := maximum (integer) - 3;
  k := i mod j;
  if k <> 3 then error ( 108 );

  
  i := func_add ( 1 , 2 );
  if i <> 3 then error ( 110 );
  i := func_add ( j , k );
  if i <> j + k then error ( 111 );

  k := func_add ( 1+2 , 4*5+6 );
  if k <> 29 then error ( 120 );
  i := 3;
  j := 7;
  k := func_add ( i+j , i*i+j div j );
  if k <> 20 then error ( 121 );

  i_byte := 1;
  j_byte := 33;
  k_byte := i_byte * ( j_byte * i_byte );
  if k_byte <> 33 then error ( 130 );

  j := 22;
  i := 11;
  byte_rec.byte_1 := j + i;
  if byte_rec.byte_1 <> 33 then error ( 140 );
  byte_rec.byte_1 := 44;
  byte_rec.byte_2 := 55;
  byte_rec.byte_3 := byte_rec.byte_1 + 22 + byte_rec.byte_2;
  if byte_rec.byte_3 <> 121 then error ( 141 );
  byte_rec.byte_3 := - byte_rec.byte_1;
  if byte_rec.byte_3 <> -44 then error ( 142 );

  if 3 <> func_add ( 1 , 2 ) then error ( 150 );
  x := 3.45;
  y := 1.55;
  z := x + y;
  r_eql (z, 5.0, 151);

  i := trunc ( x );
  if i <> 3 then error ( 160 );
  i := round ( y );
  if i <> 2 then error ( 161 );
  z := round ( x+.00999 , -1 );
  r_eql (z, 3.5, 162);

  word_rec.word_1 := 32;
  byte_rec.byte_1 := 32;
  i := 32;
  x := 32;
  xx := 32;

  (* assign all to a byte *)
  byte_rec.byte_3 := byte_rec.byte_1;
  if byte_rec.byte_3 <> 32 then error  ( 170 );
  byte_rec.byte_3 := word_rec.word_1;
  if byte_rec.byte_3 <> 32 then error ( 171 );
  byte_rec.byte_3 := i;
  if byte_rec.byte_3 <> 32 then error ( 172 );
  byte_rec.byte_3 := - byte_rec.byte_1;
  if byte_rec.byte_3 <> -32 then error ( 173 );
  byte_rec.byte_3 := - word_rec.word_1;
  if byte_rec.byte_3 <> -32 then error ( 174 );
  byte_rec.byte_3 := - i;
  if byte_rec.byte_3 <> -32 then error ( 175 );
  byte_rec.byte_3 := trunc ( x );
  if byte_rec.byte_3 <> 32 then error ( 176 );
  byte_rec.byte_3 := trunc ( xx );
  if byte_rec.byte_3 <> 32 then error ( 177 );
  byte_rec.byte_3 := - trunc ( x );
  if byte_rec.byte_3 <> -32 then error ( 178 );
  byte_rec.byte_3 := - trunc ( xx );
  if byte_rec.byte_3 <> -32 then error ( 179 );

  (* assign to a word *)
  word_rec.word_3 := byte_rec.byte_1;
  if word_rec.word_3 <> 32 then error ( 180 );
  word_rec.word_3 := word_rec.word_1;
  if word_rec.word_3 <> 32 then error ( 181 );
  word_rec.word_3 := i;
  if word_rec.word_3 <> 32 then error ( 182 );
  word_rec.word_3 := - byte_rec.byte_1;
  if word_rec.word_3 <> -32 then error ( 183 );
  word_rec.word_3 := - word_rec.word_1;
  if word_rec.word_3 <> -32 then error ( 184 );
  word_rec.word_3 := - i;
  if word_rec.word_3 <> -32 then error ( 185 );
  word_rec.word_3 := trunc ( x );
  if word_rec.word_3 <> 32 then error ( 186 );
  word_rec.word_3 := trunc ( xx );
  if word_rec.word_3 <> 32 then error ( 187 );
  word_rec.word_3 := - trunc ( x );
  if word_rec.word_3 <> -32 then error ( 188 );
  word_rec.word_3 := - trunc ( xx );
  if word_rec.word_3 <> -32 then error ( 189 );

  (* All cases of integer *)
  j := byte_rec.byte_1;
  if j <> 32 then error ( 190 );
  j := word_rec.word_1;
  if j <> 32 then error ( 191 );
  j := i;
  if j <> 32 then error ( 192 );
  j := - byte_rec.byte_1;
  if j <> -32 then error ( 193 );
  j := - word_rec.word_1;
  if j <> -32 then error ( 194 );
  j := - i;
  if j <> -32 then error ( 195 );
  j := trunc ( x );
  if j <> 32 then error ( 196 );
  j := trunc ( xx );
  if j <> 32 then error ( 197 );
  j := - trunc ( x );
  if j <> -32 then error ( 198 );
  j := - trunc ( xx );
  if j <> -32 then error ( 199 );

  (* Try single precision reals *)
  y := byte_rec.byte_1;
  if y <> 32.0 then error ( 200 );
  y := word_rec.word_1;
  if y <> 32.0 then error ( 201 );
  y := i;
  if y <> 32.0 then error ( 202 );
  y := x;
  if y <> 32.0 then error ( 203 );
  y := xx;
  if y <> 32.0 then error ( 204 );
  y := - byte_rec.byte_1;
  if y <> -32.0 then error ( 205 );
  y := - word_rec.word_1;
  if y <> -32.0 then error ( 206 );
  y := - i;
  if y <> -32.0 then error ( 307 );
  y := -x;
  if y <> -32.0 then error ( 308 );
  y := - xx;
  if y <> -32.0 then error ( 309 );

  (* try double precision reals *)
  yy := byte_rec.byte_1;
  if yy <> 32.0 then error ( 210 );
  yy := word_rec.word_1;
  if yy <> 32.0 then error ( 211 );
  yy := i;
  if yy <> 32.0 then error ( 212 );
  yy := x;
  if yy <> 32.0 then error ( 213 );
  yy := xx;
  if yy <> 32.0 then error ( 214 );
  yy := - byte_rec.byte_1;
  if yy <> -32.0  then error ( 215 );
  yy := - word_rec.word_1;
  if yy <> -32.0 then error ( 216 );
  yy := - i;
  if yy <> -32.0 then error ( 217 );
  yy := - x;
  if yy <> -32.0 then error ( 218 );
  yy := - xx;
  if yy <> -32.0 then error ( 219 );


  ch := 'a';
  i := ord ( ch );
  if i <> 97 then error ( 300 );
  
  i := 122;
  ch := chr ( i );
  if ch <> 'z' then error ( 301 );

  x := 0.0;
  y := tan ( x );
  if y <> 0.0 then error ( 302 );


  x := 16.000;
  y := sqrt ( x ) ;
  r_eql (y, 4.0, 303);

  i_byte := 1;
  y := sqrt ( i_byte );
  r_eql (y, 1.0, 304);

  i_word := 1;
  y := sqrt ( i_word );
  r_eql (y, 1.0, 305);

  i := 4;
  y := sqrt ( i );
  r_eql (y, 2.0, 306);

  i_byte := 3;
  y := targ_test ( i_byte );
  if Y <> 3.0 then error ( 307 );

  i_word := 5;
  y := targ_test ( i_word );
  if y <> 5.0 then error ( 308 );
  
  i := 5;
  y := targ_test ( i );
  if y <> 5.0 then error ( 309 );

  y := targ_test ( 1 );
  if y <> 1.0 then error ( 310 );


  y := random ( 1 );
  y := random ( 0 );

  i_byte := 1;
  y := random ( i_byte );

  i_word := 1;
  y := random ( i_word );

  bool1 := false;
  bool2 := true;
  bool3 := true;
  in_func4 := false;
  bool := bool_func1 (bool1 or (bool2 andif bool3),
		      bool2 andif (bool1 orif bool_func4 (bool1 orif bool3)) )
	    orif
	  bool_func2 (true, false andif bool_func3);
  if (not bool) or (not in_func4) then
    error (406);

  writeln ( ttyoutput , 'End EXE008' );
end.
  