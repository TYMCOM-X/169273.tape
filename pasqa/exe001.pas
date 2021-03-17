(* EXE001 - string assignments, substrings, lengths, concatenations
   and case conversions.  *)

program exe001;

public var
  s1: packed array [1..4] of char := 's123';
  s2: string[12] := 'string 12';
  t: packed array [1..16] of char;
  vflx: ^string[*];
  fflx: ^packed array [1..*] of char;
  aflx: packed array [ 1..400 ] of char;
  bigstr : string [ 400 ];
  i, j, k: integer;
  vs1: string[15];
  vs2: string [ 10 ];
  bool: boolean;
  ch1, ch2, ch3: char;

procedure error ( error_number: 0..1000 );
  begin
    writeln ( ttyoutput, 'Error ', error_number );
    break ( ttyoutput );
  end;

function func_ret_char ( ch: char ): char;
  begin
    func_ret_char := ch;
  end;


procedure many_params ( a:string[10]; b:char; c:string[400]; d:integer; e:string[10]; f:boolean );
begin
  if a <> 'string10..' then error ( 300 );
  if b <> 'c' then error ( 301 );
  if c <> 'A.VERY.BIG.STRING.ACTUALLY.DECLARED.400' then error ( 302 );
  if d <> 123 then error ( 303 );
  if e <> 'STRING.10.' then error ( 304 );
  if f then error ( 305 );
end;

procedure substr_as_param (a : packed array [1..10] of char);
begin
  if a <> 'string10..' then error( 350 );
end;

function case_passed ( A : String [ 400 ] ): boolean;
  Begin
    case_passed :=
	( substr ( A , 1 , 26 ) = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' );
    If case_passed andif ( Length ( A ) > 128 )
      then case_passed := Substr ( a , 128 , 10 ) = 'ABCDEFGHIJ';
    if case_passed andif ( Length ( A ) > 300 )
      then case_passed := Substr ( a , 300 , 10 ) = '1234567890'
  end;
  
  
procedure gory (s1, s2, s3, s4, s5: string[*]; j: integer);
  label 1;
  function f: string;
    begin
      f := 'ty';
      if s1 = s3 then begin
	error (400);
	goto 1
	(* shouldn't happen, but existence ofthe goto complicates pass4 
	   handling of the concatenations *)
      end
    end;
  
  begin
    if (s1 || s2 <> substr (s3 || s4, 1, j) || f) then
      error (401);
   1:
    if (s1 || s3 = substr (s2 || s4, 1, j) || f) then
      error (402)
  end;



begin
  rewrite ( ttyoutput );
  writeln ( ttyoutput, 'Begin EXE001' );
  break ( ttyoutput );
  i := 3;
  j := 6;
  
  new ( vflx, 12 );
  new ( fflx, 7 );
  
  vflx^ := 'very flexible';			(* 'very flexibl' *)
  if vflx^ <> 'very flexibl' then error ( 1 );
  i := length ( s2 );				(* 9 *)
  if i <> 9 then error ( 2 );
  j := length ( s1 );				(* 4 *)
  if j <> 4 then error ( 3 );
  k := length ( vflx^ );			(* 12 *)
  if k <> 12 then error ( 4 );
  i := length ( fflx^ );			(* 7 *)
  if i <> 7 then error ( 5 );

  fflx^ := 'ooof';				(* 'ooof   ' *)
  if fflx^ <> 'ooof   ' then error ( 6 );
  t := uppercase ( s1 )  ||  substr ( s2, 3, 6);	(* 'S123ring 1      ' *)
  if t <> 'S123ring 1      ' then error ( 7 );
  s1 := fflx^;					(* 'ooof' *)
  if s1 <> 'ooof' then error ( 8 );
  t := vflx^ || lowercase( ' ' ) || s1;			(* 'very flexibl ooo' *)
  if t <> 'very flexibl ooo' then error ( 9 );
  fflx^ := substr( vflx^,1, i - 2 ) || s1;	(* 'very oo' *)
  if fflx^ <> 'very oo' then error ( 10 );
  substr(vflx^, 6, length(vflx^) - 6) := uppercase ( s1 );	(* 'very OOOF  l' *)
  if vflx^ <> 'very OOOF  l' then error ( 11 );

  ch1 := s2[j];					(* 'i' *)
  if ch1 <> 'i' then error ( 12 );
  ch2 := vflx^[i];				(* 'O' *)
  if ch2 <> 'O' then error ( 13 );
  ch3 := uppercase( 'a' );			(* 'A' *)
  if ch3 <> 'A' then error ( 14 );

  t := fflx^ || '_' || vflx^ || s2;		(* 'very oo_very OOO' *)
  if t <> 'very oo_very OOO' then error ( 15 );
  fflx^ := vflx^;				(* 'very OO' *)
  if fflx^ <> 'very OO' then error ( 16 );
  fflx^ := t || lowercase( substr(vflx^, i, j + k - 12) );	(* 'very oo' *)
  if fflx^ <> 'very oo' then error ( 17 );

  vs1 := 'ooof';				(* 'ooof' *)
  if vs1 <> 'ooof' then error ( 18 );
  s2 := vs1;					(* 'ooof' *)
  if s2 <> 'ooof' then error ( 19 );
  fflx^ := s2 || vs1;				(* 'ooofooo' *)
  if fflx^ <> 'ooofooo' then error ( 20 );
  ch1 := substr(t,9,4)[2];			(* 'e' *)
  if ch1 <> 'e' then error ( 21 );
  t := '';					(* '                ' *)
  if t <> '                ' then error ( 22 );
  vs1 := '';					(* '' *);
  if vs1 <> '' then error ( 23 );
  t := 'a';					(* 'a               ' *)
  if t <> 'a' then error ( 24 );
  vs1 := 'a';					(* 'a' *)
  if vs1 <> 'a' then error ( 25 );
  vflx^ := '12345678';				(* '12345678' *)
  if vflx^ <> '12345678' then error ( 26 );
  vs1 := substr ( vflx^, 3, 1 );			(* '3' *)
  if vs1 <> '3' then error ( 27 );
  ch1 := uppercase( t )[ 1 ];				(* 'A' *)
  if ch1 <> 'A' then error ( 28 );
  vs1 := substr( vflx^, 4 );			(* '45678' *)
  if vs1 <> '45678' then error ( 29 );
  vs1 := 'string_longer_than upb_lhs';		(* 'string_longer_t' *)
  if vs1 <> 'string_longer_t' then error ( 30 );
  t := lowercase ( 'ABC' || '&' || uppercase ( s2 ) );	(* 'abc&ooof        ' *)
  if t <> 'abc&ooof' then error ( 31 );
  vs1 := uppercase ( substr ( lowercase ( t ), j ) );	(* '&OOOF           ' *)
  if vs1 <> '&OOOF' then error ( 32 );
  vs1 := substr ( t, 1, 3 ) || '.' || substr ( vflx^, 1, 3 );	(* 'abc.123' *)
  if vs1 <> 'abc.123' then error ( 33 );
  s1 := 'more than 4 chars';			(* 'more' *)
  if s1 <> 'more' then error ( 34 );
  ch1 := uppercase( vs1 )[ 3 ];			(* 'C' *)
  if ch1 <> 'C' then error ( 35 );
  s1 := substr ( t, j, 3 );			(* '&oo ' *)
  if s1 <> '&oo ' then error ( 36 );
  s2 := '123456789012';				(* '123456789012' *)
  if s2 <> '123456789012' then error ( 37 );
  substr( t, 4 ) :=  'cba';			(* 'abccba         ' *)
  if t <> 'abccba' then error ( 38 );
  substr( s2, i, 5 ) := 'abcdefg';		(* '123456abcde2' *)
  if s2 <> '123456abcde2' then error ( 39 );
  substr( fflx^, 3 ) := 'foof';			(* 'oofoof ' *)
  if fflx^ <> 'oofoof ' then error ( 40 );
  substr( fflx^, j, 3 ) := '';			(* 'oof    ' *)
  if fflx^ <> 'oof  ' then error ( 41 );
  substr( vflx^, 5, 4 ) := '43210';		(* '12344321' *)
  if vflx^ <> '12344321' then error ( 42 );
  substr( vflx^,i ) := 'abcdefgh';		(* '123443ab' *)
  if vflx^ <> '123443ab' then error ( 43 );
  t := substr ( vflx^ || ' ', 5 );	(* '43ab ' *)
  if t <> '43ab' then error ( 44 );
  substr( vs1, 4 ) := vs1;		(* 'abcabc.' *)
  if vs1 <> 'abcabc.' then error ( 45 );
  vs1 := substr ( vs1, j );		(* 'abc.' *)
  if vs1 <> 'abc.' then error ( 46 );
  substr( vs1, 3 ) := substr( vs1, j );	(* 'ab.' *)
  if vs1 <> 'ab.' then error ( 47 );
  vs1 := chr ( k + ord( 'A' ) - k );		(* 'A' *)
  if vs1 <> 'A' then error ( 48 );
  vs1 := vs1 || func_ret_char ( t[ 1 ] );	(* 'A4' *)
  if vs1 <> 'A4' then error ( 49 );

  new ( vflx , 26 );
  new ( fflx , 26 );
  vflx^ := 'abcdefghijklmnopqrstuvwxyz';
  fflx^ := vflx^;
  if (fflx^ <> 'abcdefghijklmnopqrstuvwxyz')
  or (vflx^ <> 'abcdefghijklmnopqrstuvwxyz')
  or ( vflx^ <> fflx^ )
    then error ( 50 );
  if not case_passed ( uppercase ( fflx^ ) )
    then error ( 51 );
  if not case_passed ( Uppercase ( vflx^ ) )
    then error ( 52 );
  if not case_passed (uppercase(substr(fflx^,1,10)||substr(vflx^,11)) )
    then error ( 53 );
  If not case_passed ( Uppercase(substr(fflx^,1,10))||Uppercase(substr(vflx^,11)))
    then error ( 54 );
  new (fflx , 154 );
  new (vflx , 154 );
  substr(fflx^,1,26) := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  substr(fflx^,128,26) := substr(fflx^,1,26);
  if ( substr(fflx^,1,26) <> 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' )
  or ( substr(fflx^,128,26) <> substr(fflx^,1,26) )
    then error ( 55 );
  vflx^ := fflx^;
  if ( substr(vflx^,1,26) <> 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' )
  or ( substr(vflx^,128,26) <> substr(vflx^,1,26) )
    then error ( 56 );
  if not case_passed ( fflx^ )
    then error ( 57 );
  if not case_passed ( vflx^ )
    then error ( 58 );
  if not case_passed ( uppercase ( fflx^ ) )
    then error ( 59 );
  if not case_passed ( uppercase ( lowercase ( vflx^ ) ) )
    then error ( 60 );

  new ( vflx , 400 );
  vflx^ := '';
  for i := 1 to 400 do
    vflx^ := vflx^ || 'a';
  substr ( vflx^ , 365,5 ) := '12345';
  substr ( vflx^ , 300, 10 ) := '1234567890';
  if uppercase(substr(vflx^,360,10)) <> 'AAAAA12345'
    then error ( 61 );

  if not case_passed ( 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' )
    then error ( 62 );

  substr(aflx,1,26) := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  substr(aflx,128,26) := substr(aflx,1,26);
  substr(aflx,300,10) := '1234567890';
  if not case_passed ( aflx )
    then error ( 63 );

  bigstr := substr(aflx,1,26);
  bigstr := bigstr || bigstr || bigstr || bigstr || bigstr || bigstr;
  bigstr := substr ( bigstr , 1 , 127 );
  bigstr := bigstr || substr ( bigstr , 1 , 26 ) || bigstr || bigstr;
  bigstr := substr ( bigstr , 1 , 299 ) || '1234567890';
  if not case_passed ( bigstr )
    then error ( 64 );

  vs1 := 'string10..';
  ch1 := 'c';
  bigstr := 'A.VERY.BIG.STRING.ACTUALLY.DECLARED.400';
  i := 123;
  vs2 := 'STRING.10.';
  bool := false;
  many_params(vs1, ch1, bigstr, i, vs2, bool );
  vs1 := 'AAAstring10..AA';
  substr_as_param ( substr( vs1, 4, 10) );

  gory ('NAS', 'TYnasty', 'NASTY', 'naszz', 'xxx', 8);
  
  i := 21;
  j := 19;
  t := chr( i + j );
  if t <> '(' then error ( 65 );
  writeln ( ttyoutput, 'End EXE001' );
end.
 