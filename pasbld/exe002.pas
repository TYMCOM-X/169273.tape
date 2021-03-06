(* EXE002 - string comparisons and intrinsic functions INDEX,
   SEARCH and VERIFY.  *)

program exe002;

public var
  null: string[6] := '';
  null2: string[6] := '';
  abc: packed array [1..6] of char := 'abc';
  b1: boolean;
  vs1: string[ 15 ] := 'fifteen';
  flx_vstr: ^string[ * ];
  fs6: packed array [1..6] of char := 'sixsix';
  fs10: packed array [1..10] of char := 'abc';
  null_vs: string[ 15 ] := '';
  vs6: string[ 6 ] := 'a';
  i, j: integer;
  ch1: char;
  ch2: char;
  ch_set: set of char := ['A'..'M'];
  set_array: array [1..3] of set of '0'..'Z' := ( ['A'], ['B'], ['C'] );

procedure error ( error_number: 1..255 );
  begin
    writeln ( ttyoutput, 'Error ', error_number );
  end;

begin
  rewrite ( ttyoutput );
  writeln ( ttyoutput, 'Begin EXE002' );
  break ( ttyoutput );
  ch1 := 'n';
  ch2 := 'F';
  new ( flx_vstr, 15 );
  flx_vstr^ := 'all strings are created equal';

  b1 := abc < 'abc';	(* false *)
  if b1 then error ( 1 );
  b1 := abc < 'abc   ';	(* false *)
  if b1 then error ( 2 );
  b1 := uppercase ( vs1 ) = 'FIFTEEN';	(* true *)
  if not b1 then error ( 3 );
  b1 := substr ( abc, 1, 3 ) = 'abc';	(* true *)
  if not b1 then error ( 4 );
  b1 := (uppercase(abc) || vs1) = 'ABC   fifteen';	(* true *)
  if not b1 then error ( 5 );
  b1 := flx_vstr^ = lowercase ( substr ( 'ALL STRINGS ARE NOT EQUAL' , 1,
					 length( flx_vstr^ ) ) );	(* true *)
  if not b1 then error ( 6 );
  b1 := abc = fs6;	(* false *)
  if b1 then error ( 7 );
  b1 := abc = fs10;	(* true *);
  if not b1 then error ( 8 );
  b1 := null = '';	(* true *)
  if not b1 then error ( 9 );
  b1 := null = ' ';	(* true *)
  if not b1 then error ( 10 );
  b1 := null = null_vs;	(* true *)
  if not b1 then error ( 11 );
  b1 := null = 'a';	(* false *)
  if b1 then error ( 12 );
  b1 := vs6 = 'a';	(* true *)
  if not b1 then error ( 13 );
  b1 := vs6 = abc[ 1 ];	(* true *)
  if not b1 then error ( 14 );
  b1 := uppercase ( vs6 ) = 'A';	(* true *)
  if not b1 then error ( 15 );

  i := index ( 'abcdefg', abc );	(* 0 *)
  if i <> 0 then error ( 16 );
  i := index ( substr ( fs6, 2 ), 'six', -99 );	(* 3 *)
  if i <> 3 then error ( 17 );
  i := index ( vs1 || '15', '1', -1 );	(* 8 *)
  if i <> 8 then error ( 18 );
  i := index ( 'Four score and fifteen years ago', vs1 );	(* 16 *)
  if i <> 16 then error ( 19 );
  i := index ( vs1, 'teen', 0 );	(* 4 *)
  if i <> 4 then error ( 20 );
  i := index ( fs6, 'seven', -1 );	(* -1 *)
  if i <> -1 then error ( 21 );
  i := index ( fs6, 'seven' );	(* 0 *)
  if i <> 0 then error ( 22 );
  i := index ( vs1, vs6, i );	(* 0 *)
  if i <> 0 then error ( 23 );

  i := search ( vs1, [uppercase(ch1)..chr(ord(lowercase(ch2))-1)] );		(* 5 *)
  if i  <> 5 then error ( 24 );
  i := verify ( uppercase( fs10 ), ch_set, 99 );	(* 4 *)
  if i <> 4 then error ( 25 );
  i := search ( lowercase ( flx_vstr^ ), ['z', 'i'..'n'] );	(* 2 *)
  if i <> 2 then error ( 26 );
  i := verify ( uppercase ( vs1 || ' ' || fs6 ), [ch2..ch1, 'G'..'M'], -99 );	(* 5 *)
  if i <> 5 then error ( 27 );
  i := search ( flx_vstr^, [ ch2 ], -1 );	(* -1 *)
  if i <> -1 then error ( 28 );
  i := verify ( 'oooof !!!', [','] );		(* 1 *)
  if i <> 1 then error ( 29 );
  i := search ( substr ( fs6, 3, 3 ), ['A'..ch2], -i);	(* -1 *)
  if i <> -1 then error ( 30 );

  if index ( null, null2, 99 ) <> 1 then error ( 31 );
  if index ( abc, null, 99 ) <> 1 then error ( 32 );
  if index ( null, abc, 99 ) <> 99 then error ( 33 );

  j := 2;
  i := search ( 'ABC', set_array[ j ] );	(* 2 *)
  if i <> 2 then error ( 34 );

(* generate calls to search and verify with all combinations of
   uppercase or not uppercase, O format set or L format set, and
   character source or string source *)
  i:=search ( 'abcdef',['b'..'f'] );               (* 2 *)
  if i <> 2 then error ( 35 );
  i:=search ( 'abcdef',['1','A'..'Z'] );           (* 0 *)
  if i <> 0 then error ( 36 );
  i:=search ( 'a',['a'..'e'] );                    (* 1 *)
  if i <> 1 then error ( 37 );
  i:=search ( 'a',['1','b','Z'],4 );               (* 4 *)
  if i <> 4 then error ( 38 );
  i:=search ( uppercase('1AaBb2Z'),['Z'] );        (* 7 *)
  if i <> 7 then error ( 39 );
  i:=search ( uppercase('abcdef'),['A'..'Z'],99 ); (* 1 *)
  if i <> 1 then error ( 40 );
  i:=search ( uppercase('a'),['A'..'Z'] );         (* 1 *)
  if i <> 1 then error ( 41 );
  i:=search ( uppercase('A'),['B'..'E','Z'] );     (* 0 *)
  if i <> 0 then error ( 42 );
  i:=verify ( 'abcdef',['b'..'f'] );               (* 1 *)
  if i <> 1 then error ( 43 );
  i:=verify ( 'abcdef',['1'..'9','a'..'h'] );      (* 0 *)
  if i <> 0 then error ( 44 );
  i:=verify ( 'a',['a'..'e'],99 );                 (* 99 *)
  if i <> 99 then error ( 44 );
  i:=verify ( '1',['a'..'z','A'..'Z'] );           (* 1 *)
  if i <> 1 then error ( 45 );
  i:=verify ( uppercase('1234'),['1'..'9'] );      (* 0 *)
  if i <> 0 then error ( 46 );
  i:=verify ( uppercase('bc'),['B','D'..'E'] );    (* 2 *)
  if i <> 2 then error ( 47 );
  i:=verify ( uppercase('Z'),['A'..'Z'] );         (* 0 *)
  if i <> 0 then error ( 48 );
  i:=verify ( uppercase('#'),['A'..'Z','1'..'9'] );(* 1 *)
  if i <> 1 then error ( 49 );

writeln ( ttyoutput, 'End EXE002' );
end.
   