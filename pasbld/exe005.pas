(* EXE005 - Getstring and putstring.  *)

program EXE005;

public var
  flex_fstr: ^packed array [1..*] of char;
  flex_vstr: ^string[ * ];
  fstr30: packed array [1..30] of char;
  vst30a, vst30b: string[ 30 ];
  fst300 : packed array [ 1..300 ] of char;
  vst300 : string [ 300 ];
  i,j,k: integer;
  b: boolean;
  bs: packed array [1..5] of char;
  ch: char;
  x,y,z: real;

const str150 : string [ 150 ] :=
  '0aaaaaaaa1bbbbbbbbb2ccccccccc3ddddddddd4eeeeeeeee5fffffffff6ggggggggg7hhhhhhhhh8iiiiiiiii9jjjjjjjjj100kkkkkkk1lllllllll2mmmmmmmmm3nnnnnnnnn4ooooooooo5';

public procedure string_param ( var varfl_varprm: string[ * ]; i: integer );

begin
  putstring ( varfl_varprm, i );
end;

public procedure error ( error_number: 1..999 );
  begin
    writeln ( ttyoutput, 'Error ', error_number );
  end;

begin
  rewrite ( ttyoutput );
  writeln ( ttyoutput, 'Begin EXE005' );
  
  new ( flex_fstr, 40 );
  new ( flex_vstr, 50 );

  flex_fstr^ := '12345TRUE1.2345a1e5OOOF777';
  flex_vstr^ := 'BEGIN54321,12345,false';
  fstr30 := 'abcdef17.012345string';
  vst30a := '987654321TRUE1.0e5OOOF123E2';
  vst30b := vst30a;

  getstring ( flex_fstr^, i,bs[1:4], x, ch, y, flex_fstr^[23:4], j:3:o );
  if i <> 12345 then error ( 111 );
  if bs[1:4] <> 'TRUE' then error ( 112 );
  if x <> 1.2345 then error ( 113 );
  if ch <> 'a' then error ( 114 );
  if y <> 1e5 then error ( 115 );
  if flex_fstr^[23:4] <> 'OOOF' then error ( 116 );
  if j <> #o777 then error ( 117 );

  getstring ( flex_vstr^, flex_vstr^[6:5], i, j:4, ch, k, bs );
  if flex_vstr^[6:5] <> 'BEGIN' then error ( 121 );
  if i <> 54321 then error ( 122 );
  if j <> 1234 then error ( 123 );
  if ch <> '5' then error ( 124 );
  if k <> 0 then error ( 125 );
  if bs <> 'false' then error ( 126 );

  getstring ( uppercase ( fstr30 ), fstr30[4:6], x:4, y, fstr30[1:3] );
  if fstr30[4:6] <> 'ABCDEF' then error ( 131 );
  if x <> 17.0 then error ( 132 );
  if y <> 12345.0 then error ( 133 );
  if fstr30[ 1:3 ] <> 'STR' then error ( 134 );

  getstring ( vst30a[5:22], i, bs[2:4], x, vst30a[22:4], y:3, ch );
  if i <> 54321 then error ( 141 );
  if bs[2:4] <> 'TRUE' then error ( 142 );
  if x <> 1e5 then error ( 143 );
  if vst30a[22:4] <> 'OOOF' then error ( 144 );
  if y <> 123.0 then error ( 145 );
  if ch <> 'E' then error ( 146 );

  getstring ( vst30b, i, vst30b:13, j );
  if i <> 987654321 then error ( 151 );
  if vst30b <> 'TRUE1.0e5OOOF' then error ( 152 );
  if length ( vst30b ) <> 13 then error ( 153 );
  if j <> 123 then error ( 153 );

  b := true;
  ch := '?';
  i := 123;
  j := 54321;
  k := #o777;
  x := 1e5;
  y := 1e-5;
  z := 123.0;
  flex_fstr^ := 'FIXED STRING';
  flex_vstr^ := 'FLEX STRING';
  fstr30 := 'abcdefghijklm';
  vst30a := '0123456789876543210';
  vst30b := 'hi thereits ok';
  
  putstring ( flex_fstr^[7:17], i:5:O, 1 = i, -z:6:1, flex_fstr^ );
  if flex_fstr^ <> 'FIXED 00173FALSE-123.0'
    then error ( 211 );

  putstring ( flex_vstr^, b, flex_vstr^, j - i, 'ooof' );
  if flex_vstr^ <> 'TRUEFLEX STRING 54198ooof'
    then error ( 221 );

  putstring ( fstr30, 1023:6:h, uppercase(fstr30[1:6]), fstr30[1:6], '?':10, 'too long' );
  if fstr30 <> '0003FFABCDEFabcdef         ?'
    then error ( 231 );

  putstring ( vst30a[5:13], vst30a[1:5], ch, vst30a[5:3], false, k + 1  );
  if vst30a <> '012301234?456    10'
    then error ( 241 );
  
  putstring (vst30b, uppercase (vst30b[9:6]):7, ' ', vst30b[4:5], ' now!');
  if (vst30b <> ' ITS OK there now!') or (length (vst30b) <> 18) 
    then error ( 251 );

  (* now lets try some things with big strings *)

  dispose ( flex_fstr );
  dispose ( flex_vstr );
  new ( flex_fstr , 300 );
  new ( flex_vstr , 300 );

  putstring ( flex_vstr^ , lowercase ( str150 ) , uppercase ( str150 ) );
  if ( substr ( flex_vstr^,1,150 ) <> lowercase ( str150 ) )
  or ( substr ( flex_vstr^,151,150) <> uppercase ( str150 ) )
    then error ( 310 );

  flex_fstr^ := flex_vstr^;
  fst300 := flex_fstr^;
  vst300 := flex_fstr^;

  putstring ( vst300[200:20] , uppercase(substr(flex_vstr^,1,10)), '%%%$$#!':10 );
  if vst300[200:20] <> '0AAAAAAAA1   %%%$$#!'
    then error ( 320 );

  putstring ( fst300[100:100] , uppercase(flex_vstr^[100:99]):100 );
  if fst300[100] <> ' '
    then error ( 330 );

  getstring ( uppercase(flex_fstr^) , j,flex_vstr^[1:8] , x ,fst300[1:9] , k ,
	bs , ch , vst300[1:3] );
  if j <> 0 then error ( 351 );
  if flex_vstr^[1:8] <> 'AAAAAAAA' then error ( 352 );
  if x <> 1.0 then error ( 353 );
  if fst300[1:9] <> 'BBBBBBBBB' then error ( 354 );
  if k <> 2 then error ( 355 );
  if bs <> 'CCCCC' then error ( 356 );
  if ch <> 'C' then error ( 357 );
  if vst300[1:3] <> 'CCC' then error ( 358 );

  vst300 := '654321123456789';
  i := 7;
  putstring ( substr(vst300,i,6), 'middle' );
  if (length(vst300) <> 15) or (vst300 <> '654321middle789') then error ( 360 );

  vst300 := '654321123456789';
  i := 7;
  putstring ( substr(vst300, i), 'middle' );
  if ( length(vst300) <> 15 ) or
     ( vst300 <> '654321middle   ' ) then error ( 370 );

  string_param ( vst300, 380 );
  if ( length(vst300) <> 4 ) or ( vst300 <> ' 380' ) then error ( 380 );

  writeln ( ttyoutput, 'End EXE005' );
end  (* program EXE005 *) .
   