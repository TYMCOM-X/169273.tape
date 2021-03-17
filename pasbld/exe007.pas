(* EXE007 - aggregate constructors. *)
  
program EXE007;
  
type
  rec1 = packed record
    f1: 0..15;
    f2: 0..31;
    f3: 0..7;
    f4: 0..4095;
    f5: integer;
    f6: char;
    f7: boolean;
    f8: char;
    f9: boolean;
    f10: 0..32767
  end;
  
  arr1 = packed array [5..15] of 0..8191;
  arr2 = packed array [2..32] of char;
  
  rec2 = packed record
    case tag1: boolean of 
      true: (f1: arr2);
      false: (f2: packed array [1..31] of char)
  end;
  
  rec3 = record
    case boolean of
      true: (f1: arr2);
      false: (f2: packed array [1..31] of char)
  end;
  
  flex_arr1 = packed array [2..*] of boolean;
  
  null_rec = record end;
  flex_rec1 = packed record
    f1: 0..15;
    f2: null_rec;
    f3: packed array [4..*] of boolean
  end;
  
  rec4 = record
    f1: real;
    f2: real
  end;
  
  rec5 = record
    f1: real;
    f2: real;
    f3: real
  end;
  
  rec45 = record
    f1: rec4;
    f2: rec5
  end;
  
  rec6 = packed record
    f1: real;
    f2: -6..+6;
    f3: real;
    f4: string[14]
  end;
  
  arr4 = packed array [3..*] of char;
  
  arr5 = array [1..4] of string[12];
  
  rec8 = packed record
    f1: 0..7;
    f2: -1..+5;
    f3: arr5;
    f4: 0..31
  end;
  
  rec7 = packed record
    f1: -5..5;
    f2: 0..4095;
    f3: char;
    f4: boolean;
    f5: boolean
  end;
  
  tag_value_type = (tag_value1, tag_value2, tag_value3);
  
  rec9 = record
    f1: rec7;
    case tag1: tag_value_type of
      tag_value1: (f2: integer);
      tag_value2: (f3: integer);
      tag_value3: (case tag2: tag_value_type of
		     tag_value1: (f4: integer);
		     tag_value2: (f5: integer);
		     tag_value3: (f6: rec8))
  end;

  rec10 = record
    f1: integer;
    f2: string[ * ];
  end;
  
  rec11 = packed record
    f1: null_rec;
    f2: array [1..10] of null_rec;
    f3: array [1..0] of integer;
    f4: set of 5..4;
    f5: 0..7
  end;

var
  rec1_var: rec1;
  
  fld1, fld2, fld3: 0..31;
  fld4, fld5, fld10: integer;
  fld6, fld8: char;
  fld7, fld9: boolean;
  
  arr1_var: arr1;
  arr2_var: arr2;
  
  ptr_to_flex_arr1: ^flex_arr1;
  ptr_to_flex_rec1, another_ptr: ^flex_rec1;
  upb: integer;
  
  rec2_var: rec2;
  rec3_var: rec3;
  ele1, ele2, ele3, ele4: 0..8191;
  ele5, ele6, ele7, ele8, ele9, ele10, ele11: integer;
  c1, c2, c3, c4, c5, c6, c7, c8, c9, c10: char;
  
  rec4_var: rec4;
  x, y: real;
  fld4_2: ^integer;
  fld4_4: string[12];
  dummy_int: integer;
  ele4_1, ele4_3: char;
  ele4_2: ^char;
  fld9_1, fld9_2: integer;
  rec6_var: rec6;
  rec7_var: rec7 := (-4, 4095, 'A', true, false);
  ptr_to_arr4_var: ^arr4;
  rec9_var: rec9;
  ptr_to_rec10_var: ^rec10;
  rec11_var: rec11;
  null_int: 5..4;
  
procedure error (error_number: 0..999);
  begin
    writeln (ttyoutput, 'Error: ', error_number)
  end;
  
procedure proc1 (var rec_var: rec1; fld1,fld2,fld3,fld4,fld5: integer;
		     fld6: char; fld7: boolean; fld8: char (* fld9 & 10 from static *));
  begin
    rec_var := (fld1, fld2, fld3, fld4, fld5, fld6, fld7, fld8, fld9, fld10)
  end;
  
procedure proc2 ( flex_arr: array [ 5..* ] of 0..127;
$IF GENERICS gen_arr: packed array [ * ] of 0..15
$IFNOT GENERICS gen_arr : packed array [ 1.. * ] of 0..15
);

  begin
    if (upperbound (flex_arr) <> 11) or (flex_arr[5] <> ord('H')) or
       (flex_arr[6] <> ord ('E')) or (flex_arr[7] <> ord ('L')) or (flex_arr[8] <> ord ('L')) or
       (flex_arr[9] <> ord ('O')) then
      error (14);
  if (dimension (gen_arr) <> 12) or (gen_arr[1] <> 12) or (gen_arr[12] <> 1) then
    error (15)
  end;
  
function func2 (rec45_arg: rec45): real;
  begin
    with rec45_arg.f1 do
      func2 := f1 + f2;
    with rec45_arg.f2 do
      func2 := func2 + f1 + f2 + f3;
    if round (func2, -3) <> 11.8 then
      error (20)
  end;
  
function func3 (dummy: integer; rec6_arg: rec6; arr4_arg: arr4): arr5;
  begin
    with rec6_arg do
      if (round (f1, -3) <> 1.1) or (f2 <> -6) or (round (f3, -3) <> 11.8) or (f4 <> 'Hello there!') then
        error (21);
    if (upperbound (arr4_arg) <> 5) orif (arr4_arg[3] || arr4_arg[4] || arr4_arg[5] <> '-*-') then
      error (22);
    if dummy <> 20 then 
      error (23);
    func3[1] := rec6_arg.f4; (* 'Hello there!' *)
    func3[2] := arr4_arg[3]; (* '-' *)
    func3[3] := arr4_arg[4]; (* '*' *)
    func3[4] := arr4_arg[3] || arr4_arg[4] || arr4_arg[5] (* '-*-' *)
  end;
  
begin
  rewrite (ttyoutput);
  writeln (ttyoutput, 'Begin EXE007');
  
  (* RECORDS -
	lets start off very easy *)
  
  rec1_var := (13,29,5,4093,1234565,'f',false,'g',false,32765);
  with rec1_var do
    if (f1 <> 13) or (f2 <> 29) or (f3 <> 5) or (f4 <> 4093) or (f5 <> 1234565) or
      (f6 <> 'f') or f7 or (f8 <> 'g') or f9 or (f10 <> 32765) then
      error (1);
  
  fld1 := 13; fld2 := 29; fld3 := 5; fld4 := 4093;
  fld5 := 1234565; fld6 := 'f'; fld7 := false; fld8 := 'g'; fld9 := false;
  fld10 := 32765;
  rec1_var := (fld1,fld2,fld3,fld4,fld5,fld6,fld7,fld8,fld9,fld10);
  with rec1_var do
    if (f1 <> 13) or (f2 <> 29) or (f3 <> 5) or (f4 <> 4093) or (f5 <> 1234565) or
      (f6 <> 'f') or f7 or (f8 <> 'g') or f9 or (f10 <> 32765) then
      error (2);
  
  (* some simple addressing variations *)
  
  proc1 (rec1_var, fld1,fld2,5,fld4,fld5,fld6,fld7,rec1_var.f8);
  with rec1_var do
    if (f1 <> 13) or (f2 <> 29) or (f3 <> 5) or (f4 <> 4093) or (f5 <> 1234565) or
      (f6 <> 'f') or f7 or (f8 <> 'g') or f9 or (f10 <> 32765) then
      error (3);
  
  (* verify that implementation is using a temp *)
  
  rec1_var := (0 (* zap the 13 *), fld2,fld3, rec1_var.f1 (* overlap *), fld5,
               rec1_var.f8, fld7, rec1_var.f6, fld9,fld10);
  with rec1_var do
    if (f1 <> 0) or (f2 <> 29) or (f3 <> 5) or (f4 <> 13) or (f5 <> 1234565) or
      (f6 <> 'g') or f7 or (f8 <> 'f') or f9 or (f10 <> 32765) then
      error (4);
  
  (* ARRAYS -
	      start off very easy *)
  
  arr1_var := (5, 60, 700, 8000, 900, 10, 11, 120, 1300, 1414, 15);
  if (arr1_var[5] <> 5) or (arr1_var[6] <> 60) or (arr1_var[7] <> 700) or (arr1_var[8] <> 8000) or
     (arr1_var[9] <> 900) or (arr1_var[10] <> 10) or (arr1_var[11] <> 11) or
     (arr1_var[12] <> 120) or (arr1_var[13] <> 1300) or (arr1_var[14] <> 1414) or
     (arr1_var[15] <> 15) then
      error (5);
  
  ele1 := 5; ele2 := 60; ele3 := 700; ele4 := 8000; ele5 := 900; ele6 := 10;
  ele7 := 11; ele8 := 120; ele9 := 1300; ele10 := 1414; ele11 := 15;
  arr1_var := (ele1, ele2, ele3, ele4, ele5, ele6, ele7, ele8, ele9, ele10, ele11);
  if (arr1_var[5] <> 5) or (arr1_var[6] <> 60) or (arr1_var[7] <> 700) or (arr1_var[8] <> 8000) or
     (arr1_var[9] <> 900) or (arr1_var[10] <> 10) or (arr1_var[11] <> 11) or
     (arr1_var[12] <> 120) or (arr1_var[13] <> 1300) or (arr1_var[14] <> 1414) or
     (arr1_var[15] <> 15) then
      error (6);
  
  arr2_var := ('T','h','i','s',' ','i','s',' ','p','a','c','k','e','d',' ',
               'a','r','r','a','y',' ','o','f',' ','c','h','a','r','!','!','!');
  if (arr2_var[2] <> 'T') or (arr2_var[17] <> 'a') or (arr2_var[32] <> '!') then
    error (7);
  
  c1 := 'I'; c2 := 's'; c3 := ' '; c4 := 't'; c5 := 'h'; c6 := 'i'; c7 := 's';
  c8 := '?'; c9 := '?'; c10 := '?';
  arr2_var := (c1,c2,c3,c4,c5,c6,c7,' ','p','a','c','k','e','d',' ',
               'a','r','r','a','y',' ','o','f',' ','c','h','a','r',c8,c9,c10);
  if (arr2_var[2] <> 'I') or (arr2_var[17] <> 'a') or (arr2_var[32] <> '?') then
    error (8);
  
  (* verify use of temp by introducing some overlap *)
  
  arr1_var := (ele1, 0, ele3, ele4, arr1_var[6], ele6, ele7, arr1_var[14], ele9, arr1_var[12], ele11);
  if (arr1_var[5] <> 5) or (arr1_var[6] <> 0) or (arr1_var[7] <> 700) or (arr1_var[8] <> 8000) or
     (arr1_var[9] <> 60) or (arr1_var[10] <> 10) or (arr1_var[11] <> 11) or
     (arr1_var[12] <> 1414) or (arr1_var[13] <> 1300) or (arr1_var[14] <> 120) or
     (arr1_var[15] <> 15) then
      error (9);
  
  (* now lets play with variants a little *)
  
  rec2_var := (true, arr2_var);
  if (rec2_var.f2 <> 'Is this packed array of char???') or not rec2_var.tag1 then
    error (10);
  
  rec3_var := (true, arr2_var);
  if (rec3_var.f2 <> 'Is this packed array of char???') then
    error (11);
  
  rec2_var := (true,
	       (c1,c2,c3,c4,c5,c6,c7,' ','p','a','c','k','e','d',' ',
               'a','r','r','a','y',' ','o','f',' ','c','h','a','r',c8,c9,c10));
  if (rec2_var.f2 <> 'Is this packed array of char???') or not rec2_var.tag1 then
    error (12);
  
  rec3_var := (true,
	       (c1,c2,c3,c4,c5,c6,c7,' ','p','a','c','k','e','d',' ',
               'a','r','r','a','y',' ','o','f',' ','c','h','a','r',c8,c9,c10));
  if (rec3_var.f2 <> 'Is this packed array of char???') then
    error (13);
  
  (* try some flexes *)
  
  proc2 ((ord('H'), ord('E'), ord('L'), ord('L'), ord('O'), ord(' '), ord(' ')),
	 (12,11,10,9,8,7,6,5,4,3,2,1));
  
  upb := 11;
  new (ptr_to_flex_arr1, upb (* 11 - 2 + 1 = 10 *));
  ptr_to_flex_arr1^ := (true, false, true, false, true, false, true, false, true, false);
  if (ptr_to_flex_arr1^[3] or ptr_to_flex_arr1^[5] or ptr_to_flex_arr1^[7]
	  or ptr_to_flex_arr1^[9] or ptr_to_flex_arr1^[11])
      or
     not (ptr_to_flex_arr1^[2] and ptr_to_flex_arr1^[4] and ptr_to_flex_arr1^[6]
	   and ptr_to_flex_arr1^[8] and ptr_to_flex_arr1^[10]) then
    error (16);
  
  new (ptr_to_flex_rec1, upb (* 11 - 4 + 1 = 8 *));
  new (another_ptr, upb);
  ptr_to_flex_rec1^ := (fld1, (), (true, false, true, false, true, false, true, false));
  with ptr_to_flex_rec1^ do
    if (f1 <> 13) or (f3[5] or f3[7] or f3[9] or f3[11])
        or not (f3[4] and f3[6] and f3[8] and f3[10]) then
      error (17);
  
  (* this one is the same value, but all constant - see if the code generator
     can cope with emitting an upperbound within a record *)
  another_ptr^ := (13, (), (true, false, true, false, true, false, true, false));
  with another_ptr^ do
    if (f1 <> 13) or (f3[5] or f3[7] or f3[9] or f3[11])
        or not (f3[4] and f3[6] and f3[8] and f3[10]) then
      error (18);
(*
  another_ptr^ := (ord (another_ptr^.f3[5]), (),
		   (true, (another_ptr^.f1 = 13), true, false, true, false, true, false));
  with another_ptr^ do
    if (f1 <> 0) or (f3[7] or f3[9] or f3[11]) 
        or not (f3[4] and f3[5] and f3[6] and f3[8] and f3[10]) then
      error (19);
*)
  
  (* set up a bunch of variables for the ultimate test *)
  
  x := 3.2;
  rec4_var.f1 := 3.3;
  rec4_var.f2 := 1.1;
  new (fld4_2);
  fld4_2^ := -6;
  fld4_4 := 'Hello there!';
  dummy_int := 4;
  new (ele4_2);
  ele4_1 := '-';
  ele4_2^ := '*';
  ele4_3 := '-';
  fld9_1 := 7;
  fld9_2 := -1;
  
  (* try out some components of the ultimate test separately *)
  
  y := func2 ((rec4_var, (1.2,x,3.0)));
  if round (y, -3) <> 11.8 then
    error (24);
  
  rec6_var := (rec4_var.f2, fld4_2^, func2 ((rec4_var, (1.2,x,3.0))), fld4_4);
  with rec6_var do
    if (round (f1, -3) <> 1.1) or (f2 <> -6) or (round (f3, -3) <> 11.8) or (f4 <> 'Hello there!') then
      error (25);
  
  new (ptr_to_arr4_var, 5);
  ptr_to_arr4_var^ := (ele4_1, ele4_2^, ele4_3);
  if ptr_to_arr4_var^[3] || ptr_to_arr4_var^[4] || ptr_to_arr4_var^[5] <> '-*-' then
    error (26);
  
  (* now put it all together ... *)
  
  rec9_var := (rec7_var,
	       tag_value3,
	       tag_value3,
	       (fld9_1,
		fld9_2,
		func3 (dummy_int+dummy_int**2,
		       (rec4_var.f2, fld4_2^, func2 ((rec4_var, (1.2,x,3.0))), fld4_4),
		       (ele4_1, ele4_2^, ele4_3)),
	        31));
  with rec9_var do begin
    with f1 do
      if (f1 <> -4) or (f2 <> 4095) or (f3 <> 'A') or (not f4) or f5 then
        error (27);
    if (tag1 <> tag_value3) or (tag2 <> tag_value3) then
      error (28);
    with f6 do
      if (f1 <> 7) or (f2 <> -1) or (f3[1] <> 'Hello there!') or (f3[2] <> '-')
          or (f3[3] <> '*') or (f3[4] <> '-*-') or (f4 <> 31) then
	error (29)
  end;

  (* Has this bug been fixed yet???? *)

  new(ptr_to_rec10_var, 12 );
  ptr_to_rec10_var^ := (12345, 'This string is more than twelve characters' ); 
  with ptr_to_rec10_var^ do begin  
    if (f1 <> 12345) or (upperbound(f2) <> 12) or
       (length(f2) <> 12) then error ( 30 );
  end;
  
  rec11_var := ((), ((),(),(),(),(),(),(),(),(),()), (), [], 6);
  if rec11_var.f5 <> 6 then
    error (31);
  rec11_var.f1 := ptr_to_flex_rec1^.f2;
  rec11_var.f2 := ();
  rec11_var.f3 := ();
  rec11_var.f4 := [null_int];
  if rec11_var.f5 <> 6 then
    error (32);
  
  writeln (ttyoutput, 'End EXE007')
end (* EXE007 *).
  