program exe016 options noqblock;
(* Any changes to this program should be done to EXE016.PMF *)
(*          and then regenerate EXE016.PAS with PMF         *)

var sequence: integer;

type
    access_index = 0 .. 4;
    access_array = array [access_index] of integer;
    access_record = record
	i: access_index;
	a: access_array;
    end;

var a_r_0: access_record;

label 01, 02;
$PAGE call tests
procedure ct_top_uncle ( n: integer );
begin
  case n of
    1 : begin
  if sequence <> 2 then
    writeln (tty, 'Sequence error: point 2 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[2]')
$ENDIF
  ;
  sequence := 2 + 1;
end;
    2 : begin
  if sequence <> 5 then
    writeln (tty, 'Sequence error: point 5 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[5]')
$ENDIF
  ;
  sequence := 5 + 1;
end;
    3 : begin
  if sequence <> 8 then
    writeln (tty, 'Sequence error: point 8 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[8]')
$ENDIF
  ;
  sequence := 8 + 1;
end;
  end;
end;

procedure ct_level_1 ( n: integer );

procedure ct_sub_uncle ( n: integer );
begin
  case n of
    2 : begin
  if sequence <> 23 then
    writeln (tty, 'Sequence error: point 23 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[23]')
$ENDIF
  ;
  sequence := 23 + 1;
end;
    3 : begin
  if sequence <> 16 then
    writeln (tty, 'Sequence error: point 16 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[16]')
$ENDIF
  ;
  sequence := 16 + 1;
end;
  end;
end;

procedure ct_level_2 ( n: integer );

procedure ct_level_3 ( n: integer );
begin
  case n of
    2 : begin
	  begin
  if sequence <> 7 then
    writeln (tty, 'Sequence error: point 7 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[7]')
$ENDIF
  ;
  sequence := 7 + 1;
end;
	  ct_top_uncle (3);
	  begin
  if sequence <> 9 then
    writeln (tty, 'Sequence error: point 9 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[9]')
$ENDIF
  ;
  sequence := 9 + 1;
end;
	  ct_level_3 (3);
	  begin
  if sequence <> 11 then
    writeln (tty, 'Sequence error: point 11 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[11]')
$ENDIF
  ;
  sequence := 11 + 1;
end;
	  ct_level_2 (3);
	  begin
  if sequence <> 13 then
    writeln (tty, 'Sequence error: point 13 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[13]')
$ENDIF
  ;
  sequence := 13 + 1;
end;
	  ct_level_1 (3);
	  begin
  if sequence <> 15 then
    writeln (tty, 'Sequence error: point 15 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[15]')
$ENDIF
  ;
  sequence := 15 + 1;
end;
	  ct_sub_uncle (3);
	  begin
  if sequence <> 17 then
    writeln (tty, 'Sequence error: point 17 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[17]')
$ENDIF
  ;
  sequence := 17 + 1;
end;
	end;
    3 : begin
  if sequence <> 10 then
    writeln (tty, 'Sequence error: point 10 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[10]')
$ENDIF
  ;
  sequence := 10 + 1;
end;
  end;
end;

begin (* ct_level_2 *);
  case n of
    1 : begin
	  begin
  if sequence <> 4 then
    writeln (tty, 'Sequence error: point 4 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[4]')
$ENDIF
  ;
  sequence := 4 + 1;
end;
	  ct_top_uncle (2);
	  begin
  if sequence <> 6 then
    writeln (tty, 'Sequence error: point 6 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[6]')
$ENDIF
  ;
  sequence := 6 + 1;
end;
	  ct_level_3 (2);
	  begin
  if sequence <> 18 then
    writeln (tty, 'Sequence error: point 18 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[18]')
$ENDIF
  ;
  sequence := 18 + 1;
end;
	  ct_level_2 (2);
	  begin
  if sequence <> 20 then
    writeln (tty, 'Sequence error: point 20 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[20]')
$ENDIF
  ;
  sequence := 20 + 1;
end;
	  ct_level_1 (2);
	  begin
  if sequence <> 22 then
    writeln (tty, 'Sequence error: point 22 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[22]')
$ENDIF
  ;
  sequence := 22 + 1;
end;
	  ct_sub_uncle (2);
	  begin
  if sequence <> 24 then
    writeln (tty, 'Sequence error: point 24 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[24]')
$ENDIF
  ;
  sequence := 24 + 1;
end;
	end;
    2 : begin
  if sequence <> 19 then
    writeln (tty, 'Sequence error: point 19 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[19]')
$ENDIF
  ;
  sequence := 19 + 1;
end;
    3 : begin
  if sequence <> 12 then
    writeln (tty, 'Sequence error: point 12 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[12]')
$ENDIF
  ;
  sequence := 12 + 1;
end;
  end;
end;

begin (* ct_level_1 *);
  case n of
    0 : begin
	  begin
  if sequence <> 1 then
    writeln (tty, 'Sequence error: point 1 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[1]')
$ENDIF
  ;
  sequence := 1 + 1;
end;
	  ct_top_uncle (1);
	  begin
  if sequence <> 3 then
    writeln (tty, 'Sequence error: point 3 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[3]')
$ENDIF
  ;
  sequence := 3 + 1;
end;
	  ct_level_2 (1);
	  begin
  if sequence <> 25 then
    writeln (tty, 'Sequence error: point 25 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[25]')
$ENDIF
  ;
  sequence := 25 + 1;
end;
	  ct_level_1 (1);
	  begin
  if sequence <> 27 then
    writeln (tty, 'Sequence error: point 27 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[27]')
$ENDIF
  ;
  sequence := 27 + 1;
end;
	end;
    1 : begin
  if sequence <> 26 then
    writeln (tty, 'Sequence error: point 26 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[26]')
$ENDIF
  ;
  sequence := 26 + 1;
end;
    2 : begin
  if sequence <> 21 then
    writeln (tty, 'Sequence error: point 21 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[21]')
$ENDIF
  ;
  sequence := 21 + 1;
end;
    3 : begin
	  begin
  if sequence <> 14 then
    writeln (tty, 'Sequence error: point 14 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[14]')
$ENDIF
  ;
  sequence := 14 + 1;
end;
$IF trace
	  writeln (tty, 'Traceback follows - should be');
	  writeln (tty, '   CT_LEVEL_1, CT_LEVEL_3, CT_LEVEL_2, CT_LEVEL_1, EXE016');
	  trace;
	  writeln (tty, 'Assertion failure follows.');
	  writeln (tty, 'Type REENTER, CONTINUE.');
	  writeln (tty, 'Same traceback should be repeated.');
	  assert (false);
$ENDIF
	end;
  end;
end;
$PAGE variable and parameter access tests
procedure dt_level_1 (prm_a_1: access_array; prm_i_1: access_index);
var a_r_1: access_record;

  procedure dt_level_2 (flag: boolean; prm_a_2: access_array; prm_i_2: access_index);
  var a_r_2: access_record;

    procedure dt_level_3 (flag: boolean; prm_a_3: access_array; prm_i_3: access_index);
    var a_r_3: access_record;
    begin
      begin
  a_r_3 := (3, (930, 931, 932, 933, 934));
  begin
  if a_r_0.a[a_r_0.i] <> 900 then
    writeln (tty, 'Variable access error: level 3 to level 0 - value is',
		  a_r_0.a[a_r_0.i]);
end;
begin
  if a_r_1.a[a_r_1.i] <> 911 then
    writeln (tty, 'Variable access error: level 3 to level 1 - value is',
		  a_r_1.a[a_r_1.i]);
end;
begin
  if a_r_2.a[a_r_2.i] <> 922 then
    writeln (tty, 'Variable access error: level 3 to level 2 - value is',
		  a_r_2.a[a_r_2.i]);
end;
begin
  if a_r_3.a[a_r_3.i] <> 933 then
    writeln (tty, 'Variable access error: level 3 to level 3 - value is',
		  a_r_3.a[a_r_3.i]);
end;
  begin
  if prm_a_1[prm_i_1] <> 711 then
    writeln (tty, 'Parameter access error: level 3 to level 1 - value is',
		  prm_a_1[prm_i_1]);
end;
begin
  if prm_a_2[prm_i_2] <> 722 then
    writeln (tty, 'Parameter access error: level 3 to level 2 - value is',
		  prm_a_2[prm_i_2]);
end;
begin
  if prm_a_3[prm_i_3] <> 733 then
    writeln (tty, 'Parameter access error: level 3 to level 3 - value is',
		  prm_a_3[prm_i_3]);
end;
end;
      if not flag then
	dt_level_2 (true, (720, 721, 722, 723, 724), 2);
    end;

  begin
    begin
  a_r_2 := (2, (920, 921, 922, 923, 924));
  begin
  if a_r_0.a[a_r_0.i] <> 900 then
    writeln (tty, 'Variable access error: level 2 to level 0 - value is',
		  a_r_0.a[a_r_0.i]);
end;
begin
  if a_r_1.a[a_r_1.i] <> 911 then
    writeln (tty, 'Variable access error: level 2 to level 1 - value is',
		  a_r_1.a[a_r_1.i]);
end;
begin
  if a_r_2.a[a_r_2.i] <> 922 then
    writeln (tty, 'Variable access error: level 2 to level 2 - value is',
		  a_r_2.a[a_r_2.i]);
end;
  begin
  if prm_a_1[prm_i_1] <> 711 then
    writeln (tty, 'Parameter access error: level 2 to level 1 - value is',
		  prm_a_1[prm_i_1]);
end;
begin
  if prm_a_2[prm_i_2] <> 722 then
    writeln (tty, 'Parameter access error: level 2 to level 2 - value is',
		  prm_a_2[prm_i_2]);
end;
end;
    dt_level_3 (flag, (730, 731, 732, 733, 734), 3);
  end;

begin
  begin
  a_r_1 := (1, (910, 911, 912, 913, 914));
  begin
  if a_r_0.a[a_r_0.i] <> 900 then
    writeln (tty, 'Variable access error: level 1 to level 0 - value is',
		  a_r_0.a[a_r_0.i]);
end;
begin
  if a_r_1.a[a_r_1.i] <> 911 then
    writeln (tty, 'Variable access error: level 1 to level 1 - value is',
		  a_r_1.a[a_r_1.i]);
end;
  begin
  if prm_a_1[prm_i_1] <> 711 then
    writeln (tty, 'Parameter access error: level 1 to level 1 - value is',
		  prm_a_1[prm_i_1]);
end;
end;
  dt_level_2 (false, (720, 721, 722, 723, 724), 2);
end;
$PAGE nonlocal goto tests
procedure gt_level_1 ( n: integer );

  label 11, 12;

  procedure gt_level_2 ( n: integer );

    label 21;

    procedure gt_level_3 ( n: integer );
    begin
      case n of
	1 : return;
	2 : goto 21;
	3 : goto 12;
	4 : goto 02;
      end;
    end;

  begin (* gt_level_2 *)
    case n of
      1 : begin
	    begin
  if sequence <> 30 then
    writeln (tty, 'Sequence error: point 30 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[30]')
$ENDIF
  ;
  sequence := 30 + 1;
end;
	    gt_level_3 (1);
	    begin
  if sequence <> 31 then
    writeln (tty, 'Sequence error: point 31 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[31]')
$ENDIF
  ;
  sequence := 31 + 1;
end;
	    gt_level_3 (2);
	    begin
  writeln (tty, 'Control has reached inaccessible point 1');
  stop;
end;
	  end;
      2 : goto 11;
      3 : begin
	    gt_level_3 (3);
	    begin
  writeln (tty, 'Control has reached inaccessible point 2');
  stop;
end;
	  end;
      4 : begin
	    gt_level_3 (4);
	    begin
  writeln (tty, 'Control has reached inaccessible point 3');
  stop;
end;
	  end;
    end;
  21 :
    begin
  if sequence <> 32 then
    writeln (tty, 'Sequence error: point 32 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[32]')
$ENDIF
  ;
  sequence := 32 + 1;
end;
  end;

begin (* gt_level_1 *)
  case n of
    1 : begin
	  begin
  if sequence <> 29 then
    writeln (tty, 'Sequence error: point 29 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[29]')
$ENDIF
  ;
  sequence := 29 + 1;
end;
	  gt_level_2 (1);
	  begin
  if sequence <> 33 then
    writeln (tty, 'Sequence error: point 33 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[33]')
$ENDIF
  ;
  sequence := 33 + 1;
end;
	  return;
	end;
    2 : goto 01;
    3 : begin
	  begin
  if sequence <> 36 then
    writeln (tty, 'Sequence error: point 36 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[36]')
$ENDIF
  ;
  sequence := 36 + 1;
end;
	  gt_level_2 (2);
	  begin
  writeln (tty, 'Control has reached inaccessible point 4');
  stop;
end;
	end;
  end;
11 :
  begin
  if sequence <> 37 then
    writeln (tty, 'Sequence error: point 37 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[37]')
$ENDIF
  ;
  sequence := 37 + 1;
end;
  gt_level_2 (3);
  begin
  writeln (tty, 'Control has reached inaccessible point 5');
  stop;
end;
12 :
  begin
  if sequence <> 38 then
    writeln (tty, 'Sequence error: point 38 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[38]')
$ENDIF
  ;
  sequence := 38 + 1;
end;
  gt_level_2 (4);
  begin
  writeln (tty, 'Control has reached inaccessible point 6');
  stop;
end;
end;
$PAGE main program
begin
  rewrite (tty);
  writeln (tty, 'Begin EXE016');

  (*  Simple call tests.  *)

  sequence := 0;
  begin
  if sequence <> 0 then
    writeln (tty, 'Sequence error: point 0 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[0]')
$ENDIF
  ;
  sequence := 0 + 1;
end;
  ct_level_1 (0);
  begin
  if sequence <> 28 then
    writeln (tty, 'Sequence error: point 28 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[28]')
$ENDIF
  ;
  sequence := 28 + 1;
end;

  (*  Variable and parameter access tests.  *)

  a_r_0 := (0, (900, 901, 902, 903, 904));
  dt_level_1 ((710, 711, 712, 713, 714), 1);

  (*  Nonlocal goto tests.  *)

  gt_level_1 (1);
  begin
  if sequence <> 34 then
    writeln (tty, 'Sequence error: point 34 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[34]')
$ENDIF
  ;
  sequence := 34 + 1;
end;
  gt_level_1 (2);
  begin
  writeln (tty, 'Control has reached inaccessible point 7');
  stop;
end;
01 :
  begin
  if sequence <> 35 then
    writeln (tty, 'Sequence error: point 35 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[35]')
$ENDIF
  ;
  sequence := 35 + 1;
end;
  gt_level_1 (3);
  begin
  writeln (tty, 'Control has reached inaccessible point 8');
  stop;
end;
02 :
  begin
  if sequence <> 39 then
    writeln (tty, 'Sequence error: point 39 follows point', sequence-1)
$IF points
  else
    writeln (tty, '[39]')
$ENDIF
  ;
  sequence := 39 + 1;
end;

  writeln (tty, 'End EXE016');
end.
