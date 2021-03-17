program EXE006;
  
var
  bool: boolean;
  i, j, k, l: integer;
  l1, m1, n1: 0..32767;
  l2, m2, n2: 2..32767;
  l3, m3, n3: 2..511;
  l4, m4, n4: -5..511;
  l5, m5, n5: 2..73;
  zero_based_shortset: set of 0..71;
  five_based_shortset: set of 5..71;
  zero_based_longset: set of 0..511;
  five_based_longset: set of 5..511;
  null_set: set of 0..32766;
  setx: set of 100..200;
  sety: set of 0..99;
  s_0_to_367, t_0_to_367, u_0_to_367: set of 0..367;
  s_18_to_367, t_18_to_367, u_18_to_367: set of 0..367;
  
public procedure error (msg: integer);
  
  begin
    writeln (ttyoutput, 'Error ', msg);
    break (ttyoutput)
  end;
  
public procedure intest (first_set: set of 0..71; var second_set: set of 0..511);
  
  begin
    bool := 5 in first_set;
    if not bool then error (1);
    bool := 511 in second_set;
    if not bool then error (2);
    if 5 in first_set then begin end else error (3);
    if 511 in second_set then begin end else error (4);
  end;
  
begin
  rewrite (ttyoutput);
  writeln (ttyoutput, 'Begin EXE006' );
  break ( ttyoutput );
  zero_based_shortset := [1, 5, 34, 71];
  five_based_shortset := zero_based_shortset;
  zero_based_longset := [511, 6, 8, 10];
  five_based_longset := zero_based_longset;
  
  intest (zero_based_shortset, zero_based_longset);
  
  i := 6;
  j := 256;
  k := 511;
  
  bool := 2 in zero_based_shortset;
  if bool then error (5);
  bool := i in zero_based_shortset;
  if bool then error (6);
  bool := not (i in zero_based_shortset);
  if not bool then error (7);
  bool := i in zero_based_longset;
  if not bool then error (8);
  bool := not (i in zero_based_longset);
  if bool then error (9);
  bool := 256 in ([i..j] * [j..k]) + [k, j, i];
  if not bool then error (10);
  
  if 2 in zero_based_shortset then error (11);
  if not (2 in zero_based_shortset) then begin end else error (12);
  if i in zero_based_shortset then error (13);
  if not (i in zero_based_shortset) then begin end else error (14);
  if 2 in zero_based_longset then error (15);
  if i in zero_based_longset then begin end else error (16);
  if not (i in zero_based_longset) then error (17);
  if 256 in ([i..j] * [j..k]) + [k, j, i] then begin end else error (18);
  bool := 511 in zero_based_longset;
  if not bool then error (19);
  bool := not (511 in zero_based_longset);
  if bool then error (20);
  
  if 511 in zero_based_longset then begin end else error (21);
  if not (511 in zero_based_longset) then error (22);
  
  l1 := 6;
  m1 := 256;
  n1 := 511;
  
  bool := l1 in zero_based_shortset;
  if bool then error (23);
  bool := not (l1 in zero_based_shortset);
  if not bool then error (24);
  bool := l1 in zero_based_longset;
  if not bool then error (25);
  bool := not (l1 in zero_based_longset);
  if bool then error (26);
  bool := 256 in ([l1 .. m1] * [m1 .. n1]) + [n1, m1, l1];
  if not bool then error (27); 
  
  if l1 in zero_based_shortset then error (28);
  if not (l1 in zero_based_shortset) then begin end else error (29);
  if l1 in zero_based_longset then begin end else error (30);
  if not (l1 in zero_based_longset) then error (31);
  if m1 in ([l1..m1] * [m1..n1]) + [n1, m1, l1] then begin end else error (32);
  
  
  l2 := 6;
  m2 := 256;
  n2 := 511;
  
  bool := l2 in zero_based_shortset;
  if bool then error (33);
  bool := not (l2 in zero_based_shortset);
  if not bool then error (34);
  bool := l2 in zero_based_longset;
  if not bool then error (35);
  bool := not (l2 in zero_based_longset);
  if bool then error (36);
  bool := 256 in ([l2 .. m2] * [m2 .. n2]) + [n2, m2, l2];
  if not bool then error (37); 
  
  if l2 in zero_based_shortset then error (38);
  if not (l2 in zero_based_shortset) then begin end else error (39);
  if l2 in zero_based_longset then begin end else error (40);
  if not (l2 in zero_based_longset) then error (41);
  if m2 in ([l2..m2] * [m2..n2]) + [n2, m2, l2] then begin end else error (42);
  
  
  l3 := 6;
  m3 := 256;
  n3 := 511;
  
  bool := l3 in zero_based_shortset;
  if bool then error (43);
  bool := not (l3 in zero_based_shortset);
  if not bool then error (44);
  bool := l3 in zero_based_longset;
  if not bool then error (45);
  bool := not (l3 in zero_based_longset);
  if bool then error (46);
  bool := 256 in ([l3 .. m3] * [m3 .. n3]) + [n3, m3, l3];
  if not bool then error (47); 
  
  if l3 in zero_based_shortset then error (48);
  if not (l3 in zero_based_shortset) then begin end else error (49);
  if l3 in zero_based_longset then begin end else error (50);
  if not (l3 in zero_based_longset) then error (51);
  if m3 in ([l3..m3] * [m3..n3]) + [n3, m3, l3] then begin end else error (52);
  
  
  l4 := 6;
  m4 := 256;
  n4 := 511;
  
  bool := l4 in zero_based_shortset;
  if bool then error (53);
  bool := not (l4 in zero_based_shortset);
  if not bool then error (54);
  bool := l4 in zero_based_longset;
  if not bool then error (55);
  bool := not (l4 in zero_based_longset);
  if bool then error (56);
  bool := 256 in ([l4 .. m4] * [m4 .. n4]) + [n4, m4, l4];
  if not bool then error (57); 
  
  if l4 in zero_based_shortset then error (58);
  if not (l4 in zero_based_shortset) then begin end else error (59);
  if l4 in zero_based_longset then begin end else error (60);
  if not (l4 in zero_based_longset) then error (61);
  if m4 in ([l4..m4] * [m4..n4]) + [n4, m4, l4] then begin end else error (62);
  
  
  l2 := 6;
  m2 := 256;
  n2 := 511;
  
  bool := l2 in five_based_shortset;
  if bool then error (63);
  bool := not (l2 in five_based_shortset);
  if not bool then error (64);
  bool := l2 in five_based_longset;
  if not bool then error (65);
  bool := not (l2 in five_based_longset);
  if bool then error (66);
  bool := 256 in ([l2 .. m2] * [m2 .. n2]) + [n2, m2, l2];
  if not bool then error (67); 
  
  if l2 in five_based_shortset then error (68);
  if not (l2 in five_based_shortset) then begin end else error (69);
  if l2 in five_based_longset then begin end else error (70);
  if not (l2 in five_based_longset) then error (71);
  if m2 in ([l2..m2] * [m2..n2]) + [n2, m2, l2] then begin end else error (72);
  
  
  l4 := 6;
  m4 := 256;
  n4 := 511;
  
  bool := l4 in five_based_shortset;
  if bool then error (73);
  bool := not (l4 in five_based_shortset);
  if not bool then error (74);
  bool := l4 in five_based_longset;
  if not bool then error (75);
  bool := not (l4 in five_based_longset);
  if bool then error (76);
  bool := 256 in ([l4 .. m4] * [m4 .. n4]) + [n4, m4, l4];
  if not bool then error (77); 
  
  if l4 in five_based_shortset then error (78);
  if not (l4 in five_based_shortset) then begin end else error (79);
  if l4 in five_based_longset then begin end else error (80);
  if not (l4 in five_based_longset) then error (81);
  if m4 in ([l4..m4] * [m4..n4]) + [n4, m4, l4] then begin end else error (82);
  
  
  j := 1024;
  k := 4096;
  l := 8192;
  
  bool := l4 in ([j..k] * [k..l]) + zero_based_longset;
  if not bool then error (83);
  bool := k in ([j..k] * [k..l]) + zero_based_longset;
  if not bool then error (84);
  bool := m4 in ([j..k] * [k..l]) + zero_based_longset;
  if bool then error (85);
  
  if l4 in ([j..k] * [k..l]) + zero_based_longset then begin end else error (86);
  if k in ([j..k] * [k..l]) + zero_based_longset then begin end else error (87);
  if not (m4 in ([j..k] * [k..l]) + zero_based_longset) then begin end else error (88);
  
  
  bool := m1 in [l1..n1];
  if not bool then error (89);
  bool := not (m1 in [l1..n1]);
  if bool then error (90);
  bool := l1 in [m1..n1];
  if bool then error (91);
  bool := not (l1 in [m1..n1]);
  if not bool then error (92);
  
  if m1 in [l1..n1] then begin end else error (93);
  if not (m1 in [l1..n1]) then error (94);
  if l1 in [m1..n1] then error (95);
  if not (l1 in [m1..n1]) then begin end else error (96);
  bool := l1 in ([j..k] * [k..l]) + five_based_longset;
  if not bool then error (97);
  if not (m1 in ([j..k] * [k..l]) + five_based_longset) then begin end else error (98);
  
  
  
  bool := m4 in [l4..n4];
  if not bool then error (99);
  bool := not (m4 in [l4..n4]);
  if bool then error (100);
  bool := l4 in [m4..n4];
  if bool then error (101);
  bool := not (l4 in [m4..n4]);
  if not bool then error (102);
  
  if m4 in [l4..n4] then begin end else error (103);
  if not (m4 in [l4..n4]) then error (104);
  if l4 in [m4..n4] then error (105);
  if not (l4 in [m4..n4]) then begin end else error (106);
  bool := l4 in ([j..k] * [k..l]) + five_based_longset;
  if not bool then error (107);
  if not (m4 in ([j..k] * [k..l]) + five_based_longset) then begin end else error (108);
  
  
  null_set := [];
  i := 1;
  j := 0;
  k := -1;
  l := -10;
  if i in ([j..k] + [k..l]) then error (109);
  zero_based_longset := [j..k];
  if [j..k] = null_set then else error (110);
  if [k..l] <> null_set then error (111);
  
  k := 1;
  if i in ([j..k] + [k..l]) then else error (112);
  if [j..k] = null_set then error (113);
  if [k..l] <> null_set then error (114);
  
  i := 6;
  j := 0;
  k := -5;
  l := 10;
  if i in ([j..k] * [k..l]) then error (115);
  if ([j..k] * [k..l]) = null_set then else error (116);
  if ([k..l] * [l..k]) <> null_set then error (117);
 
  k := 6;
  if i in ([j..k] * [k..l]) then else error (118);
  if ([j..k] * [k..l]) = null_set then error (119);
  if ([k..l] * [l..k]) <> null_set then error (120);
  
  l5 := 20;
  m5 := 10;
  n5 := 5;
  bool := ([l5..m5] + [n5..m5]) * [m5..n5] = null_set;
  if not bool then error (121);
  n5 := 14;
  bool := ([l5..m5] + [n5..m5]) * [m5..n5] = null_set;
  if not bool then error (122);
  
  i := 10;
  j := 110;
  bool := (([100..j] + [100..j]) * [0..i]) = ([0..i] + [0..i]);
  if bool then error (123);
  if i in (([100..j] + [100..j]) * [0..i]) then error (124);
  i := 100;
  if not (i in (([100..j] + [100..j]) * [0..i])) then error (125);
  sety := [];
  setx := [100..2200];
  i := 60;
  k := 0;
  l := 60;
  bool := ([0..i] + [0..50]) - setx - sety = [k..l];
  if not bool then error (126);
  k := 58;
  sety := [0..57,59];
  bool := ([0..i] + [0..50]) - setx - sety = [k,l];
  if not bool then error (127);
  k := k + 1;
  bool := ([0..i] + [0..50]) - setx - sety = [k,l];
  if bool then error (128);
  sety := [0..99];
  i := 180;
  k := 100;
  l := 180;
  setx := [101..179];
  bool := ([0..i] + [0..50]) - setx - sety = [k,l];
  if not bool then error (129);
  setx := setx + [k];
  bool := ([0..i] + [0..50]) - setx - sety = [k,l];
  if bool then error (130);

  n5 := 5;
  s_0_to_367 := [i, k..l, n5, m5..l5];
  t_0_to_367 := [i, k..l, n5..l5];
  if not (s_0_to_367 <= t_0_to_367) then error (131);
  t_0_to_367 := t_0_to_367 - [n5..m5-1];
  if s_0_to_367 <= t_0_to_367 then error (132);
  u_0_to_367 := s_0_to_367 - t_0_to_367;
  if u_0_to_367 <> [5] then error (133);
  u_0_to_367 := s_0_to_367 * t_0_to_367;
  if u_0_to_367 <> t_0_to_367 then error (134);
  if (u_0_to_367 + [5]) <> s_0_to_367 then error (135);
  
  s_18_to_367 := [i, k..l, n5, m5..l5];
  t_18_to_367 := [i, k..l, n5..l5];
  if not (s_18_to_367 <= t_18_to_367) then error (131);
  t_18_to_367 := t_18_to_367 - [n5..m5-1];
  if s_18_to_367 <= t_18_to_367 then error (132);
  u_18_to_367 := s_18_to_367 - t_18_to_367;
  if u_18_to_367 <> [5] then error (133);
  u_18_to_367 := s_18_to_367 * t_18_to_367;
  if u_18_to_367 <> t_18_to_367 then error (134);
  if (u_18_to_367 + [5]) <> s_18_to_367 then error (135);

  s_18_to_367 := [i, k, l, n5, m5, l5];
  if s_18_to_367 <> [100, 180, 5, 10, 20] then error (136);
  s_0_to_367 := [i, k, l, n5, m5, l5];
  if s_0_to_367 <> [100, 180, 5, 10, 20] then error (136);

  
  writeln ( ttyoutput, 'End EXE006' );
  close (ttyoutput);
end.
   