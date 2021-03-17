program djw12 options dump(noro);

type
  one_word_base_0 = set of 0..15;
  one_word_base_1 = set of 16..31;
  one_word_base_2 = set of 32..47;
  two_word_base_0 = set of 0..31;
  two_word_base_1 = set of 16..47;
  three_word_base_0 = set of 0..47;
  three_word_base_1 = set of 16..63;
  four_word_base_0 = set of 0..4*16 - 1;
  five_word_base_0 = set of 0..5*16-1;
  six_word_base_0 = set of 0..6*16-1;
  seven_word_base_0 = set of 0..7*16-1;
  eight_word_base_0 = set of 0..8*16-1;
  nine_word_base_0 = set of 0..9*16-1;
  ten_word_base_0 = set of 0..10*16-1;
  eleven_word_base_0 = set of 0..11*16-1;
  eleven_word_base_4 = set of 4*16..15*16-1;
  twelve_word_base_0 = set of 0..12*16-1;
  fifteen_word_base_0 = set of 0..15*16-1;
  twentythree_word_base_0 = set of 0..23*16-1;
  eightynine_word_base_0 = set of 0..89*16-1;

var
  ipos, jpos: 0..maximum (integer);
  i, j, k, l, m: integer;
  s1, t1, u1: one_word_base_0;
  i1, j1: lowerbound (one_word_base_0)..upperbound (one_word_base_0);
  s2, t2, u2: two_word_base_0;
  i2, j2: lowerbound (two_word_base_0)..upperbound (two_word_base_0);
  s3, t3, u3: three_word_base_0;
  s3_b1: three_word_base_1;
  i3_b1, j3_b1: lowerbound (three_word_base_1)..upperbound (three_word_base_1);
  s4, t4, u4:four_word_base_0;
  i4, j4: lowerbound (four_word_base_0)..upperbound (four_word_base_0);
  set_5_to_60: set of 5..60;
  s5, t5, u5: five_word_base_0;
  s6, t6, u6: six_word_base_0;
  s7, t7, u7: seven_word_base_0;
  s8, t8, u8: eight_word_base_0;
  s9, t9, u9: nine_word_base_0;
  s10, t10, u10: ten_word_base_0;
  s11, t11, u11: eleven_word_base_0;
  i11, j11: lowerbound (eleven_word_base_0)..upperbound (eleven_word_base_0);
  s11_b4, t11_b4: eleven_word_base_4;
  i11_b4, j11_b4: lowerbound (eleven_word_base_4)..upperbound (eleven_word_base_4);
  i_b4, j_b4: lowerbound (eleven_word_base_4)..maximum (integer);
  s12, t12, u12: twelve_word_base_0;
  i15, j15: lowerbound (fifteen_word_base_0)..upperbound (fifteen_word_base_0);
  s15, t15, u15: fifteen_word_base_0;
  s23, t23, u23: twentythree_word_base_0;
  i23, j23: lowerbound (twentythree_word_base_0)..upperbound (twentythree_word_base_0);
  s89, t89: eightynine_word_base_0;

  rec: packed record
    i_packed_byte,
    j_packed_byte: 0..255;
    i_packed_word,
    j_packed_word: 0..65535
  end;
  bool: boolean;

begin
  if [] = [] then
    i := 1;
  if [] <> [] then
    i := 2;
  if not ([] = []) then
    i := 3;
  if not ([] <> []) then
    i := 4;

  bool := [] = [];
  bool := [] <> [];
  bool := not ([] = []);
  bool := not ([] <> []);

  if [i..j] = [] then
    i := 5;
  if [] <> [i..j] then
    i := 6;
  if not ([i..j] = []) then
    i := 7;
  if not ([] <> [i..j]) then
    i := 8;
  bool := [i..j] = [];
  bool := [] <> [i..j];
  bool := not ([i..j] = []);
  bool := not ([] <> [i..j]);

  if [i] = [] then
    i := 9;
  if [] <> [i] then
    i := 10;
  if not ([i] = []) then
    i := 11;
  if not ([] <> [i]) then
    i := 12;
  bool := [i] = [];
  bool := [] <> [i];
  bool := not ([i] = []);
  bool := not ([] <> [i]);

  if s1 = [] then
    i := 101;
  if s1 <> [] then
    i := 102;
  if not (s2 <> []) then
    i := 103;
  if not ([] = s2) then
    i := 104;
  if s3 = [] then
    i := 105;
  if s4 <> [] then
    i := 106;
  if s5 = [] then
    i := 107;
  if not ([] = s6) then
    i := 108;
  if [] = s7 then
    i := 109;
  if s8 <> [] then
    i := 110;
  if s9 = [] then
    i := 111;
  if s10 <> [] then
    i := 112;
  if s11 = [] then
    i := 113;
  if s11_b4 <> [] then
    i := 114;
  if s23 = [] then
    i := 115;
  if s89 = [] then
    i := 116;
  if not (s89 = []) then
    i := 117;
  if [] = [i,j] then
    i := 118;
  if ([i] + [j]) <> [] then
    i := 119;
  bool := s1 = [];
  bool := s1 <> [];
  bool := not (s2 <> []);
  bool := not ([] = s2);
  bool := s3 = [];
  bool := s4 <> [];
  bool := s5 = [];
  bool := not ([] = s6);
  bool := [] = s7;
  bool := s8 <> [];
  bool := s9 = [];
  bool := s10 <> [];
  bool := s11 = [];
  bool := s11_b4 <> [];
  bool := s23 = [];
  bool := s89 = [];
  bool := not (s89 = []);
  bool := [] = [i,j];
  bool := ([i] + [j]) <> [];
  
  if [i] = [j] then
    i := 13;
  if [i] <> [j] then
    i := 14;
  if not ([i] = [j]) then
    i := 15;
  if not ([i] <> [j]) then
    i := 16;
  bool := [i] = [j];
  bool := [i] <> [j];
  bool := not ([i] = [j]);
  bool := not ([i] <> [j]);

  if [i] = [j..k] then
    i := 17;
  if [j..k] <> [i] then
    i := 18;
  if not ([i] = [j..k]) then
    i := 19;
  if not ([j..k] <> [i]) then
    i := 20;
  bool := [i] = [j..k];
  bool := [j..k] <> [i];
  bool := not ([i] = [j..k]);
  bool := not ([j..k] <> [i]);

  if [j..k] = [l..m] then
    i := 21;
  if [j..k] <> [l..m] then
    i := 22;
  if not ([j..k] = [l..m]) then
    i := 23;
  if not ([j..k] <> [l..m]) then
    i := 24;
  bool := [j..k] = [l..m];
  bool := [j..k] <> [l..m];
  bool := not ([j..k] = [l..m]);
  bool := not ([j..k] <> [l..m]);

  if s3_b1 = [i3_b1] then
    i := 25;
  if [i3_b1] <> s3_b1 then
    i := 26;
  if not (s3_b1 = [i3_b1]) then
    i := 27;
  if not ([i3_b1] <> s3_b1) then
    i := 28;
  bool := s3_b1 = [i3_b1];
  bool := [i3_b1] <> s3_b1;
  bool := not (s3_b1 = [i3_b1]);
  bool := not ([i3_b1] <> s3_b1);

  if s3_b1 = [i3_b1..j3_b1] then
    i := 29;
  if [i3_b1..j3_b1] <> s3_b1 then
    i := 30;
  if not (s3_b1 = [i3_b1..j3_b1]) then
    i := 31;
  if not ([i3_b1..j3_b1] <> s3_b1) then
    i := 32;
  bool := s3_b1 = [i3_b1..j3_b1];
  bool := [i3_b1..j3_b1] <> s3_b1;
  bool := not (s3_b1 = [i3_b1..j3_b1]);
  bool := not ([i3_b1..j3_b1] <> s3_b1);

  if s3_b1 = [i4] then
    i := 25;
  if [i4] <> s3_b1 then
    i := 26;
  if not (s3_b1 = [i4]) then
    i := 27;
  if not ([i4] <> s3_b1) then
    i := 28;
  bool := s3_b1 = [i4];
  bool := [i4] <> s3_b1;
  bool := not (s3_b1 = [i4]);
  bool := not ([i4] <> s3_b1);

  if s3_b1 = [i4..j4] then
    i := 29;
  if [i4..j4] <> s3_b1 then
    i := 30;
  if not (s3_b1 = [i4..j4]) then
    i := 31;
  if not ([i4..j4] <> s3_b1) then
    i := 32;
  bool := s3_b1 = [i4..j4];
  bool := [i4..j4] <> s3_b1;
  bool := not (s3_b1 = [i4..j4]);
  bool := not ([i4..j4] <> s3_b1);

  if s8 = [i] then
    i := 25;
  if [i] <> s8 then
    i := 26;
  if not (s8 = [i]) then
    i := 27;
  if not ([i] <> s8) then
    i := 28;
  bool := s8 = [i];
  bool := [i] <> s8;
  bool := not (s8 = [i]);
  bool := not ([i] <> s8);

  if s11_b4 = [i..j] then
    i := 29;
  if [i..j] <> s11_b4 then
    i := 30;
  if not (s11_b4 = [i..j]) then
    i := 31;
  if not ([i..j] <> s11_b4) then
    i := 32;
  bool := s11_b4 = [i..j];
  bool := [i..j] <> s11_b4;
  bool := not (s11_b4 = [i..j]);
  bool := not ([i..j] <> s11_b4);

  if s11_b4 = t11_b4 then
    i := 29;
  if t11_b4 <> s11_b4 then
    i := 30;
  if not (s11_b4 = t11_b4) then
    i := 31;
  if not (t11_b4 <> s11_b4) then
    i := 32;
  bool := s11_b4 = t11_b4;
  bool := t11_b4 <> s11_b4;
  bool := not (s11_b4 = t11_b4);
  bool := not (t11_b4 <> s11_b4);

  if s11_b4 = s15 then
    i := 29;
  if s15 <> s11_b4 then
    i := 30;
  if not (s11_b4 = s15) then
    i := 31;
  if not (s15 <> s11_b4) then
    i := 32;
  bool := s11_b4 = s15;
  bool := s15 <> s11_b4;
  bool := not (s11_b4 = s15);
  bool := not (s15 <> s11_b4);

  if [i_b4, j_b4] = s15 then
    i := 29;
  if s15 <> [i_b4, j_b4] then
    i := 30;
  if not ([i_b4, j_b4] = s15) then
    i := 31;
  if not (s15 <> [i_b4, j_b4]) then
    i := 32;
  bool := [i_b4, j_b4] = s15;
  bool := s15 <> [i_b4, j_b4];
  bool := not ([i_b4, j_b4] = s15);
  bool := not (s15 <> [i_b4, j_b4]);
  if s1 = t1 then i := 120;
  if not (s1 = t1) then i := 121;
  if s2 = t2 then i := 122;
  if not (t2 = s2) then i := 123;
  if t3 = s3 then i := 124;
  if not (t3 = s3) then i := 125;
  if s4 = t4 then i := 126;
  if s5 = t5 then i := 127;
  if not (s5 = t5) then i := 128;
  if s6 = t6 then i := 129;
  if not (s6 = t6) then i := 130;
  if s7 = t7 then i := 131;
  if s8 = t8 then i := 132;
  if s9 = t9 then i := 133;
  if not (s9 = t9) then i := 134;
  if s15 = t15 then i := 135;
  if s23 = t23 then i := 136;
  if not (s23 = t23) then i := 137;
  if s89 = t89 then i := 140;
  if [i,j] = [k,l] then i := 141;
  if s1 = s2 then i := 142;
  if not (s1 = s2) then i := 143;
  if s1 = s3 then i := 143;
  if s1 = s23 then i := 144;
  if s3 = s1 then i := 145;
  if not (s3 = s1) then i := 146;
  if s3 = s4 then i := 147;
  if s3 = s3_b1 then i := 148;
  if s11_b4 = s15 then i := 149;
  if not (s11_b4 = s15) then i := 150;
  if s1 = [1] then i := 151;
  if s1 = [i1] then i := 152;
  if not (s1 = [i1]) then i := 153;
  if s11_b4 = [i11_b4] then i := 154;
  if s11_b4 = [i23] then i := 155;
  if not (s11_b4 = [i23]) then i := 156;
  if s1 = [i1..j1] then i := 157;
  if s11_b4 = [i11_b4..j11_b4] then i := 158;
  if not (s11_b4 = [i11_b4..j11_b4]) then i := 159;
  if s11_b4 = [i15..j15] then i := 160;
  bool := s11_b4 = [i..j];
  bool := s1 = t1;
  bool := not (s1 = t1);
  bool := s2 = t2;
  bool := not (t2 = s2);
  bool := t3 = s3;
  bool := not (t3 = s3);
  bool := s4 = t4;
  bool := s5 = t5;
  bool := not (s5 = t5);
  bool := s6 = t6;
  bool := not (s6 = t6);
  bool := s7 = t7;
  bool := s8 = t8;
  bool := s9 = t9;
  bool := not (s9 = t9);
  bool := s15 = t15;
  bool := s23 = t23;
  bool := not (s23 = t23);
  bool := s89 = t89;
  bool := [i,j] = [k,l];
  bool := s1 = s2;
  bool := not (s1 = s2);
  bool := s1 = s3;
  bool := s1 = s23;
  bool := s3 = s1;
  bool := not (s3 = s1);
  bool := s3 = s4;
  bool := s3 = s3_b1;
  bool := s11_b4 = s15;
  bool := not (s11_b4 = s15);
  bool := s1 = [1];
  bool := s1 = [i1];
  bool := not (s1 = [i1]);
  bool := s11_b4 = [i11_b4];
  bool := s11_b4 = [i23];
  bool := not (s11_b4 = [i23]);
  bool := s1 = [i1..j1];
  bool := s11_b4 = [i11_b4..j11_b4];
  bool := not (s11_b4 = [i11_b4..j11_b4]);
  bool := s11_b4 = [i15..j15];
  bool := s11_b4 = [i..j];

  if [0..15] = s1 then i := 1601;
  if not ([0..15] = s1) then i := 1602;
  if [15..31] = s1 then i := 1603;
  if [16..31] = s1 then i := 1604;
  if [i1..j1] = s1 then i := 161;
  if not ([i1..j1] = s1) then i := 162;
  if [0..31] = s2 then i := 1621;
  if [i2..31] = s2 then i := 1622;
  if [0..i2] = s2 then i := 1623;
  if [i2..j2] = s2 then i := 163;
  if [0..15] = s3_b1 then i := 1631;
  if not ([0..15] = s3_b1) then i := 1632;
  if [0..i1] = s3_b1 then i := 1633;
  if [i1..i2] = s3_b1 then i := 1634;
  if not ([i1..i2] = s3_b1) then i := 1635;
  if s3_b1 = [i3_b1..j3_b1] then i := 164;
  if not (s3_b1 = [i3_b1..j3_b1]) then i := 165;
  if s3_b1 = [i1..j1] then i := 1640;
  if [16..63] = s3_b1 then i := 1651;
  if [16..100] = s3_b1 then i := 1652;
  if [0..100] = s3_b1 then i := 1654;
  if not ([0..100] = s3_b1) then i := 1655;
  if [64..j11_b4] = s11_b4 then i := 1653;
  if [i11_b4..100] = s11_b4 then i := 1654;
  if [i11_b4..1000] = s11_b4 then i := 1655;
  if [i11_b4..j11_b4] = s11_b4 then i := 166;
  if [i15..j15] = s11_b4 then i := 167;
  if not ([i15..j15] = s11_b4) then i := 168;
  if [i_b4..j_b4] = s11_b4 then i := 169;
  if not ([i_b4..j_b4] = s11_b4) then i := 170;
  if [16..239] = s11_b4 then i := 1700;
  if [0..239] = s11_b4 then i := 1701;
  if [0..367] = s11_b4 then i := 1702;
  if [16..i23] = s11_b4 then i := 1703;
  if [0..i23] = s11_b4 then i := 1704;
  if [i23..j23] = s11_b4 then i := 171;
  if not ([i23..j23] = s11_b4) then i := 172;
  if [16..j] = s11_b4 then i := 173;
  if [i..j] = s11_b4 then i := 1731;
  if not ([i..j] = s11_b4) then i := 174;
  if [i4..j4] = s3_b1 then i := 175;
  if not ([i4..j4] = s3_b1) then i := 176;
  if [i..j4] = s3_b1 then i := 177;
  if not ([i..j4] = s3_b1) then i := 178;
  if [0..15] = [i1, j1] then i := 179;
  if [0..31] = [i1, j1] then i := 180;
  if [0..15] = [i, j] then i := 181;
  if [0..15] = s89 then i := 1811;
  if [0..63] = s89 then i := 1812;
  if [64..127] = s89 then i := 1813;
  if [0..15] = [i_b4, j_b4] then i := 182;
  if [i..j] = [i11_b4, j11_b4] then i := 183;
  if [i..j] = [k, l] then i := 184;
  bool := [0..15] = s1;
  bool := not ([0..15] = s1);
  bool := [15..31] = s1;
  bool := [16..31] = s1;
  bool := [i1..j1] = s1;
  bool := not ([i1..j1] = s1);
  bool := [0..31] = s2;
  bool := [i2..31] = s2;
  bool := [0..i2] = s2;
  bool := [i2..j2] = s2;
  bool := s3_b1 = [i3_b1..j3_b1];
  bool := not (s3_b1 = [i3_b1..j3_b1]);
  bool := s3_b1 = [i1..j1];
  bool := [16..63] = s3_b1;
  bool := [16..100] = s3_b1;
  bool := [64..j11_b4] = s11_b4;
  bool := [i11_b4..100] = s11_b4;
  bool := [i11_b4..1000] = s11_b4;
  bool := [i11_b4..j11_b4] = s11_b4;
  bool := [i15..j15] = s11_b4;
  bool := not ([i15..j15] = s11_b4);
  bool := [i_b4..j_b4] = s11_b4;
  bool := not ([i_b4..j_b4] = s11_b4);
  bool := [16..239] = s11_b4;
  bool := [0..239] = s11_b4;
  bool := [0..367] = s11_b4;
  bool := [16..i23] = s11_b4;
  bool := [0..i23] = s11_b4;
  bool := [i23..j23] = s11_b4;
  bool := not ([i23..j23] = s11_b4);
  bool := [16..j] = s11_b4;
  bool := [i..j] = s11_b4;
  bool := not ([i..j] = s11_b4);
  bool := [i4..j4] = s3_b1;
  bool := not ([i4..j4] = s3_b1);
  bool := [i..j4] = s3_b1;
  bool := not ([i..j4] = s3_b1);
  bool := [0..15] = [i1, j1];
  bool := [0..31] = [i1, j1];
  bool := [0..15] = [i, j];
  bool := [0..15] = [i_b4, j_b4];
  bool := [i..j] = [i11_b4, j11_b4];
  bool := [i..j] = [k, l];
  if [i4..j4] = set_5_to_60 then
    i := 185;
  if not ([i4..j4] = set_5_to_60) then
    i := 186;
  bool := [i4..j4] = set_5_to_60;
  bool := not ([i4..j4] = set_5_to_60);
end.
 