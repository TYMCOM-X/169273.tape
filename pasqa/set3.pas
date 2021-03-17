program djw3 options dump(noro);

type
  word = -32768..32767;
  rec1 = ^packed record
	  f1: word
	end;
  arr1 = ^packed array [1..1] of word;
  rec2 = ^packed record
	  f1, f2: word
	end;
  arr2 = ^packed array [1..2] of word;
  rec3 = ^packed record
	  f1, f2, f3: word
	end;
  arr3 = ^packed array [1..3] of word;
  arr4 = ^packed array [1..4] of word;
  arr5 = ^packed array [1..5] of word;
  arr6 = ^packed array [1..6] of word;
  arr7 = ^packed array [1..7] of word;
  arr8 = ^packed array [1..8] of word;
  arr9 = ^packed array [1..9] of word;
  arr10 = ^packed array [1..10] of word;
  arr11 = ^packed array [1..11] of word;
  arr12 = ^packed array [1..12] of word;
  arr13 = ^packed array [1..13] of word;
  arr14 = ^packed array [1..14] of word;
  arr15 = ^packed array [1..15] of word;
  arr24 = ^packed array [1..24] of word;
  arr25 = ^packed array [1..25] of word;
  arr26 = ^packed array [1..26] of word;
  arr27 = ^packed array [1..27] of word;
  arr28 = ^packed array [1..28] of word;
  arr83 = ^packed array [1..83] of word;
  arr84 = ^packed array [1..84] of word;
  arr85 = ^packed array [1..85] of word;
  arr153 = ^packed array [1..153] of word;
  arr154 = ^packed array [1..154] of word;
  arr155 = ^packed array [1..155] of word;
  arr156 = ^packed array [1..156] of word;
  arr248 = ^packed array [1..248] of word;
  arr249 = ^packed array [1..249] of word;
  arr250 = ^packed array [1..250] of word;
  arr251 = ^packed array [1..251] of word;
  arr252 = ^packed array [1..252] of word;

var
  r1a,r1b: rec1;
  a1a,a1b: arr1;
  r2a,r2b: rec2;
  a2a,a2b: arr2;
  r3a,r3b: rec3;
  a3a,a3b: arr3;
  a4a,a4b: arr4;
  a5a,a5b: arr5;
  a6a,a6b: arr6;
  a7a,a7b: arr7;
  a8a,a8b: arr8;
  a9a,a9b: arr9;
  a10a,a10b: arr10;
  a11a,a11b: arr11;
  a12a,a12b: arr12;
  a13a,a13b: arr13;
  a14a,a14b: arr14;
  a15a,a15b: arr15;
  a24a,a24b: arr24;
  a25a,a25b: arr25;
  a26a,a26b: arr26;
  a27a,a27b: arr27;
  a28a,a28b: arr28;
  a83a,a83b: arr83;
  a84a,a84b: arr84;
  a85a,a85b: arr85;
  a153a,a153b: arr153;
  a154a,a154b: arr154;
  a155a,a155b: arr155;
  a156a,a156b: arr156;
  a248a,a248b: arr248;
  a249a,a249b: arr249;
  a250a,a250b: arr250;
  a251a,a251b: arr251;
  a252a,a252b: arr252;

begin
  r1a^ := r1b^;
  a1a^ := a1b^;
  r2a^ := r2b^;
  a2a^ := a2b^;
  r3a^ := r3b^;
  a3a^ := a3b^;
  a4a^ := a4b^;
  a5a^ := a5b^;
  a6a^ := a6b^;
  a7a^ := a7b^;
  a8a^ := a8b^;
  a9a^ := a9b^;
  a10a^ := a10b^;
  a11a^ := a11b^;
  a12a^ := a12b^;
  a13a^ := a13b^;
  a14a^ := a14b^;
  a15a^ := a15b^;
  a24a^ := a24b^;
  a25a^ := a25b^;
  a26a^ := a26b^;
  a27a^ := a27b^;
  a28a^ := a28b^;
  a83a^ := a83b^;
  a84a^ := a84b^;
  a85a^ := a85b^;
  a153a^ := a153b^;
  a154a^ := a154b^;
  a155a^ := a155b^;
  a156a^ := a156b^;
  a248a^ := a248b^;
  a249a^ := a249b^;
  a250a^ := a250b^;
  a251a^ := a251b^;
  a252a^ := a252b^;
end.
