
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 248*)
(*TEST 6.8.3.5-8, CLASS=QUALITY*)
(* This test checks a large populated case statement to check the
  limit on the size of code is not a serious one.
  The compiler has a small limit on the size of the case
  statement if the program does not compile and print PASS. *)
program t6p8p3p5d8;
var
   sum:integer;
   i:0..255;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #248');
   sum :=0;
   for i:=0 to 255 do
      case i of
       0 : sum := sum + i;
       1 : sum := sum + i;
       2 : sum := sum + i;
       3 : sum := sum + i;
       4 : sum := sum + i;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

       5 : sum := sum + i;
       6 : sum := sum + i;
       7 : sum := sum + i;
       8 : sum := sum + i;
       9 : sum := sum + i;
      10 : sum := sum + i;
      11 : sum := sum + i;
      12 : sum := sum + i;
      13 : sum := sum + i;
      14 : sum := sum + i;
      15 : sum := sum + i;
      16 : sum := sum + i;
      17 : sum := sum + i;
      18 : sum := sum + i;
      19 : sum := sum + i;
      20 : sum := sum + i;
      21 : sum := sum + i;
      22 : sum := sum + i;
      23 : sum := sum + i;
      24 : sum := sum + i;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      25 : sum := sum + i;
      26 : sum := sum + i;
      27 : sum := sum + i;
      28 : sum := sum + i;
      29 : sum := sum + i;
      30 : sum := sum + i;
      31 : sum := sum + i;
      32 : sum := sum + i;
      33 : sum := sum + i;
      34 : sum := sum + i;
      35 : sum := sum + i;
      36 : sum := sum + i;
      37 : sum := sum + i;
      38 : sum := sum + i;
      39 : sum := sum + i;
      40 : sum := sum + i;
      41 : sum := sum + i;
      42 : sum := sum + i;
      43 : sum := sum + i;
      44 : sum := sum + i;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      45 : sum := sum + i;
      46 : sum := sum + i;
      47 : sum := sum + i;
      48 : sum := sum + i;
      49 : sum := sum + i;
      50 : sum := sum + i;
      51 : sum := sum + i;
      52 : sum := sum + i;
      53 : sum := sum + i;
      54 : sum := sum + i;
      55 : sum := sum + i;
      56 : sum := sum + i;
      57 : sum := sum + i;
      58 : sum := sum + i;
      59 : sum := sum + i;
      60 : sum := sum + i;
      61 : sum := sum + i;
      62 : sum := sum + i;
      63 : sum := sum + i;
      64 : sum := sum + i;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      65 : sum := sum + i;
      66 : sum := sum + i;
      67 : sum := sum + i;
      68 : sum := sum + i;
      69 : sum := sum + i;
      70 : sum := sum + i;
      71 : sum := sum + i;
      72 : sum := sum + i;
      73 : sum := sum + i;
      74 : sum := sum + i;
      75 : sum := sum + i;
      76 : sum := sum + i;
      77 : sum := sum + i;
      78 : sum := sum + i;
      79 : sum := sum + i;
      80 : sum := sum + i;
      81 : sum := sum + i;
      82 : sum := sum + i;
      83 : sum := sum + i;
      84 : sum := sum + i;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      85 : sum := sum + i;
      86 : sum := sum + i;
      87 : sum := sum + i;
      88 : sum := sum + i;
      89 : sum := sum + i;
      90 : sum := sum + i;
      91 : sum := sum + i;
      92 : sum := sum + i;
      93 : sum := sum + i;
      94 : sum := sum + i;
      95 : sum := sum + i;
      96 : sum := sum + i;
      97 : sum := sum + i;
      98 : sum := sum + i;
      99 : sum := sum + i;
      100 : sum := sum + i;
      101 : sum := sum + i;
      102 : sum := sum + i;
      103 : sum := sum + i;
      104 : sum := sum + i;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      105 : sum := sum + i;
      106 : sum := sum + i;
      107 : sum := sum + i;
      108 : sum := sum + i;
      109 : sum := sum + i;
      110 : sum := sum + i;
      111 : sum := sum + i;
      112 : sum := sum + i;
      113 : sum := sum + i;
      114 : sum := sum + i;
      115 : sum := sum + i;
      116 : sum := sum + i;
      117 : sum := sum + i;
      118 : sum := sum + i;
      119 : sum := sum + i;
      120 : sum := sum + i;
      121 : sum := sum + i;
      122 : sum := sum + i;
      123 : sum := sum + i;
      124 : sum := sum + i;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      125 : sum := sum + i;
      126 : sum := sum + i;
      127 : sum := sum + i;
      128 : sum := sum + i;
      129 : sum := sum + i;
      130 : sum := sum + i;
      131 : sum := sum + i;
      132 : sum := sum + i;
      133 : sum := sum + i;
      134 : sum := sum + i;
      135 : sum := sum + i;
      136 : sum := sum + i;
      137 : sum := sum + i;
      138 : sum := sum + i;
      139 : sum := sum + i;
      140 : sum := sum + i;
      141 : sum := sum + i;
      142 : sum := sum + i;
      143 : sum := sum + i;
      144 : sum := sum + i;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      145 : sum := sum + i;
      146 : sum := sum + i;
      147 : sum := sum + i;
      148 : sum := sum + i;
      149 : sum := sum + i;
      150 : sum := sum + i;
      151 : sum := sum + i;
      152 : sum := sum + i;
      153 : sum := sum + i;
      154 : sum := sum + i;
      155 : sum := sum + i;
      156 : sum := sum + i;
      157 : sum := sum + i;
      158 : sum := sum + i;
      159 : sum := sum + i;
      160 : sum := sum + i;
      161 : sum := sum + i;
      162 : sum := sum + i;
      163 : sum := sum + i;
      164 : sum := sum + i;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      165 : sum := sum + i;
      166 : sum := sum + i;
      167 : sum := sum + i;
      168 : sum := sum + i;
      169 : sum := sum + i;
      170 : sum := sum + i;
      171 : sum := sum + i;
      172 : sum := sum + i;
      173 : sum := sum + i;
      174 : sum := sum + i;
      175 : sum := sum + i;
      176 : sum := sum + i;
      177 : sum := sum + i;
      178 : sum := sum + i;
      179 : sum := sum + i;
      180 : sum := sum + i;
      181 : sum := sum + i;
      182 : sum := sum + i;
      183 : sum := sum + i;
      184 : sum := sum + i;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      185 : sum := sum + i;
      186 : sum := sum + i;
      187 : sum := sum + i;
      188 : sum := sum + i;
      189 : sum := sum + i;
      190 : sum := sum + i;
      191 : sum := sum + i;
      192 : sum := sum + i;
      193 : sum := sum + i;
      194 : sum := sum + i;
      195 : sum := sum + i;
      196 : sum := sum + i;
      197 : sum := sum + i;
      198 : sum := sum + i;
      199 : sum := sum + i;
      200 : sum := sum + i;
      201 : sum := sum + i;
      202 : sum := sum + i;
      203 : sum := sum + i;
      204 : sum := sum + i;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      205 : sum := sum + i;
      206 : sum := sum + i;
      207 : sum := sum + i;
      208 : sum := sum + i;
      209 : sum := sum + i;
      210 : sum := sum + i;
      211 : sum := sum + i;
      212 : sum := sum + i;
      213 : sum := sum + i;
      214 : sum := sum + i;
      215 : sum := sum + i;
      216 : sum := sum + i;
      217 : sum := sum + i;
      218 : sum := sum + i;
      219 : sum := sum + i;
      220 : sum := sum + i;
      221 : sum := sum + i;
      222 : sum := sum + i;
      223 : sum := sum + i;
      224 : sum := sum + i;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      225 : sum := sum + i;
      226 : sum := sum + i;
      227 : sum := sum + i;
      228 : sum := sum + i;
      229 : sum := sum + i;
      230 : sum := sum + i;
      231 : sum := sum + i;
      232 : sum := sum + i;
      233 : sum := sum + i;
      234 : sum := sum + i;
      235 : sum := sum + i;
      236 : sum := sum + i;
      237 : sum := sum + i;
      238 : sum := sum + i;
      239 : sum := sum + i;
      240 : sum := sum + i;
      241 : sum := sum + i;
      242 : sum := sum + i;
      243 : sum := sum + i;
      244 : sum := sum + i;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      245 : sum := sum + i;
      246 : sum := sum + i;
      247 : sum := sum + i;
      248 : sum := sum + i;
      249 : sum := sum + i;
      250 : sum := sum + i;
      251 : sum := sum + i;
      252 : sum := sum + i;
      253 : sum := sum + i;
      254 : sum := sum + i;
      255 : sum := sum + i;
      end;
   writeln(' QUALITY TEST - SIZE OF CASE STATEMENT');
   if sum = 32640 then
      writeln(' PASS...6.8.3.5-8')
   else
      writeln(' FAIL...6.8.3.5-8');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

    