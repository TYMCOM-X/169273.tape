
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  40*)
(*TEST 6.2.1-9, CLASS=QUALITY*)
(* This test checks that a large number of labels may be declared
  in a program. It is an attempt to detect a small compiler limit on
  the number of labels. *)
program t6p2p1d9;
label
   1,2,3,4,5,6,7,8,9,10,
   11,12,13,14,15,16,17,18,19,20,
   21,22,23,24,25,26,27,28,29,30,
   31,32,33,34,35,36,37,38,39,40,
   41,42,43,44,45,46,47,48,49,50;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #040');
   1: ;
   2: ;
   3: ;
   4: ;
   5: ;
   6: ;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   7: ;
   8: ;
   9: ;
   10: ;
   11: ;
   12: ;
   13: ;
   14: ;
   15: ;
   16: ;
   17: ;
   18: ;
   19: ;
   20: ;
   21: ;
   22: ;
   23: ;
   24: ;
   25: ;
   26: ;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   27: ;
   28: ;
   29: ;
   30: ;
   31: ;
   32: ;
   33: ;
   34: ;
   35: ;
   36: ;
   37: ;
   38: ;
   39: ;
   40: ;
   41: ;
   42: ;
   43: ;
   44: ;
   45: ;
   46: ;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   47: ;
   48: ;
   49: ;
   50: ;
   writeln(' 50 LABELS DECLARED AND SITED...6.2.1-9')
end.
 