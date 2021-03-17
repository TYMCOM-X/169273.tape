
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  96*)
(*TEST 6.4.3.4-4, CLASS=IMPLEMENTATIONDEFINED*)
(* The Pascal Standard states that the largest and smallest values
  permitted in the base-type of a set-type are implementation
  defined.
  The size of the base-type permitted may be determined by
  examining which of the cases below are accepted by the compiler. *)
program t6p4p3p4d4;
type
   setone   = set of -1..+1;
   settwo   = set of char;
   setthree = set of 0..1000;
   setfour  = set of 0..10;
   setfive  = set of 0..20;
   setsix   = set of 0..30;
   setseven = set of 0..40;
   seteight = set of 0..50;
   setnine  = set of 0..60;
   setten   = set of 0..70;
var

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   s : setthree;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #096');
   s:=[1000];
   writeln(' IMPLEMENTATIONDEFINED...6.4.3.4-4 -->',
            'GOOD IMPLEMENTATION OF SETS')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

