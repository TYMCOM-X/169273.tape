
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  15*)
(*TEST 6.1.6-2, CLASS=CONFORMANCE*)
(* Labels should be distinguished by their apparent integral value
  according to the Pascal Standard.
  This program tests if this is the case for this compiler. If so
  then the program shall print PASS. *)
program t6p1p6d2;
label
   5,6,7;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #015');
      goto 5;
0006: goto 7;
   5: goto 6;
 007: writeln('PASS...6.1.6-2')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

    