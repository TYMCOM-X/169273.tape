
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  14*)
(*TEST 6.1.6-1, CLASS=CONFORMANCE*)
(* Labels are permitted in standard Pascal. This program
  simply tests if they are permitted by this compiler.
  The compiler fails if the program will not compile
  (or the message printed out is incorrect). *)
program t6p1p6d1;
label
   1,2,3,4,5;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #014');
      write(' P');
      goto 4;
   1: write('.6');
      goto 5;
   2: write('SS');
      goto 3;
   3: write('..');
      goto 1;
   4: write('A');

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      goto 2;
   5: writeln('.1.6-1');
end.
    