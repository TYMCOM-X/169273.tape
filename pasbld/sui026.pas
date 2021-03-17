
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  26*)
(*TEST 6.1.7-11, CLASS=DEVIANCE*)
(* The Pascal Standard says that a character string is a sequence of
  characters enclosed by apostrophes, consequently there is no
  NULL string. Does the compiler allow this in programs.
  The compiler conforms if the program does not compile. *)
program t6p1p7d11;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #026');
        writeln('':20);
   writeln(' DEVIATES...6.1.7-11')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

