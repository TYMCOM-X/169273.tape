
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 126*)
(*TEST 6.4.6-9, CLASS=DEVIANCE*)
(* The Pascal Standard allows assignment of integers to reals,
  but not reals to integers.
  Does this compiler allow assignment of reals to integers.
  If so, it does not conform to the Standard.
  The compiler conforms if the program does not compile. *)
program t6p4p6d9;
var
   i : real;
   j : integer;
procedure test(a:integer);
begin
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #126');
   i:=6.345;
   j:=i;
   test(6.345);
   writeln(' DEVIATES...6.4.6-9')

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end.
