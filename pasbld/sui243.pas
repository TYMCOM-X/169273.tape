
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 243*)
(*TEST 6.8.3.5-3, CLASS=DEVIANCE*)
(* This test checks that the constants of a case statement
  cannot be strings. The compiler deviates if the program
  compiles and prints DEVIATES. *)
program t6p8p3p5d3;
var
   a:char;
   i:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #243');
   for a:= 'a' to 'd' do
      case a of
      'a': i:=1;
      'b': i:=i+1;
      'c': i:=i+1;
      'de': i:=i+1;
      end;
   writeln(' DEVIATES...6.8.3.5-3, CASE');
end.
   