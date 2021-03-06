
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  20*)
(*TEST 6.1.7-5, CLASS=DEVIANCE*)
(* The Pascal Standard specifically states that character
  strings are constants of the type
      packed array[1..n] of char
  This program tests that this type is not compatible
  with unpacked arrays.
  The compiler conforms if the program fails to compile. *)
program t6p1p7d5;
var
   string1 : packed array[1..4] of char;
   string2 : array[1..4] of char;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #020');
   string1:='STR1';
   string2:='STR2';
   writeln(' DEVIATES...6.1.7-5')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 