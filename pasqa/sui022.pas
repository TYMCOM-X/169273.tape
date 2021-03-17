
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  22*)
(*TEST 6.1.7-7, CLASS=DEVIANCE*)
(* Again, as character strings are constants of the type
      packed array[1..n] of char,
  they should not be compatible with packed arrays of
  subranges of char.
  The compiler conforms if the program will not compile. *)
program t6p1p7d7;
type
   alpha = 'A'..'Z';
var
   string1 : packed array[1..4] of char;
   string2 : packed array[1..4] of alpha;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #022');
   string1:='FOUR';
   string2:='FOUR';
   writeln(' DEVIATES...6.1.7-7')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

