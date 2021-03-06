
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  23*)
(*TEST 6.1.7-8, CLASS=DEVIANCE*)
(* Similarly to 6.1.7-7, subranges of char should not be
  compatible with packed arrays of char. However, if the extension
  is allowed, it should be correctly handled.
  Standards conforming processors will not compile the program,
  compilers which permit the extension should detect the error
  at compile-time or run-time, but should not execute without
  error. *)
program t6p1p7d8;
type
   digit = '0'..'9';
var
   string1 : packed array[1..4] of char;
   string2 : packed array[1..4] of digit;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #023');
   string1:='FOUR';
   string2:='FOUR';
   writeln(' DEVIATES...6.1.7-8')

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end.
   