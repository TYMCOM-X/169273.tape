
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  25*)
(*TEST 6.1.7-10, CLASS=DEVIANCE*)
(* The Pascal Standard states that string types are compatible
  if they have the same number of components.
  Some compilers may allow assignment of one string type to another,
  padding out with spaces or truncating characters if they are
  not of the same lengths.
  All the cases below should be strictly rejected.
  The compiler deviates if one or more are accepted. *)
program t6p1p7d10;
var
   string1 : packed array[1..4] of char;
   string2 : packed array[1..6] of char;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #025');
   writeln('DEVIATES...6.1.7-10');
   string1:='AB';       (* 1-pad with spaces ? *)
   writeln('CASE 1 : ', string1);
   string1:='ABCD';
   string2:=string1;    (* 2-what happens here ? *)

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   writeln('CASE 2 : ', string2);
   string1:='ABCDEFG';  (* 3-what happens here ? *)
   writeln('CASE 3 : ', string1)
end.
