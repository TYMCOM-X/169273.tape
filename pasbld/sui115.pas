
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 115*)
(*TEST 6.4.5-11, CLASS=DEVIANCE*)
(* The Pascal Standard permits compatibility only between string
  types of the same number of components.
  Some compilers may allow compatibility between string types
  with different numbers of components. (see 6.1.7-4 and 6.....)
  The compiler conforms if the program does not compile. *)
program t6p4p5d11;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #115');
   if 'CAT' < 'HOUND' then writeln(' DEVIATES...6.4.5-11')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

