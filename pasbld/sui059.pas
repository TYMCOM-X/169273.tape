
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  59*)
(*TEST 6.4.1-3, CLASS=DEVIANCE*)
(* This program also tests that attempts to use types in
  their own definitions are detected, but inserts a nasty
  scope twist by making another type with the same identifier
  available in an outer scope.  It should be excluded from this
  scope, according to the Standard. *)
program t6p4p1d3;
type
   x  = integer;
procedure p;
type
   x  = record
            y : x
        end;
begin
   writeln(' DEVIATES...6.4.1-3: SCOPE ERROR')
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #059');

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   p
end.
 