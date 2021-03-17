
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  38*)
(*TEST 6.2.1-7, CLASS=ERRORHANDLING*)
(* The Pascal Standard states that '..local variables have values
  which are undefined at the beginning of the statement part..'.
  The undefined value is dependent on the implementation.
  Ideally the program should not run. However, if it does, the
  program shall print the value of i, whether it be a system
  initialized value or rubbish left over from procedure q. *)
program t6p2p1d7;
procedure q;
var
   i,j : integer;
begin
   i:=2;
   j:=3
end;
procedure r;
var
   i : integer;
begin

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   writeln('ERROR NOT DETECTED...6.2.1-7: THE VALUE OF I IS ', I)
end;
(* Program body *)
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #038');
   q;
   r
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

