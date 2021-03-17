
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 199*)
(*TEST 6.6.6.3-3, CLASS=ERRORHANDLING*)
(* This program causes an error to occur as the result
  returned by the round function is not a value of the
  type integer.
  The error should be detected at run-time. *)
program t6p6p6p3d3;
var
   reel : real;
   i    : integer;
   ok   : boolean;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #199');
   reel:=11111.11111;
   ok:=true;
   while ok do
   begin
      i:=round(reel);
      if (i<0) then
         ok:=false

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      else
         reel:=reel*2
   end;
   writeln(' ERROR NOT DETECTED...6.6.6.3-3')
end.
