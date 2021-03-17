
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 185*)
(*TEST 6.6.6.1-1, CLASS=IMPLEMENTATIONDEPENDENT*)
(* The Pascal Standard does not state what action takes place
  when a standard function is used as a functional
  parameter. The effect is implementation dependent.
  This program uses a standard function as a parameter to a
  procedure. The compiler may reject this as an error, or
  may permit it as it should other functional parameters. *)
program t6p6p6p1d1;
procedure quidnunk(function a(b : integer):boolean);
   var
      x : integer;
   y : boolean;
   begin
      x:=5;
      y:=a(x);
      if x=1 then
         writeln(' STANDARD FUNCTIONS PERMITTED AS PARAMETERS',
                  '...6.6.6.1-1')
      else

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

         writeln(' STANDARD FUNCTIONS NOT PERMITTED AS ',
                  'PARAMETERS...6.6.6.1-1')
   end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #185');
   quidnunk(odd)
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

