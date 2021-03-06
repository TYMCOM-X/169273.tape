
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  45*)
(*TEST 6.2.2-5, CLASS=CONFORMANCE*)
(* Similarly to 6.2.2-2, labels are allowed to be redefined
  in a range enclosed by the first defining occurrence
  (eg. procedures and functions). This program tests if
  this is permitted by this compiler. *)
program t6p2p2d5;
label
   4,5,6;
var
   i : integer;
procedure redefine;
label
   6,7,8;
var
   j : integer;
begin
      j:=1;
      goto 6;
   7: j:=j-1;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      goto 8;
   6: j:=j+1;
      goto 7;
   8: j:=0;
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #045');
      goto 4;
   5: i:=i+1;
      goto 6;
   4: i:=1;
      redefine;
      goto 5;
   6: if i=1 then
         writeln(' FAIL...6.2.2-5')
      else
         writeln(' PASS...6.2.2-5')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 