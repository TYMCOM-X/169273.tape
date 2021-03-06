
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  56*)
(*TEST 6.3-6, CLASS=DEVIANCE*)
(* A constant may not be used in its own declaration - the
  following is a pathological case which should be detected
  or at least handled with care. *)
program t6p3d6;
const
   ten = 10;
procedure p;
const
   ten = ten;
begin
   if ten=10 then
      writeln(' DEVIATES...6.3-6: SCOPE ERROR')
   else
      writeln(' DEVIATES...6.3-6: DEFINITION POINT ERROR')
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #056');
   p

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end.
    