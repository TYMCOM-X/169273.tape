
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 239*)
(*TEST 6.8.2.4-4,CLASS=DEVIANCE*)
(* This test checks that a goto statement causes an error
  when the statement(S) to which control is transferred is
  not activated either by S or a statement in the statement
  sequence of which S is an immediate constituent.
  The compiler deviates if the compiler prints DEVIATES. *)
program t6p8p2p4d4;
var
   flag:boolean;
procedure a(i:integer;b:boolean);
label 99;
   procedure r;
   begin
      goto 99;
   end;
begin
   case i of
   0:99: if (b) then
         writeln(' DEVIATES...6.8.2.4-4')

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      else
         if flag then
            writeln(' PASS...6.8.2.4-4')
         else begin
            flag := true;
            a(1,false);
         end;
   1:
      a(2,true);
   2:
      r;
   end;
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #239');
   flag := false;
   a(0,false);
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

