
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 240*)
(*TEST 6.8.3.4-1 CLASS=CONFORMANCE*)
(* This test checks a nested if statement whose syntax is apparently
  ambiguous. The compiler fails if the program does not compile
  or the program states this by writing FAIL. *)
program t6p8p3p4d1;
const
   off=false;
var
   b:boolean;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #240');
   for b:=false to true do
      begin
      if b then
         if off then
            writeln(' FAIL...6.8.3.4-1')
         else
            begin
            if not b then

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

                writeln(' FAIL...6.8.3.4-1')
            else
               writeln(' PASS...6.8.3.4-1');
            end;
      end;
end.
