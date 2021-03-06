
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 116*)
(*TEST 6.4.5-12, CLASS=CONFORMANCE*)
(* If two types are declared equivalent, they inherit all properties
  in common, including operators and special attributes. This
  is clecked by an analogue of type boolean. The compiler passes
  if the program compiles and prints PASS. *)
program t6p4p5d12;
const
   on_=true;
   off=false;
type
   logical=boolean;
var
   test:integer;
   b1,b2:boolean;
   l1,l2:logical;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #116');
   test:=0;
   b1:=true;   b2:=off;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   l1:=true;   l2:=off;
   if l2 then test:=test+1;
   l2:=b2;
   if b1=b2 then test:=test+1;
   b2:=l2;
   if b2 or l2 then test:=test+1;
   if test=0 then
      writeln(' PASS...6.4.5-12, TYPES')
   else
    writeln(' FAIL...6.4.5-12, TYPES')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

  