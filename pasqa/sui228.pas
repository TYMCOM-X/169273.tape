
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 228*)
(*TEST 6.7.2.4-3, CLASS=CONFORMANCE*)
(* This program checks the operations of set operators on sets
  of constants and variables. The compiler fails if the program
  does not compile or the program states that this is so. *)
program t6p7p2p4d3;
var
   a,b,c:set of 0..10;
   counter:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #228');
   counter:=0;
   a:=[0,2,4,6,8,10];
   b:=[1,3,5,7,9];
   c:=[0,1,2,3,4,5,6,7,8,9,10];
   if(a+[]=a) then
      counter:=counter+1;
   if(a+b=c) then
      counter:=counter+1;
   if(a+[1,3,5,7,9]=c) then

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      counter:=counter+1;
   if(a-[]=a) then
      counter:=counter+1;
   if(c-a=b) then
      counter:=counter+1;
   if(c-[0,2,4,6,8,10]=b) then
      counter:=counter+1;
   if(a*a=a) then
      counter:=counter+1;
   if(a*[]=[]) then
      counter:=counter+1;
   if(a*b=[]) then
      counter:=counter+1;
   if(a*c=a) then
      counter:=counter+1;
   if(counter=10) then
      writeln(' PASS...6.7.2.4-3 SET OPERATORS')
   else
      writeln('FAIL...6.7.2.4-3 SET OPERATORS');
end.
