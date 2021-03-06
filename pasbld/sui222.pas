
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 222*)
(*TEST 6.7.2.3-1, CLASS=CONFORMANCE*)
(* This test checks the operation of the boolean operators.
  The compiler fails if the program does not compile, or the
  program states that this is so. *)
program t6p7p2p3d1;
var
   a,b,c : boolean;
   counter : integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #222');
   counter:=0;
   a:=false;
   b:=false;
   (* OR truth table *)
   if a or b then
      writeln(' FAIL...6.7.2.3-1: OR')
   else
   begin
      b:=true;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      if a or b then
      begin
         a:=true;
         b:=false;
         if a or b then
         begin
            b:=true;
            if a or b then
               counter:=counter+1
            else
               writeln(' FAIL...6.7.2.3-1: OR')
         end
         else
            writeln(' FAIL...6.7.2.3-1: OR')
      end
      else
         writeln(' FAIL...6.7.2.3-1: OR')
   end;
   (* AND truth table *)
   a:=false;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   b:=false;
   if a and b then
      writeln(' FAIL...6.7.2.3-1: AND')
   else
   begin
      b:=true;
      if a and b then
         writeln(' FAIL...6.7.2.3-1: AND')
      else
      begin
         a:=true;
         b:=false;
         if a and b then
            writeln(' FAIL...6.7.2.3-1: AND')
         else
         begin
            b:=true;
            if a and b then
               counter:=counter+1
            else

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

               writeln(' FAIL...6.7.2.3-1: AND')
         end
      end
   end;
   (* NOTE: NOT is sometimes badly implemented by wordwise
           complementation, and for this reason the following
           two tests may fail. *)
   if (not false)=true then
      counter:=counter+1
   else
      writeln(' FAIL...6.7.2.3-1: NOT FALSE');
   if (not true)=false then
      counter:=counter+1
   else
      writeln(' FAIL...6.7.2.3-1: NOT TRUE');
   c:=false;
   a:=true;
   b:=false;
   if (a or b)=(b or a) then
      counter:=counter+1

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   else
      writeln(' FAIL...6.7.2.3-1: COMMUTATION');
   if ((a or b)or c)=(a or(b or c)) then
      counter:=counter+1
   else
      writeln(' FAIL...6.7.2.3-1: ASSOCIATIVITY');
   if (a and(b or c))=((a and b)or(a and c)) then
      counter:=counter+1
   else
      writeln(' FAIL...6.7.2.3-1: DISTRIBUTION');
   if not(a or b)=((not a) and(not b)) then
      counter:=counter+1
   else
      writeln(' FAIL...6.7.2.3-1: DEMORGAN1');
   if not(a and b)=((not a) or (not b)) then
      counter:=counter+1
   else
      writeln(' FAIL...6.7.2.3-1: DEMORGAN2');
   if not(not a)= a then
      counter:=counter+1

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   else
      writeln(' FAIL...6.7.2.3-1: INVERSION');
   if counter=10 then
      writeln(' PASS...6.7.2.3-1')
end.
    