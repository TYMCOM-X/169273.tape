
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 216*)
(*TEST 6.7.2.2-4, CLASS=QUALITY*)
(* This program checks that constant and variable operands for DIV
  produce the same result, and if negative operands are permitted. *)
program t6p7p2p2d4;
var
   i, j, k, l, m,counter : integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #216');
   (* The next few statements may cause a run-time error. *)
   writeln(' THIS PROGRAM ATTEMPTS DIVISION WITH NEGATIVE OPERANDS');
   counter := 0;
   j:=2;
   for i:= -10 to 10 do
   begin
      l:=i div j;
      m:= i div 2;
      if (l=m) then counter := counter+1;
      l:=i mod j;
      m:= i mod 2;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      if (l=m) then counter := counter+1;
      if (i-i div 2 * 2 = i mod 2) then counter := counter+1;
   end;
   if counter = 63 then
   begin
      write(' DIVISION INTO NEGATIVE OPERANDS IMPLEMENTED AND ');
      writeln('CONSISTENT');
   end else
      writeln(' INCONSISTENT DIVISION INTO NEGATIVE OPERANDS');
   counter := 0;
   j:=-2;
   for i:= -10 to 10 do
   begin
      l:=i div j;
      m:= i div (-2);
      if (l=m) then counter := counter+1;
      l:=i mod j;
      m:= i mod (-2);
      if (l=m) then counter := counter+1;
   end;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   if counter = 42 then
   begin
      write(' DIVISION BY NEGATIVE OPERANDS IMPLEMENTED AND ');
      writeln('CONSISTENT');
   end else
      writeln(' INCONSISTENT DIVISION BY NEGATIVE OPERANDS');
   i:=-3;
   if (i div 2 = -1) then
      writeln(' QUOTIENT = TRUNC(A/B) FOR NEGATIVE OPERANDS')
   else
      writeln(' QUOTIENT = TRUNC(A/B-1) FOR NEGATIVE OPERANDS');
   if (i mod 2 = 1) then
      writeln(' MOD(A,B) LIES IN (0,B-1)')
   else
      writeln(' MOD RETURNS REMAINDER OF DIV');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

