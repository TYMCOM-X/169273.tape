
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 299*)
(*TEST 6.9.4-6, CLASS=CONFORMANCE*)
(* This test checks that strings are correctly written onto a text
  file. The compiler fails if the program does not compile or
  the program prints FAIL. *)
program t6p9p4d6;
var
f:text;
i,j,k,counter:integer;
c:char;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #299');
   rewrite(f);
   counter:=0;
   for i := 1 to 10 do
      writeln(f,'AAAAA':i,'B':1);
   writeln(f,'BBBBB','C':1);
   reset(f);
   for i:=1 to 10 do
   begin

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      for j:=6 to i do begin
         read(f,c);
         if (c=' ') then
            counter:=counter+1;
      end;
      if (i>5) then k:=5 else k:=i;
      for j:=1 to k do
      begin
         read(f,c);
         if(c='A') then
            counter:=counter+1;
      end;
      read(f,c);
      if (c='B') then
         counter:=counter+1;
      readln(f);
   end;
   for i:=1 to 5 do
   begin
      read(f,c);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      if (c='B') then
         counter:=counter+1;
   end;
   read(f,c);
   if (c='C') then
      counter:=counter+1;
   if(counter=71) then
      writeln(' PASS...6.9.4-6, WRITE STRINGS')
   else
      writeln(' FAIL...6.9.4-6, WRITE STRINGS');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

