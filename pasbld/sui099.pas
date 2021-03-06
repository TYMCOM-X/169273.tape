
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number  99*)
(*TEST 6.4.3.5-2, CLASS=CONFORMANCE*)
(* The Pascal Standard provides for a predefined filetype, type
  TEXT. Variables of type TEXT are called TEXT FILES. This program
  tests that such a type is permitted and that the type adheres
  to the structure laid down in the Standard. The compiler fails
  if the program will not compile and run. *)
program t6p4p3p5d2;
var
  file1 : text;
  chare : char;
procedure ahaa;
begin
   writeln(' FAIL...6.4.3.5-2')
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #099');
   rewrite(file1);
   writeln(file1);                  (* no characters, but a linemarker*)
   writeln(file1,'ABC');            (* characters and linemarker*)

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   reset(file1);
   if eoln(file1) then get(file1)
   else ahaa;
   if file1^='A' then get(file1)
   else ahaa;
   if file1^='B' then get(file1)
   else ahaa;
   if file1^='C' then get(file1)
   else ahaa;
   if eoln(file1) and (file1^=' ') then get(file1)
   else ahaa;
   if eof(file1) then
      writeln(' PASS...6.4.3.5-2')
   else ahaa
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   