
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 127*)
(*TEST 6.4.6-10, CLASS=DEVIANCE*)
(* The Pascal Standard states that the two types T1 and T2
  (in determining assignment compatibility) must neither be a
  a file type nor a structured type with a file component.
  This program tests the first part of this statement.
  The compiler conforms if the program does not compile. *)
program t6p4p6d10;
var
   file1 : text;
   file2 : text;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #127');
   reset(file1);
   rewrite(file2);
   writeln(file1,'ABC');
   file2:=file1;
   writeln(' DEVIATES...6.4.6-10')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

