
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 295*)
(*TEST 6.9.4-2, CLASS=CONFORMANCE*)
(* This test checks that the default value for the field width of a
  character type is one. The compiler fails if the program does not
  compile or the program prints FAIL. *)
program t6p9p4d2;
var
   f:text;
   a,b:char;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #295');
   rewrite(f);
   a:='A';
   b:='B';
   writeln(f,a,b);
   reset(f);
   read(f,a,b);
   if (a='A') and (b='B') then
      writeln(' PASS...6.9.4-2, WRITE')
   else

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      write(' FAIL...6.9.4-2, WRITE');
end.
    