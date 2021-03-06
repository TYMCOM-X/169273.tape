
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 257*)
(*TEST 6.8.3.7-3, CLASS=CONFORMANCE*)
(* This test clecks that an apparently infinite loop is allowed
  by the compiler. Some compilers may detect the loop as being
  infinite. The compiler fails if the program does not compile. *)
program t6p8p3p7d3;
label
   100;
const
   eternity = false;
var
   i:integer;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #257');
   i:=0;
   repeat
      i:=i+1;
      if (i>50) then
         goto 100;
   until eternity;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

100:
   writeln(' PASS...6.8.3.7-3, REPEAT');
end.
 