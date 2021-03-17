
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 157*)
(*TEST 6.6.3.3-3, CLASS=CONFORMANCE*)
(* If the variable passed as a parameter involves the indexing
  of an array, or the dereferencing of a pointer, then these
  actions are executed before the activation of the block.
  The compiler fails if the program does not compile or the
  program states that this is so. *)
program t6p6p3p3d3;
type
   rekord = record
               a : integer;
               link : ^rekord;
               back : ^rekord
            end;
var
   urray : array[1..2] of integer;
   i     : integer;
   temptr,ptr : ^rekord;
procedure call(arrayloctn : integer;
               ptrderef : integer);

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   begin
      i:=i+1;
      ptr:=ptr^.link;
      if (urray[i-1] <> arrayloctn) or
         (ptr^.back^.a <> ptrderef) then
         writeln(' FAIL...6.6.3.3-3')
      else
         writeln(' PASS...6.6.3.3-3')
   end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #157');
   urray[1]:=1;
   urray[2]:=2;
   i:=1;
   new(ptr);
   ptr^.a:=1;
   new(temptr);
   temptr^.a:=2;
   ptr^.link:=temptr;
   temptr^.back:=ptr;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   call(urray[i],ptr^.a)
end.
