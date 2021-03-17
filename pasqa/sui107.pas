
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 107*)
(*TEST 6.4.5-3, CLASS=DEVIANCE*)
(* This test is similar to 6.4.5-2, except that deviance in the
  case of arrays is tested.
  The program should not compile/execute if the compiler
  conforms. *)
program t6p4p5d3;
type
   urrayone = array[1..10] of char;
   urraytwo = array[1..10] of char;
var
   arrayone : urrayone;
   arraytwo : urraytwo;
procedure test(var urray : urrayone);
begin
   writeln(' DEVIATES...6.4.5-3')
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #107');
   (* The two arraytypes, urrayone and urraytwo, are not identical

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

     and hence the call to TEST should fail. *)
   test(arraytwo)
end.
