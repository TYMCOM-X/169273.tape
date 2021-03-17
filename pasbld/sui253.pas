
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 253*)
(*TEST 6.8.3.5-13, CLASS=DEVIANCE*)
(* Similar to test 6.8.3.5-12, this test checks the subrange
  case extension, which may not be safely implemented. This program
  is utter confused garbage and indicates the kinds of checks
  needed. The compiler deviates if the program compiles and
  prints DEVIATES. *)
program t6p8p3p5d13;
var
   t,thing:(a,b,c,d,e,f,g,h);
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #253');
   for thing:=a to g do begin
      case thing of
      a..e: t:=thing;
      d..g: t:=succ(thing);
      b:    t:=pred(thing)
      end
   end;
   writeln(' DEVIATES...6.8.3.5-13, CASE CONSTANTS')

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end.
