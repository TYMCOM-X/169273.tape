
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 183*)
(*TEST 6.6.5.3-9, CLASS=ERRORHANDLING*)
(* This test is similar to 6.6.5.3-7, except that the
  variable created is used as an actual parameter.
  The error should be detected by the compiler or at
  run-time. *)
program t6p6p5p3d9;
type
   two      = (a,b);
   rekord   = record
               case tagfield:two of
                  a : (m : boolean);
                  b : (n : char)
              end;
var
   ptr : ^rekord;
procedure error(c : rekord);
   begin
      writeln(' ERROR NOT DETECTED...6.6.5.3-9')
   end;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #183');
   new(ptr,a);
   ptr^.m:=true;
   error(ptr^)
end.
   