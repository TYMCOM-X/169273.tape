
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 105*)
(*TEST 6.4.5-1, CLASS=CONFORMANCE*)
(* The Pascal Standard states that types designated at two or more
  different places in the program text are identical if the same
  type identifier is used at these places, or if different identifiers
  are used which have been defined to be equivalent to each other.
  This program simply tests that the compiler conforms to the
  Standard's  description of identity.
  The compiler fails if the program does not compile. *)
program t6p4p5d1;
type
   t1 = array[1..5] of boolean;
   t2 = t1;
   t3 = t2;
var
   a : t1;
   b : t2;
   c : t3;
procedure identical(var a : t1; var b : t2; var c : t3);
begin

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   a[1]:=true;
   b[1]:=false;
   c[1]:=true
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #105');
   a[1]:=true;
   b[1]:=false;
   c[1]:=false;
   identical(a,b,c);
   identical(c,a,b);
   identical(b,c,a);
   writeln(' PASS...6.4.5-1')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

