
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 242*)
(*TEST 6.8.3.5-2, CLASS=QUALITY*)
(* This test checks that the case constants are of the same type
  as the case index. A compiler of good quality will detect that
  one path of the case statement cannot be taken and issue a
  warning message.
  The case-index in this test is a subrange and the case-constants
  are of the base type of the subrange. *)
program t6p8p3p5d2;
type
   day=(mon,tue,wed);
var
   a:integer;
   d:mon..tue;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #242');
   for d:=mon to tue do
      case d of
      mon: a:=1;
      tue: a:=2;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

      wed: a:=3;   (* could give a warning *)
      end;
   writeln(' QUALITY TEST - WARNINGS FOR IMPOSSIBLE CASES');
   writeln(' PASS...6.8.3.5-2, CASE CONSTANTS');
end.
   