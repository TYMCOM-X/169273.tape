program test61 options dumpif, dumpst;

type
    s= string [40];
    s1= packed array [1..60] of char;
    s2= packed array [1..*] of char;

    a= array [1..10] of integer;
    a1= array [1..10,11..20,21..30] of integer;
    a2= array [1..*,2..20] of integer;
    a3= array [*] of integer;

var
    n: integer;

begin
  n := lowerbound(s);
  n := lowerbound (s1);
  n := lowerbound(s2);

  n := upperbound(s);
  n := upperbound(s1);
  n := upperbound(s2);

  n := dimension(s);
  n := dimension(s1);
  n := dimension(s2);

  n := lowerbound(s,0);

  n := lowerbound(a);
  n := lowerbound(a1);
  n := lowerbound(a2);
  n := lowerbound(a3);

  n := upperbound(a);
  n := upperbound(a1);
  n := upperbound(a2);
  n := upperbound(a3);

  n := dimension(a);
  n := dimension(a1);
  n := dimension(a2);
  n := dimension(a3);

  n := lowerbound(a1,3);
  n := lowerbound(a1,4);
  n := upperbound(a2,2);
  n := dimension(a3,1);
  n := dimension(a,0);

end.
    