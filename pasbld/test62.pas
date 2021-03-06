program test62 options dumpif, dumpst;

type set1 = set of 1 .. 10;
     set2 = set of 6 .. 15;

var
     s1: set1;
     s2: set2;
     n : integer;

begin
  n := lowerbound(set1);
  n := upperbound(set1);
  n := dimension(set1);

  n := lowerbound(s2);
  n := upperbound(s2);
  n := dimension(s2);

  n := lowerbound([3,5]);
  n := upperbound([3,5]);

  n := lowerbound(s1+s2);
  n := upperbound(s1+s2);

  n := lowerbound(s1*s2);
  n := upperbound(s1*s2);
end.
    