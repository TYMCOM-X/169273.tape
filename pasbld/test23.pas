program test23 options dump;

 var i: 0..1000;

begin
  case i of
    1..10: stop;
    4: stop;
    12: stop;
  end;
  case i of
    1: stop;
    'A': stop;
    others: stop;
    others: stop
  end;
  case i of
    4..6: stop;
    1,2,7: stop;
    30..32,20, 22: return;
    12: ;
    others: writeln
  end;
end.
   