program rnf2 options nocheck;

var ub : packed 0 .. 255;
    uw : packed 0 .. 65535;
    sw : packed -32768..32767;
    sl : integer;

begin
  ub := ub div ub;
  ub := ub div uw;
  ub := ub div sw;
  ub := ub div sl;

  ub := uw div ub;
  ub := uw div uw;
  ub := uw div sw;
  ub := uw div sl;

  ub := sw div ub;
  ub := sw div uw;
  ub := sw div sw;
  ub := sw div sl;

  ub := sl div ub;
  ub := sl div uw;
  ub := sl div sw;
  ub := sl div sl;

  uw := ub div ub;
  uw := ub div uw;
  uw := ub div sw;
  uw := ub div sl;

  uw := uw div ub;
  uw := uw div uw;
  uw := uw div sw;
  uw := uw div sl;

  uw := sw div ub;
  uw := sw div uw;
  uw := sw div sw;
  uw := sw div sl;

  uw := sl div ub;
  uw := sl div uw;
  uw := sl div sw;
  uw := sl div sl;

  sw := ub div ub;
  sw := ub div uw;
  sw := ub div sw;
  sw := ub div sl;

  sw := uw div ub;
  sw := uw div uw;
  sw := uw div sw;
  sw := uw div sl;

  sw := sw div ub;
  sw := sw div uw;
  sw := sw div sw;
  sw := sw div sl;

  sw := sl div ub;
  sw := sl div uw;
  sw := sl div sw;
  sw := sl div sl;

  sl := ub div ub;
  sl := ub div uw;
  sl := ub div sw;
  sl := ub div sl;

  sl := uw div ub;
  sl := uw div uw;
  sl := uw div sw;
  sl := uw div sl;

  sl := sw div ub;
  sl := sw div uw;
  sl := sw div sw;
  sl := sw div sl;

  sl := sl div ub;
  sl := sl div uw;
  sl := sl div sw;
  sl := sl div sl;

end.
   