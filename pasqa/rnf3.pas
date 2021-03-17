program rnf3 options nocheck;

var ub : packed 0 .. 255;
    uw : packed 0 .. 65535;
    sw : packed -32768..32767;
    sl : integer;

begin
  ub := ub div 2;
  ub := ub div 128;
  ub := ub div 256;
  ub := ub div 1024;

  ub := uw div 2;
  ub := uw div 128;
  ub := uw div 256;
  ub := uw div 1024;

  ub := sw div 2;
  ub := sw div 128;
  ub := sw div 256;
  ub := sw div 1024;

  ub := sl div 2;
  ub := sl div 128;
  ub := sl div 256;
  ub := sl div 1024;

  uw := ub div 2;
  uw := ub div 128;
  uw := ub div 256;
  uw := ub div 1024;

  uw := uw div 2;
  uw := uw div 128;
  uw := uw div 256;
  uw := uw div 1024;

  uw := sw div 2;
  uw := sw div 128;
  uw := sw div 256;
  uw := sw div 1024;

  uw := sl div 2;
  uw := sl div 128;
  uw := sl div 256;
  uw := sl div 1024;

  sw := ub div 2;
  sw := ub div 128;
  sw := ub div 256;
  sw := ub div 1024;

  sw := uw div 2;
  sw := uw div 128;
  sw := uw div 256;
  sw := uw div 1024;

  sw := sw div 2;
  sw := sw div 128;
  sw := sw div 256;
  sw := sw div 1024;

  sw := sl div 2;
  sw := sl div 128;
  sw := sl div 256;
  sw := sl div 1024;

  sl := ub div 2;
  sl := ub div 128;
  sl := ub div 256;
  sl := ub div 1024;

  sl := uw div 2;
  sl := uw div 128;
  sl := uw div 256;
  sl := uw div 1024;

  sl := sw div 2;
  sl := sw div 128;
  sl := sw div 256;
  sl := sw div 1024;

  sl := sl div 2;
  sl := sl div 128;
  sl := sl div 256;
  sl := sl div 1024;

end.
