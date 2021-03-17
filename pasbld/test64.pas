program test64 options dumpst, dumpif;

(*  Test of calls to procedures with forward/undefined parameter types.  *)

external procedure p1 ( t1; var t2; t3 );

external procedure p2 ( t1 );

external procedure p3 ( t4; var t5 );

type
    t2 = 0 .. 1;
    t3 = 0 .. 1;

external procedure p4 ( t2 );

var
    i, j, k: 0 .. 1;

begin
    p1 ( 0, i, 1 );
    p1 ( 0, 1, i );
    p2 ( 0 );
    p3 ( 0, 1 );
    p3 ( 0, i );
    p4 ( 1 );
    p4 ( true );
end.
