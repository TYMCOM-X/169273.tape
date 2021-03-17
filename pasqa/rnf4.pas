program rnf4 options nocheck;

(*  This program tests parameter list allocation within a quick block.  *)

type dreal = 0..1 prec 16;

const a : dreal = 0.1;
      b : dreal = 0.2;

function f12 ( x : dreal ) : dreal;
begin
  f12 := x*a + x/b;
end;

function f11 ( x : dreal ) : dreal;
begin
  f11 := x*a + x/b;
end;

function f10 ( x : dreal ) : dreal;
begin
  f10 := x*a + x/b + f12 (x);
end;

function f9 ( x : dreal ) : dreal;
begin
  f9 := x*a + x/b + f11 (x) + f12 (x);
end;

function f8 ( x : dreal ) : dreal;
begin
  f8 := x*a + x/b + f11 (x);
end;

function f7 ( x : dreal ) : dreal;
begin
  f7 := x*a + x/b;
end;

function f6 ( x : dreal ) : dreal;
begin
  f6 := x*a + x/b;
end;

function f5 ( x : dreal ) : dreal;
begin
  f5 := x*a + x/b;
end;

function f4 ( x : dreal ) : dreal;
begin
  f4 := x*a + x/b + f9 (x) + f10 (x);
end;

function f3 ( x : dreal ) : dreal;
begin
  f3 := x*a + x/b + f7 (x) + f8 (x);
end;

function f2 ( x : dreal ) : dreal;
begin
  f2 := x*a + x/b + f5 (x) + f6 (x);
end;

function f1 ( x : dreal ) : dreal;
begin
  f1 := x*a + x/b + f1 (x) + f2 (x) + f3 (x) + f4 (x);
end;

var y, z : dreal;

begin
  y := f1 (z);
end.
