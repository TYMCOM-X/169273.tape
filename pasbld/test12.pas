program test12 options , fortran, optimize, nocheck;

  external procedure foo options fortran, nooptimize;

  procedure bar options fortran, check, nooptimize;
   begin
     return
   end;

begin
  stop
end.
    