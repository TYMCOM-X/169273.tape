$TITLE GENLIB

program genlib
   options special(ptr,coe);

(* Generate the the math test library.  This file will contain
   a function name, arguments, and expected result.  The file will be
   read in by the program MTHTST which checks the result in the file against
   the result from the function implementation being tested.  The
   file contains random arguments to test IADD, ISUB, IMULT, IDIV, IMOD
   IPOWER, FADD, FSUB, FMULT, FDIV, FPOWER, SQROOT, EXP, NATLOG, LOG10,
   SIN, COS, TAN, ARCSINE, ARCCOSINE, ARCTANGENT, SINH, COSH,
   TANH.  The values are generated using the VAX FORTRAN high precision
   math library.                                                        *)

(*  Conversion routines for d_real to h_real and strings to numbers  *)

$INCLUDE HEXR.INC

(*  FORTRAN library procedures *)

$INCLUDE HFCTN.INC

var
   h_arg1:h_real;
   h_arg2:h_real;
   h_rslt:h_real;
   h_temp:h_real;
   d_temp:d_real;
   i:integer;
   i_arg1:integer;
   i_arg2:integer;
   i_rslt:integer;
   strg_arg1:string[70];
   strg_arg2:string[70];
   strg_rslt:string[70];
   sign:integer;
   arg1_neg:boolean;
   arg2_neg:boolean;
   h_pi:h_real;
   matlib:text;

procedure special_tests;
begin

(* special case tests *)

(* test addition (smaller addend first, larger first, all combinations
   of sign *)
   d_temp:=5.0;
   h_arg1:=realdh(d_temp);
   d_temp:=1.1;
   h_arg2:=realdh(d_temp);
   mth_hadd(h_rslt,h_arg1,h_arg2);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FADD ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
   writeln(matlib,'FADD ',strg_arg2);
   writeln(matlib,'   ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
   d_temp:=-5.0;
   h_arg1:=realdh(d_temp);
   mth_hadd(h_rslt,h_arg1,h_arg2);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FADD ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
   writeln(matlib,'FADD ',strg_arg2);
   writeln(matlib,'   ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
   d_temp:=-1.1;
   h_arg2:=realdh(d_temp);
   mth_hadd(h_rslt,h_arg1,h_arg2);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FADD ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
   writeln(matlib,'FADD ',strg_arg2);
   writeln(matlib,'   ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
   d_temp:=5.0;
   h_arg1:=realdh(d_temp);
   mth_hadd(h_rslt,h_arg1,h_arg2);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FADD ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
   writeln(matlib,'FADD ',strg_arg2);
   writeln(matlib,'   ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);

(* test multiplication using combinations of differant sign mantissas,
   differant sign exponents, differant size exponents *)
   d_temp:=20.0;
   h_arg1:=realdh(d_temp);
   d_temp:=0.0;
   h_arg2:=realdh(d_temp);
   mth_hmult(h_rslt,h_arg1,h_arg2);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FMULT ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
   writeln(matlib,'FMULT ',strg_arg2);
   writeln(matlib,'   ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
   d_temp:=1.0;
   h_arg2:=realdh(d_temp);
   mth_hmult(h_rslt,h_arg1,h_arg2);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FMULT ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
   writeln(matlib,'FMULT ',strg_arg2);
   writeln(matlib,'   ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
   d_temp:=16.0;
   h_arg1:=realdh(d_temp);
   mth_hmult(h_rslt,h_arg1,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FMULT ',strg_arg1);
   writeln(matlib,'   ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
   d_temp:=-16.0;
   h_arg2:=realdh(d_temp);
   mth_hmult(h_rslt,h_arg1,h_arg2);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FMULT ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
   writeln(matlib,'FMULT ',strg_arg2);
   writeln(matlib,'   ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
   d_temp:=-16.0;
   h_arg1:=realdh(d_temp);
   mth_hmult(h_rslt,h_arg1,h_arg2);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FMULT ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
   d_temp:=1600.0;
   h_arg1:=realdh(d_temp);
   d_temp:=0.16;
   h_arg2:=realdh(d_temp);
   mth_hmult(h_rslt,h_arg1,h_arg2);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FMULT ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
   writeln(matlib,'FMULT ',strg_arg2);
   writeln(matlib,'   ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);


(* check the same sort of things for division as for multiplication *)
   d_temp:=0.0;
   h_arg1:=realdh(d_temp);
   d_temp:=6.0;
   h_arg2:=realdh(d_temp);
   mth_hdiv(h_rslt,h_arg1,h_arg2);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FDIV ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
   d_temp:=1.0;
   h_arg1:=realdh(d_temp);
   mth_hdiv(h_rslt,h_arg1,h_arg2);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FDIV ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
   mth_hdiv(h_rslt,h_arg2,h_arg1);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FDIV ',strg_arg2);
   writeln(matlib,'   ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
   d_temp:=16.0;
   h_arg1:=realdh(d_temp);
   mth_hdiv(h_rslt,h_arg1,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FDIV ',strg_arg1);
   writeln(matlib,'   ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
   d_temp:=15.0;
   h_arg2:=realdh(d_temp);
   mth_hdiv(h_rslt,h_arg1,h_arg2);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FDIV ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
   d_temp:=-1024.0;
   h_arg1:=realdh(d_temp);
   d_temp:=16.0;
   h_arg2:=realdh(d_temp);
   mth_hdiv(h_rslt,h_arg1,h_arg2);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   mth_hdiv(h_rslt,h_arg2,h_arg1);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FDIV ',strg_arg2);
   writeln(matlib,'   ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
   d_temp:=-0.16;
   h_arg2:=realdh(d_temp);
   mth_hdiv(h_rslt,h_arg1,h_arg2);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FDIV ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
   d_temp:=0.0256;
   h_arg1:=realdh(d_temp);
   mth_hdiv(h_rslt,h_arg1,h_arg2);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FDIV ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);

(* do a couple of tests of pwr and pwri *)
   d_temp:=25.2;
   h_arg1:=realdh(d_temp);
   d_temp:=0;
   h_arg2:=realdh(d_temp);
   mth_hpwr(h_rslt,h_arg1,h_arg2);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FPOWER ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
   d_temp:=1.0;
   h_arg2:=realdh(d_temp);
   mth_hpwr(h_rslt,h_arg1,h_arg2);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FPOWER ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
   d_temp:=-1.0;
   h_arg2:=realdh(d_temp);
   mth_hpwr(h_rslt,h_arg1,h_arg2);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FPOWER ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);

(* do a few special tests for log, ln, and exp *)
   d_temp:=1.0;
   h_arg1:=realdh(d_temp);
   mth$hexp(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'EXPON ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
   writeln(matlib,'NATLOG ',strg_rslt);
   writeln(matlib,'   ',strg_arg1);
   d_temp:=0.0;
   h_arg1:=realdh(d_temp);
   mth$hexp(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'EXPON ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
   writeln(matlib,'NATLOG ',strg_rslt);
   writeln(matlib,'   ',strg_arg1);
   d_temp:=-1.0;
   h_arg1:=realdh(d_temp);
   mth$hexp(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'EXPON ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
   writeln(matlib,'NATLOG ',strg_rslt);
   writeln(matlib,'   ',strg_arg1);
   d_temp:=10.0;
   h_arg1:=realdh(d_temp);
   mth$hlog10(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'LOG10 ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
   d_temp:=1.0;
   h_arg1:=realdh(d_temp);
   mth$hlog10(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'LOG10 ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
   d_temp:=0.1;
   h_arg1:=realdh(d_temp);
   mth$hlog10(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'LOG10 ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);


end;


begin

rewrite(matlib);
rewrite(tty);

writeln(tty,'begin genlib');

special_tests;

d_temp:=random(3.2);       (* initialize random *)

(* generate sine arguments and results *)
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=6.2824;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   mth$hsin(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'SINE ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=200.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   d_temp:=100.0;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg1,h_arg1,h_temp);
   mth$hsin(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'SINE ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;

(*  generate arguments for cosine function *)
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=6.2824;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   mth$hcos(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'COSINE ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=200.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   d_temp:=100.0;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg1,h_arg1,h_temp);
   mth$hcos(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'COSINE ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;

(* generate arguments for tangent tests *)
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=6.2824;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   mth$htan(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'TANGENT ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=200.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   d_temp:=100.0;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg1,h_arg1,h_temp);
   mth$htan(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'TANGENT ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;


(* generate arguments for arcsine test *)
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   mth$hasin(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'ARCSINE ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;

(* generate arguments for arccosine test *)
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   mth$hacos(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'ARCCOSINE ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;

(* generate arguments for arctangent test *)
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=10.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   d_temp:=5.0;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg1,h_arg1,h_temp);
   mth$hatan(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'ARCTANGENT ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=200.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   d_temp:=100.0;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg1,h_arg1,h_temp);
   mth$hatan(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'ARCTANGENT ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;

(*generate arguments for atan2 test *)
d_temp:=-1.0;
h_temp:=realdh(d_temp);
mth$hacos(h_pi,h_temp);
for i:=1 to 100 do
begin
   arg1_neg:=false;
   arg2_neg:=false;
   d_temp:=random;
   if (d_temp < 0.5) then arg1_neg:=true;
   h_arg1:=realdh(d_temp);
   d_temp:=20.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   d_temp:=10.0;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg1,h_arg1,h_temp);
   d_temp:=random;
   if (d_temp = 0.0) then d_temp:=1.0;
   if (d_temp < 0.5) then arg2_neg:=true;
   h_arg2:=realdh(d_temp);
   d_temp:=20.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg2,h_arg2,h_temp);
   d_temp:=10.0;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg2,h_arg2,h_temp);
   mth_hdiv(h_temp,h_arg1,h_arg2);
   mth$hatan(h_rslt,h_temp);
   if arg2_neg then
      if arg1_neg then mth_hadd(h_rslt,h_rslt,h_pi)
      else mth_hsub(h_rslt,h_rslt,h_pi);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'ARCTAN2 ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
end;

(* generate arguments for natlog test *)
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=10.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   mth$hlog(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'NATLOG ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=1000.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   mth$hlog(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'NATLOG ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;

(* generate arguments for log10 test *)
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=10.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   mth$hlog10(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'LOG10 ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=10000.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   mth$hlog10(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'LOG10 ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;

(* generate arguments for sqroot *)
for i:=1 to 100 do
begin
   d_temp:=random;
arg1:=realdh(d_temp);
   d_temp:=10.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   mth$hsqrt(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'SQROOT ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=1000.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   mth$hsqrt(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'SQROOT ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;

(* generate arguments for exp test *)
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=2.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   d_temp:=1.0;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg1,h_arg1,h_temp);
   mth$hexp(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'EXPON ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=40.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   d_temp:=20.0;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg1,h_arg1,h_temp);
   mth$hexp(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'EXPON ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;

(* generate arguments for fpower test *)
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=10;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   d_temp:=random;
   if (d_temp = 0.0) then d_temp:=1.0;
   h_arg2:=realdh(d_temp);
   d_temp:=2.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg2,h_arg2,h_temp);
   d_temp:=1.0;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg2,h_arg2,h_temp);
   mth_hpwr(h_rslt,h_arg1,h_arg2);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FPOWER ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
end;
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=10.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   d_temp:=random;
   if (d_temp = 0.0) then d_temp:=1.0;
   h_arg2:=realdh(d_temp);
   d_temp:=30.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg2,h_arg2,h_temp);
   d_temp:=15.0;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg2,h_arg2,h_temp);
   mth_hpwr(h_rslt,h_arg1,h_arg2);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FPOWER ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
end;

(* generate arguments for fipower test *)
for i:=1 to 100 do
begin
   d_temp:=random;
   if (d_temp = 0.0) then d_temp:=1.0;
   h_arg1:=realdh(d_temp);
   d_temp:=20.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   d_temp:=10.0;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg1,h_arg1,h_temp);
   i_arg2:=round(random * 15);
   if (i_arg2 = 0) then i_arg2:=15;
   mth_hpwri(h_rslt,h_arg1,i_arg2);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FIPOWER ',strg_arg1);
   writeln(matlib,'   ',i_arg2);
   writeln(matlib,'   ',strg_rslt);
end;

(* generate arguments for sinh test *)
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=40;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   d_temp:=20;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg1,h_arg1,h_temp);
   mth$hsinh(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'HSINE ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;

(* generate arguments for cosh test *)
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=40;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   d_temp:=20;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg1,h_arg1,h_temp);
   mth$hcosh(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'HCOSINE ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;

(* generate arguments for tanh test *)
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=40;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   d_temp:=20;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg1,h_arg1,h_temp);
   mth$htanh(h_rslt,h_arg1);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'HTANGENT ',strg_arg1);
   writeln(matlib,'   ',strg_rslt);
end;

(* generate arguments for fadd test *)
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=2000.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   d_temp:=1000.0;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg1,h_arg1,h_temp);
   d_temp:=random;
   h_arg2:=realdh(d_temp);
   d_temp:=2000.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg2,h_arg2,h_temp);
   d_temp:=1000.0;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg2,h_arg2,h_temp);
   mth_hadd(h_rslt,h_arg1,h_arg2);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FADD ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
end;

(* generate arguments for fsub test *)
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=2000.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   d_temp:=1000.0;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg1,h_arg1,h_temp);
   d_temp:=random;
   h_arg2:=realdh(d_temp);
   d_temp:=2000.0;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg2,h_arg2,h_temp);
   d_temp:=1000.0;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg2,h_arg2,h_temp);
   mth_hsub(h_rslt,h_arg1,h_arg2);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FSUB ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
end;

(* generate arguments for fmult test *)
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=2000;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   d_temp:=1000;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg1,h_arg1,h_temp);
   d_temp:=random;
   h_arg2:=realdh(d_temp);
   d_temp:=2000;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg2,h_arg2,h_temp);
   d_temp:=1000;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg2,h_arg2,h_temp);
   mth_hmult(h_rslt,h_arg1,h_arg2);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FMULT ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
end;

(* generate arguments for fdiv test *)
for i:=1 to 100 do
begin
   d_temp:=random;
   h_arg1:=realdh(d_temp);
   d_temp:=2000;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg1,h_arg1,h_temp);
   d_temp:=1000;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg1,h_arg1,h_temp);
   d_temp:=random;
   if (d_temp = 0.0) then d_temp:=1.0;
   h_arg2:=realdh(d_temp);
   d_temp:=2000;
   h_temp:=realdh(d_temp);
   mth_hmult(h_arg2,h_arg2,h_temp);
   d_temp:=1000;
   h_temp:=realdh(d_temp);
   mth_hsub(h_arg2,h_arg2,h_temp);
   mth_hdiv(h_rslt,h_arg1,h_arg2);
   strg_arg1:=realhex(address(h_arg1),h_format);
   strg_arg2:=realhex(address(h_arg2),h_format);
   strg_rslt:=realhex(address(h_rslt),h_format);
   writeln(matlib,'FDIV ',strg_arg1);
   writeln(matlib,'   ',strg_arg2);
   writeln(matlib,'   ',strg_rslt);
end;

writeln(tty,'end genlib');
end.
 
 n5eÏ