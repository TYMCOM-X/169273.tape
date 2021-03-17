begin "tst"
require "dmath.hdr" source!file;
define !="comment", crlf="('15&'12)";
require "{}{}" delimiters;
integer expon,low,up,i;

print("dfix/dfloat");
for expon_ 0 step 1 until 34
   do begin
	up_1 lsh expon + 20;
	low_-up;
	for i_ up-40 step 1 until up, low+40 step -1 until low
	 do if i neq dfix(dfloat(i))
	 then begin
		print(crlf& "dfix(dfloat(", i, "))=dfix(", dfloat(i),
			")= ", dfix(dfloat(i)) );
		done;
	      end;
      end;

define ipart(fint,fract,ran,type)={
 print(" fint/fract ");
 for i_0 step 1 until 500
   do begin type v,fr,fi;
	if i>30
	 then begin 
		expon_ran*127; 
		v_ran*2.^expon;
		if ran>.5 then v_-v;
	      end
	 else v_(i-10)/6@@0;
	fr_fract(v);fi_fint(v);
	if fr+fi=v 
	and ((v leq 0 and -1 < fr leq 0)
	     or	(v>0 and 1 > fr geq 0))
	 then else print(crlf, v,"=",fi, "+", fr );
      end
};
ipart(fint,fract,ran,real);
ipart(dfint,dfract,dran,long real);


defIne minmax(fint,fract,floor,ceiling,ran,type)={
 print(" floor/ceiling ");
 for i_0 step 1 until 500
   do begin type v,fr,fi, fl,cl;
	expon_ran*127;
	v_ran * 2.^expon; if ran>.5 then v_-v; 
	fr_ fract(v); fi_ fint(v); fl_floor(v); cl_ceiling(v);
	if (cl=fi or fl=fi) and (fl leq v leq cl)
	 then else print(crlf, fl," leq ", v, " leq ", cl );
      end
};
minmax(fint,fract,floor,ceil,ran,real);
minmax(dfint,dfract,dfloor,dceil,dran,long real);

end $


