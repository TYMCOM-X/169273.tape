program test4;

  type
    ppn = packed record
	    proj: 0..777777B;
	    prog: 0..777777B
	  end;

    t1 = 0..1;
    t2 = 0..2;
    t3 = 0..31;
    t4 = 0..63;

    nt1 = -1..7;
    nt2 = -2..7;
    nt3 = -3..7;
    nt7 = -7..7;
    nt8 = -8..7;
    nt9 = -9..7;

     protection = packed array [1..3] of 0..7;

     xyz = packed record
		f1: 0..777777B;
	        f2: t4;
	        f3: ppn;
	        f4: t3;
	        a, b, c, d: t2;
	        pro: protection;
	        posint: 0..4;
	        arr: array[1..6] of char;
	        fstr: packed array[1..7] of char;
	        str: string[32];
		c1: char;
	        c2: char;
		c3: char;
	        sval: set of 1..12;
	        subrec:
		  packed record
		    s1: t3;
		    s2: t4
		  end;
		case tag: boolean of
		  true: ( x: ppn;
			  x2: char  );
		  false: ( x3: t2 )
	   end;

begin 
stop
end.
   