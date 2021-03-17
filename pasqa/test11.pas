program test11;

type color = (red, green, yellow, blue, orange);

type a1 = packed array [1..4] of char;

type a2 = array [color, 1..3] of boolean;

type a3 = array [2..4, 3..5] of array [4..6] of color;


type r1 =
    packed record
	a,b: color;
	c: char;
	b: (mon, tues, wed)
    end;

type r2 =
   record
     a, b: color;
     case t: color of

       red..yellow, blue, orange: (  x: char;
		      y: char  );

       green:	   (  z: boolean );

       others:	   (  str: packed array[1..5] of char;
		      b2: boolean )
   end;

begin
  stop
end.
