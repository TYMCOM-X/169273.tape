program test27 options dump;

 var i: 0..4;
 var b: boolean;
 var c: char;
 var s: string;

 var f: file of boolean;
 var t: text;
 var a: array[1..4] of 0..4;

begin
 readln;
 readln (f);
 readln (t);
 read (i, b, c);
 readln (f, i);
 read (t, s);
 read (t, a);
 read;
 read ();
 read (f);
 read (t);
 read (t, 2+3);
end.
  