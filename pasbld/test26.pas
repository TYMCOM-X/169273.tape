program test26 options dump;

 var f: file of 0..4;
 var t: text;
 var v: 0..6;

begin
 break;
 break (t);
 page;
 page (t);
 page (f);
 close;
 close (f);
 close (v);
 get;
 get (f);
 put;
 put (f);
 open;
 open (f);
 open (f, 'abcde');
 open (f, 2);
 reset (f, 'a');
 rewrite (f, 'abc');
 rewrite (f, 'abc', true);
 rewrite (f, 'abc', 3);
 rewrite (f, 'abc', 'a');
end.
    