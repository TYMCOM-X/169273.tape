program test3;

var a: 0..4 := 2;
external var b: a;
public var c,d: (red, green, blue);
var string_name_greater_than_10_chars: string;
var s2: string[4];
var xx: ftype;
var p: ^char;
var t: packed file of char;
var setvar: set of 3..4;
var afun: procedure (a: char; var b: boolean): boolean;
    afun2: procedure (p1, p2: char; var boolean);
    afun3: function (char; boolean);
    afun4: procedure ();
    afun5: function: boolean;

begin
  a := 2
end.
    