$title listing test
$width (80)

  (* test various listing parameters *)

program main;
var a, b: 0..100;
$PAGE inner_procedure
  procedure p;
  begin
    a := 3;
  end;
begin
$PAGE test_length

$length (15)
(*
*)
$PAGE test_width
$width (50)
(*
12345678901234567890123456789012345678901234567890
*)
$include test99.pas

$header test98.pas

$system test97

$if foo,bar  a := 3;


$ifnot foo
  a := 5
  b := 6;
$end

$if foo

  a := 0;

$end

$PAGE end


end.
    