$WIDTH=100
$LENGTH=55
$TITLE FMTSYM.PAS, last modified 10/14/83, zw

PROGRAM dofmtsym;
(*format a symbolic structure*)

$SYSTEM pasutl
$SYSTEM symutl

BEGIN
  WHILE start('FMTSYM', '') DO fmtsym
END.
  