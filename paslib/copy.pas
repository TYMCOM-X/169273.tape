$WIDTH=100
$LENGTH=55
$TITLE COPY.PAS, last modified 10/14/83, zw

PROGRAM copy;
(*copy input file to output file*)

$SYSTEM pasutl

VAR
    lin: STRING[132];

BEGIN
  WHILE start('COPY', '') DO BEGIN
    lin := '';
    WHILE rdlin(lin) DO BEGIN
      IF EOPAGE THEN
	PAGE;
      wrlin(lin)
    END
  END
END.
   