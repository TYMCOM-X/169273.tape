$WIDTH=100
$LENGTH=55
$TITLE CRYPT.PAS, last modified 10/14/83, zw

PROGRAM encrypt;
(*encrypt input to output*)
$SYSTEM pasutl

VAR
    key: wrdtyp;

BEGIN
  WHILE start('CRYPT', '') DO BEGIN
    IF NOT getarg(1, key) THEN
      key := '';
    crypt(key)
  END
END.
