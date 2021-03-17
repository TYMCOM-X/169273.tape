(*tools.inc, last modified 5/16/83, zw*)
$IFNOT toolsinc

$INCLUDE TOOLS.TYP

EXTERNAL PROCEDURE setup(progname: STRING[*]);
  (*set up TTY, TTYOUTPUT, INPUT and OUTPUT, identify program*)

EXTERNAL FUNCTION getio: BOOLEAN;
  (*get the standard INPUT and OUTPUT files*)

EXTERNAL FUNCTION getarg(n: INTEGER;
  VAR arg: PACKED ARRAY [1 .. *] OF CHAR; maxsize: INTEGER): INTEGER;
  (*get the nth argument from the command line, return length of arg*)

EXTERNAL FUNCTION getb: BOOLEAN;
  (*try to get buffer from input device, FALSE if end of file*)

EXTERNAL FUNCTION getc(VAR c: CHAR): BOOLEAN;
  (*try to get next character from input, FALSE if end of file*)

EXTERNAL FUNCTION getl(VAR l: PACKED ARRAY [1 .. *] OF CHAR;
  len: INTEGER): INTEGER;
  (*get a line of characters from the standard input*)

EXTERNAL PROCEDURE putb;
  (*send buffer of characters to output device*)

EXTERNAL PROCEDURE putc(c: CHAR);
  (*put character to output*)

EXTERNAL PROCEDURE putl(l: PACKED ARRAY [1 .. *] OF CHAR);
  (*put line to standard output*)

EXTERNAL PROCEDURE putdec(n, w: INTEGER);
  (*output number n in field of minimum width w*)

EXTERNAL FUNCTION strnum(s: STRING[*]): INTEGER;
  (*convert a string of digits to an integer*)

$ENABLE toolsinc
$ENDIF
(*end of tools.inc*)
