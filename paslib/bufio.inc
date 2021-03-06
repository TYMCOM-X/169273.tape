$IFNOT bufioinc

$SYSTEM ask.typ
$SYSTEM err.typ
$SYSTEM dbg.typ
$SYSTEM copy.typ
$SYSTEM setup.typ

$INCLUDE bufio.typ

EXTERNAL PROCEDURE put_buf;
(*write buffer to OUTPUT file*)

EXTERNAL PROCEDURE put_ch(ch: CHAR);
(*write character to OUTPUT file*)

EXTERNAL PROCEDURE put_str(str: STRING[o_buf_siz]);
(*write string to OUTPUT file*)

EXTERNAL PROCEDURE get_buf;
(*read buffer from INPUT file*)

EXTERNAL PROCEDURE get_ch;
(*read character from INPUT file*)

EXTERNAL VAR ch: CHAR;
(*current character set by get_ch*)

EXTERNAL VAR lin_pos, chr_pos: INTEGER;
(*line and character position numbers*)

$ENABLE bufioinc
$ENDIF
   