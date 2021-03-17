$PAGE UTLLIB.TYP, last modified 4/9/84, zw
$IFNOT utllibtyp
(*TYM-Pascal type utility*)

(*HEADER UTLLIB.HDR*)

CONST
maximum_argument = #o377777; (*maximum positive PDP10 half word*)
yes = TRUE;
no = FALSE;

TYPE
integer_argument = -maximum_argument .. maximum_argument;
positive_argument = 0 .. MAXIMUM(integer_argument);
ordinal_argument = 1 .. MAXIMUM(positive_argument);
string_argument = STRING[32];
generic_string = STRING[*];
number = MINIMUM(REAL) .. MAXIMUM(REAL) PREC 16; (*real number*)
positive_integer = 0 .. MAXIMUM(INTEGER);
ordinal_integer = 1 .. MAXIMUM(INTEGER);
yes_no = BOOLEAN;
binary_file = FILE OF *;
text_file = TEXT;
char_set = SET OF CHAR;

$ENABLE utllibtyp
$ENDIF
