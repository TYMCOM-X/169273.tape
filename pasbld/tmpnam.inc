$PAGE TMPNAM.INC, last modified 5/11/84, zw

TYPE
name_code = PACKED ARRAY [1..3] OF CHAR;
EXTERNAL
FUNCTION tempname ( name_code ): FILE_NAME;
