$WIDTH=100
$LENGTH=55
$TITLE VERSIO.PAS, last modified 12/20/83, zw
MODULE VERSION OPTIONS SPECIAL(coercions);
(*program version identification from .JBVER*)

$HEADER VERSIO.HDR

$INCLUDE VERSIO.TYP

$PAGE version

PUBLIC FUNCTION version: version_string;
(*returns a string containing the program version identifier*)
CONST jbver = #o137;
TYPE version_word = PACKED RECORD
  mod_code: 0 .. #o7; (*bits 0-2: "last modified by"*)
  major_version: 0 .. #o777; (*bits 3-11: major version number*)
  minor_version: 0 .. #o77; (*bits 12-17: minor version number*)
  edit_number: 0 .. #o777777 (*bits 18-35: edit number*)
END;
VAR version_ptr: ^version_word;
TYPE oct = 0 .. #o100000;
VAR step: oct;
FUNCTION digit(od: oct): CHAR;
BEGIN digit := CHR(ORD('0') + (od MOD #o10)) END;
BEGIN
  version_ptr := PTR(jbver);
  WITH version_ptr^ DO BEGIN
    version := '';
    IF major_version >= #o100
    THEN version := version || digit(major_version DIV #o100);
    IF major_version >= #o10
    THEN version := version || digit(major_version DIV #o10);
    version := version || digit(major_version);
    IF minor_version > 26
    THEN version := version || CHR(ORD(PRED('A')) + (minor_version DIV 26));
    IF minor_version <> 0
    THEN version := version || CHR(ORD(PRED('A')) + (minor_version MOD 26));
    IF edit_number <> 0 THEN BEGIN
      version := version || '('; step := #o100000;
      WHILE step <> 0 DO BEGIN
	IF edit_number >= step
	THEN version := version || digit(edit_number DIV step);
	step := step DIV #o10;
      END;
      version := version || ')';
    END;
    IF mod_code <> 0
    THEN version := version || '-' || digit(mod_code);
  END;
END.
    