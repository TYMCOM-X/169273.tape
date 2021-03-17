$TITLE RUNUTL.PAS, last modified 4/3/84, zw
MODULE runutl;
(*TYM-Pascal program run utility*)

(*HEADER RUNUTL.HDR*)

$SYSTEM TYPUTL.TYP
$INCLUDE RUNUTL.TYP

(*These are defined in RUNMAC.MAC*)
EXTERNAL VAR _runoffset: INTEGER;
EXTERNAL PROCEDURE _runprogram(PACKED ARRAY [1 .. *] OF CHAR; INTEGER);

PUBLIC FUNCTION runoff: run_offset;
BEGIN
  runoff := _runoffset
END;

PUBLIC FUNCTION runprg(program_name: file_name; offset: run_offset): yes_no;
BEGIN
  _runprogram(program_name, offset);
  runprg := no
END.
