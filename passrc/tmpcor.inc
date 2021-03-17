(*TMPCOR.INC, last modified 11/8/83, zw*)
$IFNOT tmpcorinc

$SYSTEM TMPCOR.TYP

(*defined in TMPCOR.MAC*)

EXTERNAL FUNCTION tmpcor(tmpcor_name; (*three_character file name*)
			 tmpcor_opcode; (*operation to be performed*)
			 tmpcor_addr; (*buffer address*)
			 VAR tmpcor_length) (*buffer length in characters*)
			 : BOOLEAN; (*True = success, false = error*)

$ENABLE tmpcorinc
$ENDIF
