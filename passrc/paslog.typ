$PAGE PASLOG.TYP, last modified 1/13/84, zw
$IFNOT paslogtyp

(*SYSTEM DTIME.TYP*)

(*this must be altered to remove machine dependency if possible*)

TYPE
filblock = ARRAY [1..4] OF MACHINE_WORD; (* internal file name block*)
log_file_record = PACKED RECORD
  file_name: filblock; (*encoded file block*)
  run_time: INTEGER; (*in milliseconds*)
  no_lines: INTEGER; (*source lines read*)
  no_incl_lines: INTEGER; (*lines from include files*)
  no_errors: INTEGER; (*errors detected*)
  users_ppn: MACHINE_WORD; (*from GETPPN uuo*)
  date_and_time: dtime_int;
  lowseg_size: 0..#o777777; (*generated code low segment*)
  highseg_size: 0..#o777777; (*generated code high segment*)
  alloc_strategy: 0 .. 99; (*ALLOC=n*)
  ki10: BOOLEAN; (*no/yes*)
  kl10: BOOLEAN; (*yes*)
  opt_debug: BOOLEAN; (*DEBUG*)
  opt_double: BOOLEAN; (*no - no such thing*)
  opt_check: BOOLEAN; (*anything in CHECK mode?*)
  opt_main: BOOLEAN; (*program vs. module*)
  opt_overlay: BOOLEAN; (*OVERLAY*)
  opt_progress: BOOLEAN; (*no*)
  opt_source: BOOLEAN; (*SOURCE ever specified?*)
  opt_special: BOOLEAN; (*anything in SPECIAL mode*)
  opt_terse: BOOLEAN; (*TERSE vs. VERBOSE*)
  opt_trace: BOOLEAN; (*TRACE ever specified?*)
  opt_xref: BOOLEAN; (*GLOBAL*)
  opt_virtual: BOOLEAN; (*no*)
  opt_auto_run: BOOLEAN; (*run at offset 1*)
  opt_tmpcor: BOOLEAN; (*run from tmpcor file*)
  opt_hash: BOOLEAN; (*run from ###PAS.TMP*)
  fill1, fill2, fill3: INTEGER (*record size = 16 (decimal) words*)
END;

$ENABLE paslogtyp
$ENDIF
