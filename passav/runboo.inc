(* External declaration for routine RUN (from RDLIB). *)

type
  run_filename = packed array [1..30] of char;

external procedure run ( fn: run_filename; offset: boolean );
 