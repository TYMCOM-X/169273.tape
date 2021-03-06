$PAGE TMPCOR.INC, last modified 5/11/84, zw

TYPE
tmpcor_name = PACKED ARRAY [1..3] OF CHAR;
tmpcor_opcode = ( tmpcor_fs, tmpcor_rf, tmpcor_df, tmpcor_wf, tmpcor_rd,
  tmpcor_dd );
tmpcor_addr = 0 .. 777777b;
tmpcor_length = 0 .. 777777b;
EXTERNAL
FUNCTION tmpcor ( tmpcor_name; (* The three_character file name. *)
tmpcor_opcode; (* The operation to be performed. *)
tmpcor_addr; (* The buffer address. *)
VAR tmpcor_length) (* The buffer length in characters. *)
: BOOLEAN; (* True = success, false = error. *)
   