type
    tmpcor_name = packed array [1..3] of char;

    tmpcor_opcode =
     (  tmpcor_fs, tmpcor_rf, tmpcor_df, tmpcor_wf, tmpcor_rd, tmpcor_dd  );

    tmpcor_addr = 0 .. 777777b;

    tmpcor_length = 0 .. 777777b;

external function tmpcor ( tmpcor_name; (* The three_character file name. *)
			   tmpcor_opcode; (* The operation to be performed. *)
			   tmpcor_addr; (* The buffer address. *)
			   var tmpcor_length) (* The buffer length in characters. *)
			 : boolean; (* True = success, false = error. *)
