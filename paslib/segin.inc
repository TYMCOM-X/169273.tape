external procedure rd_word ( var f : library;
			    entry_idx : dir_idx;	(* ptr to dir to read *)
			    var index : word;		(* start index for read *)
			    num_to_read : word;		(* words to read *)
			    var words_left : word;	(* words not read *)
			    var word_array : array[1..*] of word;
			    var err : errcode );	(* error code *)
(* This procedure fills the array with num_to_read words, unless end-of-
   segment is encountered *)

external procedure rd_char ( var f : library;
			    entry_idx : dir_idx;	(* dir to read from *)
			    var index : char_idx;	(* start index *)
			    num_to_read : char_idx;	(* chars to read *)
			    var chars_left : char_idx;	(* chars not read *)
			    var char_array : array[1..*] of char;
			    var err : errcode );
(* This procedure fills the array with num_to_read characters, unless end-
of-segment is encountered *)

external procedure rd_line ( var f: library;
			   entry_idx : dir_idx;	(* dir to read from *)
			   var index : char_idx;	(* start index *)
			   var line : string[*];
			   var err : errcode );
(* This procedure reads the character-indexed line into the given string *)
