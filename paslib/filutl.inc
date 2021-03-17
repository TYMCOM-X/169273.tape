(**********  FILUTL.inc  last modified 6/19/78 **********)



(* PR FILE ID extracts a file title form an input string. If the title parses
   correctly, file_id information is set, the string cursor is advanced past
   the title and true is returned.  If the title is incorrectly formed, false
   is returned, and the cursor is left pointing to the character which is in
   error.  The file_id information is not changed. *)

external function pr_file_id
	    (	line: cmdline; var idx: cmdlineidx;
		var fid: file_name		      ): boolean;



(* OPEN FILE opens a text file for input or output. The mode is specified by
   the caller. For an output file the user can also request append mode and
   old/new file prompting. The caller may supply a default extension to be used
   in the file_id if none appears.  A flag is returned indicating if the open
   was successful. *)

type
  io_mode = ( input_mode, output_mode );
  io_optns = ( append_mode, confirm_open );
  io_option_set = set of io_optns;

external function open_file
	    (   var f: text;
		fid: file_id;
		ext: extension;
		mode: io_mode;
		option_set: io_option_set   ): boolean;



(* PATHNAME returns the actual file_id of an opened PASCAL file.  This file_id
   reflects any directory searching in effect. *)

external function pathname  ( var f: text ):  file_id;

