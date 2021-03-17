module filutl;

(*   +--------------------------------------------------------------+
     I                                                              I
     I                        F I L U T L                           I
     I                        - - - - - -                           I
     I                                                              I
     +--------------------------------------------------------------+

     PURPOSE: Serves as a command utility  to  handle  filenames  and
	opennings.  System  dependencies,  such  as  the  format of a
	filename,  are isolated in this routine,  and need not be  of
	concern to the caller.

     ENTRY POINTS:

	pr_file_id	Scans an input line, parsing a filename.

	open_file	Opens a file described by a file_id record.

     NOTES:  While the format of a file_id is system dependent,  such
	data is always represented as a string, and is thus printable.

     CHANGES: 
                    	Modified pr_file_id so that file protection codes
			may preceed the directory.
                   	Rewrote PR_FILE_ID to scan VAX/VMS file names.
			Incorporated an automatically generated table 
			driven lexical analyzer.

     ---------------------------------------------------------------- *)
$PAGE includes
$SYSTEM cmdutl.typ
$SYSTEM query.inc


$PAGE pr_file_id
(* PR FILE ID extracts a file title from an input string. If the title parses
   correctly, file_id information is set, the string cursor is advanced past
   the title and true is returned.  If the title is incorrectly formed, false
   is returned, and the cursor is left pointing to the character which is in
   error. The file_id information is not changed. 

   The algorithm used is a direct implementation of a table driven finite
   state machine.  The tables used were generated automatically by the
   program LEXGEN (see Dave Wilson).  *)

public function pr_file_id
	    (	line: cmdline; var idx: cmdlineidx;
		var fid: file_id		      ): boolean;

CONST
   MAXINDEX    =  cmdlinelen;  (* MAX INDEX USED TO ACCESS BUFFER *)
   BUFFERSIZE  =  maximum ( cmdlineidx );  (* MAXINDEX + 1 *)
   MAXTOKEN    =    2;
   DFASTATE1   =    6;  (* CODE FOR INITIAL STATE OF DFA *)
   MAXDFASTATE =   29;  (* CODE FOR MAX STATE OF DFA *)
   MINTERMINAL =  -16;  (* MIN TERMINAL CODE *)
   EODATA      =   -1;  (* CODE FOR END-OF-DATA *)


TYPE
   STATERANGE  = 1..MAXDFASTATE;
   EXSTATERANGE= 0..MAXDFASTATE;
   INDEXRANGE  = 0..MAXINDEX;
   LEXTOKEN    = ( file_token, error_token );
   terminal_range = minterminal..eodata;
   terminal_array = packed array [terminal_range] of exstaterange;
   transition_matrix = packed array [staterange] of terminal_array;
   final_array = packed array [exstaterange] of 0..maxtoken;

const
   DELTA: transition_matrix := (
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,12,0,14,0,0,4,8,8,8,2,2,2,0,
      0,0,0,0,19,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17,9,0,0,0,0,0,5,0,0,
      0,0,0,0,0,0,0,9,0,0,0,0,0,5,0,0,0,22,0,0,0,0,0,0,0,8,8,8,2,2,2,1,0,
      0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,12,0,14,0,0,4,8,8,8,8,8,8,0,0,
      0,0,0,0,0,17,0,0,0,0,0,0,9,0,0,0,0,0,0,0,0,0,0,0,15,15,15,15,15,15,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,0,0,0,0,14,0,0,4,12,12,12,12,
      12,12,0,0,0,0,0,0,0,20,25,0,0,0,0,0,29,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,11,0,0,0,0,0,12,0,14,0,0,4,15,15,15,15,15,15,0,0,0,0,0,0,0,0,0,
      0,16,16,16,16,16,16,0,0,0,0,0,0,28,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,
      0,0,0,0,18,18,18,18,18,18,0,0,0,0,0,0,0,0,0,13,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,26,0,0,0,0,0,0,0,0,0,0,0,21,21,21,0,0,0,
      0,0,0,0,0,0,0,0,0,0,21,21,21,0,0,0,0,0,20,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,23,0,0,0,0,0,0,0,0,20,0,0,0,0,0,0,25,0,0,0,
      0,0,0,0,0,0,0,0,18,18,18,18,18,18,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,27,0,0,0,0,0,0,0,0,0,25,0,0,0,0,0,29,0,
      0);

   (* FINAL[X] = 0 IF STATE X IS NOT A FINAL STATE
                 1 IF STATE X RECOGNIZES <*****END OF DATA****>
                 2 IF STATE X RECOGNIZES <FILE SPEC>
                                                                 *)
   final: final_array := (
      0,1,2,0,0,0,0,0,2,0,0,0,2,0,0,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0);

var
   END_INDEX: INDEXRANGE;
   LEXEME: LEXTOKEN;


PROCEDURE SCAN(BEGIN_INDEX: indexrange; var END_INDEX: INDEXRANGE;
               VAR LEXEME: LEXTOKEN);

   VAR
      CURRSTATE, CURRFINAL: EXSTATERANGE;
      OLDINDEX:  INDEXRANGE;

  (* GETCHAR maps characters from the input line into the terminal classes
     expected by SCAN and used to index the transition matrix.  *)

  function getchar: terminal_range;

    var
      ch: char;

    begin

      if end_index >= length (line) then begin	(* past end of string *)
	getchar := eodata;
      end
      else begin
        end_index := end_index + 1;

        ch := uppercase ( line[ end_index ] );

        if (ch >= 'A') and (ch <= 'Z') 	(* alphas *)
	  then getchar := -2
	else if (ch >= '0') and (ch <= '7')	(* octal digits *)
	  then getchar := -3
	else begin

	  case ch of

	    '8','9':	getchar := -4;
	    '#':	getchar := -5;
	    '?':	getchar := -6;
	    '*':	getchar := -7;
	    '[':	getchar := -8;
	    ',':	getchar := -9;
	    ']':	getchar := -10;
	    '<':	getchar := -11;
	    '>':	getchar := -12;
	    '.':	getchar := -13;
	    ':':	getchar := -14;
	    '(':	getchar := -15;
	    ')':	getchar := -16;
	    others:	getchar := -1		(* end of data *)

	  end (* case *) ;
	end  (* else *) ;
      end  (* else *) ;
    end  (* proc getchar *);


   BEGIN (* SCAN *)
      CURRSTATE := DFASTATE1;  (* START IN INITIAL STATE *)
      CURRFINAL := 0;
      OLDINDEX  := 0;  (* width of lexeme AS OF LAST FINAL STATE *)
      end_index := begin_index - 1;

      WHILE CURRSTATE <> 0 DO
         BEGIN
            IF FINAL[CURRSTATE] <> 0 THEN
               BEGIN
                  CURRFINAL := CURRSTATE;
                  OLDINDEX := END_INDEX - BEGIN_INDEX + 1
               END;
            CURRSTATE := DELTA[CURRSTATE, getchar ()];
         END;
      END_INDEX := BEGIN_INDEX + OLDINDEX;	(* on exit, END_INDEX is index of 1st *)
						(* char PAST file name *)

      if final[ currfinal ] = 2
	then lexeme := file_token
	else lexeme := error_token;

   END; (* SCAN *)


begin	(* body of PR_FILE_ID *)

  scan ( idx, end_index, lexeme );

  pr_file_id := (idx <> end_index) and
	        (lexeme = file_token);

  if pr_file_id
    then fid := substr ( line, idx, end_index - idx );
  idx := end_index;

end (* proc pr_file_id *) ;
$PAGE open_file
(* OPEN FILE opens a text file for input or output. The mode is specified by
   the caller. For an output file the user can also request append mode and
   old/new file prompting.  The caller may supply a default extension for the
   file name. A flag is returned indicating if the open was successful. *)

public function open_file
	    (	var f: text;
		fid: file_id; ext: extension;
		mode: ( input_mode, output_mode );
		option_set: set of ( append_mode, confirm_open, ascii_mode )
	    ): boolean;

 var question: query_string;
     lext: packed array[1..5] of char;
 begin
  open_file := false;				(* assume failure *)
  lext := '.' || ext (* || ' ' *);
  case mode of 

    input_mode:
      begin
	if (option_set - [ascii_mode]) <> [] then
	  return; (* none applicable *)
	if ascii_mode in option_set
	  then open (f, lext || fid, [ascii])
	  else open (f, lext || fid);
	open_file := (iostatus = io_ok);
      end;

    output_mode:
      begin
	if confirm_open in option_set then begin
	  open (f, lext || fid);
	  if eof (f)
	    then question := 'New file: ' || fid
	    else begin
		question := 'Old file: ' || filename (f);   (* used full file_id of that found *)
		close (f)
	    end;
	  if not query (question) then return
	end;
	if append_mode in option_set
	  then rewrite (f, lext || fid, [preserve])
	  else rewrite (f, lext || fid);
	open_file := (iostatus = io_ok)
      end

  end;
 end.
