module query;

(*   +--------------------------------------------------------------+
     I                                                              I
     I                         Q U E R Y                            I
     I                         - - - - -                            I
     I                                                              I
     +--------------------------------------------------------------+

     PURPOSE: Serves as a command ulitity to ask  a  caller  supplied
	question,  and  check  for  a yes or no reply.  YES,  Y,  NO,
	N,  or <eoln> meaning yes  are  accepted  as  valid  replies.
	Also,   REPEAT  is  accepted  as  a  request  to  repeat  the
	question.

     USAGE:
	result := query ('question');

     INPUT: 

	question   is the question to be asked.  This routine appends
		   a  carriage  return  to  the  end  of  the  string
		   provided.

     OUTPUT:

	result     is true  if  an  affirmative  response  is  given;
		   false,  if a negative response is given.

     REQUIREMENTS: It  is  assumed  that  the files TTY and TTYOUTPUT
	have been opened before this routine is called.

     ALGORITHM: Displays the question and waits for  a  response.  If
	an  invalid  response  is  received,  another is requested an
	processed.

     ---------------------------------------------------------------- *)
$PAGE query
type query_string = string[256];

public function query (question: query_string): boolean;

 procedure ask;
  begin
   write (tty, question, '?  '); break
  end;

 var response: (good, bad);
 var line: query_string;
 const eoln_chars : set of char :=
                   [chr(15b), chr(33b), chr(32b), chr(12b), chr(7b)];
                (* [  <cr>,     <esc>,   <^Z>,     <lf>,    <^G>  ]  *)

 begin
   ask;						(* print question *)
   repeat
     readln (tty);
     read (tty, line);
     line := uppercase (line);
     if (line <> '') andif
	(line [length (line)] in eoln_chars) then (* possible if TTY opened ASCII *)
       line := substr (line, 1, length(line)-1);

     response := good;				(* process, assume ok *)
     if (line = 'YES') or (line = 'Y') or (line = '')
	then query := true
     else if (line = 'NO') or (line = 'N')
	then query := false
     else if (line = 'REPEAT')
	then begin
	  ask;
	  response := bad
	end
     else
	begin
	  write (tty, 'YES/NO?  '); break;
	  clear (tty);				(* don't want user typing ahead of error *)
	  response := bad
	end
   until response = good
 end.
 