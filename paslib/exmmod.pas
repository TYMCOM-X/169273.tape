MODULE module_rpt OPTIONS DEBUG, XREF;
$INC EXTYPE.INC
EXTERNAL CONST table_dir : ARRAY [directive] OF STRING [4];
EXTERNAL CONST method_of_mpayment_table : ARRAY [method_of_payment] OF 
     STRING [13] ;
EXTERNAL CONST day_table : ARRAY [days] OF STRING [3] ;
EXTERNAL CONST category_table : ARRAY [category] OF STRING [22] ;
EXTERNAL VAR data_save : BOOLEAN;
EXTERNAL VAR rate_per_mile : REAL;
         EXTERNAL VAR conversion_factor : REAL;
VAR found : BOOLEAN;
VAR s4    : STRING [4];
VAR dir   : directive;
VAR buffer: STRING [85];
VAR r: exp_record;


PUBLIC FUNCTION read_string (VAR buffer : STRING [85]) : BOOLEAN;

(* Reads a contiguous string of non-blank characters               *)

BEGIN;
     READLN (TTY);
     READ   (TTY, buffer);
     buffer := buffer || '  '; (*Marks end of buffer assurs not null*)
     buffer := SUBSTR(buffer, VERIFY(buffer, [' '],1));
     buffer := SUBSTR(buffer,1, SEARCH (buffer, [' ']));
     (*Striped out a string starting with first non blank to a blank *)
     IF LENGTH (buffer) = 1 THEN
         BEGIN;
             read_string  := FALSE;
             buffer      := '';
             RETURN;
         END;
     buffer := SUBSTR (buffer, 1, (LENGTH (buffer) - 1));
     read_string := TRUE;
     (*Assuming at least 2 in length, and stripping the blank *)
END;

(*********************************************************************)

PUBLIC FUNCTION command : directive;

  (*this procedure reads a command from the terminal and returns a 
   command name as an enumerated type directive.*)


     BEGIN;
          buffer := ''; (*Clears buffer*)
          WRITE (TTY, 'COMMAND?');
          BREAK;
          found := FALSE;
          IF NOT (read_string (buffer))THEN buffer := 'badc';
          s4 := LOWERCASE (buffer);
          FOR dir := MINIMUM (directive) TO MAXIMUM (directive) DO
               BEGIN;
                   EXIT IF table_dir [dir] = s4 DO
                        BEGIN;
                             command := dir;
                             found   := TRUE;
                        END;
               END;
          IF NOT found THEN command := badc;
     END;



(*********************************************************************)

PUBLIC FUNCTION y_n :CHAR;

   (*this function ask for a y or n, and loops until one or the other 
    is entered.*)

VAR c : CHAR;
BEGIN;
     WRITELN (TTY,''); (*Puts cursor on a new line*)
     WRITE   (TTY, 'Enter a y or n:');
     BREAK;
     READLN  (TTY); (*Brings line into file buffer**)
     READ    (TTY, c);
     
     WHILE NOT (c IN ['Y', 'y', 'N', 'n']) DO
          BEGIN;
               WRITE (TTY, 'Y or N please:');
               BREAK;
               READLN (TTY); (*Brings line into file buffer*)
               READ   (TTY, c); (*Puts the terminal input string into program varible*)
          END;
     y_n := LOWERCASE (c);
END;


PUBLIC FUNCTION got_day (VAR start_day : days) : BOOLEAN;

VAR day : days;
LABEL 1;

BEGIN;
     1: WRITELN (TTY, 'Enter a 3 char starting day or N to skip all days');
     BREAK;
     IF NOT (read_string (buffer)) THEN
          BEGIN;
               start_day := sun;
               got_day := TRUE;
               RETURN;
          END
     ELSE
          FOR day := MINIMUM (days) TO MAXIMUM (days) DO
               BEGIN;
                    IF day_table [day] = UPPERCASE ( buffer) THEN
                         BEGIN;
                              start_day := day;
                              got_day := TRUE;
                              RETURN;
                         END;
               END;
     IF LOWERCASE (buffer) = 'n' THEN
          BEGIN;
               got_day := FALSE;
               RETURN;
          END;
     WRITELN (TTY, 'Enter a day, N to skip all, or nothing for SUN');
     BREAK;
     GOTO 1;
END;

(*********************************************************************)

PUBLIC FUNCTION got_a_real (VAR buffer: STRING [85]; VAR return_real: REAL;
			    VAR max_digits : INTEGER;
                            VAR msg: STRING [40]) : BOOLEAN;
VAR X: INTEGER;
VAR decimal_point: INTEGER;
VAR digits_after_point_count: INTEGER;
    digits_before_point_count : INTEGER;
BEGIN;
     got_a_real := TRUE;
     return_real:= 0.0;
     msg        := '';
     decimal_point := 0;
     digits_after_point_count := 0;
     digits_before_point_count := 0;
     
     IF  NOT (read_string (buffer)) THEN
         BEGIN;
             got_a_real  := TRUE;
             buffer      := '';
             return_real := 0.0;
             RETURN;
         END;
     IF buffer [1] = '.' THEN
         BEGIN;
             msg := 'Degit must proceed decimal';
             got_a_real := FALSE;
             RETURN;
         END;
     FOR x := 1 TO  LENGTH(buffer) DO
          BEGIN;
               IF buffer [x] = '.' THEN
                    decimal_point := decimal_point + 1
               ELSE
                    BEGIN;
			 IF NOT (buffer [x] IN
                           ['0','1','2','3','4','5','6','7','8','9'])      
			 THEN
                             BEGIN;
                                 got_a_real := FALSE;
                                 msg := 'Digit not entered';
                                 RETURN;
                             END;
                        IF decimal_point >= 1 THEN
                             digits_after_point_count := 
                             digits_after_point_count + 1
			ELSE
			     digits_before_point_count :=
			     digits_before_point_count + 1;
                   END;
     END; (*End of for*)
     IF decimal_point > 1 THEN
         BEGIN;
            got_a_real := FALSE;
            msg        := 'Only one decimal point allowed';
            RETURN;
         END;
     IF digits_after_point_count  <> 2   THEN
         BEGIN;
             got_a_real := FALSE;
             msg        := 'Two degits required after decimal';
             RETURN;
         END;
     IF digits_before_point_count > max_digits THEN
	  BEGIN;
               PUTSTRING (msg, max_digits);
	       msg := 'Only ' || msg || ' allowed before decimal.';
	       got_a_real := FALSE;
	       RETURN;
	  END;
     (*AT THIS POINT IT IS A REAL NUMBER IN buffer *)
     GETSTRING (buffer, return_real);
END;

(*********************************************************************)

PUBLIC FUNCTION done_entering_expenses( cat: category;  day: days):
       BOOLEAN;

VAR amount :REAL;
    error_msg : STRING [40];
    max_digits : INTEGER;

LABEL 1;

BEGIN;

1: error_msg := '';  (*Initalize string to null*)
done_entering_expenses := FALSE;
max_digits := 4;
IF got_a_real (buffer, amount, max_digits, error_msg)  THEN

     BEGIN;
	IF cat = personal_auto_milage THEN
	     amount := rate_per_mile * amount;

	r.expenses [cat, day].expense := conversion_factor * amount;
        IF amount <> 0.0 THEN
            BEGIN;
		WRITELN (TTY, 'Is this company paid?');
		IF y_n = 'y' THEN
		     r.expenses [cat, day].how_paid  :=  company_paid
		ELSE
		     r.expenses [cat, day].how_paid  := employee_paid;
            END;
	RETURN;
     END
ELSE
     IF LOWERCASE (buffer) = 's' THEN
	  BEGIN;
	     done_entering_expenses := TRUE;
	     RETURN; (* An s causes a skip out of the category *)
	  END
     ELSE
	  BEGIN;
	     WRITELN (TTY, error_msg);
	     WRITELN (TTY, 'Enter an amount as xxx.xx, or S to stop'||
	     'or a return for no $ but to continue');
	     BREAK;
	     GOTO 1; (* Goes back to try again for an error *)
	  END;
WRITELN (TTY, 'done_entering_expenses should never see this'); 
END; (*END of done_entering_expenses *)
(*********************************************************************)

PUBLIC PROCEDURE quit_proc (VAR ok : BOOLEAN);

   (*this procedure checks to see if data has been displayed or saved
    and if not ask the person if this is an ok situration.*)


BEGIN;
     IF NOT data_save THEN
          BEGIN;
               WRITELN (TTY, 'No data has been printed or saved. Is'||
                              'this ok?');
               IF y_n = 'y' THEN ok := TRUE ELSE ok := FALSE;
          END
     ELSE
          ok := TRUE;
END;


(*********************************************************************)

PUBLIC PROCEDURE init_proc;
BEGIN;
     data_save := FALSE; (*Signifies new data that user may want to 
                           perserve: save or print*)
     WRITELN (TTY, '');
     WRITELN (TTY, 'init_proc');
     BREAK;
END;

(*********************************************************************)

PUBLIC PROCEDURE entr_proc;
VAR err_msg: STRING [40];
    amount: REAL;
    cat : category;
    d   : days;
    max_digits :  INTEGER;
    start_day : days;
    end_day   : days;
BEGIN;
     data_save := FALSE; (*Signifies new data that user may want to 
                           perserve: save or print*)
     WRITELN (TTY, '');
     WRITELN (TTY, 'entr_proc');
     BREAK;
     WRITELN (TTY, 'Enter anounts as xxx.xx or return for no amount'||
                   ' or S to skip out of a category');
     BREAK;
     
     FOR cat := MINIMUM (category) TO MAXIMUM (category) DO
         BEGIN;
             WRITELN (TTY, category_table [cat]);
             BREAK;
             max_digits := 4;
             IF got_day (start_day)  THEN
                  BEGIN;
 		      IF cat = personal_auto_milage THEN
 			  BEGIN;
			     WRITELN (TTY, 'Is 0.20 per mile ok?');
			     BREAK;
			     IF y_n = 'n' THEN
			       BEGIN;
				   WRITELN (TTY, 'Enter rate per mile'||
						 ' as 0.xx');
				   BREAK;
				   buffer := '     ';
                                   max_digits := 1;
				   WHILE NOT (got_a_real
					(buffer, amount,
					 max_digits, err_msg)) DO
					BEGIN;
					    WRITELN (TTY, err_msg);
					    BREAK;
				        END;
			           rate_per_mile := amount;
                                   max_digits := 4;
			       END;
			  END;
                      FOR d := start_day TO MAXIMUM (days) DO
			  BEGIN;
			     WRITE (TTY, day_table [d] || ':');
			     BREAK;
			     EXIT IF done_entering_expenses (cat, d);
			  END;
                  END;
         END;

END;

(*********************************************************************)

PUBLIC PROCEDURE prnt_proc;
BEGIN;
     data_save := TRUE; (*Signifies new data is perserved.*)
     WRITELN (TTY, '');
     WRITELN (TTY, 'prnt_proc');
     BREAK;
END;

(*********************************************************************)

PUBLIC PROCEDURE save_proc;
BEGIN;
     data_save := TRUE; (*Signifies new data is perserved.*)
     WRITELN (TTY, '');
     WRITELN (TTY, 'save_proc');
     BREAK;
END;

(*********************************************************************)

PUBLIC PROCEDURE load_proc;
VAR ok : BOOLEAN;
BEGIN;
     quit_proc (ok); (*Use to check if modified data has been seen.*)
     IF ok THEN 
          WRITELN (TTY, 'ACTUALLY DO LOAD') (*NO data_save PROBLEM.*)
     ELSE
          WRITELN (TTY, 'NO LOAD DONE. data_save NOT TRUE.');
     WRITELN (TTY, '');
     WRITELN (TTY, 'load_proc');
     BREAK;
END;

(*********************************************************************)

PUBLIC PROCEDURE help_proc;
BEGIN;
     WRITELN (TTY, '');
     WRITELN (TTY, 'help_proc');
     BREAK;
END;

(*********************************************************************)

PUBLIC PROCEDURE badc_proc;
BEGIN;
     WRITELN (TTY, '');
     WRITELN (TTY, 'You entered an invalid command. Enter help to see' ||
                   ' a list of the valid commands.');
     BREAK;
END.
 