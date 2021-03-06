$TITLE EXPENSE REPORT PROGRAM JULY 1981
PROGRAM prog_rpt OPTIONS DEBUG, XREF;
       (****************************************************
        *                                                  *
        *  THIS PROGRAM AIDS IN PREPARING A MDSI EXPENSE   *
        *  REPORT.                                         *
        *                                                  *
        *  AUTHOR: Paul J. Pinkstaff                       *
        *                                                  *
        *  DATE: July 24, 1981                             *
        *                                                  *
        ****************************************************)
  
$INC EXTYPE.INC
EXTERNAL VAR data_save : BOOLEAN;
             rate_per_mile : REAL;
             conversion_factor : REAL;
EXTERNAL FUNCTION  command : directive;
EXTERNAL PROCEDURE                  init_proc;
EXTERNAL PROCEDURE                  entr_proc;
EXTERNAL PROCEDURE                  prnt_proc;
EXTERNAL PROCEDURE                  save_proc;
EXTERNAL PROCEDURE                  load_proc;
EXTERNAL PROCEDURE                  quit_proc (VAR BOOLEAN);
EXTERNAL PROCEDURE                  help_proc;
EXTERNAL PROCEDURE                  badc_proc;
VAR ok : BOOLEAN;
LABEL 1;
BEGIN;
     data_save := TRUE;
     rate_per_mile := 0.20;
     conversion_factor := 1.0;
        (*The Boolean varible data_save is used to check if a user
          has in some way perserved new data. This means either printed
          or saved it. 

          
          The routines prnt=proc and save_proc both set it to true.
          The routines init_proc and entr_proc set it to false.
          Both of these routines could potentially result in new data.
          Here in the main program, data_save is set to true because
          nothing has happened yet and consequently no new data has
          appeared. The routines quit_proc and load_proc both  check
          to see that data_save is true.  If not, they prompt the 
          user asking if not having printed or saved what potentially
          could be new data is ok.*)
     OPEN (TTY); REWRITE (TTY);
     WRITELN (TTY, 'This program allows you to create data for an MDSI'||
                    'expense report. enter help to obtain a description '||
                    'of the commands and instructions on how to enter'||
                    'the data');
     BREAK;
     1: CASE command OF
        init: init_proc;
        entr: entr_proc;
        prnt: prnt_proc;
        save: save_proc;
        load: load_proc;
        quit: BEGIN;
                   quit_proc (ok);
                   IF ok then BEGIN; WRITELN (TTY, 'GOOD BY.'); STOP; END; 
              END;
        help: help_proc;
        badc: badc_proc;
     END; (*End of case*)
     GOTO 1; (*Returns for next command*)
END.
    