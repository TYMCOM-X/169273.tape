PROGRAM make_help;
(*make up a binary help file*)

$SYSTEM UTLHLP.INC
$SYSTEM CMDUTL.INC

VAR
txt_fil, bin_fil: FILE_NAME;
cmd: STRING[80];
idx: INTEGER;

BEGIN
  OPEN(TTY); REWRITE(TTYOUTPUT);
  WRITELN(TTYOUTPUT, 'This program makes a binary help file.');
  cmd_getline('Enter input text file name: ', txt_fil, idx);
  cmd_getline('Enter output binary file name: ', bin_fil, idx);
  makhlp(txt_fil, bin_fil);
  LOOP
    cmd_getline('Test:', cmd, idx);
    EXIT IF cmd_eol(cmd, idx) ORIF NOT hlp(cmd, idx, bin_fil)
  END
END.
 