PROGRAM copy; (*note that "(INPUT, OUTPUT)" not specified here*)
(*a simple TYM-Pascal program to copy text files*)

(*This program demonstrates some of the non-obvious features of the
TYM-Pascal language which should be of use to the novice programmer.*)

FUNCTION read_line(VAR line: STRING[*]): BOOLEAN;
(*read next line of text from INPUT, or return FALSE if end of file*)
BEGIN
  IF INPUT = TTY THEN BEGIN (*special case for terminal input*)
    READLN(INPUT); (*flush previous line, wait for carriage return*)
    READ(INPUT, LINE); (*read what was just typed*)
    read_line := TRUE (*terminal has no end of file*)
  END
  ELSE IF NOT EOF(INPUT) THEN READLN(INPUT, LINE) (*standard file input*)
  ELSE read_line := FALSE
END;

VAR tty_is_open: BOOLEAN := FALSE; (*flag used by open_terminal*)

PROCEDURE open_terminal;
(*open terminal input and output*)
BEGIN
  IF tty_is_open THEN RETURN; (*note use of flag*)
  OPEN(TTY);
  REWRITE(TTYOUTPUT);
  tty_is_open := TRUE (*remember that terminal is open*)
  (*we assume that TTY or TTYOUTPUT will never be closed*)
END;

PROCEDURE prompt(message: STRING[*]);
(*write message to terminal to prompt for interactive input*)
BEGIN
  open_terminal; (*in case it wasn't*)
  WRITE(TTYOUTPUT, message, ' '); (*note the space*)
  BREAK(TTYOUTPUT) (*flush internal buffer -- write it NOW*)
END;

PROCEDURE terminal_message(message: STRING[*]);
(*write a message to the terminal*)
BEGIN
  open_terminal; (*in case it wasn't*)
  WRITELN(TTYOUTPUT, message);
  BREAK(TTYOUTPUT) (*flush internal buffer -- write it NOW*)
END;

FUNCTION successful(success: BOOLEAN; message: STRING[*]): BOOLEAN;
(*display error message to terminal if not success, return success*)
BEGIN
  IF NOT success THEN terminal_message(message);
  successful := success (*pass it on*)
END;

PROCEDURE solicit(question: STRING[*]; VAR answer: STRING[*]);
(*interactive terminal solicitation: ask question, return answer*)
VAR saved_input: TEXT;
BEGIN
  open_terminal; (*in case it wasn't*)
  saved_input := INPUT; INPUT := TTY; (*temporary terminal input*)
  prompt(question); (*ask the question*)
  IF NOT read_line(answer) THEN STOP; (*should never fail*)
  INPUT := saved_input; (*restore original INPUT*)
END;

PROCEDURE solicit_input(message: STRING[*]);
(*solicit input file name with message and open INPUT*)
VAR name: FILE_NAME;
BEGIN
  REPEAT
    solicit(message, name); (*get file name*)
    IF UPPERCASE(name) = 'TTY:' THEN INPUT := TTY
    ELSE RESET(INPUT, name) (*try to open file*)
  UNTIL successful(IOSTATUS = IO_OK, 'Can not RESET "' || name || '".')
END;

PROCEDURE solicit_output(message: STRING[*]);
(*solicit output file name with message and open OUTPUT*)
VAR name: FILE_NAME;
BEGIN
  REPEAT
    solicit(message, name); (*get file name*)
    IF UPPERCASE(name) = 'TTY:' THEN OUTPUT := TTYOUTPUT
    ELSE REWRITE(OUTPUT, name) (*try to open file*)
  UNTIL successful(IOSTATUS = IO_OK, 'Can not REWRITE "' || name || '".')
END;

FUNCTION query(question: STRING[*]): BOOLEAN;
(*ask question, return TRUE if answer is YES, FALSE if answer is NO*)
VAR answer: STRING[3];
BEGIN
  REPEAT
    solicit(question, answer);
    answer := UPPERCASE(answer);
  UNTIL successful((answer = 'YES') OR (answer = 'NO'),
    'Please respond YES or NO.');
  query := answer = 'YES'
END;

VAR line: STRING[132]; (*a line of text*)

BEGIN
  terminal_message('This program will copy a text file.');
  REPEAT
    solicit_input('Copy From:'); solicit_output('Copy To:');
    IF INPUT = TTY THEN BEGIN
      terminal_message('Type text into ' || FILENAME(OUTPUT));
      terminal_message('Use "#" to terminate input.')
    END;
    IF OUTPUT = TTYOUTPUT THEN BEGIN
      terminal_message('Text of ' || FILENAME(INPUT) || ' follows:');
      IF INPUT = TTY THEN terminal_message('This will echo what you type.')
    END;
    WHILE read_line(line) ANDIF ((INPUT <> TTY) ORIF (line <> '#')) DO BEGIN
      WRITELN(line) (*copy the file*)
    END;
    IF INPUT <> TTY THEN CLOSE(INPUT); (*don't close TTY!*)
    IF OUTPUT <> TTYOUTPUT THEN CLOSE(OUTPUT); (*don't close TTYOUTPUT!*)
  UNTIL NOT query('Copy another file?')
END.
