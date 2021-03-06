PROGRAM zappas;
  (*startup program for pascal compiler when compiler is to be zapped*)
  
CONST pascal_compiler = 'PASCAL[3,325601]';

EXTERNAL PROCEDURE run
  (nam: PACKED ARRAY [1 .. *] OF CHAR; do_auto_run: BOOLEAN);
  (*from RUN.MAC, runs specified program with specified auto_run flag*)

EXTERNAL VAR auto_run: BOOLEAN; (*from AUTORU.MAC, TRUE -> use cmd fil*)

PROCEDURE display_message;
  BEGIN
    WRITELN(TTY);
    WRITELN(TTY, 'This is a test version of TYM-PASCAL.');
    WRITELN(TTY);
    BREAK(TTY)
    END;

PROCEDURE start_pascal;
  BEGIN
    run(pascal_compiler, auto_run);
    REWRITE(TTY);
    WRITELN(TTY, '? Can not run PASCAL compiler.')
    END;

PROCEDURE set_up;
  BEGIN
    REWRITE(TTY)
    END;

PROCEDURE clean_up;
  BEGIN
    END;

BEGIN
  set_up;
  display_message;
  start_pascal;
  clean_up
  END.
    