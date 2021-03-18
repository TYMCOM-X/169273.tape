PROGRAM getfile;
VAR l: STRING[132];
BEGIN
  OPEN(TTY);
  REWRITE(TTYOUTPUT); OUTPUT := TTYOUTPUT;
  WRITELN('This program will help in file transferrs to a PC.');
  LOOP
    WRITE('Enter file name: '); BREAK; READLN(TTY); READ(TTY, l);
    IF l = '' THEN STOP;
    RESET(INPUT, l);
    IF IOSTATUS <> IO_OK THEN WRITELN('Can not open file.')
    ELSE BEGIN
      WRITELN('Turn on your file reciever, type CR to begin/end.');
      BREAK;READLN(TTY);
      WHILE NOT EOF DO BEGIN READLN(l); WRITELN(l) END;
      BREAK;READLN(TTY);
      CLOSE(INPUT)
    END
  END
END.
