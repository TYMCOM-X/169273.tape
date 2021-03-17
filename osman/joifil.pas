PROGRAM JoinFiles;

PROCEDURE Concatenate( VAR ToFile: TEXT; FromFile: String[10] );
VAR CurrentCharacter: CHAR;
    Input : TEXT;
BEGIN
  RESET (Input,FromFile);
  WHILE NOT EOF (Input) DO BEGIN
    WHILE NOT EOLN (Input) DO BEGIN
      READ (Input, CurrentCharacter);
      WRITE (ToFile,CurrentCharacter);
    END;(*while not eoln*)
    READLN (Input);
    WRITELN (ToFile);
  END;(*end while not eof*)
END;(* end concatenata*)

BEGIN
OPEN(Output);
REWRITE(Output,'TemFil');
Concatenate (Output, 'FileA');
Concatenate (Output, 'FileB');
CLOSE(Output);
OPEN(Output);
REWRITE(Output,'FileA');
Concatenate (Output,'Temfil');
CLOSE(Output);
END.(*end joinfiles*)
