PROGRAM num;
(*number lines in text*)

VAR
  num: INTEGER;
  lin: STRING[132];

BEGIN
  RESET(INPUT, 'LETTER.TXT');
  REWRITE(OUTPUT, 'LETTER.NUM');
  num := 1;
  WHILE NOT EOF DO BEGIN
    READLN(lin);
    IF lin = '' THEN BEGIN
      WRITELN
    END
    ELSE BEGIN
      WRITELN(num: 3, '  ', lin);
      num := num + 1
    END;
  END;
END.
