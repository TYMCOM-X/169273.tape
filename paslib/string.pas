PROCEDURE rtrim(VAR s: STRING[*]);
VAR i: INTEGER;
BEGIN (*right trim string of spaces and tabs*)
  s := SUBSTR(s, 1, SEARCH(s, [' ', CHR(9)], LENGTH(s) + 1) - 1)
END;

