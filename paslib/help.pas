$WIDTH=100
$LENGTH=55
$TITLE HELP.PAS, last modified 10/14/83, zw

PROGRAM help;
(*display help file*)
$SYSTEM pasutl

VAR
    done: BOOLEAN;
    topic: STRING[6];

BEGIN
  IF start('HELP', ';') THEN BEGIN
    REPEAT
      done := NOT getcmd('Enter help topic: ', topic);
      IF NOT done THEN BEGIN
	IF topic = '' THEN
	  topic := '?';
	IF topic = '?' THEN BEGIN
	  wrlin('A help topic is a word of up to six characters.');
	  wrlin('For example, a program name can be a help topic.');
	  wrlin('Use "/EXIT" to exit.')
	END
	ELSE IF abbrev(topic, '/EXIT') THEN
	  done := TRUE
	ELSE
	  hlpfil(UPPERCASE(topic))
      END
    UNTIL done
  END
END.
    