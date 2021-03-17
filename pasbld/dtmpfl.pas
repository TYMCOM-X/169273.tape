
PROGRAM dtmpfl;
$INCLUDE infpac.inc

VAR
jb_rcd: jobrec;
jb_numb: INTEGER;
sng: PACKED ARRAY[1..10] OF CHAR;
cmd_flnm,tmp_flnm: FILE_NAME;
cmd_fl,tmp_fl: TEXT;
tmpnm: PACKED ARRAY[1..3] OF CHAR;
ln_sng: STRING[256];

BEGIN
  jobinfo(jb_rcd);
  jb_numb:=jb_rcd.jobnum;
  PUTSTRING(sng,jb_numb:10);
  IF sng[9]=' ' THEN sng[9]:='0';
  IF sng[8]=' ' THEN sng[8]:='0';
  OPEN(TTY);
  REWRITE(TTY);
  WRITE(TTY,' command file name : ');
  BREAK(TTY);
  READLN(TTY);
  READ(TTY,cmd_flnm);
  WRITE(TTY,' temp file name (only 3 letters) : ');
  BREAK(TTY);
  READLN(TTY);
  READ(TTY,tmpnm);
  tmp_flnm:=SUBSTR(sng,8,3)||tmpnm||'.tmp';
  OPEN(cmd_fl,cmd_flnm);
  READLN(cmd_fl);
  REWRITE(tmp_fl,tmp_flnm);
  WHILE NOT EOF(cmd_fl) DO BEGIN
    READLN(cmd_fl,ln_sng);
    WRITELN(tmp_fl,ln_sng)
  END;
  CLOSE
END.
