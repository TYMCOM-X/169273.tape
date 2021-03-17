$WIDTH=100
$LENGTH=55
$TITLE UTLOPN.PAS, last modified 12/20/83, zw
MODULE utlopn;
(*TYM-Pascal source file open utility*)

$HEADER UTLSRC.HDR

$INCLUDE UTLOPN.TYP

$PAGE newsrch, zapsrch

PUBLIC FUNCTION newsrch(VAR lst: srch_lst; nam: STRING[*]): srch_lst;
(*append a name into the specified search list*)
BEGIN
  NEW(newsrch, LENGTH(nam));
  newsrch^.nxt := lst;
  newsrch^.nam := nam
END;

PUBLIC PROCEDURE zapsrch(VAR lst: srch_lst);
(*dispose of a search list*)
VAR tmp: srch_lst;
BEGIN
  WHILE lst <> NIL DO BEGIN
    tmp := lst;
    lst := lst^.nxt;
    DISPOSE(tmp)
  END
END;

$PAGE wrsrch, rdsrch

PUBLIC PROCEDURE wrsrch(lst: srch_lst; VAR fil: FILE OF * );
(*write search list to binary file*)
VAR tmp: srch_lst; len: INTEGER;
BEGIN
  tmp := lst;
  WHILE tmp <> NIL DO BEGIN
    len := LENGTH(tmp^.nam);
    WRITE(fil, len, tmp^: SIZE(tmp^, len));
    tmp := tmp^.nxt
  END;
  len := 0;
  WRITE(fil, len)
END;

PUBLIC FUNCTION rdsrch(VAR fil: FILE OF * ): srch_lst;
(*read search list from binary file*)
VAR tmp, last_rcd: srch_lst; len: INTEGER;
BEGIN
  rdsrch := NIL;
  LOOP
    READ(fil, len);
    EXIT IF len = 0;
    NEW(tmp, len);
    READ(fil, tmp^: SIZE(tmp^, len));
    tmp^.nxt := NIL;
    IF rdsrch = NIL THEN rdsrch := tmp
    ELSE last_rcd^.nxt := tmp;
    last_rcd := tmp
  END
END;

$PAGE opnsrc

PUBLIC FUNCTION opnsrc(lst: srch_lst; VAR fil: TEXT; nam: FILE_NAME): BOOLEAN;
(*try to open source file in current directory or directory in search list*)
VAR tmp: srch_lst;
BEGIN
  RESET(fil, nam);
  tmp := lst;
  WHILE EOF(fil) AND (tmp <> NIL) DO BEGIN
    RESET(fil, tmp^.nam || nam);
    tmp := tmp^.nxt
  END;
  opnsrc := NOT EOF(fil)
END.
