PROGRAM src_f_r_d;
(*source file relationship database, zw*)

TYPE
pos_int = 0 .. MAXIMUM(INTEGER);
ord_int = 1 .. MAXIMUM(pos_int);
opt_nam = STRING[20];
rel_nam = STRING[10];
fil_nam = FILE_NAME;
opt_arg = ARRAY[1 .. *] OF opt_nam;
fil_ptr = ^fil_rcd;
fil_rcd = RECORD nam: fil_nam; nxt: fil_ptr END;
rel_ptr = ^rel_rcd;
rel_rcd = RECORD
  nam: rel_nam; to_nam, from_nam: fil_nam; nxt: rel_ptr
END;

CONST
bld_opts: ARRAY[1 .. 3] OF opt_nam = ('LIST', 'SOURCE', 'RELATION');
del_opts: ARRAY[1 .. 3] OF opt_nam = ('RELATION', 'SOURCE', 'DATABASE');
qry_opts: ARRAY[1 .. 9] OF opt_nam =
  ('EXIT', 'SOURCE', 'RELATION', 'SHOW', 'CLEAR', 'FROM', 'TO', 'SAVE', 'ALL');
cmd_opts: ARRAY[1 .. 7] OF opt_nam =
  ('EXIT', 'BUILD', 'DELETE', 'QUERY', 'DATABASE', 'ABORT', 'HELP');

VAR
rels_fil: fil_nam;
fils: fil_ptr;
rels: rel_ptr;

PROCEDURE opn_tty;
BEGIN
  OPEN(TTY); REWRITE(TTYOUTPUT)
END;

PROCEDURE msg(msg: STRING[*]);
BEGIN
  WRITELN(TTYOUTPUT, msg); BREAK(TTYOUTPUT)
END;

PROCEDURE ask(qst: STRING[*]; VAR ans: STRING[*]);
BEGIN
  msg(qst); READLN(TTY); READ(TTY, ans); ans := UPPERCASE(ans)
END;

FUNCTION abbrev(str1, str2: STRING[*]): BOOLEAN;
BEGIN
  abbrev := (LENGTH(str1) > 0) ANDIF (LENGTH(str1) <= LENGTH(str2))
    ANDIF (UPPERCASE(str1) = UPPERCASE(SUBSTR(str2, 1, LENGTH(str1))))
END;

FUNCTION opt_num(opts: opt_arg; opt: opt_nam): pos_int;
VAR opt_idx: pos_int;
BEGIN
  opt_num := 0;
  FOR opt_idx := 1 TO UPPERBOUND(opts) DO
    EXIT IF abbrev(opt, opts[opt_idx]) DO opt_num := opt_idx
END;

FUNCTION query(qst: STRING[*]; opts: opt_arg): ord_int;
VAR opt: pos_int; i: ord_int; ans: opt_nam;
BEGIN
  REPEAT
    msg(''); ask(qst, ans); opt := opt_num(opts, ans);
    IF opt < 1 THEN BEGIN
      msg('Your options are:');
      FOR i := 1 TO UPPERBOUND(opts) DO msg(opts[i]);
    END
  UNTIL opt > 0;
  query := opt
END;

PROCEDURE zap_fil(VAR fil: fil_ptr);
VAR tmp: fil_ptr;
BEGIN
  WHILE fil <> NIL DO BEGIN
    tmp := fil; fil := fil^.nxt; tmp^.nxt := NIL; DISPOSE(tmp)
  END
END;

PROCEDURE zap_rel(VAR rel: rel_ptr);
VAR tmp: rel_ptr;
BEGIN
  WHILE rel <> NIL DO BEGIN
    tmp := rel; rel := rel^.nxt; tmp^.nxt := NIL; DISPOSE(tmp)
  END
END;

FUNCTION new_fil(nam: fil_nam): fil_ptr;
BEGIN
  NEW(new_fil); new_fil^.nam := nam; new_fil^.nxt := NIL
END;

FUNCTION new_rel(nam: rel_nam; to_nam, from_nam: fil_nam): rel_ptr;
BEGIN
  NEW(new_rel);
  new_rel^.nam := nam;
  new_rel^.to_nam := to_nam; new_rel^.from_nam := from_nam;
  new_rel^.nxt := NIL
END;

PROCEDURE add_fil(nam: fil_nam);
VAR fil: fil_ptr;
BEGIN
  fil := fils;
  IF fil = NIL THEN fils := new_fil(nam)
  ELSE BEGIN
    WHILE fil^.nxt <> NIL DO BEGIN
      EXIT IF fil^.nam = nam;
      fil := fil^.nxt
    END;
    IF NOT (fil^.nam = nam) THEN fil^.nxt := new_fil(nam)
  END
END;

PROCEDURE add_rel(nam: rel_nam; to_nam, from_nam: fil_nam);
VAR rel: rel_ptr;
BEGIN
  add_fil(to_nam); add_fil(from_nam); rel := rels;
  IF rel = NIL THEN rels := new_rel(nam, to_nam, from_nam)
  ELSE BEGIN
    WHILE rel^.nxt <> NIL DO BEGIN
      EXIT IF (rel^.nam = nam) ANDIF (rel^.to_nam = to_nam) ANDIF
	(rel^.from_nam = from_nam);
      rel := rel^.nxt
    END;
    IF NOT ((rel^.nam = nam) ANDIF (rel^.to_nam = to_nam) ANDIF
      (rel^.from_nam = from_nam)) THEN
	rel^.nxt := new_rel(nam, to_nam, from_nam)
  END
END;

FUNCTION fix_nam(nam, ext: STRING[*]): fil_nam;
VAR nam_pos, nam_len: ord_int;
BEGIN
  IF nam = '' THEN fix_nam := ''
  ELSE BEGIN
    nam_pos := VERIFY(nam, [' ']);
    nam_len := SEARCH(SUBSTR(nam || ' ', nam_pos), [' ']) - 1;
    IF INDEX(nam, '.') > 0 THEN
      fix_nam := SUBSTR(nam, nam_pos, MIN(nam_len, UPPERBOUND(fil_nam)))
    ELSE BEGIN
      nam_len := MIN(nam_len + LENGTH(ext) + 1, UPPERBOUND(fil_nam));
      fix_nam := SUBSTR(nam || '.' || ext, nam_pos, nam_len)
    END
  END
END;

PROCEDURE rd_fils(fils_fil: fil_nam);
VAR nam: fil_nam;
BEGIN
  RESET(INPUT, fils_fil);
  WHILE NOT EOF DO BEGIN
    READLN(nam); add_fil(UPPERCASE(nam))
  END;
  IF INPUT <> TTY THEN CLOSE(INPUT)
END;

PROCEDURE wr_fils(fils_fil: fil_nam);
VAR fil: fil_ptr;
BEGIN
  REWRITE(OUTPUT, fils_fil); fil := fils;
  WHILE fil <> NIL DO BEGIN
    WRITELN(fil^.nam); fil := fil^.nxt
  END;
  IF OUTPUT <> TTYOUTPUT THEN CLOSE(OUTPUT)
END;

PROCEDURE rd_rels(rels_fil: fil_nam);
VAR nam: rel_nam; to_nam, from_nam: fil_nam;
BEGIN
  RESET(INPUT, rels_fil);
  WHILE NOT EOF DO BEGIN
    READLN(nam, to_nam, from_nam);
    add_rel(UPPERCASE(nam), UPPERCASE(to_nam), UPPERCASE(from_nam))
  END;
  IF INPUT <> TTY THEN CLOSE(INPUT)
END;

PROCEDURE wr_rels(rels_fil: fil_nam);
VAR rel: rel_ptr;
BEGIN
  REWRITE(OUTPUT, rels_fil); rel := rels;
  WHILE rel <> NIL DO BEGIN
    WRITELN(rel^.nam, ', ', rel^.to_nam, ', ', rel^.from_nam);
    rel := rel^.nxt
  END;
  IF OUTPUT <> TTYOUTPUT THEN CLOSE(OUTPUT)
END;

PROCEDURE chk_fil(nam: fil_nam);
VAR lin: STRING[132];
BEGIN
  RESET(INPUT, nam); msg('Checking source file: ' || nam);
  IF NOT EOF THEN add_rel('SELF', nam, nam)
  ELSE msg('No such file "' || nam || '"!');
  WHILE NOT EOF DO BEGIN
    READLN(lin);
    IF INDEX(lin, '$') = 1 THEN BEGIN
      lin := UPPERCASE(lin);
      IF INDEX(lin, 'HEADER') = 2 THEN
	add_rel('HEADER', nam, fix_nam(SUBSTR(lin, 8), 'HDR'))
      ELSE IF INDEX(lin, 'SYSTEM') = 2 THEN
	add_rel('SYSTEM', nam, fix_nam(SUBSTR(lin, 8), 'INC'))
      ELSE IF INDEX(lin, 'INCLUDE') = 2 THEN
	add_rel('INCLUDE', nam, fix_nam(SUBSTR(lin, 9), 'INC'))
    END
  END;
  IF INPUT <> TTY THEN CLOSE(INPUT)
END;

PROCEDURE all_fils;
VAR rel: rel_ptr;
BEGIN
  rel := rels; zap_fil(fils);
  WHILE rel <> NIL DO BEGIN
    IF rel^.nam = 'SELF' THEN add_fil(rel^.to_nam);
    rel := rel^.nxt
  END
END;

PROCEDURE del_src(src_fil: fil_nam);
VAR rel, last_rel: rel_ptr;
BEGIN
  rel := rels; last_rel := NIL;
  WHILE rel <> NIL DO BEGIN
    IF (rel^.to_nam = src_fil) OR (rel^.from_nam = src_fil) THEN BEGIN
      IF last_rel = NIL THEN BEGIN
	rels := rel^.nxt; rel^.nxt := NIL;
	zap_rel(rel); rel := rels
      END
      ELSE BEGIN
	last_rel := rel^.nxt; rel^.nxt := NIL;
	zap_rel(rel); rel := last_rel^.nxt
      END
    END
    ELSE BEGIN last_rel := rel; rel := rel^.nxt END
  END
END;

PROCEDURE del_rel(nam: rel_nam; to_nam, from_nam: fil_nam);
VAR rel, last_rel: rel_ptr;
BEGIN
  rel := rels; last_rel := NIL;
  WHILE rel <> NIL DO BEGIN
    IF (rel^.nam = nam) ANDIF (rel^.to_nam = to_nam) ANDIF
      (rel^.from_nam = from_nam)
    THEN BEGIN
      IF last_rel = NIL THEN BEGIN
	rels := rel^.nxt; rel^.nxt := NIL;
	zap_rel(rel); rel := rels
      END
      ELSE BEGIN
	last_rel := rel^.nxt; rel^.nxt := NIL;
	zap_rel(rel); rel := last_rel^.nxt
      END
    END
    ELSE BEGIN last_rel := rel; rel := rel^.nxt END
  END
END;

PROCEDURE qry_from(nam: rel_nam);
VAR fil: fil_ptr; rel: rel_ptr;
BEGIN
  fil := fils;
  WHILE fil <> NIL DO BEGIN
    rel := rels;
    WHILE rel <> NIL DO BEGIN
      EXIT IF ((nam = '*') ORIF (rel^.nam = nam))
        ANDIF (rel^.to_nam = fil^.nam) DO add_fil(rel^.from_nam);
      rel := rel^.nxt
    END;
    fil := fil^.nxt
  END
END;

PROCEDURE qry_to(nam: rel_nam);
VAR fil: fil_ptr; rel: rel_ptr;
BEGIN
  fil := fils;
  WHILE fil <> NIL DO BEGIN
    rel := rels;
    WHILE rel <> NIL DO BEGIN
      EXIT IF ((nam = '*') ORIF (rel^.nam = nam)) ANDIF
        (rel^.from_nam = fil^.nam) DO add_fil(rel^.to_nam);
      rel := rel^.nxt
    END;
    fil := fil^.nxt
  END
END;

PROCEDURE bld_cmd;
VAR fils_fil, src_fil, to_nam, from_nam: fil_nam;
  fil: fil_ptr; nam: rel_nam;
BEGIN
  all_fils; fil := fils;
  IF fil <> NIL THEN WHILE fil^.nxt <> NIL DO fil := fil^.nxt;
  CASE query('From what should the database be built?', bld_opts) OF
    1: BEGIN
      ask('Enter the file name of the file list input.', fils_fil);
      rd_fils(fils_fil)
    END;
    2: BEGIN
      ask('Enter the source file name.', src_fil); add_fil(src_fil)
    END;
    3: BEGIN
      msg('Enter the relation, <name, from, to>.');
      READLN(TTY); READ(TTY, nam, to_nam, from_nam);
      add_rel(UPPERCASE(nam), UPPERCASE(to_nam), UPPERCASE(from_nam));
      zap_fil(fils); fil := NIL
    END
  END;
  IF fil = NIL THEN fil := fils;
  WHILE fil <> NIL DO BEGIN chk_fil(fil^.nam); fil := fil^.nxt END;
  zap_fil(fils)
END;

PROCEDURE del_cmd;
VAR nam: rel_nam; to_nam, from_nam, src_fil: fil_nam;
BEGIN
  CASE query('What should be deleted?', del_opts) OF
    1: BEGIN
      msg('Enter the relation <name, from, to> to be deleted.');
      READLN(TTY); READ(nam, to_nam, from_nam);
      del_rel(UPPERCASE(nam), UPPERCASE(to_nam), UPPERCASE(from_nam))
    END;
    2: BEGIN
      ask('Enter the source file name to be deleted.', src_fil);
      del_src(src_fil)
    END;
    3: zap_rel(rels)
  END
END;

PROCEDURE qry_cmd;
VAR src_fil, fils_fil: fil_nam; nam: rel_nam;
BEGIN
  nam := '*'; zap_fil(fils);
  LOOP
    CASE query('Enter a query option.', qry_opts) OF
      1: RETURN;
      2: BEGIN
	ask('Enter a source file name.', src_fil); add_fil(src_fil)
      END;
      3: ask('Enter a relation name, or "*".', nam);
      4: BEGIN
	msg('The relation name is "' || nam || '".');
	IF fils = NIL THEN msg('The file list is empty.')
	ELSE BEGIN msg('The file list follows.'); wr_fils('TTY:') END
      END;
      5: zap_fil(fils);
      6: qry_from(nam);
      7: qry_to(nam);
      8: BEGIN
	ask('Enter a file name for the file list output.', fils_fil);
	wr_fils(fils_fil)
      END;
      9: all_fils
    END
  END
END;

PROCEDURE hlp_cmd;
BEGIN
  msg('The "EXIT" command terminates this program after writing the');
  msg('database back to its file.');
  msg('');
  msg('The BUILD command is used to load in new relation entries into');
  msg('the database structure.  Source files are searched for "$HEADER",');
  msg('"$SYSTEM" and "$INCLUDE" to make up the corresponding "HEADER",');
  msg('"SYSTEM" and "INCLUDE" relations with their indicated files.');
  msg('The default extension for "SYSTEM" and "INCLUDE" files is "INC"');
  msg('and the default extension for "HEADER" files is "HDR". ');
  msg('Source files are always included in the "SELF" relation.  The');
  msg('"SOURCE" option searches a single source file and all of its');
  msg('related files.  The "LIST" option allows the search to be applied');
  msg('to many source files given in a file of one source file name per');
  msg('line.  The "RELATION" option adds a single relation entry.');
  msg('');
  msg('The "DELETE" command is used to remove relation entries from the');
  msg('database.  The "SOURCE" option will remove all relations involving');
  msg('the specified source file.  The "RELATION" option will remove a');
  msg('specific relation.  The "DATBASE" option removes all relations.');
  msg('');
  msg('Enter a carriage-return to continue.');
  READLN(TTY);
  msg('The "QUERY" command is used to extract information from the file');
  msg('relationship database.  All "QUERY" operations involve the file');
  msg('list, which is simply a list of file names. The "EXIT" option');
  msg('terminates "QUERY". The "SOURCE" option inserts a source file name');
  msg('into the file list. The "RELATION" sets the current relation, one');
  msg('of "SELF", "HEADER", "SYSTEM" or "INCLUDE".  Use "*" for all.  The');
  msg('"SHOW" option will display the current relation and file list.');
  msg('The "CLEAR" option empties the file list.  The "FROM" option adds');
  msg('to the file list all files related "FROM" files in the list.  The');
  msg('"TO" option adds to the file list all files related "TO" files in');
  msg('the file list.  Use "FROM" to make up "requires file" lists and');
  msg('"TO" to make up "required by file" lists.  The "SAVE" option will');
  msg('write out the current file name list to a specified text file, one');
  msg('name per line.  The "ALL" option makes a list of all source files.');
  msg('');
  msg('The "DATABASE" command is used to access another database file.');
  msg('');
  msg('The "ABORT" command exits without updating the database.');
  msg('');
  msg('The "HELP" command displays this documentation.');
END;

BEGIN
  opn_tty; fils := NIL; rels := NIL; rels_fil := 'SRCFRD.DAT';
  msg('This program maintains a source file relationship database.');
  (*ask('Enter the name of a database file.', rels_fil);*)
  rd_rels(rels_fil);
  LOOP
    CASE query('Enter a command.', cmd_opts) OF
      1: BEGIN wr_rels(rels_fil); STOP END;
      2: bld_cmd;
      3: del_cmd;
      4: qry_cmd;
      5: BEGIN
	ask('Enter the name of the database file.', rels_fil);
	wr_rels(rels_fil); zap_rel(rels); rd_rels(rels_fil);
      END;
      6: STOP;
      7: hlp_cmd
    END
  END
END.
   