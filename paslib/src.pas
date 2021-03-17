MODULE srcutl;
(*source file utility*)

CONST
ver_siz = 10;

TYPE
int = INTEGER;
pos_int = 0 .. MAXIMUM(int);
ord_int = 1 .. MAXIMUM(int);
bin_fil = FILE OF *;
ver_str = PACKED ARRAY[1 .. ver_siz] OF CHAR;

$PAGE definitions

TYPE
src_fil = FILE OF CHAR; (*an internal source file*)
src_nam = FILE_NAME; (*an external source file*)
src_pos = RECORD (*position of a line within a source file*)
  opr: (nxt, prv, fst, lst, csr, lin); (*see srcpos for interpretation*)
  lin: ord_int; (*relative line number, first line is line 1*)
  csr: pos_int (*internal file cursor value for random access*)
END;
src_fil_ptr = ^src_fil_rcd; (*user source files are dynamic*)
src_fil_rcd = RECORD (*user source file*)
  nam: src_nam; (*external name*)
  fil: src_fil; (*internal structure*)
  pos: src_pos (*position of next source line to be read*)
END;
src_lin_ptr = ^src_lin_rcd; (*source lines are dynamic*)
src_lin_rcd = RECORD (*source line*)
  src: src_nam; (*source file from which source line was read*)
  pos: src_pos; (*position of source line within source file*)
  lin: PACKED ARRAY[1 .. *] OF CHAR (*actual text of source line*)
END;
src_lst_ptr = ^src_lst_rcd; (*nodes of source file list are dynamic*)
src_lst_rcd = RECORD (*node of source file list*)
  src: src_fil_ptr; (*the source file*)
  nxt: src_lst_ptr (*link to next node in list*)
END;

CONST
src_lst_ver = 'SRCLST 1.0'; (*version of source list structure*)
src_stk_ver = 'SRCSTK 1.0'; (*version of source stack structure*)

PUBLIC CONST
srcnxt: src_pos = (nxt, 1, 0); (*directs srcpos to seek next line*)
srcprv: src_pos = (prv, 1, 0); (*directs srcpos to seek previous line*)
srcfst: src_pos = (fst, 1, 0); (*directs srcpos to seek first line*)
srclst: src_pos = (lst, 1, 0); (*directs srcpos to seek last line*)

VAR
src_stk: src_lst_ptr := NIL; (*stack of currently "open" source files*)
src_lst: src_lst_ptr := NIL; (*list of available source files*)

$PAGE srccls, srcopn

PUBLIC PROCEDURE srccls(src: src_fil_ptr);
BEGIN (*close a source file, expand file name*)
  WITH src^ DO IF fil <> NILF THEN BEGIN
    nam := FILENAME(fil);
    CLOSE(fil);
    fil := NILF
  END
END;

PUBLIC PROCEDURE srcopn(src: src_fil_ptr);
BEGIN (*open a source file, expand file name*)
  WITH src^ DO IF fil = NILF THEN BEGIN
    RESET(fil, nam, [SEEKOK]);
    nam := FILENAME(fil);
    SEEK(fil, pos.csr)
  END
END;

$PAGE srcnew, srcnam

PUBLIC PROCEDURE srcnew(VAR src: src_fil_ptr; nam: src_nam);
BEGIN (*allocate and open a new source file*)
  NEW(src);
  src^.nam := nam; src^.fil := NILF; src^.pos := srcfst;
  srcopn(src)
END;

PUBLIC FUNCTION srcnam(nam: src_nam): src_fil_ptr;
(*retrieve source file record, allocate if necessary*)
VAR nxt: src_lst_ptr;
BEGIN
  (*search for existing source file record*)
  srcnam := NIL; (*assume not found*)
  nxt := src_lst;
  WHILE nxt <> NIL DO BEGIN
    EXIT IF nxt^.src^.nam = nam;
    nxt := nxt^.nxt
  END;
  (*return pointer to existing record or allocate a new one*)
  IF nxt <> NIL THEN srcnam := nxt^.src (*found*)
  ELSE BEGIN (*allocate new source file and list node*)
    NEW(nxt); srcnew(nxt^.src, nam); nxt^.nxt := src_lst; src_lst := nxt
  END
END;

$PAGE srcpsh, srcpop

PUBLIC PROCEDURE srcpsh(src: src_fil_ptr);
(*push source file*)
VAR nxt: src_lst_ptr;
BEGIN
  NEW(nxt);
  nxt^.src := src;
  nxt^.nxt := src_stk;
  src_stk := nxt
END;

PUBLIC PROCEDURE srcpop(VAR src: src_fil_ptr);
(*pop source file*)
VAR nxt: src_lst_ptr;
BEGIN
  IF src_stk <> NIL THEN BEGIN
    nxt := src_stk^.nxt;
    src := src_stk^.src;
    DISPOSE(src_stk);
    src_stk := nxt
  END
  ELSE src := NIL
END;

$PAGE srcput, srcget

PUBLIC PROCEDURE srcput(VAR fil: bin_fil);
(*store src module structures to binary file*)
VAR nxt: src_lst_ptr; len: pos_int; src: src_fil_ptr;
BEGIN
  (*store source file list, close any open source files*)
  WRITE(fil, src_lst_ver: SIZE(src_lst_ver));
  nxt := src_lst;
  WHILE nxt <> NIL DO BEGIN
    srccls(nxt^.src);
    len := SIZE(nxt^.src^);
    WRITE(fil, len: SIZE(len), nxt^.src^: len);
    nxt := nxt^.nxt
  END;
  len := 0; WRITE(fil, len: SIZE(len));
  (*store source file stack*)
  WRITE(fil, src_stk_ver: SIZE(src_stk_ver));
  WHILE src_stk <> NIL DO BEGIN
    srcpop(src);
    len := SIZE(src^.nam);
    WRITE(fil, len: SIZE(len), src^.nam: len)
  END;
  len := 0; WRITE(fil, len: SIZE(len));
  (*dispose of list nodes and source file nodes*)
  WHILE src_lst <> NIL DO BEGIN
    nxt := src_lst^.nxt;
    DISPOSE(src_lst^.src); DISPOSE(src_lst);
    src_lst := nxt
  END;
END;

$PAGE srcget

PUBLIC PROCEDURE srcget(VAR fil: bin_fil);
(*load source module structures from binary file*)
PROCEDURE revlst(VAR lst: src_lst_ptr);
(*reverse source file list*)
VAR lst_end, new_end: src_lst_ptr;
BEGIN
  IF (lst <> NIL) ANDIF (lst^.nxt <> NIL) THEN BEGIN
    (*search for end of list*)
    lst_end := lst; new_end := NIL;
    WHILE lst_end^.nxt <> NIL DO BEGIN
      new_end := lst_end; lst_end := lst_end^.nxt
    END;
    (*make end of list beginning, establish new end of list*)
    lst_end^.nxt := lst; lst := lst_end; new_end^.nxt := NIL
  END
END;
VAR len: pos_int; nxt: src_lst_ptr; nam: src_nam; ver: ver_str;
BEGIN
  ASSERT((src_stk = NIL) AND (src_lst = NIL));
  (*load source file list*)
  READ(fil, ver: SIZE(ver)); ASSERT(ver = src_stk_ver);
  READ(fil, len: SIZE(len));
  WHILE len > 0 DO BEGIN
    IF src_lst = NIL THEN BEGIN NEW(nxt); src_lst := nxt END
    ELSE BEGIN NEW(nxt^.nxt); nxt := nxt^.nxt END;
    nxt^.nxt := NIL;
    NEW(nxt^.src); READ(fil, nxt^.src^: len, len: SIZE(len))
  END;
  (*load source file stack*)
  READ(fil, ver: SIZE(ver)); ASSERT(ver = src_stk_ver);
  READ(fil, len: SIZE(len));
  WHILE len > 0 DO BEGIN
    READ(fil, nam: len, len: SIZE(len)); srcpsh(srcnam(nam));
  END;
  revlst(src_stk) (*reverse ordering of stack*)
END;

$PAGE srcpos

PUBLIC PROCEDURE srcpos(src: src_fil_ptr; pos: src_pos);
(*seek to specified position in source file*)
PROCEDURE skp(src: src_fil_ptr; n: int);
(*skip n lines in source file*)
VAR i: ord_int;
BEGIN
  IF n > 0 THEN BEGIN (*skip forward*)
    FOR i := 1 TO n DO BEGIN
      EXIT IF EOF(src^.fil);
      REPEAT GET(src^.fil) UNTIL EOF(src^.fil) ORIF EOLN(src^.fil)
    END
  END
  ELSE IF n < 0 BEGIN (*skip backward*)
    n := src^.pos.lin + n;
    srcpos(src, srcfst);
    skp(src, n)
  END
END;
VAR new_pos: src_pos;
BEGIN
  srcopn(src);
  CASE pos.opr OF
    nxt: new_pos := (lin, pos.lin + 1, 0); (*seek to next line*)
    prv: new_pos := (lin, pos.lin - 1, 0); (*seek to previous line*)
    fst: new_pos := (csr, 1, 1); (*seek to first line*)
    lst: BEGIN (*seek to last line*)
      skp(src, MAXIMUM(src^.pos.lin)); new_pos := srcprv
    END;
    csr: new_pos := pos; (*seek to specified cursor position*)
    lin: BEGIN (*seek to specified line position*)
      skp(src, pos.lin - src^.pos.lin); new_pos := src^.pos
    END;
  END;
  IF new_pos.opr <> csr THEN srcpos(src, new_pos)
  ELSE WITH src^ DO IF pos <> new_pos THEN BEGIN
    pos := new_pos; SEEK(fil, pos.csr)
  END
END;

$PAGE srclin

PUBLIC FUNCTION srclin(src: src_fil_ptr; VAR lin: src_lin_ptr): BOOLEAN;
(*read in the next source line from the specified source file*)
CONST buf_siz = 80;
TYPE
buf_ptr = ^buf_rcd;
buf_rcd := RECORD str: STRING[buf_siz]; nxt: buf_ptr END;
VAR lin_pos: src_pos; buf, nxt: buf_ptr; len: pos_int; i: ord_int;
BEGIN
  srcopn(src);
  IF EOF(src^.fil) THEN srclin := FALSE
  ELSE BEGIN
    lin_pos := src^.pos; (*remember source file position of this line*)
    (*read line into a dynamic buffer*)
    buf := NIL; len := 0;
    WHILE NOT EOLN(src^.fil) DO BEGIN
      IF buf = NIL THEN BEGIN NEW(buf); nxt := buf END
      ELSE BEGIN NEW(nxt^.nxt); nxt := nxt END;
      nxt^.nxt := NIL;
      READ(src^.fil, nxt^.str); len := len + LENGTH(nxt^.str)
    END;
    (*allocate source line record*)
    NEW(lin, len); lin^.src := src^.nam; lin^.pos := lin_pos; lin^.lin := '';
    (*transferr contents of dynamic buffer to source line record*)
    WHILE buf <> NIL DO BEGIN
      lin^.lin := lin^.lin || buf^.str;
      nxt := buf^.nxt; DISPOSE(buf); buf := nxt
    END;
    (*prepare for next line*)
    READLN(src^.fil);
    WITH src^.pos DO BEGIN
      lin : lin + 1; csr := CURSOR(src^.fil)
    END
  END
END.
    