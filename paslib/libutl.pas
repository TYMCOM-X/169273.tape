
$WIDTH=100
$LENGTH=55
$TITLE libutl.pas, lst modified 4/6/83, zw
MODULE libutl OPTIONS SPECIAL(WORD);
  (*file library utility*)

$SYSTEM WRTUTL
$SYSTEM PIOUTL
$SYSTEM ERRUTL

$PAGE libutl types
$INCLUDE LIBUTL.TYP

CONST
  num_blks = 1000;
  num_dir_fils = 34;
  num_dat_wrds = 127;
  old_spc = 'OLD SPACE ';
  new_spc = 'NEW SPACE ';
  use_spc = 'USED SPACE';
  fre_dir = 'FREE DIREC';
  opn_lib = 'OPN_LIB';
  no_lib = 'NO_LIB';
  no_spc = 'NO_SPC';
  no_fil = 'NO_FIL';
  bad_dir = 'BAD_DIR';
  bad_ext_fil = 'BAD_EXT_FIL';
  bad_int_fil = 'BAD_INT_FIL';

$PAGE
TYPE
  blk_is = (is_dir, is_dat);
  nxt_ptr_typ = zero .. num_blks;
  blk_ptr_typ = one .. num_blks;
  dat_wrd_ptr_typ = one .. num_dat_wrds;
  dir_rcd_ptr_typ = one .. num_dir_fils;
  dir_rcd_typ = RECORD
    nam : PACKED ARRAY [one .. 10] OF chr;
    len : pos_int;
    dat_blk_ptr : nxt_ptr_typ;
    (*file status information should go here*)
    END;
  dat_wrd_typ = -#o377777777777 .. #o377777777777;
  dir_blk_typ = RECORD
    nxt : nxt_ptr_typ;
    fils : PACKED ARRAY [dir_rcd_ptr_typ] OF dir_rcd_typ;
    spc : PACKED ARRAY [one .. three] OF dat_wrd_typ
    END;
  dat_blk_typ = RECORD
    nxt : nxt_ptr_typ;
    wrds : PACKED ARRAY [dat_wrd_ptr_typ] OF dat_wrd_typ
    END;
  blk_typ = RECORD
    CASE blk_is OF
      is_dir : (dir_blk : dir_blk_typ);
      is_dat : (dat_blk : dat_blk_typ)
    END;
  lib_fil_typ = FILE OF blk_typ; dat_fil_typ = FILE OF dat_wrd_typ;
  dat_typ = RECORD
    blk : dat_blk_typ;
    blk_ptr : blk_ptr_typ; nxt : nxt_ptr_typ
    END;
  fil_typ = RECORD
    nam : fil_nam;
    len : pos_int;
    dat : RECORD blk_ptr : nxt_ptr_typ END;
    dir : RECORD fil_ptr : dir_rcd_ptr_typ; blk_ptr : blk_ptr_typ END
    (*file status information should go here*)
    END;
  dir_typ = RECORD
    blk : dir_blk_typ;
    blk_ptr : blk_ptr_typ; nxt : nxt_ptr_typ
    END;
  lib_typ = RECORD
    nam : fil_nam; fil : lib_fil_typ;
    dir : dir_typ
    END;

VAR
  cnt_set_cln : int := zero;
  lib : lib_typ;

$PAGE external file status
PROCEDURE get_fil_stat(dat_fil : dat_fil_typ; VAR fil : fil_typ);
  (*read system status of data file to directory record*)
  BEGIN (*read system dependant file attributes*)
    END;

PROCEDURE put_fil_stat(VAR dat_fil : dat_fil_typ; fil : fil_typ);
  (*set system status of data file from directory record*)
  BEGIN (*set system dependant file attributes*)
    END;

$PAGE external/internal data block get and put
PROCEDURE get_dat(VAR dat_fil : dat_fil_typ; VAR dat : dat_typ;
  VAR dat_cnt : pos_int);
  (*get next block from data file, count data words actually read*)
  VAR idx : one .. num_dat_wrds;
  BEGIN (*this should eventually be an actual block transferr*)
    FOR idx := one TO num_dat_wrds DO BEGIN (*one word at a time*)
      EXIT IF EOF(dat_fil);
      dat.blk.wrds[idx] := dat_fil^; GET(dat_fil); inc(dat_cnt)
      END
    END;

PROCEDURE put_dat(VAR dat_fil : dat_fil_typ; VAR dat : dat_typ;
  VAR dat_cnt : pos_int);
  (*put next block to data file, count data words actually written*)
  VAR idx : one .. num_dat_wrds;
  BEGIN (*this should eventually be an actual block transferr*)
    FOR idx := one TO num_dat_wrds DO BEGIN (*one word at a time*)
      EXIT IF dat_cnt < one;
      dat_fil^ := dat.blk.wrds[idx]; PUT(dat_fil); inc(dat_cnt)
      END
    END;

FUNCTION get_nxt_dat_blk(VAR dat : dat_typ) : succeed_or_fail;
  (*try to get next data block from library*)
  VAR buf : blk_typ;
  BEGIN (*fails if end of linked list*)
    get_nxt_dat_blk := dat.nxt <> zero;
    IF get_nxt_dat_blk THEN WITH dat DO BEGIN (*do block transferr*)
      blk_ptr := nxt; READRN(lib.fil, blk_ptr, buf);
      blk := buf.dat_blk; nxt := blk.nxt
      END
    END;

PROCEDURE put_dat_blk(VAR dat : dat_typ);
  (*put data block into library*)
  VAR buf : blk_typ;
  BEGIN (*dat.blk_ptr must be valid*)
    WITH dat DO BEGIN (*do block transferr*)
      blk.nxt := nxt; buf.dat_blk := blk; WRITERN(lib.fil, blk_ptr, buf)
      END
    END;

$PAGE directory block get and put, directory block set up
FUNCTION get_nxt_dir_blk : succeed_or_fail;
  (*try to get next directory block from library*)
  VAR buf : blk_typ;
  BEGIN (*does not put current block! fails if end of linked list*)
    get_nxt_dir_blk := lib.dir.nxt <> zero;
    IF get_nxt_dir_blk THEN WITH lib.dir DO BEGIN (*do block transferr*)
      blk_ptr := nxt; READRN(lib.fil, blk_ptr, buf);
      blk := buf.dir_blk; nxt := blk.nxt
      END
    END;

PROCEDURE put_dir_blk;
  (*put directory block into library*)
  VAR buf : blk_typ;
  BEGIN (*lib.dir.blk_ptr must be valid*)
    WITH lib.dir DO BEGIN (*do block transferr*)
      blk.nxt := nxt; buf.dir_blk := blk; WRITERN(lib.fil, blk_ptr, buf)
      END
    END;

PROCEDURE dir_set_up;
  (*set up directory block with initial values*)
  VAR idx : one .. num_dir_fils;
  BEGIN (*all file entries become fre_dir, lib.dir totally clobbered*)
    FOR idx := one TO num_dir_fils DO
      WITH lib.dir.blk.fils[idx] DO BEGIN (*set up a file record*)
        nam := fre_dir; dat_blk_ptr := zero; len := zero
        (*should load initial status values here*)
        END;
    lib.dir.blk.nxt := zero; lib.dir.nxt := zero
    END;

$PAGE directory file record get and put
PROCEDURE get_dir_fil(VAR fil : fil_typ);
  (*get directory file record from library*)
  BEGIN (*puts current lib.dir block if a different one is needed*)
    IF lib.dir.blk_ptr <> fil.dir.blk_ptr THEN BEGIN
      put_dir_blk; lib.dir.nxt := fil.dir.blk_ptr;
      IF NOT get_nxt_dir_blk THEN sgnl_fatl_err(bad_dir)
      END;
    WITH lib.dir.blk.fils[fil.dir.fil_ptr] DO BEGIN (*load file record*)
      fil.nam := nam; fil.len := len; fil.dat.blk_ptr := dat_blk_ptr
      (*would transferr file status information here*)
      END
    END;

FUNCTION get_nxt_dir_fil(VAR fil : fil_typ) : succeed_or_fail;
  (*try to get next directory file record from library*)
  BEGIN (*fails if at end of linked list*)
    get_dir_fil(fil);
    IF fil.dir.fil_ptr < num_dir_fils THEN BEGIN (*stay in current block*)
      inc(fil.dir.fil_ptr); get_nxt_dir_fil := succeed
      END
    ELSE BEGIN (*set up to get new block*)
      get_nxt_dir_fil := lib.dir.nxt <> zero;
      IF get_nxt_dir_fil THEN fil.dir.blk_ptr := lib.dir.nxt
      END;
    IF get_nxt_dir_fil THEN get_dir_fil(fil)
    END;

PROCEDURE put_dir_fil(VAR fil : fil_typ);
  (*put directory record back into library*)
  BEGIN (*does not put the lib.dir block for this file back into the lib.fil*)
    IF lib.dir.blk_ptr <> fil.dir.blk_ptr THEN BEGIN (*different dir block*)
      put_dir_blk; lib.dir.nxt := fil.dir.blk_ptr;
      IF NOT get_nxt_dir_blk THEN sgnl_fatl_err(bad_dir)
      END;
    WITH lib.dir.blk.fils[fil.dir.fil_ptr] DO BEGIN (*store file record*)
      nam := fil.nam; len := fil.len; dat_blk_ptr := fil.dat.blk_ptr
      (*would transferr file status information here*)
      END
    END;

$PAGE directory lookup: equal name, find next file, find file foobar
FUNCTION eq_nam(nam1, nam2 : fil_nam) : yes_or_no;
  (*see if two possibly ambiguous names are equal*)
  VAR idx : pos_int;
  BEGIN (*question marks match anything*)
    ASSERT(LENGTH(nam1) = LENGTH(nam2)); eq_nam := yes;
    FOR idx := one TO LENGTH(nam1) DO
      EXIT IF (nam1[idx] <> '?') ANDIF (nam2[idx] <> '?')
        ANDIF (nam1[idx] <> nam2[idx]) DO eq_nam := no
    END;

FUNCTION fnd_nxt_fil(nam : fil_nam; VAR fil : fil_typ) : succeed_or_fail;
  (*try to find next occurrance of specified file*)
  BEGIN (*walk dir list, lib.dir left at last block in list if failure*)
    LOOP
      fnd_nxt_fil := get_nxt_dir_fil(fil);
      EXIT IF NOT fnd_nxt_fil ORIF eq_nam(nam, fil.nam)
      END
    END;

FUNCTION fnd_fil(nam : fil_nam; VAR fil : fil_typ) : succeed_or_fail;
  (*try to find first occurrance of specified file*)
  BEGIN (*walk dir list, lib.dir left at last block in list if failure*)
    fil.dir.blk_ptr := one; fil.dir.fil_ptr := one; get_dir_fil(fil);
    fnd_fil := eq_nam(nam, fil.nam) ORIF fnd_nxt_fil(nam, fil);
    END;

$PAGE produce new blocks, space file check
PROCEDURE new_blk(VAR blk_ptr : blk_ptr_typ);
  (*produce a new block from library*)
  VAR fil : fil_typ; dat : dat_typ;
  FUNCTION use_old_spc : succeed_or_fail;
    BEGIN (*re-use garbage space*)
      use_old_spc := fnd_fil(old_spc, fil) ANDIF (fil.len > zero);
      IF use_old_spc THEN BEGIN (*found some garbage*)
        dat.nxt := fil.dat.blk_ptr;
        IF NOT get_nxt_dat_blk(dat) THEN sgnl_fatl_err(bad_int_fil);
        fil.dat.blk_ptr := dat.nxt; fil.len := fil.len - num_dat_wrds;
        put_dir_fil(fil)
        END
      END;
  FUNCTION use_new_spc : succeed_or_fail;
    BEGIN (*create new space at end of lib.fil*)
      use_new_spc := fnd_fil(new_spc, fil) ANDIF (fil.dat.blk_ptr > zero);
      IF use_new_spc THEN BEGIN (*new space block pointer found*)
        dat.blk_ptr := fil.dat.blk_ptr;
        IF fil.dat.blk_ptr < num_blks THEN inc(fil.dat.blk_ptr)
        ELSE fil.dat.blk_ptr := zero;
        put_dir_fil(fil)
        END
      END;
  BEGIN (*try to recycle garbage first before expanding lib.fil*)
    IF (NOT use_old_spc) ANDIF (NOT use_new_spc) THEN sgnl_err(no_spc);
    blk_ptr := dat.blk_ptr
    END;

FUNCTION chk_spc_fil(nam : fil_nam) : yes_or_no;
  (*check if file is internal space file*)
  BEGIN (*don't want external access to these magic files*)
    chk_spc_fil :=
      (nam = new_spc) OR (nam = old_spc) OR (nam = use_spc) OR (nam = fre_dir)
    END;

$PAGE library file create and delete, make used space file
PROCEDURE cre_fil(nam : fil_nam; VAR fil : fil_typ);
  (*create a directory entry for specified file*)
  VAR blk_ptr : blk_ptr_typ;
  BEGIN (*use a fre_dir record*)
    IF NOT fnd_fil(fre_dir, fil) THEN BEGIN (*expand dir list*)
      fil.dir.fil_ptr := one; fil.dir.blk_ptr := lib.dir.blk_ptr;
      new_blk(blk_ptr); get_dir_fil(fil); lib.dir.nxt := blk_ptr; put_dir_blk;
      lib.dir.blk_ptr := lib.dir.nxt; dir_set_up;
      fil.dir.blk_ptr := lib.dir.blk_ptr; fil.dir.fil_ptr := one;
      get_dir_fil(fil)
      END;
    fil.nam := nam; put_dir_fil(fil)
    END;

PROCEDURE del_fil(VAR fil : fil_typ);
  BEGIN (*mark as deleted, eventually want an un-delete*)
    fil.nam := use_spc; put_dir_fil(fil)
    END;

PROCEDURE mak_use_spc(blk_ptr : blk_ptr_typ; len : pos_int);
  (*place list of blocks into a used space file*)
  VAR fil : fil_typ;
  BEGIN (*mark as garbage*)
    IF len < one THEN sgnl_fatl_err(bad_int_fil);
    cre_fil(use_spc, fil);
    fil.dat.blk_ptr := blk_ptr; fil.len := len;
    put_dir_fil(fil)
    END;

$PAGE get file from library, transferr to external data file
PROCEDURE get_fil(fil : fil_typ);
  (*get current file from library*)
  VAR was_bsy : yes_or_no;
    dat : dat_typ; dat_cnt : pos_int; dat_fil : dat_fil_typ;
  PROCEDURE get_cln_up;
    BEGIN (*simply done with data file*)
      IF was_bsy THEN CLOSE(dat_fil)
      END;
  BEGIN (*will walk data list*)
    was_bsy := no; REWRITE(dat_fil, fil.nam, [RETRY]);
    IF IOSTATUS(dat_fil) <> IO_OK THEN sgnl_err(bad_ext_fil);
    was_bsy := yes; put_fil_stat(dat_fil, fil); 
    dat.nxt := fil.dat.blk_ptr; dat_cnt := fil.len;
    LOOP
      EXIT IF NOT get_nxt_dat_blk(dat);
      IF dat_cnt < one THEN sgnl_fatl_err(bad_int_fil);
      put_dat(dat_fil, dat, dat_cnt)
      END;
    IF dat_cnt > zero THEN sgnl_fatl_err(bad_int_fil);
    get_cln_up
    EXCEPTION
      ALLCONDITIONS : BEGIN get_cln_up; SIGNAL() END
    END;

$PAGE put file into library, transferr from external data file
PROCEDURE put_fil(VAR fil : fil_typ);
  (*put current file into library*)
  VAR was_bsy, fix_dir : yes_or_no; buf, old_buf : dat_typ;
    dat : dat_typ; dat_cnt : pos_int; dat_fil : dat_fil_typ;
  PROCEDURE put_buf;
    BEGIN (*transferr buffer, try to recycle lib.fil data blocks*)
      IF NOT get_nxt_dat_blk(buf) THEN BEGIN (*must expand data list*)
        new_blk(buf.blk_ptr); buf.nxt := zero
        END;
      dat.nxt := buf.nxt; buf := dat;
      IF fix_dir THEN BEGIN (*first time through only*)
        fix_dir := no;
        IF fil.dat.blk_ptr < one THEN BEGIN (*file was empty before*)
          fil.dat.blk_ptr := buf.blk_ptr; put_dir_fil(fil)
          END
        END
      ELSE put_dat_blk(old_buf);
      old_buf := buf
      END;
  PROCEDURE put_cln_up;
    VAR use_blk_ptr : nxt_ptr_typ; use_len : pos_int;
    BEGIN (*flush buffer and close file*)
      IF was_bsy THEN BEGIN (*data file was open*)
        IF fix_dir THEN BEGIN (*first time through -- null data file*)
          use_blk_ptr := fil.dat.blk_ptr; use_len := fil.len;
          fil.dat.blk_ptr := zero
          END
        ELSE BEGIN (*flush buffer -- not null data file*)
          use_blk_ptr := old_buf.nxt; old_buf.nxt := zero;
          IF use_blk_ptr > zero THEN use_len := fil.len - dat_cnt;
          put_dat_blk(old_buf)
          END;
        CLOSE(dat_fil); fil.len := dat_cnt; put_dir_fil(fil);
        IF use_blk_ptr > zero THEN mak_use_spc(use_blk_ptr, use_len)
        END
      END;
  BEGIN (*open data file, buffered transferr of blocks into lib.fil*)
    was_bsy := off; RESET(dat_fil, fil.nam, [RETRY]);
    IF IOSTATUS(dat_fil) <> IO_OK THEN sgnl_err(bad_ext_fil);
    was_bsy := on; get_fil_stat(dat_fil, fil); fix_dir := yes;
    buf.blk.nxt := fil.dat.blk_ptr; dat_cnt := zero;
    LOOP
      EXIT IF EOF(dat_fil);
      get_dat(dat_fil, dat, dat_cnt); put_buf
      END;
    put_cln_up;
    EXCEPTION
      ALLCONDITIONS : BEGIN put_cln_up; SIGNAL() END
    END;

$PAGE library open and close
PROCEDURE lib_cls;
  (*close current library*)
  BEGIN (*puts lib.dir block*)
    IF lib.fil <> NILF THEN BEGIN put_dir_blk; CLOSE(lib.fil) END
    END;

PUBLIC PROCEDURE lib_opn(nam : fil_nam);
  (*open file to be current library*)
  PROCEDURE new_lib;
    VAR fil : fil_typ;
    BEGIN (*format new library file*)
      put_lin('New library!');
      lib.dir.blk_ptr := one; dir_set_up;
      WITH fil DO BEGIN (*set up garbage list -- null*)
        nam := old_spc; dat.blk_ptr := zero; len := zero;
        (*should set up null file status*)
        dir.blk_ptr := one; dir.fil_ptr := one; put_dir_fil(fil)
        END;
      WITH fil DO BEGIN (*set up expansion pointer -- block #two*)
        nam := new_spc; dat.blk_ptr := two; len := zero;
        (*should set up null file status*)
        dir.blk_ptr := one; dir.fil_ptr := two; put_dir_fil(fil)
        END;
      put_dir_blk;
      END;
  BEGIN (*if error, no lib.fil left open*)
    lib_cls; lib.nam := nam; UPDATE(lib.fil, lib.nam, [RETRY]);
    IF IOSTATUS(lib.fil) <> IO_OK THEN sgnl_err(opn_lib)
    ELSE BEGIN (*lib.fil is open*)
      IF EOF(lib.fil) THEN new_lib;
      lib.dir.nxt := one; IF NOT get_nxt_dir_blk THEN sgnl_fatl_err(bad_dir)
      END
    EXCEPTION
      ALLCONDITIONS : BEGIN lib_cls; SIGNAL() END
    END;

$PAGE library delete, directory
PUBLIC PROCEDURE lib_del(nam : fil_nam);
  (*delete file from current library*)
  VAR fil : fil_typ;
  BEGIN (*mark files as deleted*)
    IF lib.fil = NILF THEN sgnl_err(no_lib);
    IF NOT fnd_fil(nam, fil) THEN sgnl_err(no_fil);
    REPEAT
      IF NOT chk_spc_fil(nam) THEN BEGIN
        put_lin('Delete: ' || fil.nam); del_fil(fil)
        END
      UNTIL NOT fnd_nxt_fil(nam, fil)
    END;

PUBLIC PROCEDURE lib_dir(nam : fil_nam);
  (*display directory information about specified file*)
  VAR fil : fil_typ;
  BEGIN
    IF lib.fil = NILF THEN sgnl_err(no_lib);
    IF NOT fnd_fil(nam, fil) THEN put_lin('File does not exist: ' || nam)
    ELSE REPEAT 
      IF NOT chk_spc_fil(fil.nam) THEN
        put_lin(fil.nam || spc || int_str(fil.len));
      UNTIL NOT fnd_nxt_fil(nam, fil)
    END;

$PAGE library pack (remove unused space from file, optimize structure*)
PUBLIC PROCEDURE lib_pak;
  (*compact current library*)
  VAR blk_cnt : zero .. num_blks;
  BEGIN
    IF lib.fil = NILF THEN sgnl_err(no_lib);
    blk_cnt := zero;
    put_lin('Data space recovered: ' || int_str(blk_cnt * num_dat_wrds))
    END;

$PAGE  library get and put file
PUBLIC PROCEDURE lib_get(nam : fil_nam);
  (*get file from current library*)
  VAR fil : fil_typ;
  BEGIN
    IF lib.fil = NILF THEN sgnl_err(no_lib);
    IF NOT fnd_fil(nam, fil) THEN sgnl_err(no_fil);
    REPEAT
      IF NOT chk_spc_fil(fil.nam) THEN BEGIN
        put_lin('Get: ' || fil.nam); get_fil(fil)
        END
      UNTIL NOT fnd_nxt_fil(nam, fil)
    END;

PUBLIC PROCEDURE lib_put(nam : fil_nam);
  (*put file into current library*)
  VAR fil : fil_typ;
  BEGIN
    IF lib.fil = NILF THEN sgnl_err(no_lib);
    IF NOT fnd_fil(nam, fil) THEN BEGIN
      IF SEARCH(nam, ['?']) > zero THEN sgnl_err(no_fil);
      put_lin('Create: ' || nam); cre_fil(nam, fil)
      END;
    REPEAT
      IF NOT chk_spc_fil(fil.nam) THEN BEGIN
        put_lin('Put: ' || fil.nam); put_fil(fil)
        END
      UNTIL NOT fnd_nxt_fil(nam, fil)
    END;

$PAGE set up and clean up
PUBLIC PROCEDURE lib_cln_up;
  BEGIN
    IF cnt_cln(cnt_set_cln) THEN RETURN;
    lib_cls;
    err_cln_up;
    pio_cln_up;
    wrt_cln_up
    END;

PUBLIC PROCEDURE lib_set_up;
  BEGIN
    IF cnt_set(cnt_set_cln) THEN RETURN;
    wrt_set_up;
    pio_set_up;
    err_set_up;
    set_err_msg(opn_lib, 'Can not open library file.');
    set_err_msg(no_lib, 'There is no current library.');
    set_err_msg(no_spc, 'There is no more space left in the library.');
    set_err_msg(no_fil, 'There is no such file.');
    set_err_msg(bad_ext_fil, 'External file is bad.');
    set_err_msg(bad_int_fil, 'Internal file is bad.');
    set_err_msg(bad_dir, 'Bad directory structure.');
    lib.fil := NILF
    END.
 