module mmdbpr;

(* Database Primitive routines for ODMS. *)

$INCLUDE MMSSYM.TYP
$INCLUDE TIMUTL.INC
$INCLUDE MMDBPR.TYP
$PAGE vars and things for this module, externals too
external var
  curmod: modptr;			(* module from MAIN *)
  dbvers, dbnvers: integer;		(* version numbers from command *)
  dbfile: file of *;			(* global file variable *)
  curfptr, prevfptr: integer;		(* database file pointers *)

external procedure io_error (
  boolean;				(* true if unexpected *)
  string [80] );			(* the message *)

var
  bitmap: packed array [0..4607] of boolean;
  dir: directoryblock;			(* both maintained in core *)
  i: integer;				(* useful integer index *)
$PAGE dbopen gets control information from an old database

(* DBOPEN  does what DBINIT does, but uses a RESET rather than an UPDATE.
   if the file does not open, then nothing is opened. *)

public procedure dbopen (fn: file_name );

begin

  reset ( dbfile, fn, [seekok,retry] );
  if iostatus <> io_ok then io_error (false, 'open failure');

  read ( dbfile, bitmap);
  if iostatus <> io_ok then io_error ( true, 'while reading bitmap');

  read ( dbfile , dir );
  if iostatus <> io_ok then io_error ( true, 'while reading module directory');

end (* procedure dbopen *);
$PAGE dbinit initialize a database
public procedure dbinit (fn: file_name);

(* DBINIT either initializes a new database or gets control information
   from an old database. *)

begin
  update (dbfile, fn, [seekok,retry]);
  if iostatus <> io_ok then io_error (false, 'initialization failure');

  if extent (dbfile) = 0 then begin	(* a new file *)
    for i := 1 to 4607 do bitmap[i] := false;	(* ecch! *)
    write (dbfile, bitmap);
    if iostatus <> io_ok then io_error (true, 'New file bitmap');
    dir.nextdirect := 0;
    for i := 1 to 127 do dir.modptrs [i] := 0;
    write (dbfile, dir);
    if iostatus <> io_ok then io_error (true, 'New file directory block')
    end
  else begin
    read (dbfile, bitmap);
    if iostatus <> io_ok then io_error (true, 'while reading bitmap');
    read (dbfile, dir);
    if iostatus <> io_ok then io_error (true, 'while reading module directory')
    end
end (* procedure dbinit *);
$PAGE getspace block allocator
public function getspace (nbl: integer): integer;

(* GETSPACE returns a pointer to a block of NBL contiguous blocks. *)

var
  candidate, runningsize: integer;

begin
  candidate := 2; runningsize := 0;

  loop
    while not bitmap [candidate] do begin
      if candidate >= 4607 then io_error (false, 'Database too full');
      candidate := candidate + 1;
      runningsize := runningsize + 1;
    exit (* while loop *) if runningsize = nbl do getspace := candidate - nbl;
      end (* while *);

  exit (* loop *) if getspace <> 0;
    runningsize := 0;
    candidate := candidate + 1
  end (* loop *);

  for candidate := getspace to getspace + nbl - 1 do
    bitmap [candidate] := true;
  writern (dbfile, 1, bitmap);		(* update disk bitmap *)
  if iostatus <> io_ok then io_error (true, 'getspace bitmap update');
  getspace := getspace * 128
end (* function getspace *);
$PAGE freespace block recycling routine
public procedure freespace (nbl: integer);

(* FREESPACE returns a block pointed to by CURFPTR, length of NBL. *)

var
  localindex: integer;
  warn: boolean;

begin
  warn := false;

  for localindex := (curfptr div 128) to
    (curfptr div 128) + nbl - 1 do begin
    if not bitmap [localindex] then warn := true;
    bitmap [localindex] := false
    end;

  if warn then writeln (tty, '%ODMFRE -- Multiply freed space ', curfptr:6:o,
    ' for ', nbl, ' blocks.');
  writern (dbfile, 1, bitmap);
  if iostatus <> io_ok then io_error (true, 'freespace bitmap update')
end (* procedure freespace *);
$PAGE findversion tries to locate version and its forepointer
public function findversion (var dt: dtime_int): boolean;

(* FINDVERSION tries to locate version DBVERS, using pointers CURFPTR
   and PREVFPTR to romp down the version chains.  If FINDVERSION returns
   true, CURFPTR points to the block containing the .EXE directory of
   the correct version, and PREVFPTR points to the exact pointer pointing
   to the located version (for inserting and deleting). *)

var
  littleblock: array [0 .. max_page_size] of integer;
  page_size : integer;
  coerce: coerce_rec;

begin
  prevfptr := 200b + curmod^.modid;	(* points into directory block *)
  curfptr := dir.modptrs [curmod^.modid];
  findversion := false;
  while curfptr <> 0 do begin
    readrn (dbfile, curfptr + 1, littleblock);	(* get exe header *)
    if iostatus <> io_ok then io_error (true, 'findversion reading EXEdir');
    coerce.int := littleblock [ 0 ];
    page_size := coerce.rh;
  exit if dbvers = littleblock [ page_size + versionid ]
    do findversion := true;
    prevfptr := curfptr + nextptr + page_size;
    curfptr := littleblock [ page_size + nextptr ]
    end;

  if findversion then begin		(* coerce for parameter *)
    coerce.int := littleblock [ page_size + timestamp ];
    dt := coerce.dt
    end
end (* function findversion *);
$PAGE chainonend to install a new version
public procedure chainonend (var exefile: file of * );

(* CHAINONEND installs a new version by hunting for a nonexistent one.
   By doing so, the global PREVFPTR will point to the place from
   which a pointer will point to a new version of that module.  Then,
   save the value and GETSPACE for the new .EXE.  We must read the
   .EXE directory first, to get the total size of the new overlay. *)

var
  page: array [0..511] of integer;
  page_size : integer;
  hook: integer;
  exesize: integer;
  coerce: coerce_rec;
  curdir, dirsiz: integer;

begin
  read (exefile, page);			(* page one with the pointers *)
  if iostatus <> io_ok then io_error (true, 'chainonend reading EXEdir');
  coerce.int := page[0];		(* make sure it's an exe file *)
  if coerce.lh <> 1776b then io_error (false, 'File is not in .EXE format');
  if coerce.rh > max_page_size then io_error (false, 'internal ODMS error, chainonend in mmdbpr' );
  page_size := coerce.rh;
  dirsiz := coerce.rh - 1;		(* don't count header word *)
  curdir := 1;
  exesize := 1;				(* count directory page too *)
  while dirsiz > 0 do begin		(* count up number of pages *)
    coerce.int := page [curdir];
    if coerce.rh <> 0 then begin	(* a real file page, non-zero *)
      coerce.int := page [curdir + 1];	(* next one has number of pages *)
      exesize := exesize + (coerce.lh div 1000b) + 1	(* in high 9 bits *)
      end;
    curdir := curdir + 2;
    dirsiz := dirsiz - 2
    end;

  curdir := dbvers;			(* save version id to install *)
  dbvers := maximum (integer);		(* trash for the moment *)
  if findversion (coerce.dt) then
    io_error (true, 'Illegal version number exists in database');
  dirsiz := getspace (exesize * 4);	(* allocate new block and save *)
  hook := prevfptr + 1;			(* save dirsiz and exesize for chain *)

  coerce.dt := daytime;
  if page [ page_size + timestamp ] = 0 then	(* must be .EXE *)
    page [ page_size + timestamp ] := coerce.int;
  page [ page_size + nextptr ] := 0;
  page [ page_size + versionid ] := curdir;
  writern (dbfile, dirsiz + 1, page);
  if iostatus <> io_ok then io_error (true,'chainonend installing new EXEdir');
  while exesize > 1 do begin		(* the whole shot *)
    exesize := exesize - 1;
    read (exefile, page);
    if iostatus <> io_ok then io_error (true,'chainonend reading EXE page');
    write (dbfile, page);
    if iostatus <> io_ok then io_error (true,'chainonend writing EXE page')
    end;

  writern (dbfile, hook, dirsiz);	(* chain it in *)
  if iostatus <> io_ok then io_error (false, 'chainonend writing new pointer');
  readrn (dbfile, 201b, dir);		(* update core directory block *)
  if iostatus <> io_ok then io_error (false, 'chainonend reading dirblk')
end (* procedure chainonend *);
$PAGE delver delete a version
public procedure delver;

(* DELVER removes a version.  It is assumed that CURFPTR and PREVFPTR
   have been initialized by a FINDVERSION call which returned true.  We
   have to walk the .EXE file to determine the size of the creature, then
   unchain it, then deallocate the space. *)

var
  page: array [0..max_page_size] of integer;	(* for the exe directory *)
  exesize, curdir, dirsize, newnext: integer;
  coerce: coerce_rec;
  page_size : integer;

begin
  readrn (dbfile, curfptr + 1, page);	(* whole first page *)
  if iostatus <> io_ok then io_error (true, 'delver reading target EXEdir');
  exesize := 1; curdir := 1;
  coerce.int := page[0];
  if coerce.lh <> 1776b then io_error (false, 'File not in DB format');
  page_size := coerce.rh;
  newnext := page [ page_size + nextptr ];
  dirsize := coerce.rh - 1;
  while dirsize > 0 do begin
    coerce.int := page[curdir];
    if coerce.rh <> 0 then begin	(* corresponding to file page *)
      coerce.int := page [curdir + 1];	(* entry with repeat count *)
      exesize := exesize + (coerce.lh div 1000b) + 1
      end;
    dirsize := dirsize - 2;
    curdir := curdir + 2
    end;

  writern (dbfile, prevfptr + 1, newnext);
  if iostatus <> io_ok then io_error (false, 'delver overwriting pointer');
  readrn (dbfile, 201b, dir);		(* keep core copy correct *)
  if iostatus <> io_ok then io_error (false, 'delver reading dirblk');

  freespace (exesize * 4)
end (* procedure delver *);
$PAGE dbclose done with database operations
public procedure dbclose;

(* DBCLOSE just closes the file.  If we maintained the only correct copy
   of the bitmap in core, we could write it here. *)

begin
  if dbfile <> nilf
    then begin
      close ( dbfile );
      dbfile := nilf
    end
end (* procedure dbclose *);
$PAGE deleteall remove all versions of a module
public procedure deleteall;

(* DELETEALL removes all versions of a module from the database, printing
   a line for each one.  First, the version is unchained using delver.
   Then we do the next one.  Easy. *)

var
  littleblock: array [0..max_page_size] of integer;
  page_size : integer;
  coerce: coerce_rec;

begin
  curfptr := dir.modptrs [curmod^.modid];

  if curfptr = 0 then writeln (tty, '%ODMDEL -- No versions exist.')
  else repeat
    prevfptr := 200b + curmod^.modid;
    readrn (dbfile, curfptr + 1, littleblock);
    if iostatus <> io_ok then io_error (true, 'delete_all getting next');
    coerce.int := littleblock [ 0 ];
    page_size := coerce.rh;
    delver;				(* do it *)
    coerce.int := littleblock [ page_size + timestamp ];
    writeln (tty, '%ODMDEL -- Deleting version ', littleblock[versionid]:0,
      ' of ', dc_ext (coerce.dt));
    curfptr := littleblock [ page_size + nextptr ]
    until curfptr = 0
end (* procedure deleteall *);
$PAGE prdbvers print database versions
public procedure  prdbvers (db: file_name);

(* PRDBVERS prints out the versions of CURMOD. *)

var
  p: integer;				(* for walking chain *)
  coerce: coerce_rec;
  littleblock: array [0..max_page_size] of integer;
  page_size : integer;

begin
  dbopen ( '.ODB ' || db);
  p := dir.modptrs [curmod^.modid];
  while p <> 0 do begin
    readrn (dbfile, p + 1, littleblock);
    if iostatus <> io_ok then io_error (true, 'print finding exedir');
    coerce.int := littleblock [ 0 ];
    page_size := coerce.rh;
    coerce.int := littleblock [ page_size + timestamp ];
    writeln ( tty , ' Version ' , littleblock [ page_size + versionid ]:0,
      ' of ', dc_ext (coerce.dt));
    p := littleblock [ page_size + nextptr ]
  end;
  dbclose
end (* procedure prdbvers *).
   