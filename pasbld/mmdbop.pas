module mmdbop;

(* Database operations for ODMS *)

$INCLUDE MMSSYM.TYP
$INCLUDE TIMUTL.INC
$INCLUDE MMDBPR.TYP
$PAGE external declarations for this module
external function getspace (integer): integer;
external procedure freespace (integer);
external function findversion (var dtime_int): boolean;
external procedure chainonend (var file of * );
external procedure dbinit (file_name);
external procedure dbopen (file_name);
external procedure delver;
external procedure deleteall;
external procedure io_error (boolean; string[80]);
external procedure dbclose;
external procedure nsovfy (var file of *; string[60] );

external var
  curmod: modptr;
  dbvers, dbnvers: integer;
  dbfile: file of *;
  curfptr, prevfptr: integer;
$PAGE vfyvers verify explicit database version
public procedure vfyvers (db: file_name);

(* VFYVERS verifies a specific version in a database.  Calls findversion
   to get a pointer to it, then winds back dbfile and calls nsovfy. *)

var
  dt: dtime_int;			(* for garbage *)

begin
  dbopen ('.ODB ' || db);
  if not findversion (dt) then
    writeln (tty, '?ODMVFV -- version not found.')
  else begin
    seek (dbfile, curfptr + 1);		(* wind file back to exedir *)
    nsovfy (dbfile, 'request')
    end;
  dbclose
end (* procedure vfyvers *);
$PAGE vfdbvers verify every version in a database
public procedure vfdbvers (db: file_name);

(* VFDBVERS walks through a database, calling nsovfy for every version
   found in the database. *)

var
  m: integer;
  block: array [0..max_page_size] of integer;
  pt: integer;
  st: string[60];
  coerce : coerce_rec;
  page_size : integer;

begin
  dbinit ('[,].ODB ' || db);
  readrn (dbfile, 201b, block);	(* get home block *)
  m := curmod^.modid;
  while m > 127 do begin
    if block [0] = 0 then io_error (false, 'Unknown module index');
    readrn (dbfile, block [0], block);
    m := m - 127
    end;
  curfptr := block [m];

  while curfptr <> 0 do begin
    readrn (dbfile, curfptr + 1, block);
    coerce.int := block [ 0 ];
    page_size := coerce.rh;
    pt := block [ page_size + nextptr ];	(* for next time *)
    seek (dbfile, curfptr + 1);		(* position it for verify routine *)
    putstring (st, 'database ', filename (dbfile), 
      ' version ', block [ page_size + versionid ]:0 );
    nsovfy (dbfile, st);
    curfptr := pt
    end;

  dbclose
end (* procedure vfdbvers *);
$PAGE del_ovl delete overlay from database
public procedure del_ovl (db: file_name);

(* DEL_OVL supervises the database primitives findversion, delver,
   and deleteall.  Just check before you destroy. *)

var dt: dtime_int;

begin
  dbinit ('[,].ODB ' || db);
  if dbvers = -1 then			(* ALL specified on command *)
    deleteall				(* just pawn it off *)
  else begin
    if not findversion (dt) then	(* can't delete what's not there *)
      writeln (tty, '?ODMDEL -- Version ', dbvers:0, ' not found.')
    else begin
      writeln (tty, '%ODMDEL -- Deleting version of ', dc_ext (dt), '.');
      delver				(* just like that *)
      end
    end;
  dbclose				(* finish up *)
end (* procedure del_ovl *);
$PAGE ren_ovl rename an overlay from database
public procedure ren_ovl (db: file_name);

(* REN_OVL only has to change one word in the file -- the version number
   to modify.  Before doing so, check  to make sure that the old version
   exists, and that the new version number doesn't already exist. *)

var
  dt: dtime_int;
  save_old: integer;			(* we can use findversion *)
  coerce: coerce_rec;

begin
  dbinit ('[,].ODB ' || db);
  save_old := dbvers;
  dbvers := dbnvers;			(* implicit parameter to findversion *)
  if findversion (dt) then
    writeln (tty, '?ODMREN -- Version ', dbvers:0, ' already exists.')
  else begin
    dbvers := save_old;			(* fix up again *)
    if not findversion (dt) then	(* gotta have one to modify! *)
      writeln (tty, '?ODMREN -- Version ', dbvers:0, ' not found.')
    else begin
      writeln (tty, '%ODMREN -- renaming version of ', dc_ext (dt), '.');
      readrn (dbfile, curfptr + 1, coerce );
      if iostatus <> io_ok then
	io_error(true, 'in REN_OVL reading EXE directory' );
      writern (dbfile, curfptr + coerce.rh +versionid + 1, dbnvers);
      if iostatus <> io_ok then io_error (true, 'renaming version')
      end
    end;
  dbclose
end (* procedure ren_ovl *);
$PAGE upd_ovl update overlay in database
public procedure upd_ovl (db: file_name; ov: file_name);

(* UPD_OVL updates the named database from the named executable. *)

var
  dt: dtime_int;
  save_prev, save_cur: integer;
  exefile: file of *;

begin
  reset (exefile, '.EXE ' || ov);
  if eof (exefile) then io_error (false, 'Can''t open overlay file');
  dbinit ('[,].ODB ' || db);
  save_prev := 0;
  if findversion (dt) then begin
    writeln (tty, '%ODMUPD -- Superseding version of ',
      dc_ext (dt), '.');
    save_prev := prevfptr;
    save_cur := curfptr
    end;

  chainonend (exefile);
  if save_prev <> 0 then begin
    curfptr := save_cur;
    prevfptr := save_prev;
    delver
    end;
  close (exefile);
  dbclose
end (* procedure upd_ovl *).
