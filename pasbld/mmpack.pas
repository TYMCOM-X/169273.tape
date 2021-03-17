module mmpack;

(* Routine to perform PACK function for ODMS, using MMDBPR primitives. *)

$SYSTEM MMSSYM.TYP
$SYSTEM MMDBPR.TYP
$SYSTEM TIMUTL.INC
$SYSTEM JOBNUM.INC
$PAGE external declarations for this package
external procedure rename (file_name; file_name);
external function findversion (var dtime_int): boolean;
external procedure chainonend (var file of * );
external procedure delver;
external procedure dbinit (file_name);
external procedure dbclose;
external procedure io_error (boolean; string[60]);

external var
  dbvers: integer;
  curmod: modptr;
$PAGE nsovfy declarations and initializations
public procedure nsovfy (		(* overlay verify *)
  var f: file of *;			(* the file, already positioned *)
  id: string[60] );			(* what to print if it fails *)

(* NSOVFY is to verify overlays.  By looking at the .EXE
   directory, we can see if there are any low segment pages, as well
   as finding the high segment, and its size and relocation.  Assuming
   that all checks, we read in the high segment to check the module
   index.  If there are any low segment pages, then we check it against
   the declared static limits. *)

var
  diridx, dirsize, storg, stend, act_stat_strg : integer;
  coerce: coerce_rec;
  hscount, hsaddr, hspage: integer;
  page: array [0..511] of integer;
  error, seen_static: boolean;

  procedure vfyerr (str: string[60]);

  (* VFYERR prints out an error message for failed verification.  For the
     first failure in a given call, it prints out the ID message. *)

  begin
    if not error then begin
      writeln (tty);
      writeln (tty, '?ODMVFY -- failure in ', id);
      end;
    error := true;
    writeln (tty, '?ODMVFY -- ', str)
  end (* procedure vfyerr *);
$PAGE nsovfy the code
begin					(* procedure nsovfy *)
  error := false;
  read (f, page);			(* get the .EXE directory *)
  if iostatus <> io_ok then
    io_error (false, 'verify reading EXEdir');
  coerce.int := page[0];		(* EXE director header *)
  if coerce.lh <> 1776b then io_error (false, 'file is not in .EXE format');
  dirsize := coerce.rh - 1;		(* size less header word *)
  diridx := 1;				(* index into page array *)
  hspage := 0; hscount := 0; hsaddr := 0;
  seen_static := false;

  while dirsize > 0 do begin
    coerce.int := page [diridx];	(* lh with type bits, rh filepage *)
    if coerce.rh <> 0 then		(* a non-blank file page *)
      if coerce.lh < 0 then begin	(* a high segment page *)
	if hspage = 0 then begin	(* the first one seen *)
	  hspage := coerce.rh;		(* remember it *)
	  coerce.int := page [diridx + 1];
	  hsaddr := coerce.rh * 1000b	(* process addr *)
	  end
	else coerce.int := page [diridx + 1];	(* still gotta get next *)
	hscount := hscount + (coerce.lh div 1000b) + 1
	end
      else seen_static := true;		(* that's all we care *)
    diridx := diridx + 2;		(* on to the next exe entry *)
    dirsize := dirsize - 2
    end;

  writeln ( tty );		(* For appearance *)

  (* If sharable overlays print out an indication of that. If not sharable,
     then print out the code sizes.    *)

  if curmod^.marea <> Nil
    then begin
      (* Print out the actual code sizes and base addresses vs. the MDL
	 available sizes. *)

      writeln ( tty , ' Actual code at         ' , hsaddr:6:O, ', ' , 
		    (hscount * 1000b):6:O, '(' , (hscount * 1000b):0 ,
		    ' decimal) words' );
      writeln ( tty , ' MDL declared code at   ' , curmod^.marea^.aorig:6:O ,
		    ', ' , curmod^.marea^.asize:6:O , '(' ,
		    curmod^.marea^.asize:0 , ' decimal) words' );

      if hsaddr <> curmod^.marea^.aorig then
	vfyerr ('overlay area origin mismatch');
      if (hscount * 1000b) > curmod^.marea^.asize then
	vfyerr ('overlay area overflow');
    end
  else begin
    writeln ( tty , ' ', curmod^.name^.text , ' is a sharable overlay' )
  end;

  repeat				(* advance to first high seg page *)
    read (f, page);
    hspage := hspage - 1		(* keep count! *)
  until hspage <= 0;

  writeln ( tty );
  writeln ( tty , ' Actual module index is ' , page [ 10b ]:0 );
  if curmod^.modid <> page [10b]
    then begin
      vfyerr ('incorrect module index' );
      writeln ( tty , ' MDL declared module index is ' , curmod^.modid:0 )
    end;
  writeln ( tty );

  coerce.int := page [11b];		(* static verification word *)
  storg := coerce.lh; stend := coerce.rh;
  if seen_static then begin		(* only if overlay has any *)
    coerce.int := page [2];		(* loseg break from VJDA *)

    act_stat_strg:=coerce.lh-storg;
    if act_stat_strg<0 then act_stat_strg:=0;

    (* Print out the actual, and MDL declared, static sizes. *)

    writeln ( tty , ' Actual static at       ' , storg:6:O, ', ' ,
		act_stat_strg:6:O, '(' , act_stat_strg:0 , ' decimal) words');
    writeln ( tty , ' MDL declared static at ' , curmod^.storig:6:O,
		', ', curmod^.stsize:6:O, '(' , curmod^.stsize:0 ,
		' decimal) words' );

    if curmod^.stsize <= 0 then
      vfyerr ('overlay static area overflow')
    else if storg <> curmod^.storig then
      vfyerr ('overlay static area origin conflict')
    else if stend > (curmod^.storig + curmod^.stsize - 1) then
      vfyerr ('overlay static limit exceeds MDL limit')
    else if coerce.lh > stend then
      vfyerr ('overlay static area overflow')
  end
  else begin
    writeln ( tty , 'No static is being used.' );
    writeln ( tty , ' Static size available: ', curmod^.stsize:6:O, '(' ,
	curmod^.stsize:0 , ' decimal) words' )
  end;

  writeln ( tty )		(* For looks only *)
end (* procedure nsovfy *);
$PAGE vfyext called from main to verify external file
public procedure vfyext (ov: file_name);

(* VFYEXT is a wrapper for NSOVFY that opens the file. *)

var f: file of *;

begin
  reset (f, '.EXE ' || ov);
  if iostatus (f) <> io_ok then
    writeln (tty, '?ODMVFE -- Can''t find external file')
  else begin
    nsovfy (f, 'external file ' || filename (f) );
    close (f)
    end
end (* procedure vfyext *);
$PAGE packdb declarations and initializations
public procedure packdb (db: file_name; mdl: ^pnode);

(* PACKDB explicitly stomps through the old database, using the primitives
   to help it create a new database without holes and duplicates. *)

var
  olddb: file of *;			(* the one we muck with *)
  dt: dtime_int;			(* coming back from findversion *)
  coerce : coerce_rec;
  vptr, newvptr: integer;		(* points to current version *)
  oldname: file_name;			(* to remember old db's name *)
  page: array [0..511] of integer;	(* for mucking *)
  page_size : integer;

begin
  reset (olddb, '.ODB ' || db, [seekok]);	(* try to find database *)
  if eof (olddb) then io_error (false, 'Can''t find old database');

  dbinit (jobnum || 'ODB.TMP');		(* create a new database *)
  oldname := filename (olddb);		(* get name of old one for later *)
$PAGE packdb the rest of it
  curmod := mdl^.mlist;			(* walk the chain *)
  while curmod <> nil do begin
    if curmod^.modid > 0 then begin	(* nonsharable, potential entries *)
      vptr := curmod^.modid;		(* gotta find first version *)
      readrn (olddb, 201b, page);	(* whack in the directory block *)

      if vptr >= 128 then repeat	(* gotta get right direct entry *)
      exit if page [0] = 0 do vptr := 0;  (* none more, none to copy *)
	readrn (olddb, page[0] + 1, page);  (* next block *)
	vptr := vptr - 127
	until vptr < 128;

      vptr := page [vptr];		(* the pointer to first version *)

      while vptr <> 0 do begin		(* down the chain *)
	readrn ( olddb, vptr + 1 , page:max_page_size );	(* read what we need *)
	coerce.int := page [ 0 ];
	page_size := coerce.rh;
	newvptr := page [ page_size + nextptr ];
	dbvers := page [ page_size + versionid ];
	if findversion (dt) then begin	(* ecch, it's already there *)
	  writeln (tty, '%ODMPAC -- Deleting duplicate version ',
	    dbvers:0, ' module ', curmod^.name^.text, ' ',
	    dc_ext (dt) );
	  delver			(* blast it *)
	  end;
	seek (olddb, vptr + 1);		(* position olddb to the version *)
	chainonend (olddb);		(* and pass it down *)
	vptr := newvptr
	end
      end;
    curmod := curmod^.next
    end;

  dbclose;				(* finish everything up *)
  close (olddb);
  rewrite (olddb, oldname);		(* delete old version *)
  if iostatus <> io_ok then io_error (false, 'Can''t write access old copy');
  scratch (olddb);
  if iostatus <> io_ok then io_error (false, 'Can''t scratch old copy');
  rename (jobnum || 'ODB.TMP', oldname)
end (* procedure packdb *).
 