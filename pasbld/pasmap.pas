$TITLE pasmap - Line number / Code address cross reference map
$LENGTH 43

module pasmap;
$PAGE declarations

$SYSTEM pascal.inc
$SYSTEM ptmcon.inc
$SYSTEM pasfil.inc
$SYSTEM pasist.inc
$SYSTEM pascv.inc
$SYSTEM tmpnam.inc

type
    map_record = record
        srcid: source_id;
        address: 0 .. 777777b
    end;

var map_file: file of map_record;
    map_size: int_type;

type
    map_array = array [1..*] of map_record;

var map: ^ map_array;

var mod_ident: string;
$PAGE map_init

(*  MapInit simply opens the map file.  *)

public procedure map_init;
begin
  if list_file = '' then return;
  rewrite (map_file, tempname ('MAP'));
  map_size := 0;
end;
$PAGE map_write

(*  MapWrite adds a record to the map file.  *)

public procedure map_write ( srcid: source_id; addr: unit_range );

begin
  if list_file = '' then return;
  map_file^.srcid := srcid;
  map_file^.address := addr;
  put (map_file);
  map_size := map_size + 1;
end;
$PAGE map_gtr

(*  MapGtr returns true if one line number is greater than another.  *)

function map_gtr ( a, b: source_id ): boolean;

begin
  if a.file_no > b.file_no then
    map_gtr := true
  else if a.file_no < b.file_no then
    map_gtr := false
  else if a.page_no > b.page_no then
    map_gtr := true
  else if a.page_no < b.page_no then
    map_gtr := false
  else
    map_gtr := (a.line_no > b.line_no);
end (* map_gtr *);
$PAGE sort_map

(*  SortMap sorts the map array on the source id and source index fields.
    The algorithm is an iterative realization of Quicksort, as described in
    "Implementing Quicksort Programs" by Robert Sedgewick in the October
    1978 issue of CACM.  Since all keys are known to be unique, all tests
    "A[i] >= A[j]" have been replaced by tests "A[i] > A[j]".  *)

procedure sort_map;

const
    small = 9; (* Use linear sort if partition is smaller. *)

var
  l, r, i, j: int_type; (* Index variables. *)
  stack: array [1..40] of int_type;
  stack_top: 0 .. 40;
  done: boolean;
  v, t: map_record;

begin
  stack_top := 0;
  l := 1;
  r := map_size;
  done := (map_size <= small);
  while not done do begin
    t := map^ [(l+r) div 2];
    map^ [(l+r) div 2] := map^ [l+1];
    map^ [l+1] := t;
    if map_gtr (map^[l+1].srcid, map^[r].srcid) then begin
      t := map^ [l+1];
      map^ [l+1] := map^ [r];
      map^ [r] := t;
    end;
    if map_gtr (map^[l].srcid, map^[r].srcid) then begin
      t := map^ [l];
      map^ [l] := map^ [r];
      map^ [r] := t;
    end;
    if map_gtr (map^[l+1].srcid, map^[l].srcid) then begin
      t := map^ [l+1];
      map^ [l+1] := map^ [l];
      map^ [l] := t;
    end;
    i := l + 1;
    j := r;
    v := map^ [l];

    (*  MAP^[L+1] <= V = MAP^[L] <= MAP^[R]  *)

    loop
      repeat
        i := i + 1
      until map_gtr (map^[i].srcid, v.srcid);
      repeat
        j := j - 1
      until map_gtr (v.srcid, map^[j].srcid);
    exit if j < i;
      t := map^ [i];
      map^ [i] := map^ [j];
      map^ [j] := t;
    end;
    t := map^ [l];
    map^ [l] := map^ [j];
    map^ [j] := t;

    (*  L < J < I <= R
        L <= k <  J  =>  MAP^[k] < V
        J <= k <  I  =>  MAP^[k] = V
        I <= k <= R  =>  MAP^[k] > V  *)

    if j - l >= r - i + 1 then begin (* Right partition smaller. *)
      if j - l <= small then (* Both partitions small. *)
        if stack_top = 0 then
          done := true
        else begin (* Pop a partition and continue. *)
          l := stack [stack_top - 1];
          r := stack [stack_top];
          stack_top := stack_top - 2;
        end
      else if r - i + 1 <= small then (* Right partition small, must sort left. *)
        r := j - 1
      else begin (* Neither small; push left & sort right. *)
        stack [stack_top + 1] := l;
        stack [stack_top + 2] := j - 1;
        stack_top := stack_top + 2;
        l := i;
      end;
    end
    else (* j - l < r - i + 1 *) begin (* Left partition smaller. *)
      if r - i + 1 <= small then (* Both partitions small. *)
        if stack_top = 0 then
          done := true
        else begin (* Pop a partition and continue. *)
          l := stack [stack_top - 1];
          r := stack [stack_top];
          stack_top := stack_top - 2;
        end
      else if j - l <= small then (* Left partition small, must sort right. *)
        l := i
      else begin (* Neither small; push right and sort left. *)
        stack [stack_top + 1] := i;
        stack [stack_top + 2] := r;
        stack_top := stack_top + 2;
        r := j - 1;
      end;
    end;
  end (* while not done *);

  (*  A linear sort will straighten out the remaining inversions.  *)

  for i := map_size - 1 downto 1 do
    if map_gtr (map^[i].srcid, map^[i+1].srcid) then begin
      v := map^ [i];
      j := i + 1;
      repeat
        map^ [j-1] := map^ [j];
        j := j + 1;
      until (j > map_size) orif map_gtr (map^[j].srcid, v.srcid);
      map^ [j-1] := v;
    end;
end (* sort_map *);
$PAGE a_l_map_header

(*  ALMapHeader will print the page heading for the address -> line number map. *)

procedure a_l_map_header ( var fb: file_block );

begin
  fio_write (fb, 'Code Address -> Line Number Map for Module ' || mod_ident);
  fio_tab (fb, fb.width - 5 - width (fb.pageno));
  fio_line (fb, 'Page ' || cv_int (listfb.pageno));
end;
$PAGE l_a_map_header

(*  LAMapHeader will print the page heading for the line number -> address map.  *)

procedure l_a_map_header ( var fb: file_block );

begin
  fio_write (fb, 'Line Number -> Code Address Map for Module ' || mod_ident);
  fio_tab (fb, fb.width - 5 - width (fb.pageno));
  fio_line (fb, 'Page ' || cv_int (fb.pageno));
end;
$PAGE print_entry

(*  PrintEntry prints a single map file entry.  The form of the entry depends
    on the MapAddrToLine parameter (see the comment in OutputMap).  *)

procedure print_entry ( index: int_type; map_addr_to_line: boolean );

var prt_string: string [20];

begin
  with map^[index] do begin
    prt_string := cv_source_id (srcid);
    if map_addr_to_line then begin
      fio_write (listfb, cv_radix (address, adr_width) || '  ' || prt_string);
      fio_space (listfb, 18 - adr_width - length (prt_string));
    end
    else begin
      fio_space (listfb, 18 - adr_width - length (prt_string));
      fio_write (listfb, prt_string || '  ' || cv_radix (address, adr_width));
    end;
  end;
end (* print_entry *);
$PAGE output_map

(*  OutputMap will write the map to the listing file, in columns, with entries
    having either the form
        file-page/line:index  address
    or
        address  file-page/line:index
    depending on the MapAddrToLine parameter.  *)

procedure output_map ( map_addr_to_line: boolean );

var n_lines,
    n_columns,
    n_slots,
    ipage,
    iline,
    icolumn,
    nprinted,
    nprtleft,
    ncol,
    nleft: int_type;

begin
  n_columns := listfb.width div (14 + adr_width);
  n_lines := listfb.plength - 2;
  n_slots := n_columns * n_lines;

  for ipage := 1 to map_size div n_slots do begin
    fio_page (listfb);
    for iline := 1 to n_lines do begin
      fio_skip (listfb);
      for icolumn := 1 to n_columns do begin
        print_entry (((ipage - 1) * n_slots) + ((icolumn - 1) * n_lines) + iline,
                     map_addr_to_line);
      end;
    end;
  end;

  nprinted := (map_size div n_slots) * n_slots;
  nleft := map_size - nprinted;
  if nleft <> 0 then begin
    fio_page (listfb);
    n_lines := nleft div n_columns;
    ncol := nleft mod n_columns;
    nprtleft := nprinted + ncol * (n_lines + 1);
    for iline := 1 to n_lines do begin
      fio_skip (listfb);
      for icolumn := 1 to ncol + 1 do begin
        print_entry (nprinted + ((icolumn-1) * (n_lines + 1)) + iline, map_addr_to_line);
      end;
      for icolumn := ncol + 2 to n_columns do begin
        print_entry (nprtleft + ((icolumn - ncol - 1) * n_lines) + iline, map_addr_to_line);
      end;
    end;
    if ncol <> 0 then begin
      fio_skip (listfb);
      for icolumn := 1 to ncol do begin
        print_entry (nprinted + icolumn * (n_lines + 1), map_addr_to_line);
      end;
    end;
  end;
end (* print_entry *);
$PAGE map_print

(*  MapPrint prints the line number / code address cross reference map.  It
    first reads the map file into a memory array.  Since the map file was
    written in code address order, the code address -> line number map can
    be written directly.  The array is then sorted by line number, and the
    line number -> code address map is written.  *)

public procedure map_print;

var i: int_type;

begin
  if list_file = '' then return;
  with root_block^.children^ do begin
    if id <> nil
      then mod_ident := id^.text
      else mod_ident := '??????';
  end;

  (*  Read the map file into memory.  *)

  close (map_file);
  reset (map_file, tempname ('MAP'));
  new (map, map_size);
  for i := 1 to map_size do begin
    map^ [i] := map_file^;
    get (map_file);
  end;
  scratch (map_file);

  (*  Prepare the listing file.  *)

  case lf_status of
    unopened:    fio_open (listfb, '.LST ' || list_file);
    prev_opened: fio_reopen (listfb)
  end;
  with listfb do begin
    if not eof (file_var) then begin
      writeln (tty, '?Unable to open listing file ', list_file);
      list_file := '';
      return;
    end;
  end;
  listfb.new_page := fio_eject;
  listfb.page_header := fio_nop;
  listfb.width := prog_options.page_width;
  listfb.plength := prog_options.page_length;
  fio_page (listfb);

  (*  Print the address -> line number listing.  *)

  listfb.pageno := 1;
  listfb.page_header := a_l_map_header;
  output_map (true);

  (*  Sort the map.  *)

  sort_map;

  (*  Print the line number -> address listing.  *)

  listfb.pageno := 0;
  listfb.page_header := l_a_map_header;
  output_map (false);

  dispose (map);
  fio_close (listfb);
  lf_status := prev_opened;
end (* map_print *).
   