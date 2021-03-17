$TITLE LIB.PAS, last modified 5/1/84, zw
PROGRAM lib OPTIONS SPECIAL(WORD);
$include cmdutl.typ
$include filutl.inc
$include timutl.inc
$include query.inc
$include versio.inc
(*external procedure rename ( string[40]; string[40]; boolean);*)
PROCEDURE rename(s1, s2: STRING[40]; f: BOOLEAN);
BEGIN
END;

CONST
maximum_number_blocks := 377777;
number_directory_blocks := 4;
directory_entries_per_block := 16;
words_per_block:= 127;
characters_per_word = 5;
characters_per_block := characters_per_word * words_per_block;

TYPE
character_pointer = 1 .. characters_per_block;
word_pointer = 1 .. words_per_block;
directory_pointer = 1 .. directory_entries_per_block;
segment_address = 0 .. maximum_number_blocks;
directory_address = 0 .. number_directory_blocks;
address = 0 .. maximum_number_blocks;
word = MACHINE_WORD;
directory_index = 0 .. number_directory_blocks * directory_entries_per_block;
character_index = 1 .. MAXIMUM(INTEGER);
error_code =  (lib_ok, lib_notopen, lib_openfail, lib_iofatal,
  seg_notopen, seg_notfind, bad_diridx, file_notfind, intr_fatal);
name_field = PACKED RECORD (*9 characters -> 2 words*)
  name: PACKED ARRAY[1 .. 6] OF CHAR;
  extension: PACKED ARRAY[1 .. 3] OF CHAR
END;
directory_entry = RECORD (*8 words*)
  name: name_field;
  address: INTEGER;
  length: INTEGER;
  write_date: dtime_int;
  access_date: dtime_int;
  valid: BOOLEAN;
  space: INTEGER;
END;
directory_block = ARRAY[directory_pointer] OF directory_entry;
word_block = RECORD
  checksum : word;
  data: PACKED ARRAY[word_pointer] OF word
END;
character_block = record
  checksum : word;
  data: PACKED ARRAY[character_pointer] OF char
END;
block_type = (directory_type, word_type, character_type);
lib_block = RECORD
  CASE block_type OF
    directory_type: (entries: directory_block);
    word_type: (words: word_block);
    character_type: (characters: character_block)
END;
library = RECORD
  lib_file : FILE OF lib_block;
  directory_buffer, input_buffer, output_buffer : lib_block;
  directory_buffer_address : directory_address;
  input_address, output_address: segment_address;
  output_pointer: character_pointer;
  segment_is_open: BOOLEAN;
  open_directory_pointer: directory_index;
  last_directory : 0 .. number_directory_blocks * directory_entries_per_block
END;
(*EXTERNAL FUNCTION checksum(buffer : lib_block): word;*)
FUNCTION checksum (buffer: lib_block): word;
BEGIN
  checksum := 0
END;

FUNCTION error_check(status: io_status): error_code;
(*this function returns the appropriate error_code given an iostatus*)
BEGIN
  CASE status OF
    io_ok: error_check := lib_ok;
    io_intr, io_outf, io_inpf: error_check := lib_iofatal;
    io_rewr: error_check := lib_notopen;
    io_opnf: error_check := lib_openfail;
    io_illc: error_check := intr_fatal;
    others: error_check := lib_iofatal
  END
END;

PROCEDURE read_segment_block ( var f: library;
                            addr: segment_address; (*physical address*)
                             var err: error_code );
(*This procedure reads the physical addressed block into the
   segin buffer and sets the segin address pointer. *)
begin
  if addr <= number_directory_blocks then err := intr_fatal (*ADDR is not a segment block!*)
  else begin
    readrn ( f.lib_file, addr, f.input_buffer );
    err := error_check (iostatus (f.lib_file) );
    if (( err = lib_ok ) and
   ( f.input_buffer.words.checksum = checksum ( f.input_buffer )))
     then f.input_address := addr
     else err := lib_iofatal
  end
end; (*read_segment_block*)

procedure write_segment_block ( var f: library;
                             var err: error_code );
(*This procedure flushes the segout buffer to the segout address*)
begin
  f.output_buffer.words.checksum := checksum ( f.output_buffer );
  writern ( f.lib_file, f.output_address, f.output_buffer );
  err := error_check ( iostatus (f.lib_file) )
end; (*write_segment_block*)

procedure read_directory_block ( var f: library;
                             addr: directory_address; (*physical address*)
                             var err: error_code );
(*This procedure reads the physical addressed directory block into
   the directory buffer and sets the dir address pointer*)
begin
  if addr > number_directory_blocks then err := intr_fatal
  else begin
    readrn (f.lib_file, addr, f.directory_buffer );
    err := error_check ( iostatus (f.lib_file) );
    if err = lib_ok then f.directory_buffer_address := addr
  end
end; (*read_directory_block*)

procedure write_directory_block ( var f: library;
                         var err: error_code );
(*This procedure flushes the directory buffer to the dir address*)
begin
  writern ( f.lib_file, f.directory_buffer_address, f.directory_buffer );
  err := error_check ( iostatus (f.lib_file) )
end; (*write_directory_block*)

procedure read_directry_entry ( var f: library;
                         dir_index: directory_index;  (*directory pointer*)
                          var entry: directory_entry; (*entry record*)
                        var err: error_code );
(*This reads the indexed directory entry and returns it. Error
   if the index is past the last directory entry *)
var addr : address;
begin
  if dir_index > f.last_directory then err := bad_diridx
  else begin
    addr := (dir_index-1) div directory_entries_per_block + 1;  (*block address of entry*)
    if f.directory_buffer_address <> addr then read_directory_block (f, addr, err);
    if err = lib_ok then
      entry := f.directory_buffer.entries[dir_index - (addr-1) * directory_entries_per_block]
    end
end;  (*read_directry_entry*)

procedure write_directory_entry ( var f: library;
                       dir_index: directory_index;  (*directory pointer*)
                          entry: directory_entry; (*entry to write*)
                          var err: error_code );
(*This procedure writes over the indexed directory entry and flushes
   the dir buffer. If index > f.last_directory then the last_directory is 
   updated   *)
var addr : address;
begin
  if dir_index > (number_directory_blocks*directory_entries_per_block) then err := bad_diridx 
  else begin
    addr := (dir_index-1) div directory_entries_per_block + 1;
    if f.directory_buffer_address <> addr then read_directory_block (f, addr, err);
    if err = lib_ok then begin
      f.directory_buffer.entries[dir_index - (addr-1) * directory_entries_per_block] := entry;
      write_directory_block ( f, err);

      if ((dir_index > f.last_directory) and (err = lib_ok)) then
        f.last_directory := dir_index
    end
  end
end;  (*write_directory_entry*)

function lookup_directory_entry ( var f: library;
                    seg_name: name_field; (*entry to find*)
                      var dir_index: directory_index  (*index of find*)
                                ): boolean; (*true if successful find*)
(*This searches for the segname directory entry and returns its index.
   The boolean function value is an indicator of the success of the
   search. The search begins at dir_index+1    *)

  function equal ( name1,
               name2: name_field ): boolean;
  (*Returns TRUE if name1 matches name2. Name1 may use wildcarding.*)
function match (str1,str2: packed array[1 .. *] of char): boolean;
var i,j: word;
begin
  match := true;
  if index(str1, '*') = 0 then
    for i := 1 to upperbound (str1) do
      exit if not((str1[i]=str2[i]) orif (str1[i] = '?')) do match := false
  else if verify (str1, ['*','?',' ']) <> 0 then begin
    for i := 1 to (index(str1,'*')-1) do
      exit if not((str1[i]=str2[i]) orif (str1[i] = '?')) do match := false;
    j := index (str2,' ', upperbound(str2)+1) - 1;
    for i := (index(str1,' ',upperbound(str1)+1)-1) downto (index(str1,'*')+1) do begin
      exit if not((str1[i]=str2[j]) orif (str1[i] = '?')) do match := false;
      j := j-1
    end
  end
end;
begin
  equal := (match(name1.name, name2.name) and match(name1.extension, name2.extension))
end;
var idx : directory_index;
    entry : directory_entry;
    err : error_code;
begin
  lookup_directory_entry := false;
  idx := dir_index;
  err := lib_ok;
  while ((lookup_directory_entry = false) and (idx < f.last_directory)) do begin
    idx := idx + 1;
    read_directry_entry ( f, idx, entry, err );
  exit if err <> lib_ok;
    lookup_directory_entry := equal ( seg_name, entry.name )
  end;
  if lookup_directory_entry then dir_index := idx
end; (*lookup_directory_entry*)

procedure open_library ( var f: library;  (*library to open*)
                         lib_name: name_field;
                          var new: boolean;
                             var err: error_code );
(*This procedure opens or creates a library and initializes the library
   record*)
var entry : directory_entry;
(*Note -- all lines with a 'XXX' should be deleted later.*)
static var f_str: string[10]; (*XXX*)
begin
  new := false;
  with lib_name do
(*XXX
    update ( f.lib_file, name||'.'||extension, [retry] );
XXX*)
    f_str := name || '.' || extension; (*XXX*)
    update ( f.lib_file, f_str, [retry] ); (*XXX*)
  if iostatus (f.lib_file) <> io_ok then err := lib_openfail
  else if eof(f.lib_file) then begin  (*new library opened*)
         new := true;
         f.last_directory := 0;
       f.output_address := number_directory_blocks + 1
  end
       else begin  (*previously existing library opened*)
     f.last_directory := number_directory_blocks*directory_entries_per_block;
      read_directry_entry ( f, f.last_directory, entry, err );
       while (not(entry.valid) and (err = lib_ok)) do begin  (*find the last dir entry*)
    f.last_directory := f.last_directory - 1;
         read_directry_entry ( f, f.last_directory, entry, err )
        end;
        f.output_address := entry.address + (entry.length-1)div characters_per_block + 1
  end;
  f.directory_buffer_address := 0;
  f.input_address := 0;
  f.output_pointer := 1;
  f.segment_is_open := false
end;  (*open_library*)

procedure close_library ( var f: library );
(*This procedure closes a library file*)
begin
  close (f.lib_file)
end; (*close_library*)

procedure close_segment ( var f: library );
(*This procedure closes the currently open segment.*)
var err : error_code;
begin
  if f.segment_is_open then begin
  (*first, flush the current segout buffer*)
    f.output_buffer.words.data[(f.output_pointer-1) div 5 + 1]:= 0;
    while ((f.output_pointer + 4) < characters_per_block) do begin
      f.output_pointer := f.output_pointer + 5;
      f.output_buffer.words.data[(f.output_pointer-1) div 5 + 1] := 0
    end;
    f.segment_is_open := false;
    write_segment_block (f, err );
    f.output_address := f.output_address + 1;
    f.output_pointer := 1
  end
end; (*close_segment*)

procedure open_segment ( var f: library;
                                seg_name: name_field; (*segment to open*)
                            var err: error_code );
(*This procedure creates a new directory entry open for writing,
   and closes any previously opened segment. *)
var entry : directory_entry;

  procedure set_entry_ptr;
  (*This procedure attempts to find a not valid directory entry
     in the currently read block and sets the open_directory_pointer there.
     If one can't be found in the current block, then succeeding
     blocks are searched.   *)
  var idx, index : directory_index;
  begin  (*set_entry_ptr*)
    index := 0;
    (*first, find if it already exists. If so, write over the entry*)
    if lookup_directory_entry (f, seg_name, index ) then f.open_directory_pointer := index
    else begin  (*to find an free directory entry*)
      if f.directory_buffer_address = 0 then idx := 1
      else begin
       idx := (f.directory_buffer_address-1) * directory_entries_per_block + 1;
  read_directry_entry ( f, idx, entry, err );
      while ((entry.valid) and (idx <= f.last_directory) and (err = lib_ok)) do begin
      read_directry_entry (f, idx, entry, err );
         idx := idx + 1
      end
      end;
      if err = lib_ok then f.open_directory_pointer := idx
    end
  end;  (*set_entry_ptr*)

begin  (*body of open_segment*)
  if f.segment_is_open then close_segment (f);
  set_entry_ptr;
  with entry do begin
    name.name := seg_name.name;
    name.extension := seg_name.extension;
    address := f.output_address;
    length := 0;
    write_date := daytime;
    access_date := daytime;
    valid := true
  end;
  f.segment_is_open := true;
  f.output_pointer:=1;
  write_directory_entry ( f, f.open_directory_pointer, entry, err );
end; (*open_segment*)
 
procedure read_words ( var f : library;
                      entry_idx : directory_index;        (*ptr to dir to read*)
                          var index : word;           (*start index for read*)
                        num_to_read : word;         (*words to read*)
                       var words_left : word;      (*words not read*)
                      var word_array : array[1 .. *] of word;
                         var err : error_code );        (*error code*)
(*This procedure fills the array with num_to_read words, unless end-of-
   segment is encountered*)
var
  entry : directory_entry;
  addr : address;  (*block address in library to read*)
  buffer_ptr : 1 .. words_per_block+1;  (*pointer into segin buffer*)
  idx : 0 .. words_per_block;  (*index for word_array selection*)
begin
  read_directry_entry (f, entry_idx, entry, err);
  if err = lib_ok then
    if ((index*5) > entry.length) then err := intr_fatal
    else begin
      addr := (index-1) div words_per_block + entry.address ;
(*   make sure the segin buffer is current*)
      if addr <> f.input_address then
   read_segment_block (f, addr, err);
      if err = lib_ok then begin
   entry.access_date := daytime;
 write_directory_entry (f, entry_idx, entry, err);
    buffer_ptr := (index-1) mod words_per_block + 1;
        idx := 0;
   loop  (*to file the word array*)
  exit if ((idx >= num_to_read) or
               ((index+idx)*5 > entry.length) or
             (err <> lib_ok));
    idx := idx + 1;
       word_array[idx] := f.input_buffer.words.data[buffer_ptr];
     buffer_ptr := buffer_ptr + 1;
          (*read a new buffer if it is read past*)
         if buffer_ptr > words_per_block then begin
        buffer_ptr := 1;
      read_segment_block (f, f.input_address + 1, err)
       end
       end; (*loop*)
     index := index + idx
      end  (*if err = lib_ok begin*)
    end;
    words_left := num_to_read - idx
end;  (*read_wordss*)
procedure read_characters ( var f : library;
                     entry_idx : directory_index;        (*dir to read from*)
                            var index : character_index;       (*start index*)
                         num_to_read : character_index;     (*characters to read*)
                       var characters_left : character_index;  (*characters not read*)
                      var char_array : array[1 .. *] of char;
                         var err : error_code );
(*This procedure fills the array with num_to_read characters, unless end-
of-segment is encountered*)
var
  entry : directory_entry;
  addr : address;  (*library address to read segin buffer*)
  buffer_ptr : 1 .. characters_per_block+1;  (*pointer into segin buffer*)
  idx : 0 .. characters_per_block;  (*index into char_array*)
begin
  read_directry_entry ( f, entry_idx, entry, err);
  if err = lib_ok then
    if (index > entry.length) then err := bad_diridx
    else begin
      addr := (index-1) div characters_per_block + entry.address ;
(*   make sure the segin buffer is current*)
      if addr <> f.input_address then
 read_segment_block (f, addr, err);
      if err = lib_ok then begin
   entry.access_date := daytime;
 write_directory_entry (f, entry_idx, entry, err);
    buffer_ptr := (index-1) mod characters_per_block + 1;
        idx := 0;
   loop  (*to fill the char_array*)
  exit if ((idx >= num_to_read) or
               ((index + idx) > entry.length) or
             (err <> lib_ok));
    idx := idx + 1;
       char_array[idx] := f.input_buffer.characters.data[buffer_ptr];
     buffer_ptr := buffer_ptr + 1;
          (*if buffer is read past, read the next one*)
    if buffer_ptr > characters_per_block then begin
        buffer_ptr := 1;
      read_segment_block ( f, f.input_address + 1, err)
      end
       end; (*loop*)
       index := index + idx
      end  (*if err = lib_ok begin*)
    end;
    characters_left := num_to_read - idx
end;  (*read_characters*)

procedure read_line ( var f: library;
                           entry_idx : directory_index; (*dir to read from*)
                           var index : character_index;        (*start index*)
                        var line : string[*];
                         var err : error_code );
(*This procedure reads the character-indexed line into the given string*)
var
  entry : directory_entry;
  addr : address;  (*library address to read from*)
  buffer_ptr : 1 .. characters_per_block+1;  (*pointer into segin buffer*)
  idx : 0 .. characters_per_block;  (*index for line length*)
begin
  read_directry_entry (f, entry_idx, entry, err);
  if err = lib_ok then
    if index > entry.length then err := bad_diridx
    else begin
      addr := (index-1) div characters_per_block + entry.address ;
(*   make sure the segin buffer is current*)
      if addr <> f.input_address then
  read_segment_block (f, addr, err);
      if err = lib_ok then begin
   entry.access_date := daytime;
 write_directory_entry ( f, entry_idx, entry, err);
   buffer_ptr := (index-1) mod characters_per_block + 1;
        idx := 0;
   loop  (*to build the line*)
       exit if ((line[idx] = chr(10)) or
              ((index + idx) >= entry.length) or
            (err <> lib_ok));
    idx := idx + 1;
       line := line || f.input_buffer.characters.data[buffer_ptr];
        buffer_ptr := buffer_ptr + 1;
          (*if the buffer is read past, read the next*)
    if buffer_ptr > characters_per_block then begin
      fer_ptr := 1;
      read_segment_block (f, f.input_address + 1, err)
       end
       end; (*loop*)
     index := idx
      end (*if err = lib_ok begin*)
    end
end;  (*read_line*)
  
procedure write_words ( var f: library;
                      var num_to_write: word;      (*number to write,written*)
                    word_array: array[1 .. *] of word;(*array to write*)
                          var err: error_code );
(*This procedure writes num_to_write words from the array to the end of
   the currently open segment  *)
var
  i: word;  (*loop counter*)
  entry: directory_entry;
begin
  if not f.segment_is_open then err := seg_notopen
  else begin
    if num_to_write > upperbound (word_array) then
      num_to_write := upperbound (word_array);
    read_directry_entry (f, f.open_directory_pointer, entry, err);
    i := 1;
    while i <= num_to_write do begin  (*write to the segout buffer*)
      f.output_buffer.words.data[(f.output_pointer-1) div 5 + 1] :=
 word_array[i];
(*   if the buffer is full, flush it to the library file*)
      if ((f.output_pointer+4) = characters_per_block) then begin
        f.output_pointer := 1;
    write_segment_block (f, err);
   entry.length := entry.length + characters_per_block;
 write_directory_entry (f, f.open_directory_pointer, entry, err);
     f.output_address := f.output_address + 1
      end
      else f.output_pointer := f.output_pointer + 5;
    exit if err <> lib_ok;
      i := i + 1
    end;  (*while*)
    entry.length := entry.length + f.output_pointer - 1;
    write_directory_entry (f, f.open_directory_pointer, entry, err);
    num_to_write := i - 1
  end
end;  (*write_words*)

procedure write_characters ( var f: library;
                          var num_to_write: character_index;  (*number to write,written*)
                    char_array: array[1 .. *] of char;(*array to write*)
                          var err: error_code );
(*This procedure writes num_to_write characters from the array to the end of
   the currently open segment  *)
var
  i: character_index;  (*loop counter*)
  entry: directory_entry;
begin
  if not f.segment_is_open then err := seg_notopen
  else begin
    if num_to_write > upperbound (char_array) then
      num_to_write := upperbound (char_array);
    read_directry_entry (f, f.open_directory_pointer, entry, err);
    i := 1;
    while i <= num_to_write do begin  (*write characters to the segout buffer*)
      f.output_buffer.characters.data[f.output_pointer] := char_array[i];
(*   if the buffer is full, flush it to the library*)
      if f.output_pointer = characters_per_block then begin
 f.output_pointer := 1;
    write_segment_block (f, err);
   entry.length := entry.length + characters_per_block;
 write_directory_entry (f, f.open_directory_pointer, entry, err);
     f.output_address := f.output_address + 1
      end
      else f.output_pointer := f.output_pointer + 1;
    exit if err <> lib_ok;
      i := i + 1
    end;  (*while*)
    entry.length := entry.length + f.output_pointer - 1;
    write_directory_entry (f, f.open_directory_pointer, entry, err);
    num_to_write := i - 1
  end
end;  (*write_characters*)

procedure write_line ( var f: library;
                          line: string[*];             (*string to write*)
                    var err: error_code );
(*This procedure writes the given string to the end of
   the currently open segment  *)
var
  i: word;  (*loop counter*)
  entry: directory_entry;
  tline: string[255];
begin
  if not f.segment_is_open then err := seg_notopen
  else begin
    read_directry_entry (f, f.open_directory_pointer, entry, err);
    i := 1;
    tline := line || chr(13) || chr(10);  (*add EOLN characters*)
    while i <= length (tline) do begin  (*write the line to the segout buffer*)
      f.output_buffer.characters.data[f.output_pointer] := tline[i];
(*   if the buffer is full, flush it to the library*)
      if f.output_pointer = characters_per_block then begin
   f.output_pointer := 1;
    write_segment_block (f, err);
   entry.length := entry.length + characters_per_block;
 write_directory_entry (f, f.open_directory_pointer, entry, err);
     f.output_address := f.output_address + 1
      end
      else f.output_pointer := f.output_pointer + 1;
    exit if err <> lib_ok;
      i := i + 1
    end;  (*while*)
    entry.length := entry.length + f.output_pointer - 1;
    write_directory_entry (f, f.open_directory_pointer, entry, err)
  end
end;  (*write_line*)

procedure pack ( var f: library;
                 f_name: name_field;
                    var err: error_code );
(*This procedure packs the library into a temp file ###PAK.TMP, which is later renamed.*)
var
  templib : library;  (*temp library to write packed library*)
  dir_count, dir_index : directory_index;  (*dir indices for packing process*)
  disp : address;  (*address displacement*)
  entry : directory_entry;
  wr_addr : address;  (*address to write pack segments*)
  new,ren_err : boolean;
  l_name,b_name,t_name: string[40];  (*renaming variables*)
  b_lib: name_field;                      (*the backup library name*)
begin
  writeln(tty,'Pack procedure started for ',filename(f.lib_file));
  break;
  dir_index := 0;  (*new packed directory index*)
  b_lib.name := f_name.name;
  b_lib.extension := 'BAK';
  open_library (templib, b_lib, new, err);
  wr_addr := templib.output_address;
  for dir_count := 1 to f.last_directory do begin  (*sequence through all unpacked segments*)
    read_directry_entry (f, dir_count, entry, err);
  exit if err <> lib_ok;
    if entry.valid then begin  (*write it to new packed library*)
      for disp := 0 to ((entry.length-1)div characters_per_block) do begin
        read_segment_block (f, entry.address + disp, err);
      exit if err <> lib_ok;
       templib.output_buffer := f.input_buffer;
      templib.output_address := wr_addr + disp;
     write_segment_block ( templib, err)
      end;
      entry.address := wr_addr;
      entry.write_date := daytime;
      entry.access_date := daytime;
      dir_index := dir_index + 1;
      write_directory_entry ( templib, dir_index, entry, err );
      wr_addr := templib.output_address + 1
    end;
  exit if err <> lib_ok
  end; (*for loop*)
  if err = lib_ok then begin
  (*now a packed library is in B_NAME, unpacked in L_NAME.
     Put L_NAME in a TMP file (T_NAME), and B_NAME into
     L_NAME to finish the packing process.*)
    close_library (f);
    close_library (templib);
    with f_name do begin
      l_name := name||'.'||extension||chr(15b);
      t_name := name||'.tmp'||chr(15b);
      b_name := name||'.bak'||chr(15b);
      rename ( l_name, t_name, ren_err);
      rename ( b_name, l_name , ren_err)
    end;
    if ren_err then err := lib_iofatal
    else begin
      open_library (f, f_name, new, err);
      f.last_directory := dir_count - 1       (*make sure this is right*)
      end
  end;
  if err = lib_ok then begin
    writeln(tty, 'Pack procedure successful.')
    end
  else writeln(tty, 'Pack procedure aborted.')
end; (*pack*)

TYPE
command_key = RECORD
  name: STRING[10];
  abbrev: 1 .. 10
END;
lib_command = (open_command, directory_command, delete_command,
  add_command, extract_command, pack_command, help_command, quit_command);
lib_commands_list = array[lib_command] of command_key;

VAR
lib_commands: lib_commands_list :=
 (('OPEN',        1),
  ('DIRECTORY',   3),
  ('DELETE',      3),
  ('ADD',         1),
  ('EXTRACT',     1),
  ('PACK',        4),
  ('HELP',        4),
  ('QUIT',        1));
f: library;                     (*the library*)
real_f: file of word;      (*external file record*)
real_f_array: array[1 .. words_per_block] of word;    (*input file block*)
f_name, s_name: name_field;            (*file name var*)
line: cmdline;           (*a command line*)
idx: cmdlineidx;                (*idx to cmdline*)
fid: FILE_NAME;                           (*filutl file ID*)
cmd: lib_command;                   (*parsed command*)
open_lib: boolean;              (*open library flag*)
dirptr: directory_index;             (*utility pointer into directory*)
entry: directory_entry; (*a directory entry*)
i,j: word;                   (*utility vars (loops, etc.)*)
err: error_code;                       (*error code*)
f_count: directory_index;           (*utility counters*)
blk_count: 0 .. maximum_number_blocks;
num_to_write,
num_to_read: word;              (*i/o controls*)
words_left: word;
word_var : array[1 .. words_per_block] of word;  (*flex_array parameter*)
new : boolean;            (*new/old library flag*)

FUNCTION lookup_command
 (line: cmdline;
  VAR lindex: cmdlineidx;
  VAR list: lib_commands_list;
  VAR nameidx: lib_command): BOOLEAN;
VAR name: STRING[10]; i: lib_command; l: cmdlineidx;
BEGIN
  WHILE (lindex <= LENGTH(line)) ANDIF (line[lindex] <= ' ') DO
    lindex := lindex + 1;
  IF lindex > LENGTH(line) THEN BEGIN lookup_command := FALSE; RETURN END;
  IF UPPERCASE(line[lindex]) IN ['A' .. 'Z'] THEN BEGIN
    name := ''; l := 0;
    REPEAT
      l := l + 1;
      IF l <= 10 THEN name := name || UPPERCASE(line[lindex]);
      lindex := lindex + 1
    UNTIL (lindex > LENGTH(line)) ORIF
      NOT (UPPERCASE(line[lindex]) IN ['A' .. 'Z'])
  END
  ELSE BEGIN name := line[lindex]; l := 1; lindex := lindex + 1 END;
  IF l <= 10 THEN BEGIN
    FOR i := MINIMUM(lib_command) TO MAXIMUM(lib_command) DO
      IF (list[i].abbrev <= l) ANDIF (l <= LENGTH(list[i].name)) THEN
	IF SUBSTR(list[i].name, 1, l) = NAME
	  THEN BEGIN nameidx := i; lookup_command := TRUE; RETURN END
  END;
  lookup_command := FALSE;
  lindex := lindex - l
END;
function get_name ( line : cmdline; (*line to be searched*)
                 var idx : cmdlineidx;       (*search pointer into line*)
                    var name : name_field;  (*name found on line*)
               var fid : FILE_NAME   (*file ID found*)
                   ): boolean;     (*TRUE on a good name find*)
(*This function searches the line from the given idx for the next file
   name. If one is found the idx is updated to point after the name, and
   if one is not found the function returns a FALSE value. *)
type
  del_set = set of char;
var
  colonidx,
  periodidx : cmdlineidx;
  valid_delimiters : del_set;
begin
  get_name := false;
  fid := '';
  valid_delimiters := [' ', ',', '/'];
  (*first, skip whitespace and delimiters*)
  while ((idx <= length(line)) and (line[idx] in valid_delimiters)) do idx := idx + 1;
  get_name := pr_file_id (line, idx, fid);  (*parse the file name*)
  if get_name then begin  (*parse the name, extensionension into a name_field*)
    colonidx := index(fid, ':') + 1;
    periodidx := index(fid,'.');
    if periodidx = 0 then begin
      periodidx := length(fid) + 1;
      name.name := uppercase (substr(fid, colonidx, periodidx-colonidx));
      name.extension := ''
      end
    else begin
      name.name := uppercase (substr(fid, colonidx, periodidx-colonidx));
      name.extension := uppercase (substr(fid,periodidx+1))
    end
  end
end;  (*get_name*)
procedure print_name (f: name_field);
(*This procedure prints filenames in a suitable format for the TTY*)
var i: 1 .. 6;
begin
  for i := 1 to 6 do
    if f.name[i] <> ' ' then
      write(tty, f.name[i] );
  write(tty,'.');
  for i := 1 to 3 do
    write(tty, f.extension[i] )
end;  (*print_name*)

procedure blk_msg (blks: segment_address; files: segment_address);
(*This procedure prints the  'X block(s) in Y file(s)' message.*)
begin
  write(tty,blks: 4, ' block');
  if blks <> 1 then write(tty, 's');
  write(tty, ' in ');
  write(tty,files: 3, ' file');
  if files <> 1 then write(tty, 's')
end;  (*blk_msg*)
procedure check_error (var err : error_code;  (*the error condition to check*)
              name : name_field);    (*file involved with error*)
(*This procedure checks the given error condition and prints an appropriate
   error message   *)
begin
  case err of
lib_ok: ;
lib_notopen:
   writeln(tty, '?There is no library currently open.');
lib_openfail: begin
  write(tty, '?Unable to open library file ');
  print_name (name); writeln(tty)
       end;
lib_iofatal:
  writeln(tty, '?I/O with the library has failed.');
seg_notopen: begin
      write(tty, '?Segment ');
      print_name (name);
    writeln(tty, ' is not open.')
 end;
seg_notfind: begin
    write(tty, '?Unable to find ');
       print_name (name); writeln(tty)
       end;
bad_diridx:
   writeln(tty, '?Fatal internal error.');
file_notfind: begin
        write(tty, '?Unable to find ');
       print_name (name); writeln(tty, '.')
  end;
intr_fatal:
   writeln(tty, '?Fatal internal error.')
end;(*case*)
err := lib_ok
end; (*check_error*)

PROCEDURE do_open;
(*The OPEN command opens a new/old library file using the OPEN_LIBRARY
   routine.*)
begin
  err := lib_ok;
  if open_lib then close_library (f);
  if not get_name (line, idx, f_name, fid) then err := lib_openfail
  else begin
    if f_name.extension = '   ' then f_name.extension := 'LIB'; (*default extensionension*)
    open_library (f, f_name, new, err)
  end;
  if err = lib_ok then begin
    open_lib := true;
    if new then write (tty, 'New ') else write (tty, 'Old ');
    writeln(tty, 'library file ', filename(f.lib_file), ' opened.');
    writeln(tty)
  end
  else begin
    open_lib := false;
    check_error (err, f_name)
  end
end;  (*opencmd*)

PROCEDURE do_packcmd;
(*The PACK command uses the PACK routine to pack the library file*)
begin
  if err = lib_ok then begin
    pack (f, f_name, err);   (*pack the library*)
    check_error(err, f_name);
    writeln(tty)
  end
  else check_error(err, f_name)
end;  (*packcmd*)

PROCEDURE do_add;
(*The ADD command adds external files to the currently open library
   using the write_words routine. All disk transfers are done in one block
   chunks. *)
begin
  f_count := 0;
  blk_count := 0;
  writeln(tty, 'Files added:');
  writeln(tty); break;
  while (get_name(line, idx, s_name, fid) and (err = lib_ok)) do begin
    reset (real_f, fid, [RETRY]); (*open the external file*)
    exit if iostatus (real_f) <> io_ok do err := file_notfind;
    open_segment (f, s_name, err);  (*open a new library segment*)
    read_directry_entry (f,f.open_directory_pointer, entry, err);
    num_to_write := words_per_block;
    (*data transfer block (+ 1 checksum = 128 words )*)
    i := 0;  (*number of words in the current block*)
    loop
      i := i + 1;
      exit if (eof(real_f) orif (err <> lib_ok)) do begin
      (*output a partially filled block*)
      write_directory_entry (f, f.open_directory_pointer, entry, err); (*enter what we have*)
      num_to_write := i - 1;
      write_words (f, num_to_write, real_f_array, err)
    end;   (*exit if - do*) 
(*we keep two arrays, because unpacked are not compatible with packed arrays*)
    f.output_buffer.words.data[i] := real_f^;
    real_f_array[i] := real_f^;
      get (real_f);
      if i = words_per_block then begin  (*write only full blocks*)
        write_segment_block (f, err);
        entry.length := entry.length + characters_per_block;
        f.output_address := f.output_address + 1;
        i := 0
      end
    end;  (*loop*)
    close_segment (f);
    close (real_f);
    if err = lib_ok then begin
      read_directry_entry (f, f.open_directory_pointer, entry, err);
      f_count := f_count + 1;
      blk_count := blk_count + (entry.length-1) div 640 + 1;
      print_name (s_name);
      writeln(tty); break
    end
  end; (*big while*)
  check_error(err, s_name );
  writeln(tty);
  blk_msg (blk_count, f_count);
  writeln(tty, ' added.')
end;  (*add*)

PROCEDURE do_extract;
(*The extract command copies a library segment to an external file of the
   same name using the read_wordsS routine*)
begin
  num_to_read := words_per_block;
  f_count := 0;
  blk_count := 0;
  i := 1;
  words_left := 0;
  writeln(tty, 'Files extracted:');
  writeln(tty); break;
  while get_name (line, idx, s_name, fid) do begin
    dirptr := 0;
    while lookup_directory_entry (f, s_name, dirptr) do begin
      i := 1;
      read_directry_entry(f, dirptr, entry, err);
      if ((err = lib_ok) and (entry.valid)) then begin
        with entry do rewrite(real_f,name.name||'.'||name.extension,[RETRY]);
        if iostatus (real_f) <> io_ok then begin
          err := seg_notopen; check_error (err, entry.name)
        end
        else begin
          repeat  (*read segment blocks and write to a file*)
            read_words (f, dirptr, i, num_to_read, words_left, word_var, err);
            if err <> lib_ok then check_error(err, s_name)
            else for j:= 1 to (num_to_read-words_left) do begin
              real_f^ := word_var[j];
              put (real_f)
            end  (*for*)
          until words_left <> 0;
          close (real_f);
          f_count := f_count + 1;
          blk_count := blk_count + (entry.length-1) div 640 + 1;
          with entry do print_name (name);
          writeln(tty); break
        end  (*else begin*)
      end;  (*if*)
      check_error (err, s_name)
    end; (*while lookup_directory_entry*)
  end;  (*big while*)
  check_error(err, s_name );
  writeln(tty);
  blk_msg (blk_count, f_count);
  writeln (tty, ' extracted.')
end; (*extract*)

PROCEDURE do_delete;
(*The DELETE command deletes library segments (no packing here) by
   simply changing a segment's valid flag in the directory entry*)
VAR confirm: boolean;                (*for delete command*)
begin
    confirm := query ('Confirm');   (*to confirm deletions*)
    f_count := 0;
    blk_count := 0;
    writeln(tty, 'Files deleted:');
    writeln(tty);
    while get_name (line, idx, s_name, fid) do begin
    dirptr := 0;
    while lookup_directory_entry (f, s_name, dirptr) do begin
      read_directry_entry (f, dirptr, entry, err);
      if ((entry.valid) and (err = lib_ok)) then begin
        with entry do print_name(name);
        if (not confirm) orif (query (' -- OK')) then begin
          entry.valid := false;
          write_directory_entry (f, dirptr, entry, err);
          f_count := f_count + 1;
          blk_count := blk_count + (entry.length-1) div 640 + 1
        end;
        writeln(tty)
      end; (*if*)
      check_error (err, s_name );
    end (*while lookup_directory_entry*)
  end;(*while get_name*)
  writeln(tty);
  blk_msg (blk_count, f_count);
  writeln( tty, ' deleted.')
end; (*delete*)

PROCEDURE do_directory;
(*The DIR command uses the read_directry_entry and lookup_directory_entry routines to read and find
   the specified directory entries and print information about them.*)
VAR verbose: BOOLEAN; print_holes: BOOLEAN;
begin
    if ((idx < length(line)) and (line[idx] = '/') and
      (uppercase(line[idx+1]) = 'A')) then begin
      verbose := true;
      idx := idx + 2
    end
    else verbose := false;
    print_holes := false;
    f_count := 0;
    blk_count := 0;
    writeln(tty,'      Directory for library file ',filename(f.lib_file));
    if verbose then writeln(tty,
      '   Name        Blk Size         Access Date           Write Date');
    writeln(tty);
    if not get_name (line, idx, s_name, fid) then begin (*list all entries*)
    print_holes := true;
    for dirptr := 1 to f.last_directory do begin
      read_directry_entry (f, dirptr, entry, err);
      exit if err <> lib_ok;
      if entry.valid then with entry do begin
        write (tty, name.name, '.', name.extension, '       ');
        f_count := f_count + 1;
        blk_count := blk_count + (length-1)div 640 + 1;
        if verbose then begin
          write(tty, (length-1) div 640 + 1:3, '         ');
          writeln(tty, dc_ext(access_date), '    ', dc_ext(write_date))
        end
        else if (f_count mod 4) = 0 then writeln(tty)  (*next line*)
      end
    end (*for dirptr*)
  end
  else repeat (*list only the selected entries*)
    dirptr := 0;
    while lookup_directory_entry ( f, s_name, dirptr) do begin
      read_directry_entry (f, dirptr, entry, err);
      if ((entry.valid) and (err = lib_ok)) then with entry do begin
        write (tty, name.name, '.', name.extension, '       ');
        f_count := f_count + 1;
        blk_count := blk_count + (length-1) div 640 + 1;
        if verbose then begin
          write(tty, (length-1) div 640 + 1:3, '         ');
          writeln(tty, dc_ext(access_date), '    ', dc_ext(write_date))
        end
        else if (f_count mod 4) = 0 then writeln(tty) (*next line*)
      end; 
      check_error (err, s_name)
    end  (*while *)
  until not get_name (line, idx, s_name, fid);
  if err <> lib_ok then check_error (err,f_name)
  else begin
    writeln(tty);
    blk_msg (blk_count, f_count);
    if print_holes then begin
      write (tty, ' +', EXTENT(f.lib_file)-number_directory_blocks-blk_count:3,
                     ' discarded block');
      if (EXTENT(f.lib_file)<>(number_directory_blocks-blk_count+1))
      then writeln(tty,'s.')
      else writeln(tty,'.')
    end
    else writeln (tty, '.')
  end
end; (*dir*)

PROCEDURE do_help;
(*Display information from help file*)
VAR help_f: TEXT;
begin
  reset (help_f, 'libhlp.msg', [retry]);
  if iostatus (help_f) <> io_ok then writeln(tty, 'Help file not found.')
  else if not lookup_command (line, idx, lib_commands, cmd)
  then while not eof (help_f) do begin
    read (help_f, line); writeln(tty, line); readln(help_f)
  end
  else repeat (*show only selected commands*)
    while (substr(line, 1, index(line,' ',1)-1) <> lib_commands[cmd].name)
    do begin readln(help_f); read (help_f, line) end;
    REPEAT
      writeln(tty, line); readln(help_f); read(help_f, line)
    UNTIL line[1] = '.';
    writeln(tty)
  until not lookup_command (line, idx, lib_commands, cmd);
  close (help_f)
end;  (*help*)

BEGIN
  OPEN(TTY); REWRITE(TTYOUTPUT);
  WRITELN(TTYOUTPUT, 'TYM-Pascal File Library, Version ', version());
  WRITELN(TTYOUTPUT);
  open_lib := FALSE; err := lib_notopen;
  REPEAT
    WRITE(TTYOUTPUT, '*'); BREAK(TTYOUTPUT); READLN(TTY); READ(TTY, line);
    idx := 1;
    IF NOT lookup_command (line, idx, lib_commands, cmd)
    THEN WRITELN(TTY, 'Invalid command -- try HELP.')
    ELSE case cmd of
      open_command: do_open;
      pack_command: do_packcmd;
      add_command: do_add;
      extract_command: do_extract;
      delete_command: do_delete;
      directory_command: do_directory;
      help_command: do_help;
      quit_command:
    END;
  UNTIL cmd = quit_command
END.
`6&ý