$PAGE library management facility
program lib
  options special(word);
$include lib.typ
$include blkio.inc
$include dirio.inc
$include libasn.inc
$include segasn.inc
$include segin.inc
$include segout.inc
$include libpak.inc
$include cmdutl.typ[31024,320156]
$include lookup.typ[31024,320156]
$include filutl.inc[31024,320156]
$include dtime.inc[31024,320156]
$include query.inc[31024,320156]
$PAGE program types
type
  libcmds = (opencmd, dir, delete, add, extract, packcmd, help, quit);

  libcmd_list = array[libcmds] of cmdlist;

var
  libcmd : libcmd_list :=
    (     ('OPEN',        1),
   ('DIRECTORY',   3),
   ('DELETE',      3),
   ('ADD',         1),
   ('EXTRACT',     1),
   ('PACK',        4),
   ('HELP',        4),
   ('QUIT',        1)      );

  external function lookuplibcmds
    ( line : cmdline;
      var idx : cmdlineidx;
      var list : libcmd_list;
      max_libcmds : libcmds;
      var nameidx : libcmds ): boolean;
$PAGE get_name function
function get_name ( line : cmdline; (* line to be searched *)
                 var idx : cmdlineidx;       (* search pointer into line *)
                    var name : name_type;  (* name found on line *)
               var fid : file_id   (* file ID found *)
                   ): boolean;     (* TRUE on a good name find *)

(* This function searches the line from the given idx for the next file
   name. If one is found the idx is updated to point after the name, and
   if one is not found the function returns a FALSE value.  *)

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

  (* first, skip whitespace and delimiters *)
  while ((idx <= length(line)) and (line[idx] in valid_delimiters)) do idx := idx + 1;

  get_name := pr_file_id (line, idx, fid);  (* parse the file name *)

  if get_name then begin  (* parse the name, extension into a name_type *)
    colonidx := index(fid, ':') + 1;
    periodidx := index(fid,'.');
    if periodidx = 0 then begin
      periodidx := length(fid) + 1;
      name.name := uppercase (substr(fid, colonidx, periodidx-colonidx));
      name.ext := ''
      end
    else begin
      name.name := uppercase (substr(fid, colonidx, periodidx-colonidx));
      name.ext := uppercase (substr(fid,periodidx+1))
    end
  end
end;  (* get_name *)
$PAGE filename printer and blk_msg
procedure print_name (f: name_type);

(* This procedure prints filenames in a suitable format for the TTY *)

var i: 1..6;

begin
  for i := 1 to 6 do
    if f.name[i] <> ' ' then
      write(tty, f.name[i] );

  write(tty,'.');

  for i := 1 to 3 do
    write(tty, f.ext[i] )
end;  (* print_name *)



procedure blk_msg (blks: seg_address; files: seg_address);

(* This procedure prints the  'X block(s) in Y file(s)' message. *)

begin

  write(tty,blks: 4, ' block');
  if blks <> 1 then write(tty, 's');
  write(tty, ' in ');

  write(tty,files: 3, ' file');
  if files <> 1 then write(tty, 's')
end;  (* blk_msg *)
$PAGE chkerr routine
procedure chkerr (var err : errcode;  (* the error condition to check *)
              name : name_type);    (* file involved with error *)

(* This procedure checks the given error condition and prints an appropriate
   error message    *)

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

end;(* case *)

err := lib_ok

end; (* chkerr *)
$PAGE main program vars
var
  f: library;                     (* the library *)
  real_f: file of word;      (* external file record *)
  real_f_array: array[1..words_per_blk] of word;    (* input file block *)
  f_name, s_name: name_type;            (* file name var *)
  line: cmdline;           (* a command line *)
  idx: cmdlineidx;                (* idx to cmdline *)
  fid: file_id;                           (* filutl file ID *)
  cmd: libcmds;                   (* parsed command *)
  open_lib: boolean;              (* open library flag *)
  dirptr: dir_idx;             (* utility pointer into directory *)
  entry: directory_entry; (* a directory entry *)
  i,j: word;                   (* utility vars (loops, etc.) *)
  err: errcode;                       (* error code *)
  f_count: dir_idx;           (* utility counters *)
  blk_count: 0..max_blocks;
  num_to_write,
  num_to_read: word;              (* i/o controls *)
  words_left: word;
  word_var : array[1..words_per_blk] of word;  (* flex_array parameter *)
  new : boolean;            (* new/old library flag *)
  help_f: text;                     (* help file *)
  verbose: boolean;            (* for directory command *)
  confirm: boolean;                (* for delete command *)
  print_holes: boolean;               (* for dir command *)
$PAGE mainline program
begin (* main program *)

  (* initialize *)
  open(tty); rewrite(tty);

  writeln(tty, 'File Stash Facility, Version 2.0');
  writeln(tty);

  open_lib := false;
  err := lib_notopen;

  loop  (* the command loop *)

    writeln(tty);
    write(tty, '>'); break;  (* prompt the user *)
    readln(tty);
    read(tty, line);

    idx := 1;

    if not lookuplibcmds (line, idx, libcmd, maximum(libcmds), cmd) then
      writeln(tty, 'Invalid command')

    else begin
      case cmd of

$PAGE open command
(* The OPEN command opens a new/old library file using the OPEN_LIBRARY
   routine. *)

OPENCMD: begin
 err := lib_ok;
        if open_lib then close_library (f);
   if not get_name (line, idx, f_name, fid) then err := lib_openfail
     else begin
      if f_name.ext = '   ' then f_name.ext := 'LIB'; (* default extension *)
       open_library (f, f_name, new, err)
  end;

        if err = lib_ok then begin
      open_lib := true;

   if new then write (tty, 'New ')
              else write (tty, 'Old ');
      writeln(tty, 'library file ', filename(f.iofile), ' opened.');
        writeln(tty)
        end

 else begin
      open_lib := false;
    chkerr (err, f_name)
        end
  end;  (* opencmd *)

$PAGE pack command
(* The PACK command uses the PACK routine to pack the library file *)

PACKCMD: begin

   if err = lib_ok then begin
      pack (f, f_name, err);   (* pack the library *)
       chkerr(err, f_name);
          writeln(tty)
        end

 else chkerr(err, f_name)
  end;  (* packcmd *)

$PAGE add command
(* The ADD command adds external files to the currently open library
   using the WR_WORD routine. All disk transfers are done in one block
   chunks.  *)

ADD: begin
      f_count := 0;
 blk_count := 0;

     writeln(tty, 'Files added:');
 writeln(tty); break;

        while (get_name(line, idx, s_name, fid) and (err = lib_ok)) do begin

          reset (real_f, fid, [RETRY]); (* open the external file *)

        exit if iostatus (real_f) <> io_ok do err := file_notfind;

    opn_segment (f, s_name, err);  (* open a new library segment *)
       rd_dir (f,f.open_dir_ptr, entry, err);

      num_to_write := words_per_blk;  (* data transfer block (+ 1 checksum = 128 words ) *)
         i := 0;  (* number of words in the current block *)

         loop
            i := i + 1;
         exit if (eof(real_f) orif (err <> lib_ok)) do begin
         (* output a partially filled block *)
         wr_dir (f, f.open_dir_ptr, entry, err); (* enter what we have *)
              num_to_write := i - 1;
                wr_word (f, num_to_write, real_f_array, err)
        end;   (* exit if - do *) 

        (* we keep two arrays, because unpacked are not compatible with packed arrays *)
      f.segout_buffer.words.data[i] := real_f^;
     real_f_array[i] := real_f^;
           get (real_f);
         if i = words_per_blk then begin  (* write only full blocks *)
           wr_segblk (f, err);
           entry.length := entry.length + chars_per_blk;
         f.segout_address := f.segout_address + 1;
             i := 0
      end
         end;  (* loop *)

    cls_segment (f);
      close (real_f);

     if err = lib_ok then begin
      rd_dir (f, f.open_dir_ptr, entry, err);
       f_count := f_count + 1;
       blk_count := blk_count + (entry.length-1) div 640 + 1;
        print_name (s_name);
          writeln(tty); break
          end
        end; (* big while *)

        chkerr(err, s_name );

       writeln(tty);
 blk_msg (blk_count, f_count);
 writeln(tty, ' added.')
  end;  (* add *)
$PAGE extract command
(* The EXTRACT command copies a library segment to an external file of the
   same name using the RD_WORDS routine *)

EXTRACT: begin
   num_to_read := words_per_blk;
 f_count := 0;
 blk_count := 0;
       i := 1;
       words_left := 0;

    writeln(tty, 'Files extracted:');
     writeln(tty); break;

        while get_name (line, idx, s_name, fid) do begin
        dirptr := 0;
          while find_dir (f, s_name, dirptr) do begin
     i := 1;
       rd_dir(f, dirptr, entry, err);
        if ((err = lib_ok) and (entry.in_use)) then begin
       with entry do
           rewrite(real_f, file_seg.name||'.'||file_seg.ext, [RETRY]);

       if iostatus (real_f) <> io_ok then begin
                err := seg_notopen;
           chkerr (err, entry.file_seg)
        end
           else begin
              repeat  (* read segment blocks and write to a file *)
           rd_word (f, dirptr, i, num_to_read, words_left, word_var, err);
               if err <> lib_ok then chkerr(err, s_name)
             else
                    for j:= 1 to (num_to_read-words_left) do begin
                  real_f^ := word_var[j];
               put (real_f)
                end  (* for *)
            until words_left <> 0;

              close (real_f);
               f_count := f_count + 1;
               blk_count := blk_count + (entry.length-1) div 640 + 1;
                with entry do
           print_name (file_seg);
                writeln(tty); break
       end  (* else begin *)
       end;  (* if *)
        chkerr (err, s_name)
        end; (* while find_dir *)
   end;  (* big while *)
 chkerr(err, s_name );

       writeln(tty);
 blk_msg (blk_count, f_count);
 writeln (tty, ' extracted.')
  end; (* extract *)

$PAGE delete command
(* The DELETE command deletes library segments (no packing here) by
   simply changing a segment's IN_USE flag in the directory entry *)

DELETE: begin
        confirm := query ('Confirm');   (* to confirm deletions *)
    f_count := 0;
 blk_count := 0;

     writeln(tty, 'Files deleted:');
       writeln(tty);

       while get_name (line, idx, s_name, fid) do begin
        dirptr := 0;
          while find_dir (f, s_name, dirptr) do begin
     rd_dir (f, dirptr, entry, err);
       if ((entry.in_use) and (err = lib_ok)) then begin
       with entry do
           print_name(file_seg);

             if (not confirm) orif (query (' -- OK')) then begin
             entry.in_use := false;
                wr_dir (f, dirptr, entry, err);
               f_count := f_count + 1;
               blk_count := blk_count + (entry.length-1) div 640 + 1
         end;
        writeln(tty)
        end; (* if *)
         chkerr (err, s_name );
      end (* while find_dir *)

  end;(* while get_name *)
      writeln(tty);
 blk_msg (blk_count, f_count);
 writeln( tty, ' deleted.')
  end; (* delete *)

$PAGE dir command
(* The DIR command uses the RD_DIR and FIND_DIR routines to read and find
   the specified directory entries and print information about them. *)

DIR: begin
        if ((idx < length(line)) and
      (line[idx] = '/') and
         (uppercase(line[idx+1]) = 'A')) then begin
                verbose := true;
              idx := idx + 2
                end
   else verbose := false;

      print_holes := false;
 f_count := 0;
 blk_count := 0;
       writeln(tty,'      Directory for library file ',filename(f.iofile));

        if verbose then
         writeln(tty,'   Name        Blk Size         Access Date           Write Date');
    writeln(tty);

       if not get_name (line, idx, s_name, fid) then begin (* list all entries *)
      print_holes := true;
          for dirptr := 1 to f.last_dir do begin
          rd_dir (f, dirptr, entry, err);

   exit if err <> lib_ok;

        if entry.in_use then with entry do begin
          write (tty, file_seg.name, '.', file_seg.ext, '       ');
             f_count := f_count + 1;
               blk_count := blk_count + (length-1)div 640 + 1;
               if verbose then begin
           write(tty, (length-1) div 640 + 1:3, '         ');
            writeln(tty, dc_ext(access_date), '    ', dc_ext(write_date))
                 end
         else if (f_count mod 4) = 0 then writeln(tty)  (* next line *)

          end
         end (* for dirptr *)
        end

 else repeat (* list only the selected entries *)
          dirptr := 0;
          while find_dir ( f, s_name, dirptr) do begin
            rd_dir (f, dirptr, entry, err);
       if ((entry.in_use) and (err = lib_ok)) then with entry do begin
         write (tty, file_seg.name, '.', file_seg.ext, '       ');
             f_count := f_count + 1;
               blk_count := blk_count + (length-1) div 640 + 1;
              if verbose then begin
           write(tty, (length-1) div 640 + 1:3, '         ');
            writeln(tty, dc_ext(access_date), '    ', dc_ext(write_date))
                 end
         else if (f_count mod 4) = 0 then writeln(tty) (* next line *)

             end; 
         chkerr (err, s_name)
        end  (* while  *)
   until not get_name (line, idx, s_name, fid);

        if err <> lib_ok then chkerr (err,f_name)
     else begin
      writeln(tty);
         blk_msg (blk_count, f_count);
         if print_holes then begin
       write (tty, ' +', extent(f.iofile)-num_dir_blks-blk_count: 3,
                     ' discarded block');
              if (extent(f.iofile) <> (num_dir_blks-blk_count+1)) then writeln(tty,'s.')
            else writeln(tty,'.')
         end
         else writeln (tty, '.')
     end
  end; (* dir *)
$PAGE help command
HELP: begin
      reset (help_f, 'libhlp.msg[31024,332216]', [retry]);

   if iostatus (help_f) <> io_ok then writeln(tty, 'Help file not found.')

     else
    if not lookuplibcmds (line, idx, libcmd, maximum(libcmds), cmd) then
            while not eof (help_f) do begin
         read (help_f, line);
          writeln(tty, line);
           readln(help_f)
      end
         else repeat (* show only selected commands *)

         while (substr(line, 2, index(line,' ',1)-1) <> libcmd[cmd].name) do begin
         readln(help_f);
               read (help_f, line)
       end;

        repeat
            writeln(tty, line);
           readln(help_f);
               read(help_f, line)
        until line[1] = '-';

        writeln(tty)

      until not lookuplibcmds (line, idx, libcmd, maximum(libcmds), cmd);

       close (help_f)
  end;  (* help *)

$PAGE end command loop
QUIT:  ;

OTHERS:  

    end  (* case *)
  end;

exit if cmd = QUIT

  end (* command loop *)

end.  (* program *)
   