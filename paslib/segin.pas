$PAGE module segin
module segin
  options special(word);

$INCLUDE lib.typ
$INCLUDE blkio.inc
$INCLUDE dirio.inc
$INCLUDE dtime.inc[31024,320156]
$PAGE rd_word
public procedure rd_word ( var f : library;
                      entry_idx : dir_idx;        (* ptr to dir to read *)
                          var index : word;           (* start index for read *)
                        num_to_read : word;         (* words to read *)
                       var words_left : word;      (* words not read *)
                      var word_array : array[1..*] of word;
                         var err : errcode );        (* error code *)

(* This procedure fills the array with num_to_read words, unless end-of-
   segment is encountered *)

var
  entry : directory_entry;
  addr : address;  (* block address in library to read *)
  buffer_ptr : 1..words_per_blk+1;  (* pointer into segin buffer *)
  idx : 0..words_per_blk;  (* index for word_array selection *)

begin
  rd_dir (f, entry_idx, entry, err);
  if err = lib_ok then
    if ((index*5) > entry.length) then err := intr_fatal
    else begin
      addr := (index-1) div words_per_blk + entry.address ;
(*    make sure the segin buffer is current *)
      if addr <> f.segin_address then
   rd_segblk (f, addr, err);
      if err = lib_ok then begin
   entry.access_date := daytime;
 wr_dir (f, entry_idx, entry, err);
    buffer_ptr := (index-1) mod words_per_blk + 1;
        idx := 0;

   loop  (* to file the word array *)

  exit if ((idx >= num_to_read) or
               ((index+idx)*5 > entry.length) or
             (err <> lib_ok));

    idx := idx + 1;
       word_array[idx] := f.segin_buffer.words.data[buffer_ptr];
     buffer_ptr := buffer_ptr + 1;

          (* read a new buffer if it is read past *)
         if buffer_ptr > words_per_blk then begin
        buffer_ptr := 1;
      rd_segblk (f, f.segin_address + 1, err)
       end
       end; (* loop *)

     index := index + idx
      end  (* if err = lib_ok begin *)
    end;
    words_left := num_to_read - idx
end;  (* rd_words *)
$PAGE rd_char
public procedure rd_char ( var f : library;
                     entry_idx : dir_idx;        (* dir to read from *)
                            var index : char_idx;       (* start index *)
                         num_to_read : char_idx;     (* chars to read *)
                       var chars_left : char_idx;  (* chars not read *)
                      var char_array : array[1..*] of char;
                         var err : errcode );

(* This procedure fills the array with num_to_read characters, unless end-
of-segment is encountered *)

var
  entry : directory_entry;
  addr : address;  (* library address to read segin buffer *)
  buffer_ptr : 1..chars_per_blk+1;  (* pointer into segin buffer *)
  idx : 0..chars_per_blk;  (* index into char_array *)

begin
  rd_dir ( f, entry_idx, entry, err);
  if err = lib_ok then
    if (index > entry.length) then err := bad_diridx
    else begin
      addr := (index-1) div chars_per_blk + entry.address ;
(*    make sure the segin buffer is current *)
      if addr <> f.segin_address then
 rd_segblk (f, addr, err);
      if err = lib_ok then begin
   entry.access_date := daytime;
 wr_dir (f, entry_idx, entry, err);
    buffer_ptr := (index-1) mod chars_per_blk + 1;
        idx := 0;

   loop  (* to fill the char_array *)

  exit if ((idx >= num_to_read) or
               ((index + idx) > entry.length) or
             (err <> lib_ok));

    idx := idx + 1;
       char_array[idx] := f.segin_buffer.chars.data[buffer_ptr];
     buffer_ptr := buffer_ptr + 1;

          (* if buffer is read past, read the next one *)
    if buffer_ptr > chars_per_blk then begin
        buffer_ptr := 1;
      rd_segblk ( f, f.segin_address + 1, err)
      end
       end; (* loop *)

       index := index + idx
      end  (* if err = lib_ok begin *)
    end;
    chars_left := num_to_read - idx
end;  (* rd_chars *)
$PAGE rd_line
public procedure rd_line ( var f: library;
                           entry_idx : dir_idx; (* dir to read from *)
                           var index : char_idx;        (* start index *)
                        var line : string[*];
                         var err : errcode );

(* This procedure reads the character-indexed line into the given string *)

var
  entry : directory_entry;
  addr : address;  (* library address to read from *)
  buffer_ptr : 1..chars_per_blk+1;  (* pointer into segin buffer *)
  idx : 0..chars_per_blk;  (* index for line length *)

begin
  rd_dir (f, entry_idx, entry, err);
  if err = lib_ok then
    if index > entry.length then err := bad_diridx
    else begin
      addr := (index-1) div chars_per_blk + entry.address ;
(*    make sure the segin buffer is current *)
      if addr <> f.segin_address then
  rd_segblk (f, addr, err);
      if err = lib_ok then begin
   entry.access_date := daytime;
 wr_dir ( f, entry_idx, entry, err);
   buffer_ptr := (index-1) mod chars_per_blk + 1;
        idx := 0;

   loop  (* to build the line *)

       exit if ((line[idx] = chr(10)) or
              ((index + idx) >= entry.length) or
            (err <> lib_ok));

    idx := idx + 1;
       line := line || f.segin_buffer.chars.data[buffer_ptr];
        buffer_ptr := buffer_ptr + 1;

          (* if the buffer is read past, read the next *)
    if buffer_ptr > chars_per_blk then begin
        buffer_ptr := 1;
      rd_segblk (f, f.segin_address + 1, err)
       end
       end; (* loop *)

     index := idx
      end (* if err = lib_ok begin *)
    end
end.  (* rd_line *)
  
