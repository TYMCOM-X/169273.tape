$PAGE module blkio
module blkio
  options special(word);

$include lib.typ

external function checksum (buffer : library_rec): word;
(* an external assembler checksumming routine *)
$PAGE utilities

function chkerr ( status : io_status ): errcode;

(* this function returns the appropriate errcode given an iostatus *)

begin
  case status of

io_ok:         chkerr := lib_ok;

io_intr,
io_outf,
io_inpf:       chkerr := lib_iofatal;

io_rewr:      chkerr := lib_notopen;

io_opnf:      chkerr := lib_openfail;

io_illc:     chkerr := intr_fatal;

others:                chkerr := lib_iofatal

  end;  (* case *)
end;  (* function chkerr *)
$PAGE rd_segblk
public procedure rd_segblk ( var f: library;
                            addr: seg_address; (* physical address *)
                             var err: errcode );

(* This procedure reads the physical addressed block into the
   segin buffer and sets the segin address pointer.  *)

begin
  if addr <= num_dir_blks then err := intr_fatal (* ADDR is not a segment block! *)

  else begin
    readrn ( f.iofile, addr, f.segin_buffer );
    err := chkerr (iostatus (f.iofile) );

    if (( err = lib_ok ) and
   ( f.segin_buffer.words.checksum = checksum ( f.segin_buffer )))
     then f.segin_address := addr
     else err := lib_iofatal
  end

end; (* rd_segblk *)
$PAGE wr_segblk
public procedure wr_segblk ( var f: library;
                             var err: errcode );

(* This procedure flushes the segout buffer to the segout address *)

begin
  f.segout_buffer.words.checksum := checksum ( f.segout_buffer );

  writern ( f.iofile, f.segout_address, f.segout_buffer );
  err := chkerr ( iostatus (f.iofile) )
end; (* wr_segblk *)
$PAGE rddirblk
public procedure rddirblk ( var f: library;
                             addr: dir_address; (* physical address *)
                             var err: errcode );

(* This procedure reads the physical addressed directory block into
   the directory buffer and sets the dir address pointer *)

begin
  if addr > num_dir_blks then err := intr_fatal

  else begin
    readrn (f.iofile, addr, f.dir_buffer );
    err := chkerr ( iostatus (f.iofile) );

    if err = lib_ok then f.dir_buf_loc := addr
  end
end; (* rddirblk *)
$PAGE wrdirblk
public procedure wrdirblk ( var f: library;
                         var err: errcode );

(* This procedure flushes the directory buffer to the dir address *)

begin
  writern ( f.iofile, f.dir_buf_loc, f.dir_buffer );

  err := chkerr ( iostatus (f.iofile) )
end. (* wrdirblk *)
