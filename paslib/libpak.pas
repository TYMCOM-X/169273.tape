$PAGE module libpak
module libpak
  options special(word);

$include lib.typ
$include blkio.inc
$include dirio.inc
$include libasn.inc
$include dtime.inc[31024,320156]

external procedure rename ( string[40]; string[40]; boolean);
(* Rosen-built assembler routine to rename a file *)
$PAGE procedure pack
public procedure pack ( var f: library;
                 f_name: name_type;
                    var err: errcode );

(* This procedure packs the library into a temp file ###PAK.TMP, which is later renamed. *)

var
  templib : library;  (* temp library to write packed library *)
  dir_count, dir_index : dir_idx;  (* dir indices for packing process *)
  disp : address;  (* address displacement *)
  entry : directory_entry;
  wr_addr : address;  (* address to write pack segments *)
  new,ren_err : boolean;
  l_name,b_name,t_name: string[40];  (* renaming variables *)
  b_lib: name_type;                      (* the backup library name *)

begin
  writeln(tty,'Pack procedure started for ',filename(f.iofile));
  break;
  dir_index := 0;  (* new packed directory index *)
  b_lib.name := f_name.name;
  b_lib.ext := 'BAK';
  open_library (templib, b_lib, new, err);
  wr_addr := templib.segout_address;

  for dir_count := 1 to f.last_dir do begin  (* sequence through all unpacked segments *)
    rd_dir (f, dir_count, entry, err);

  exit if err <> lib_ok;

    if entry.in_use then begin  (* write it to new packed library *)

      for disp := 0 to ((entry.length-1)div chars_per_blk) do begin
        rd_segblk (f, entry.address + disp, err);
      exit if err <> lib_ok;
       templib.segout_buffer := f.segin_buffer;
      templib.segout_address := wr_addr + disp;
     wr_segblk ( templib, err)
      end;
      entry.address := wr_addr;
      entry.write_date := daytime;
      entry.access_date := daytime;
      dir_index := dir_index + 1;
      wr_dir ( templib, dir_index, entry, err );
      wr_addr := templib.segout_address + 1
    end;
  exit if err <> lib_ok
  end; (* for loop *)

  if err = lib_ok then begin
  (* now a packed library is in B_NAME, unpacked in L_NAME.
     Put L_NAME in a TMP file (T_NAME), and B_NAME into
     L_NAME to finish the packing process. *)
    close_library (f);
    close_library (templib);
    with f_name do begin
      l_name := name||'.'||ext||chr(15b);
      t_name := name||'.tmp'||chr(15b);
      b_name := name||'.bak'||chr(15b);
      rename ( l_name, t_name, ren_err);
      rename ( b_name, l_name , ren_err)
    end;
    if ren_err then err := lib_iofatal
    else begin
      open_library (f, f_name, new, err);
      f.last_dir := dir_count - 1       (* make sure this is right *)
      end
  end;
  if err = lib_ok then begin
    writeln(tty, 'Pack procedure successful.')
    end
  else writeln(tty, 'Pack procedure aborted.')
end. (* pack *)
  
