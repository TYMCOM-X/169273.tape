$WIDTH=100
$LENGTH=55
$TITLE PASDMP.PAS, last modified 1/3/84, zw
MODULE pasdmp OPTIONS SPECIAL(WORD);
(*TYM-Pascal compiler -- pass 1 and 2 dump routines*)

$PAGE system modules

$SYSTEM PASCAL.INC
$SYSTEM PASFIL.INC
$SYSTEM PASIST.INC
$SYSTEM PTMCON.INC
$SYSTEM PASPT.TYP
$SYSTEM PASIF.TYP
$SYSTEM PASOPT.TYP
$SYSTEM PASCV.INC
$SYSTEM UTLSW.INC
$SYSTEM PASIFU.INC
$SYSTEM UTLSET.INC
$SYSTEM PASBNF.NAM

$PAGE dmp_open

PUBLIC PROCEDURE dmp_open(separator: BOOLEAN);
(*open the dump file if it is not currently open, and start a new
  page on the dump file if the file is open and the 'separator'
  parameter is true.  A new page is simply a line of dashes if TTY has been
  specified as a dump switch.*)
PROCEDURE tty_page(VAR fb: file_block);
BEGIN
  fio_skip(fb); fio_skip(fb);
  fio_line(fb, '----------------------------------------');
  fio_skip(fb); fio_skip(fb)
END;
BEGIN
  IF df_status = unopened THEN BEGIN
    fio_open(dumpfb, main_file || ' DSK:[,].DMP');
    IF sw(prog_options.dump_switches, 'TTY') THEN BEGIN
      dumpfb.width := 80; dumpfb.new_page := tty_page
    END
    ELSE BEGIN
      dumpfb.width := 100; dumpfb.plength := 44
    END;
  END
  ELSE BEGIN
    IF df_status = prev_opened THEN fio_reopen(dumpfb);
    IF sw(prog_options.dump_switches, 'TTY')
    THEN dumpfb.new_page := tty_page
    ELSE dumpfb.new_page := fio_eject;
    dumpfb.page_header := fio_nop;
    IF separator THEN fio_page(dumpfb)
  END;
  df_status := now_open
END;

$PAGE cv_ptr
  
 
PUBLIC FUNCTION cv_ptr(ptr_val: INTEGER): PACKED ARRAY [1..6] OF CHAR;
(*return a string representing an address in octal*)
BEGIN
  PUTSTRING(cv_ptr, ptr_val:6:o)
END;

$PAGE sym_text

PUBLIC FUNCTION typ_text(t: typ): line_string; FORWARD;

PUBLIC FUNCTION sym_text(s: sym): line_string;
(*return the name of a symbol, if the symbol has a name.  If the
  symbol is a heap class, then 'Class of <type>' will be returned.
  Otherwise, the symbol's address will be returned, with an '@'.*)
BEGIN
  IF s = NIL THEN sym_text := '*NIL*'
  ELSE WITH s^ DO IF name <> NIL THEN sym_text := name^.text
  ELSE IF dcl_class = dynamic_sc THEN
    sym_text := 'Heap class of ' || typ_text(type_desc)
  ELSE IF dcl_class = fileblk_sc THEN
    sym_text := 'File class of ' || typ_text(type_desc)
  ELSE IF kind IN [vars, labels] THEN
    sym_text := 'V.' || cv_int(id_number)
  ELSE
    sym_text := 'C.' || cv_int(id_number)
END;

$PAGE typ_text

FUNCTION typ_text(t: typ): line_string;
(*called with a pointer to a type node, and returns the type name,
  if it has a name; 'TYPE OF <symbol>', if it is an unnamed scalar
  type; and the octal pointer value if all else fails.*)
BEGIN
  IF t = NIL THEN typ_text := '*NIL*'
  ELSE WITH t^ DO BEGIN
    IF type_id <> NIL THEN BEGIN
      typ_text := sym_text(type_id);
      IF type_id^.kind <> types THEN typ_text := 'Type of ' || typ_text
    END
    ELSE typ_text := '@' || cv_ptr(ORD(t))
  END
END;

$PAGE block_id

PUBLIC FUNCTION block_id(block: blk): line_string;
(*return the block number, level, type, and name of a block node*)
TYPE blk_names = ARRAY[block_kind] OF STRING[11];
CONST block_kind: blk_names =
  ('ROOT', 'PROGRAM', 'MODULE', 'SUBROUTINE', 'DATA MODULE',
   'CLASS', 'EXTERNAL'  );
BEGIN
  IF block = NIL THEN block_id := '*NIL*'
  ELSE WITH block^ DO BEGIN
    block_id := cv_int(number) || ' AT LEVEL ' || cv_int(level) ||
      ':  ' || block_kind [kind] || ' ';
    CASE kind OF
      program_blk, module_blk, data_blk: BEGIN
        IF id <> NIL THEN block_id := block_id || id^.text
      END;
      subr_blk: block_id := block_id || sym_text(subr_sym);
      class_blk: block_id := block_id || typ_text(class_type);
      others: (*no action*)
    END
  END
END;

$PAGE prt_title

VAR saved_title: line_string;

PROCEDURE title_page(VAR fb: file_block);
(*called with a string.  It prints the string, centered,
  underlined, and followed by a blank line.  In addition, if the dump
  mode is not "TTY", then the new-page routine is set to print the
  title and a page number at the start of each new page.*)
VAR col: line_index;
BEGIN
  fio_write(fb, saved_title);
  col := fb.width - 5 - width(fb.pageno);
  IF fb.column >= col THEN fio_skip(fb) ELSE fio_tab(fb, col+1);
  fio_line(fb, 'PAGE ' || cv_int(fb.pageno)); fio_skip(fb)
END;

PUBLIC PROCEDURE prt_title(ttl: line_string);
VAR dashes: line_string; i: line_index;
BEGIN
  dmp_open(TRUE);
  i := SEARCH(ttl, ['$']);
  IF i = 0 THEN saved_title := ttl
  ELSE saved_title := 
    SUBSTR(ttl, 1, i - 1) || block_id(cur_block) || SUBSTR(ttl, i + 1);
  i := MAX(0, (dumpfb.width - LENGTH(saved_title)) DIV 2) + 1;
  dumpfb.c_column := 0;
  fio_tab(dumpfb, i); fio_line(dumpfb, saved_title); fio_tab(dumpfb, i);
  dumpfb.page_header := title_page; dumpfb.pageno := 1; dashes := saved_title;
  FOR i := 1 TO LENGTH(dashes) DO IF dashes[i] <> ' ' THEN dashes[i] := '-';
  fio_line(dumpfb, dashes); fio_skip(dumpfb)
END;

$PAGE dmp_close

PUBLIC PROCEDURE dmp_close;
(*close the dump file, if it is open, and change its status
  from "now open" to "previously opened".*)
BEGIN
  IF df_status = now_open THEN BEGIN
    fio_close(dumpfb); df_status := prev_opened
  END
END.
  