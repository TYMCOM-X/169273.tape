$TITLE pa1xrf -- Pascal Cross Reference Utilities, Pass 1
$LENGTH 43

(*   +--------------------------------------------------------------+
     |                                                              |
     |                P A S X R F   -   P a s s   1                 |
     |                - - - - - - - - - - - - - - -                 |
     |                                                              |
     +--------------------------------------------------------------+
     
     PURPOSE:  This  module  handles  the  generation  of  the  cross
        reference files.  The files it manipulates are:
     
             name.XRF -- the cross reference file
             name.XST -- the symbol table file
             name.XNM -- the name file
     
     ENTRY POINTS:
     
        xrf_init    initializes  the  cross  reference  module at the
                    start of compilation.
     
        xrf_write (record_type)
                    will  write  a  parameterless   record   of   the
                    specified type to the XRF file.
     
        xrf_use (symbol, source_id, record_type)
                    will  write a record of the the specified type to
                    the XRF file, with the id number of the symbol as
                    a  parameter.  File,  page, and line records will
                    be written, if necessary, to  bring  the  current
                    source  id of the file up to the specified source
                    id.
     
        xrf_with (expression, symbol, source_id, record_type)
                    will write  an  abbreviated  description  of  the
                    with-record  'expression',  followed  by a wfield
                    record for the specified field 'symbol'.
     
        xrf_block (block, source_id)
                    will write a block record to the XRF  file,  with
                    the  block  number  of  the  specified block as a
                    parameter.  File, page, and line records will  be
                    written,  if  necessary, to establish the correct
                    source id.
     
        xrf_freeze  sets an internal marker so that  subsequent  xref
                    records will be kept in an internal chain, rather
                    than written to the XRF file.  XrfFreeze's may be
                    stacked,   thus  establishing  multiple  internal
                    chains.
     
        xrf_unfreeze (record_type)
                    changes the record type of the first  use  record
                    in  the current frozen internal chain, and writes
                    the chain to the XRF file.
     
        xrf_scratch discards the current frozen internal chain.
     
        xrf_close   will close the cross reference files.
     
     ---------------------------------------------------------------- *)
$PAGE includes

$INCLUDE pascal.inc
$INCLUDE pasist.inc
$INCLUDE pasfil.inc
$INCLUDE paspt.typ
$INCLUDE pasif.typ
$INCLUDE pasxrf.typ
$INCLUDE paserr.inc
$INCLUDE tmpnam.inc
$PAGE declarations

var
    xrf_file: file of xrf_record;
(*  xst_file: file of xst_record;
    xnm_file: text;  *)

    current_file,
    current_page,
    current_line: int_type;
$PAGE frozen chain declarations

type
    xrf_chain = ^ xrf_chain_node;

    xrf_chain_node = record
        rec: xrf_record;
        next: xrf_chain
    end;

    frozen_chain = record
        first_rec, last_rec: xrf_chain;
        stacked_source: source_id
    end;


var
    freeze: frozen_chain;
    frozen: boolean;
$PAGE xrf_init

(*  XrfInit opens the cross reference file and initializes the source id
    to force a full source id write prior to the first data write.  *)

public procedure xrf_init;

begin
  sym_vl_number := vl_base; (* Initialize the symbol numberings. *)
  sym_nvl_number := nvl_base;

  current_file := -1;
  current_page := -1;
  current_line := -1;

  frozen := false;

  rewrite (xrf_file, tempname ('XRF'));
end (* xrf_init *);
$PAGE emit_xrf_record

(*  EmitXrfRecord writes an Xrf record either to the current frozen chain or
    to the XRF file.  *)

procedure emit_xrf_record ( record_type: xrf_class; var_lab: boolean; parm: xparm_val );

var xrf_rec: xrf_record;

begin
  with xrf_rec do begin
    code := record_type;
    var_lab_parm := var_lab;
    parameter := parm;
  end;

  if not frozen then begin
    xrf_file^ := xrf_rec;
    put (xrf_file);
  end

  else begin
    if freeze.first_rec = nil then begin
      new (freeze.first_rec);
      freeze.last_rec := freeze.first_rec;
    end
    else begin
      new (freeze.last_rec^.next);
      freeze.last_rec := freeze.last_rec^.next;
    end;
    freeze.last_rec^.rec := xrf_rec;
    freeze.last_rec^.next := nil;
  end;
end (* emit_xrf_record *);
$PAGE xrf_freeze

(*  XrfFreeze creates a new Freeze chain.  *)

public procedure xrf_freeze;

var new_freeze: frozen_chain;

begin
  assert (not frozen);
  with freeze do begin
    first_rec := nil;
    last_rec := nil;
    with stacked_source do begin
      file_no := current_file;
      page_no := current_page;
      line_no := current_line;
    end;
  end;
  frozen := true;
end (* xrf_freeze *);
$PAGE xrf_unfreeze

(*  XrfUnfreeze writes the current frozen chain out to the XRF file, setting
    the record type of the first use record to a specified type.  *)

public procedure xrf_unfreeze ( record_type: xrf_class );

var scan: xrf_chain;

begin
  with freeze do begin
    scan := first_rec;
    while (scan <> nil) andif
      not (scan^.rec.code in [value_ctxt, mod_ctxt, var_parm_ctxt, ref_ctxt]) do
        scan := scan^.next;
    if scan <> nil then
      scan^.rec.code := record_type;

    while first_rec <> nil do begin
      xrf_file^ := first_rec^.rec;
      put (xrf_file);
      scan := first_rec^.next;
      dispose (first_rec);
      first_rec := scan;
    end;
  end;
  frozen := false;
end (* xrf_unfreeze *);
$PAGE xrf_scratch

(*  XrfScratch discards the current frozen chain.  *)

public procedure xrf_scratch;

var next_rec: xrf_chain;

begin
  with freeze do begin
    while first_rec <> nil do begin
      next_rec := first_rec^.next;
      dispose (first_rec);
      first_rec := next_rec;
    end;
    with stacked_source do begin
      current_file := file_no;
      current_page := page_no;
      current_line := line_no;
    end;
  end;
  frozen := false;
end (* xrf_scratch *);
$PAGE xrf_write

(*  XrfWrite writes a parameterless record to the cross reference file.  *)

public procedure xrf_write ( record_type: xrf_class );

begin
  emit_xrf_record (record_type, false, 0);
end;
$PAGE set_source

(*  SetSource writes file, page, and line records to the cross reference file,
   if necessary, to establish 'source' as the current source id.  *)

procedure set_source ( source: source_id );

begin
  with source do begin
    if current_file <> file_no then begin
      emit_xrf_record (file_xrf, false, file_no);
      current_file := file_no;
    end;
    if current_page <> page_no then begin
      emit_xrf_record (page_xrf, false, page_no);
      current_page := page_no;
    end;
    if current_line <> line_no then begin
      emit_xrf_record (line_xrf, false, line_no);
      current_line := line_no;
    end;
  end (* with source *);
end (* set_source *);
$PAGE xrf_use

(*  XrfUse writes a record with a parameter and a source id to the cross
    reference file.  *)

public procedure xrf_use ( symbol: sym;  source: source_id;  record_type: xrf_class );

begin
  assert (symbol <> nil);
  set_source (source);
  with symbol^ do
    emit_xrf_record (record_type, (kind in [vars, for_inds, labels]), id_number);
end (* xrf_use *);
$PAGE xrf_block

(*  XrfBlock writes a block record with a source id to the cross reference
    file.  *)

public procedure xrf_block ( block: blk; source: source_id );

begin
  set_source (source);
  emit_xrf_record (block_xrf, false, block^.number);
end (* xrf_block *);
$PAGE xrf_with

(*  XrfWith writes a wfield record to the cross reference file.  *)

public procedure xrf_with ( x: expr; symbol: sym; source: source_id; record_type: xrf_class );

    (*  TraceExpr writes an abbreviated description of the record expression
        which 'sym' is a field of.  *)

    procedure trace_expr ( x: expr );
    begin
      with x^ do
        case opcode of
          ident_ref:
            xrf_use (id_sym, source, record_type);
          ptr_ref:
            xrf_use (base_ptr^.desc.base^.heap_class, source, record_type);
          buffer_ref:
            xrf_use (base_file^.desc.base^.file_class, source, record_type);
          field_ref:
            begin
              trace_expr (base_rec);
              xrf_use (field_sym, source, field_xrf);
            end;
          array_ref:
            trace_expr (base_array);
          func_call_op,
          agg_val:
            xrf_write (baserec_xrf);
          others:
            error (err_xrf_ref)
        end;
    end (* trace_expr *);

begin
  set_source (source);
  trace_expr (x);
  emit_xrf_record (wfield_xrf, false, symbol^.id_number);
end (* xrf_with *);
$PAGE xrf_close

(*  XrfClose simply closes the cross reference file.  *)

public procedure xrf_close;

begin
  close (xrf_file);
end.
  