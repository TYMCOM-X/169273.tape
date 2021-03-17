$title VAXDEB - VAX debug supports
module vaxdeb;
$PAGE includes

$system pascal.inc
$system pasist.inc
$system paspt.typ
$system pasif.typ
$system pasfil.inc
$system pascv.inc
$system passw.inc
$system vaxcg.typ
$system vaxgen.inc
$system vaxcgu.inc
$system vaxexp.inc
$system vaxopc.inc
$system vaxmac.inc
$system vaxrel.inc
$system vaxutl.inc
$system ptmcon.inc
$PAGE static declarations

static var

  trace_stmt: code;	(* Indicates code word emitted for forward reference
			   from trace block to last stmt in routine (for fixup) *)

  last_stmt: def;	(* definition node for last stmt block emitted in current routine *)
$PAGE deb_init

  (* DEB_INIT initializes the structures used in support of the debugger. *)

  public procedure deb_init;

  var fil: source_ptr;
      page: page_ptr;

  begin
      fil := file_list;	(* first in list of source files *)
      while fil <> nil do with fil^ do begin
	page := pages;	(* first page in file *)
	page^.following_page := nil;	(* def record for file block *)
	while page <> nil do with page^ do begin
	  left_page := nil;	(* def record for page block, if emitted *)
	  right_page := nil;	(* def record for last stmt block in page *)
	  page := next_page;
	end;
	fil := next_file;
      end;
  end;
$PAGE locate_file, locate_page

  (* LOCATE_FILE accepts a file number and returns a pointer to the
     src_id record for that file *)

  function locate_file (f_no: file_range): source_ptr;

  begin
    locate_file := file_list;	(* first file in list *)
    while locate_file^.file_no <> f_no do
      locate_file := locate_file^.next_file;
  end;

  (* LOCATE_PAGE accepts a pointer to a file record and a page number within
     the file, returning a pointer to the page_id record for that page *)

  function locate_page (fil: source_ptr; page: page_range): page_ptr;

  begin
    locate_page := fil^.pages;	(* first page in file *)
    while locate_page^.page_number <> page do
      locate_page := locate_page^.next_page;
  end;
$PAGE page_link_block

(* PAGE_LINK_BLOCK generates a page link block.  The first statement
   block of each page contains a self-relative pointer to the page link
   block for the page.  The page link block contains a self-relative
   pointer to the page block for the page.  A page link block is used
   to avoid longword displacements in the statement blocks.  Parameter
   PAGE_BLK_DEF is the definition record for the page block being linked
   to.  The function return value is a definition record for the start
   of the page link block (i.e., the address of the branch around the
   link block).  *)

function page_link_block ( page_blk_def: def ): def;

var
  end_of_page_link: def;

begin

  (* Create and define a label marking the beginning of the page
     link block.  *)

  page_link_block := make_def ( code_def );
  mark_def ( code_area, page_link_block );

  (* Generate a branch around the page link block.  *)

  end_of_page_link := make_def ( code_def );
  gen_branch ( brb, end_of_page_link );

  (* Generate the page link block id byte.  *)

  gen_byte ( code_area, -3 );

  (* Generate the self-relative (longword) displacement from the
     start of the branch instruction to the page block.  
     Define the label at the end of the page link block.  *)

  gen_displacement ( page_link_block, page_blk_def, 4 * byte_size );
  mark_def ( code_area, end_of_page_link );

end  (* proc page_link_block *) ;
$PAGE stmt_block

  (* STMT_BLOCK determines if a statement block must be emitted for
     the current source statement. Assumes it is the first statement
     on the line.	*)

  public procedure stmt_block (stmt: source_id; stmt_type: symbols)  options special(coercions);

  var
    fil: source_ptr;	(* file containing stmt *)
    page: page_ptr;	(* page containing stmt *)
    d: def;
    kind: int_type;
    link_block_def: def;

  begin
    case stmt_type of
	simple_stmt:	kind := 0;
	if_stmt:	kind := 1;
	for_stmt:	kind := 2;
	loopsy:		kind := 3;
	while_stmt:	kind := 4;
	case_stmt:	kind := 5;
	with_stmt:	kind := 6;
	goto_stmt:	kind := 7;
	io_stmt:	kind := 8;
	return_stmt:	kind := 9;
	stop_stmt:	kind := 10;
	exit_clause:	kind := 11;
	untilsy:	kind := 12;
	repeatsy:	kind := 13;
	endsy:		kind := 14;
	func_qualifier: kind := 15;
	others:		return	(* ignore *)
    end;
    fil := locate_file (stmt.file_no);	(* get file record *)
    page := locate_page (fil, stmt.page_no);	(* and page *)
    if fil^.pages^.following_page = nil then 	(* no file block yet *)
      fil^.pages^.following_page := ptr (ord (make_def (code_def)));	(* for fwd refs *)
    if page^.left_page = nil then	(* no page block yet *)
      page^.left_page := ptr (ord (make_def (code_def)));
    if page^.right_page = nil	(* if 1st stmt of page, gen *)
						(* page link block *)
      then link_block_def := page_link_block ( ptr ( ord ( page^.left_page ) ) );
    gen1 (jsb, rt_addr (rt_stmt));
    d := make_def (code_def);
    mark_def (code_area, d);	(* mark location of stmt block *)
    last_stmt := d;	(* Track until last stmt block in routine *)
    if page^.right_page = nil	(* first stmt emitted, backlink to page link block *)
      then gen_displacement (d, link_block_def, 2 * byte_size) (* disp to page link block *)
      else gen_displacement (d, ptr (ord (page^.right_page)), 2 * byte_size); (* disp to prev stmt block *)
    page^.right_page := ptr (ord (d));	(* backlink to this stmt next time *)
    gen_byte (code_area, kind);
    gen_word (code_area, stmt.line_no);
  end (* stmt_block *);
$PAGE trace_block
(* TRACE BLOCK generates the trace control block for a procedure or main program
   given the "name" of the block.  A definition for the start of the block is
   generated and returned. *)

public function trace_block: def;
  var
    name: nam;

begin
  trace_block := make_def (code_def);
  mark_def (code_area, trace_block);
  gen_displacement (trace_block, trace_block, 2 * byte_size);	(* Actual last stmt def supllied later *)
  trace_stmt := code_area.last;	(* for patch of forward reference to last stmt *)
  case cur_block^.kind of
    program_blk, module_blk, data_blk:
      name := cur_block^.id;
    subr_blk:
      name := cur_block^.subr_sym^.name
  end;
  gen_string (code_area, name^.text);

  (* Since no stmt blocks have been as yet emitted for this routine,
     NIL the "last_stmt" def pointer. If on the call to BLK_END it is
     not NIL then the displacement word indicated by TRACE_STMT is patched
     to point to the last stmt block emitted. Otherwise the displacement
     remains to the trace block itself and a zero results.	*)

  last_stmt := nil;
end;
$PAGE blk_end
(* BLK END resolves the forward reference from the trace block to the last
   stmt block in the routine (if any).	*)

public procedure blk_end;

begin
  if last_stmt <> nil then
    trace_stmt^.to_def := last_stmt;
end;
$PAGE fp_blocks

  (* FP_BLOCKS emits page and file blocks for the debugger *)

  public procedure fp_blocks  options special(coercions);

  var 
    fil: source_ptr;	(* current file *)
    page: page_ptr;	(* current page in fil *)
    def_page, def_file: def;	(* for backlinking through blocks *)
    start_ix, len: int_type;

  begin
    fil := file_list;	(* first file *)
    def_file := nil;	(* no previous file before first *)
    while fil <> nil do with fil^ do begin
      if pages^.following_page <> nil then begin	(* stmt blocks emitted for file *)
	def_page := ptr (ord (pages^.following_page));	(* first page links to file block *)
	page := pages;	(* first page in file *)
	while page <> nil do with page^ do begin
	  if left_page <> nil then begin	(* stmts in page *)
	    mark_def (code_area, ptr (ord (left_page)));	(* define addr of page block *)
	    gen_displacement (ptr (ord (left_page)), def_page, 2 * byte_size);
						(* disp to prev page *)
	    gen_byte (code_area, -1);
	    gen_displacement (ptr (ord (left_page)), ptr (ord (right_page)), 4 * byte_size);
						(* disp to last stmt block *)
	    gen_word (code_area, page_number);

	    (* emit only initial alphanumeric portion of subtitle *)

	    len := verify (subtitle, ['A'..'Z','0'..'9','_','$'], length (subtitle) + 1) - 1;
	    if len > 0
	      then gen_string (code_area, substr (subtitle, 1, len))
	      else gen_word (code_area, 0);
	    def_page := ptr (ord (left_page));	(* backlink to this block next *)
	  end;
	  page := next_page;
	end (* while *);
	mark_def (code_area, ptr (ord (pages^.following_page)));	(* define addr of file block *)
	if def_file = nil then	(* no previous file block *)
	  gen_word (code_area, 0)
	else gen_displacement (ptr (ord (pages^.following_page)), def_file, 2 * byte_size);
						(* disp to prev file block *)
	gen_byte (code_area, -2);
	gen_displacement (ptr (ord (pages^.following_page)), def_page, 2 * byte_size);
						(* disp to last page block in file *)
	gen_word (code_area, file_no);
	start_ix := search (file_name, [':']) + 1;	(* strip device *)
	len := search (substr (file_name, start_ix), ['.','[']) - 1;
	gen_string (code_area, substr (file_name, start_ix, len));	(* generate filename *)
	def_file := ptr (ord (pages^.following_page));
      end;
      fil := next_file;
    end (* while *);
  end (* fp_blocks *).
    