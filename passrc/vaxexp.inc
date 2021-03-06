$PAGE VAXEXP.INC, last modified 1/13/84, zw
$IFNOT vaxexpinc
TYPE
    inst_opt_list = ( commutative, (* operands may be exchanged *)
    mem_form, (* opcM may be generated *)
    double ); (* must apply inst twice to process 2 word operands *)
    inst_opt_set = set OF inst_opt_list;


EXTERNAL VAR
regdesc: reg_descriptor;
max_reg: registers;
reg_use_by_block: ^reg_set_vector;

EXTERNAL PROCEDURE exp_init;
EXTERNAL FUNCTION precision ( exp: expr ): bit_range;
EXTERNAL FUNCTION expr_size ( expr ): bit_range;
EXTERNAL FUNCTION load (exp: expr): registers;
EXTERNAL FUNCTION load_addr
  (maddr: addr_desc; align: data_alignment ) : registers;
EXTERNAL FUNCTION copy_load ( expr ): registers;
EXTERNAL FUNCTION move_address ( addr_desc ): registers;
EXTERNAL FUNCTION array_addr ( expr ): addr_desc;
EXTERNAL FUNCTION alignment (exp: expr): data_alignment;
EXTERNAL FUNCTION do_fetch ( exp: expr; targ_addr: addr_desc ): addr_desc;
EXTERNAL FUNCTION fetch (exp: expr; targ_addr: addr_desc ): addr_desc;
EXTERNAL FUNCTION fetch_fullword (exp: expr): addr_desc;
EXTERNAL FUNCTION argument ( arg: expr ): addr_desc;
EXTERNAL PROCEDURE reg_init;
EXTERNAL FUNCTION get_reg (p: bit_range): registers;
EXTERNAL FUNCTION get_vax_reg ( vax_type ): registers;
EXTERNAL PROCEDURE tag_reg (reg1, reg2: registers);
EXTERNAL FUNCTION is_register (maddr: addr_desc): boolean;
EXTERNAL FUNCTION is_immediate (addr_desc): boolean;
EXTERNAL FUNCTION is_disp_mode (addr_desc): boolean;
EXTERNAL FUNCTION is_symbol_addr (addr_desc): boolean;
EXTERNAL FUNCTION indexable (addr_desc): boolean;
EXTERNAL PROCEDURE free (maddr: addr_desc);
EXTERNAL FUNCTION iconstp (exp: expr; VAR val: int_type): boolean;
EXTERNAL FUNCTION aconstp (maddr: addr_desc; VAR val: int_type): boolean;
EXTERNAL FUNCTION do_binary_op
  (opc_range; expr; expr; inst_opt_set; addr_desc): addr_desc;
EXTERNAL PROCEDURE with_start (tuple);
EXTERNAL FUNCTION upper_bound (expr; addr_desc): addr_desc;
EXTERNAL FUNCTION dynamic_flex (expr): boolean;
EXTERNAL FUNCTION unpacked_vax_type ( typ ): vax_type;
EXTERNAL FUNCTION expr_vax_type (expr): vax_type;
EXTERNAL FUNCTION vax_type_size ( vax_type ): align_range;
EXTERNAL FUNCTION duplicate_addr ( addr_desc ): addr_desc;
EXTERNAL FUNCTION increment_addr ( addr_desc ; unit_range ): addr_desc;
EXTERNAL PROCEDURE regs_used (addr_desc; VAR registers; VAR registers);
EXTERNAL FUNCTION offset_addr ( VAR addr_desc; addr_desc ): addr_desc;
EXTERNAL PROCEDURE mark_regs_used ( set_of_registers );
EXTERNAL PROCEDURE update_usages ( addr_desc; usage_range );
EXTERNAL FUNCTION cvt_word ( addr_desc; data_alignment ): addr_desc;
EXTERNAL FUNCTION cvt_long ( addr_desc; data_alignment ): addr_desc;
EXTERNAL PROCEDURE decr_reg_usages ( registers );
EXTERNAL FUNCTION nonindexed_fetch ( expr ): addr_desc;
EXTERNAL FUNCTION mem_fetch ( expr ): addr_desc;

$ENABLE vaxexp
$ENDIF
  