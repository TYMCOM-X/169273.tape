type
  reg_set_vector = array [0..*] of set_of_registers;

external var
  regdesc: array[registers] of reg_desc;
  regs_used: set_of_registers;
  regs_allocated: set_of_registers;
  reg_use_by_block: ^ reg_set_vector;
  vdesc_list: val_desc;
  temp_id_counter: id_range;
external procedure prepare (treg: reg_selector; exp: expr);
external procedure prep_direct (treg: reg_selector; exp: expr);
external function prep_set_addr (treg: reg_selector; exp: expr): addr_desc;
external function load (treg: reg_selector; exp: expr; p: bit_range): registers;
external procedure load_reg (treg: registers; exp: expr; p: bit_range);
external function fetch (exp: expr; p: bit_range): addr_desc;
external function fetch_direct (exp: expr; p: bit_range): addr_desc;
external function argument ( arg: expr ): addr_desc;
external function prep_operand (exp: expr; treg: reg_selector;
				srcsize, dstsize: bit_range): registers;
external function dload (treg: reg_selector; exp: expr; p: bit_range): registers;
external procedure reg_init;
external procedure reg_term;
external procedure reg_reset (n_blocks: index_range);
external procedure temp_reset;
external function get_reg (treg: reg_selector; p: bit_range): registers;
external function is_register (maddr: addr_desc): boolean;
external procedure free_reg (reg: reg_desc);
external procedure store_reg (reg: registers; mem: addr_desc);
external procedure consume (exp: expr);
external procedure clr_reg (reg: registers);
external procedure kill_regs (regs: set_of_registers);
type usage_cnt = 0..4095;
external function usage (e: expr): usage_cnt;
external function get_value_desc (exp: expr): val_desc;
external function get_temp (temp_size: unit_range): val_desc;
external procedure free (exp: expr);
external procedure tag_loc (mloc: addr_desc; exp: expr; aux: expr);
external procedure tag_reg (reg: registers; exp: expr);
external procedure tag_rv (exp: expr; func: blk);
external procedure clr_rv;
external procedure rs_save (var rs: reg_state);
external procedure bb_merge ( bb: tuple; rs: reg_state );
external procedure bb_end;
external procedure bb_start;
external function locate (exp: expr): addr_desc;
external procedure lock (reg: registers);
external procedure unlock (reg: registers);
external procedure do_move (reg: registers; mem: addr_desc; alignment: data_alignment; p: bit_range);
external procedure mem_lock (mem: addr_desc);
external procedure mem_unlock (mem: addr_desc);
external procedure clear (reg: registers);
external function int_value ( val: int_type ): addr_desc;
external function iconstp (exp: expr; var val: int_type): boolean;
type
  inst_opt_list =
      (  commutative,			(* operands may be exchanged *)
	 mem_form,			(* opcM may be generated *)
	 double	     );			(* must apply inst twice to process 2 word operands *)
  inst_opt_set = set of inst_opt_list;

external function do_inst
      (	opc_group: opc_range;			(* instruction to emit *)
	treg: reg_selector;		(* prefered target register *)
	op1, op2: expr;			(* left and right operands *)
	regsize: bit_range;		(* size of register in which to perform operation *)
	opsize: bit_range;		(* size to use for operands *)
	inst_options: inst_opt_set	(* properties of instruction to generate *)
			): registers;	(* register containing result returned *)
external procedure right_shift (reg: registers; p: bit_range; n: int_type);
