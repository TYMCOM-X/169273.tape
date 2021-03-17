type set_desc = packed record
       nargs: 0..3;
       lwb_exp,
       upb_exp: expr;
       arg: array [1..3] of addr_desc
     end;
external var
  stemps_dynamic: boolean;  (* true if a runtime upper bound has been determined *)
  rtime_upb: addr_desc; (* the runtime upper bound, which must be freed *)
  
external const
  set_dynamic_cutover: set_range;
  
external procedure shape_set (set_exp: expr; var shaped_lwb, shaped_len: set_range);
external function set_fetch (set_exp: expr; ctxt_lwb, ctxt_len: set_range): set_desc;
external procedure set_assignment (tuple);
external procedure left_mask (registers; bit_range; bit_range);
external function long_set (expr): boolean;
external procedure make_set (set_desc; set_range; set_range; boolean);
external procedure set_free (set_desc);
external procedure force_out_of_reg (var set_desc);
external procedure set_parameter (expr; param_desc; var registers; boolean);
external procedure o_format_arg (addr_desc; addr_desc);
external procedure l_format_arg (addr_desc; addr_desc; addr_desc);
external procedure set_elem_reference (var set_desc; registers; registers; int_type; int_type; opc_range);
