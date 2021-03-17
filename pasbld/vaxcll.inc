external procedure procedure_call (node: tuple);
external function scl_function_call (node: expr; targ_addr:addr_desc ): addr_desc;
external procedure pas_call (node: expr; rv_addr: expr);


external procedure add_to_param_ary(desc: addr_desc;
			align:data_alignment;
			val:boolean;
			var params : ^param_ary;
			var param_ary_idx : parm_index );
