(* Q10STR.INC - include file containing external declarations for
   the public routines of module Q10STR.PAS.  *)

external function fetchstring ( expr; str_len_context ): str_desc;
external procedure str_assignment (tuple);
external procedure free_string ( str_desc );
external function skip_desc_word ( str_desc ): addr_desc;
external function skip_len_words ( str_desc ): addr_desc;
external function fetchtranslated ( expr; str_len_context ): str_desc;
external function do_index_op ( expr ): addr_desc;
external function str_search_verify ( expr ): addr_desc;
external procedure str_compare ( expr );
external procedure str_parameter ( expr; param_desc; registers );
external function rt_open_call ( tuple ): addr_desc;
external procedure read_write_call ( tuple );
external procedure io_begins (tuple);
external procedure io_ends (tuple);
external function arg_addr (addr_desc): addr_desc;
external function alc_dynamic_temp (addr_desc; integer; bit_range): addr_desc;
