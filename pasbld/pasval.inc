external function makestring ( len: char_range ): val;
external function makeset ( min_range, max_range: bit_range ): val;
external function makearray ( n_comps: int_type ): val;
external function makerecord ( n_fields: int_type ): val;
external function cst_scalar ( svalue: int_type ): val;
external function mkreal ( rval: real_type; rprec: prec_type ): val;
  