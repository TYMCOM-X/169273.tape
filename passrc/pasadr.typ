$PAGE PASADR.TYP, last modified 3/21/84, zw
$IFNOT pasadrtyp

(*SYSTEM UTLTYP.TYP*)

(*bit_range can not count all bits???*)
(*char_range can not count all characters???*)

CONST (*PDP10 oriented*)
maximum_scope = #o7777; (*maximum nesting depth*)
maximum_parameter = #o1777; (*maximum number of parameters*)
maximum_set = #o777777; (*maximum number of items in a set*)
maximum_identifier = #o777777; (*maximum number of identifiers*)
maximum_debug_symbol = #o377777; (*maximum debugger symbol*)
maximum_bit_aligns = 128; (*maximum number of bit alignments*)
maximum_real_prec = 16; (*maximum real precision*)

TYPE
unit_range = positive_integer; (*addressing units in memory*)
unit_offset = integer; (*offset to unit_range*)
bit_range = positive_integer; (*bits in memory DIV bits per unit*)
bit_offset = integer; (*offset to bit_range*)
character_range = positive_integer; (*chars in memory DIV chars per unit*)
character_offset = integer; (*offset to character_range*)
alignment_range = 0 .. maximum_bit_aligns; (*possible bit alignments*)
precision_type = 1 .. maximum_real_prec; (*possible real precisions*)
set_range = 0 .. maximum_set; (*items in a set*)
identifier_range = 0 .. maximum_identifier; (*for numbering symbols, etc.*)
scope_range = 0 .. maximum_scope; (*for numbering scope and control blocks*)
parameter_range = 0 .. maximum_parameter; (*for counting parameters*)
parameter_index = 1 .. maximum_parameter; (*for indexing parameters*)
debug_index = 0 .. maximum_debug_symbol; (*index into debugger symbol table*)

$ENABLE pasadrtyp
$ENDIF
 