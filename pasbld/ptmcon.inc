$PAGE PTMCON.INC, last modified 5/11/84, zw
(*********     Target Machine Customization     *********)

TYPE
fstr_3 = PACKED ARRAY [1..3] OF CHAR;
vstr_40 = STRING [40];
radix_type = ( hex_radix, octal_radix );
EXTERNAL VAR
(*  TMPREFIX is a three-letter target machine prefix which is used in
    constructing the names of the individual passes of the compiler.  *)
tmprefix: fstr_3;
(*  TTYINAME and TTYONAME are the external file names to be associated with the
    terminal input and terminal output files.  *)
ttyiname: vstr_40;
ttyoname: vstr_40;
(*  REL EXTENSION is the default extension for the compiler's relocatable
    binary output file.  *)
rel_extension: fstr_3;
(*  HAVE CHECKOUT and HAVE OPTIMIZER indicate whether checkout and optimizing
    code generators are available for this compiler.  *)
have_checkout: BOOLEAN;
have_optimizer: BOOLEAN;
(*  RADIX is HexRadix or OctalRadix to indicate the output mode for target
    machine numeric constants.  *)
radix: radix_type;
(*  ADR WIDTH is the number of digits (in the target machine radix) to be
    printed for a target machine address value.  *)
adr_width: INTEGER;
(*  SREALPREC is the number of significant digits which the target machine
    will make available in a variable of type "Real" (i.e., a "single-
    precision" real number.  *)
srealprec: prec_type;
(*  ARR LEN SIZE and STR LEN SIZE are the number of bits required by the
    length words for a flexible array and a flexible string, respectively,
    in a dynamically allocated variable.  *)
arr_len_size: INTEGER;
str_len_size: INTEGER;
(*  SET LWB LIMIT and SET UPB LIMIT are the smallest and largest possible
    set elements.  SET SIZE LIMIT is the largest allowable base size for a
    set type (the base size of a set type is computed by AllocType).  *)
set_lwb_limit: INTEGER;
set_upb_limit: INTEGER;
set_size_limit: INTEGER;
(*  Extra elements may be allocated at the beginning or end of a set, if
    this will improve the efficiency of set operations.  When allocating
    a set, its first element will be rounded down to a multiple of
    SET LQUANTUM, or to SET LBASE if that is smaller.  The last element
    will be rounded up to one less than a multiple of SET UQUANTUM, or to
    SET UBASE if that is larger.  *)
set_lquantum: INTEGER;
set_uquantum: INTEGER;
set_lbase: INTEGER;
set_ubase: INTEGER;
(*  BYTE SIZE is the number of bits in the smallest addressable unit on the
    target machine.  *)
byte_size: INTEGER;
(*  INT PREC LIMIT is the maximum number of bits which can be used in an
    integer computation.  (Generally, this will be the same as the word
    size.  It needn't be larger than the number of bits required to
    represent a MachineWord variable.  *)
int_prec_limit: bit_range;
(*  QBL ALLOWED is a flag which indicates whether quick blocks may be
    constructed for this target machine.  *)
qbl_allowed: BOOLEAN;
    