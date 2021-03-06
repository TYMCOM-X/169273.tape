$PAGE UTLSET.TYP, last modified 1/13/84, zw
$IFNOT utlsettyp

CONST
(*PDP10 dependant, change these constants for other machines*)
set_block_size = 72; (*two 36-bit words*)
set_range_size = 32767; (*multiple of set block size*)

TYPE (*set representation structures*)
elem_number = 0 .. set_range_size; (*range that can be represented in a set*)
set_number = 0 .. set_range_size; (*range of sets possible in an array*)
word_number = 0 .. (MAXIMUM(elem_number) DIV set_block_size) + 1;
set_node = ARRAY [0 .. *] OF SET OF 0 .. set_block_size - 1;
set_list = ARRAY [0 .. *] OF ^set_node;
svector = RECORD
  n_sets: set_number; (*number of sets in this vector*)
  n_words: word_number; (*number of 72-bit sets in each set*)
  sets: ^set_list (*pointer to the actual set list*)
END;

$ENABLE utlsettyp
$ENDIF
   