$PAGE UTLSET.TYP, last modified 4/3/84, zw
$IFNOT utlsettyp

CONST
(*PDP10 dependant, change these constants for other machines*)
set_block_size = 72; (*two 36-bit words*)
set_range_size = 32767; (*multiple of set block size*)

TYPE (*set representation structures*)
element_number = 0 .. set_range_size; (*range that can be represented*)
set_number = 0 .. set_range_size; (*range of sets possible in an array*)
word_number = 0 .. (MAXIMUM(element_number) DIV set_block_size) + 1;
set_node = ARRAY [0 .. *] OF SET OF 0 .. set_block_size - 1;
set_list = ARRAY [0 .. *] OF ^set_node;
set_vector = RECORD
  set_count: set_number; (*number of sets in this vector*)
  word_count: word_number; (*number of 72-bit sets in each set*)
  sets: ^set_list (*pointer to the actual set list*)
END;

$ENABLE utlsettyp
$ENDIF
   