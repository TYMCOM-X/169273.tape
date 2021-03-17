type
    elem_number = 0 .. 32767; (* The range that can be represented in a set. *)

    set_number = 0 .. 32767; (* The range of sets possible in an array. *)

    word_number = 0 .. 456; (* = maximum(elem_number) div 72 + 1 *)

    set_node = array [0..*] of set of 0 .. 71;

    set_list = array [0..*] of ^ set_node;

    svector = record
	n_sets: set_number; (* The number of sets in this vector. *)
	n_words: word_number; (* The number of 72-bit sets in each set. *)
	sets: ^ set_list (* Pointer to the actual set list. *)
    end;
