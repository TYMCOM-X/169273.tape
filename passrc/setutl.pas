$WIDTH=100
$LENGTH=55
$TITLE UTLSET.PAS, last modified 1/13/84, zw
(*TYM-Pascal bit set manipulation utility*)

$HEADER UTLSET.HDR

$INCLUDE UTLSET.TYP

$PAGE definitions

VAR
i_set: set_number;
i_word: word_number;

$PAGE new_svector, clr_set, unv_set, add_elem, del_elem, cpy_set, mov_set

PUBLIC FUNCTION new_svector(n_sets: set_number; n_elems: elem_number): svector;
BEGIN
  new_svector.n_sets := n_sets;
  new_svector.n_words := n_elems DIV set_block_size;
  NEW(new_svector.sets, n_sets);
  FOR i_set := 0 TO n_sets DO
    NEW(new_svector.sets^[i_set], new_svector.n_words)
END;

PUBLIC PROCEDURE clr_set(v: svector; i: set_number);
BEGIN
  WITH v DO FOR i_word := 0 TO n_words DO sets^[i]^[i_word] := []
END;

PUBLIC PROCEDURE unv_set(v: svector; i: set_number);
BEGIN
  WITH v DO
    FOR i_word := 0 TO n_words DO
      sets^[i]^[i_word] := [0..(set_block_size - 1)]
END;

PUBLIC PROCEDURE add_elem(v: svector; i: set_number; e: elem_number);
BEGIN
  i_word := e DIV set_block_size;
  v.sets^[i]^[i_word] := v.sets^[i]^[i_word] + [e MOD set_block_size]
END;

PUBLIC PROCEDURE del_elem(v: svector; i: set_number; e: elem_number);
BEGIN
  i_word := e DIV set_block_size;
  v.sets^[i]^[i_word] := v.sets^[i]^[i_word] - [e MOD set_block_size]
END;

PUBLIC PROCEDURE cpy_set(v: svector; i, j: set_number);
BEGIN
  WITH v DO FOR i_word := 0 TO n_words DO
    sets^[j]^[i_word] := sets^[i]^[i_word]
END;

PUBLIC PROCEDURE mov_set(v: svector; i: set_number; w: svector; j: set_number);
BEGIN
  WITH v DO FOR i_word := 0 TO n_words DO
    w.sets^[j]^[i_word] := sets^[i]^[i_word]
END;

$PAGE union, intersect, subtract, set_eq, in_set, is_empty, del_svector

PUBLIC PROCEDURE union(v: svector; i, j: set_number);
BEGIN
  WITH v DO FOR i_word := 0 TO n_words DO
    sets^[j]^[i_word] := sets^[i]^[i_word] + sets^[j]^[i_word]
END;

PUBLIC PROCEDURE intersect(v: svector; i, j: set_number);
BEGIN
  WITH v DO FOR i_word := 0 TO n_words DO
    sets^[j]^[i_word] := sets^[i]^[i_word] * sets^[j]^[i_word]
END;

PUBLIC PROCEDURE subtract(v: svector; i, j: set_number);
BEGIN
  WITH v DO FOR i_word := 0 TO n_words DO
    sets^[j]^[i_word] := sets^[j]^[i_word] - sets^[i]^[i_word]
END;

PUBLIC FUNCTION set_eq(v: svector; i, j: set_number): BOOLEAN;
BEGIN
  set_eq := TRUE;
  WITH v DO FOR i_word := 0 TO n_words DO
    EXIT IF sets^[i]^[i_word] <> sets^[j]^[i_word] DO set_eq := FALSE
END;

PUBLIC FUNCTION in_set(v: svector; i: set_number; e: elem_number): BOOLEAN;
BEGIN
  in_set := (e MOD set_block_size) IN v.sets^[i]^[e DIV set_block_size]
END;

PUBLIC FUNCTION is_empty(v: svector; i: set_number): BOOLEAN;
BEGIN
  is_empty := TRUE;
  WITH v DO FOR i_word := 0 TO n_words DO
    EXIT IF sets^[i]^[i_word] <> [] DO is_empty := FALSE
END;

PUBLIC PROCEDURE del_svector(v: svector);
BEGIN
  FOR i_set := 0 TO v.n_sets DO DISPOSE(v.sets^[i_set]);
  DISPOSE(v.sets)
END.
    