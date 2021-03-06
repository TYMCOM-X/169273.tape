$PAGE PASPT.TYP, last modified 3/27/84, zw
$IFNOT paspttyp

(*SYSTEM UTLTYP.TYP*)
(*SYSTEM PASSY.TYP*)
(*SYSTEM PASIST.TYP*)

TYPE
parse_node = ^token_record;
token_record = PACKED RECORD
  next_node: parse_node; (*The next node at the current level.*)
  defining_nodes: parse_node; (*This chains through parse_node^.next.*)
  source: source_position; (*The line in which the token appears.*)
  position: source_line_cursor; (*The starting position of the token.*)
  size: source_line_cursor; (*The length of token text.*)
  no_text: yn; (*This token has no text?*)
  CASE symbol_code: symbol OF
    ident: (name: name_pointer);
    intconst, realconst, stringconst: (value: simple_value);
    notsy, powerop, mulop, addop, relop, iosy: (operator_code: operator)
  END;

$ENABLE paspttyp
$ENDIF
 