$PAGE PASPT.TYP, last modifed 5/11/84, zw
(* Basic symbol and operator type definitions *)
$INCLUDE pasbnf.sym

TYPE
symbol_set = SET OF badsymbol..nonterminal;

TYPE
operators = ( mul, rdiv, idiv, expon, imod, andop, andifop, plus, minus, orop,
  orifop, catop, leop, ltop, gtop, geop, eqop, neop, inop, readsy, writesy,
  readlnsy, writelnsy, readrnsy, writernsy, getstrsy, putstrsy, noop );
(* Token descriptor *)

TYPE
parse_node = ^ token_type; (* token's are the leaves of the parse tree *)
token_type = PACKED RECORD
  next: parse_node; (* next node at current level *)
  defn: parse_node; (* defining or qualifying nodes of current node;
					     chains through parse_node^.next *)
  source: source_id; (* id of line on which token appears *)
  column: line_index; (* starting position of token in line *)
  LENGTH: line_index; (* length of token text in line *)
  dummy: BOOLEAN; (* true => token has no text *)
  CASE sym: symbols OF (* class token belongs to *)
    ident: (
      name: nam ); (* name table entry giving name *)
    intconst, realconst, stringconst: (
      value: val ); (* constant table entry giving value *)
    notsy, powerop, mulop, addop, relop, iosy: (
      op: operators ) (* operator code, undiscriminated *)
END;
