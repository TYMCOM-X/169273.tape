$PAGE PASSY.TYP, last modified 3/27/84, zw
$IFNOT passytyp

(*SYSTEM UTLTYP.TYP*)
(*SYSTEM SRCTYP.TYP*)
(*SYSTEM PASIST.TYP*)


TYPE

symbols =
  (badsymbol, ident, intconst, realconst, stringconst, nilsy, notsy, 
  powerop, mulop, addop, relop, lparent, rparent, lbracket, rbracket, 
  comma, period, arrow, colon, becomes, semicolon, elipsis, labelsy, 
  constsy, typesy, varsy, functionsy, proceduresy, exceptionsy, 
  externalsy, publicsy, staticsy, forwardsy, optionssy, packedsy, setsy, 
  arraysy, recordsy, stringsy, filesy, precsy, programsy, modulesy, 
  datamodsy, envmodsy, beginsy, ifsy, casesy, repeatsy, whilesy, forsy, 
  withsy, loopsy, gotosy, returnsy, stopsy, iosy, exitsy, endsy, thensy, 
  elsesy, untilsy, ofsy, dosy, tosy, downtosy, otherssy, allcondsy, 
  eofsy,
  nonterminal,program_id,module_id, datamod_id, envmod_id, subr_options, 
  null_stmt, simple_stmt, goto_stmt, io_stmt, return_stmt, stop_stmt, 
  if_stmt, for_stmt, while_stmt, case_stmt, with_stmt, exit_clause, 
  until_clause, exprtree, not_op, sign_op, paren_expr, set_expr, 
  array_qualifier, field_qualifier, ptr_qualifier, func_qualifier, 
  type_decl, packed_type,set_type, string_type, pointer_type, subr_type, 
  array_type, pk_array_type, record_type, file_type, parm_list, 
  var_parm_decl, value_parm_decl, const_id_decl, var_id_decl, 
  type_id_decl, id_list, field_id_decl, variant_part, tag_field, 
  variant_case, range_list, declaration, label_declaration, 
  const_declaration, var_declaration, type_declaration, cond_declaration, 
  subr_decl, starsy, io_arg  );

symbol_set = SET OF badsymbol .. nonterminal;

operator = ( mul, rdiv, idiv, expon, imod, andop, andifop, plus, minus,
  orop, orifop, catop, leop, ltop, gtop, geop, eqop, neop, inop, readsy,
  writesy, readlnsy, writelnsy, readrnsy, writernsy, getstrsy, putstrsy,
  noop );

pmsemop =
  (nosemop, attach, attachnt, attachqual, infixop, returnnt, returnterm, 
   returnsubr, returnqual, attachopt, extratext, progidexp, semiexp, 
   progexp, dotexp, declexp, labelidexp, nodecl, noending, typeexp, 
   initvalexp, colonexp, subrnameexp, subrbodyexp, parmdeclexp, idexp, 
   chkeq, chkstar, eqexp, lbracketexp, idxtypeexp, ofexp, nofieldsemi, 
   novariant, nocaselab, lparenexp, nostatement, nosemi, norhs, noifpred, 
   thenexp, chksemielse, foridxexp, becomesexp, forinitexp, forwayexp, 
   forfinexp, doexp, untilexp, untilpredexp, whilepredexp, nocase, 
   stmtincaseexp, nocasedelim, nowithref, labelexp, endstmtexp, ifexp, 
   operandexp, expressionexp, rparenexp, rbracketexp, rangeexp, upbexp, 
   fieldexp, eqinstead, chksign, declrequired, nohandler );

symnamtable = array [symbols] of string [22];

const symbol_names: symnamtable =
  ('*BADSYMBOL*',
   '<IDENTIFIER>',
   '<INTCONST>',
   '<REALCONST>',
   '<STRINGCONST>',
   'NIL',
   'NOT',
   '**',
   '<MULOP>',
   '<ADDOP>',
   '<RELOP>',
   '(',
   ')',
   '[',
   ']',
   ',',
   '.',
   '^',
   ':',
   ':=',
   ';',
   '..',
   'LABEL',
   'CONST',
   'TYPE',
   'VAR',
   'FUNCTION',
   'PROCEDURE',
   'EXCEPTION',
   'EXTERNAL',
   'PUBLIC',
   'STATIC',
   'FORWARD',
   'OPTIONS',
   'PACKED',
   'SET',
   'ARRAY',
   'RECORD',
   'STRING',
   'FILE',
   'PREC',
   'PROGRAM',
   'MODULE',
   'DATAMODULE',
   'ENVMODULE',
   'BEGIN',
   'IF',
   'CASE',
   'REPEAT',
   'WHILE',
   'FOR',
   'WITH',
   'LOOP',
   'GOTO',
   'RETURN',
   'STOP',
   'IOCALL',
   'EXIT',
   'END',
   'THEN',
   'ELSE',
   'UNTIL',
   'OF',
   'DO',
   'TO',
   'DOWNTO',
   'OTHERS',
   'ALLCONDITIONS',
   '<EOF>',

   '*NONTERMINAL*',
   '<PROGRAM ID>',
   '<MODULE ID>',
   '<DATAMODULE ID>',
   '<ENVMODULE ID>',
   '<SUBR OPTIONS>',
   '<STATEMENT>',
   '<SIMPLE STATEMENT>',
   '<GOTO STATEMENT>',
   '<IO STATEMENT>',
   '<RETURN STATEMENT>',
   '<STOP STATEMENT>',
   '<IF HEAD>',
   '<FOR HEAD>',
   '<WHILE HEAD>',
   '<CASE HEAD>',
   '<WITH HEAD>',
   '<EXIT HEAD>',
   '<UNTIL CLAUSE>',
   '<EXPRESSION>',
   '<NOT OP>',
   '<SIGN OP>',
   '<PAREN EXPR>',
   '<SET EXPRESSION>',
   '<ARRAY QUALIFIER>',
   '<FIELD QUALIFIER>',
   '<PTR QUALIFIER>',
   '<FUNCTION QUALIFIER>',
   '<TYPE DECL>',
   '<PACKED TYPE>',
   '<SET TYPE>',
   '<STRING TYPE>',
   '<POINTER TYPE>',
   '<SUBR TYPE>',
   '<ARRAY TYPE>',
   '<PACKED ARRAY TYPE>',
   '<RECORD TYPE>',
   '<FILE TYPE>',
   '<PARAMETER LIST>',
   '<VAR PARM DECL>',
   '<VALUE PARM DECL>',
   '<CONST ID DECL>',
   '<VAR ID DECL>',
   '<TYPE ID DECL>',
   '<IDENTIFIER LIST>',
   '<FIELD ID DECL>',
   '<VARIANT PART>',
   '<TAG FIELD>',
   '<VARIANT CASE>',
   '<RANGE LIST>',
   '<DECLARATION>',
   '<LABEL DECLARATION>',
   '<CONST DECLARATION>',
   '<VAR DECLARATION>',
   '<TYPE DECLARATION>',
   '<COND DECLARATION>',
   '<SUBR DECL>',
   '<STAR>',
   '<IO ARG>');

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
$ENBLE pasanltyp
$ENDIF
   