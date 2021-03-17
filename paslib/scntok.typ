(*********   Tokens   **********)


type token_type =

     (  nosymbol,

	(*  The following token kinds represent terminal symbols of the scanner
	    grammar.  Getsymbol returns them in Insymbol.  *)

	endsy,		scannersy,	namesy,		semicolonsy,
	alphabetsy,	asciisy,	numericsy,	commasy,
	equalsy,	actionsy,	ignoresy,	symbolssy,
	stringsy,	numbersy,	orsy,		dashsy,
	andsy,		ellipsissy,	lambdasy,	lparensy,
	rparensy,	questionsy,	starsy,		timessy,
	plussy,		notsy,		issy,		eofsy,

	(*  The following token kinds represent nonterminal symbols of the
	    scanner grammar.  They serve a documentary function.  *)

	accept_nt,	scanner_nt,	title_nt,	decls_nt,
	decl_nt,	alpha_nt,	names_nt,	defs_nt,
	def_nt,		symdef_nt,	prmlst_nt,	parm_nt,
	regexp_nt,	term_nt,	factor_nt,	elemnt_nt,
	lit_nt,		opt_is_nt  );


(*  The value of a token is represented by a 'sym_value'.  Invalue is a 'sym_
    value', as are the entries in the parse stack.  'Sym_value' is actually
    an undiscriminated union with different field types for the various token
    kinds.  The proper field for an input token may be selected on the basis
    of Insymbol; the fields for a stack entry may be selected on the basis of
    the current parser state.  *)


type
    sym_value = packed record
      line_no: number;
      column_no: line_index;
      case token_type of

	(*  The value of a name or string input token is a 'str_ptr' to a
	    string node containing its text.  *)

	namesy,
	stringsy:
	  ( lit_val: str_ptr );

	(*  The value of a numeric input token is a 'number' with its value.  *)

	numbersy:
	  ( num_val: number );

	(*  The value of an action input token is a list of strings representing
	    the lines of the action specification.

	    The value of a <symdef> symbol definition is the value of the
	    action input token for the SYMBOLS definition.  *)

	actionsy:
	  ( action_list: str_list );

	(*  The value of a <names> list is a 'number' which is one less than
	    the number of names in the list.  *)

	names_nt:
	  ( name_count: number );

	(*  The value of a <prmlst> parameter list is a pair of pointers to the
	    first and last entries in a 'str_list' whose nodes contain the
	    texts of the individual parameters.  *)

	prmlst_nt:
	  ( parm_list, last_parm: str_list );

	(*  The value of one of the various regular expression symbols
	    (<regexp>, <term>, <factor>, <elems>, <elemnt> or <lit>) is a
	    pointer to a regular expression node.  *)

	regexp_nt,
	term_nt,
	factor_nt,
	elemnt_nt,
	lit_nt:
	  ( re_val: reg_exp );

      end;
