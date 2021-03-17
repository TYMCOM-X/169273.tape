
(*********   Regular Expressions   **********

    Regular expressions are represented internally by pointers of type
    'reg_exp', which point to 'reg_exp_node's.  Regular expression nodes
    are created and disposed by the SCNREA (Regular Expression Allocation)
    module.  SCNREA contains NewRe, which creates skeletal nodes; UseRe,
    which records references to nodes; and FreeRe, which deletes references
    and disposes the nodes.  The contents of nodes are manipulated by the
    SCNREU (Regular Expression Utility) module.  SCNREU contains a collec-
    tion of routines for creating regular expressions with specified oper-
    ators and operands; Derivative, which returns the derivative of an
    expression with respect to a given symbol; and Similar, which tests
    two expressions for similarity.  Outside SCNREA and SCNREU, only the
    pointers to regular expression nodes should be manipulated.  No routine
    outside of these modules should ever create or dispose of a regular
    expression node, or access any field of one.

    The fields of a regular expression node are:

    REFS -- The number of extant references to the node.  This field should
	be accessed only by routines in the SCNREA module, which use it for
	node allocation and deallocation.

    L_IN -- A boolean flag, indicating whether the empty string (lambda) is
	in the set of strings denoted by this regular expression.  This
	field is set by the creation routines in SCNREU, and tested by the
	Derivative routine.

    REG_OP -- The type of regular expression this is.  This field is set
	when the node is created by NewRe, and is determined by which of
	the SCNREU routines the node is created by.  The regular expression
	operators are:
	    lambda_op:	    The empty string
	    phi_op:	    The set containing no strings (the empty set)
	    literal_op:	    A string containing a single literal symbol
	    lit_range_op:   The set of strings containing single literal
			    symbols from some range
	    star_op:	    The set of all sequences of zero or more strings
			    from a given regular expression
	    not_op:	    The set of all strings which are not in a
			    given regular expression
	    cat_op:	    The set of all strings formed by concatenation
			    of strings from two given regular expressions
	    or_op:	    The set of all strings which are in either of
			    two other given regular expressions
	    and_op:	    The set of all strings which are in both of
			    two other given regular expressions

    The following fields are set by the creation routines in SCNREU:

    LIT_VAL -- The literal symbol for a 'literal_op' node.

    MIN_LIT, MAX_LIT -- The bounds of the range of literal symbols for a
	'lit_range_op' node.

    OPERAND -- The given regular expressions for a 'star_op' or 'not_op'
	node.

    LEFTOP, RIGHTOP -- The two given regular expressions for a 'cat_op',
	'or_op', or 'and_op' node.

    The conditional compilation flag CHAR controls the type of the basic data
    elements from which regular expressions are constructed.  If CHAR is true,
    the DataElement type is equivalent to type Char.  Normally, DataElement is
    equivalent to type Integer.  *)


type

$IF    char  data_element = char;
$IFNOT char  data_element = 0 .. maximum (integer);

    reg_ops =
      ( lambda_op, phi_op, literal_op, lit_range_op,
	star_op, not_op, cat_op, or_op, and_op );

    reg_exp_node = packed record
      refs: integer;
      l_in: boolean;
      case reg_op: reg_ops of
	lambda_op,
	phi_op:
	  ( );
	literal_op:
	  ( lit_val: data_element );
	lit_range_op:
	  ( min_lit, max_lit: data_element );
	star_op,
	not_op:
	  ( operand: reg_exp );
	cat_op,
	or_op,
	and_op:
	  ( leftop, rightop: reg_exp )
    end (* reg_exp_node *);
