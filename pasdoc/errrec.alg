
						Dmitry Lenkov

			ERROR RECOVERY ALGORITHM.
			-------------------------

		The algorithm described below is designed to be used
	with the LL(1) Parser Machine.

	Let us designate:

  T - a set of terminals;
  N - a set of nonterminals;
  V = T + N;
  E - the empty set;
  ST(v) - a set of start terminals for the symbol v in V;  If v in T
	  then ST(v)=v;
  S(v,f) - a lookup set for the symbol v if its father is the f symbol;
	   S(v,f) is defined as follows:
		1. v is the last son of f -> S(v,f)=E;
		2. otherwise S(v,f)=ST(u), where u is the right brother
		   of v for the same father f;
  R(v,f)=(f,Af,Av) - a reference to the father f of the v symbol, where
		     Af is the start address of f in the parser machine
		     and Av is the address of v inside of father's part
		     in the parser machine.  It is clear that some 
		     symbols have several references to different 
		     fathers.  

		We split V into four sets: VU, VC, VE, and V1.  These
	sets are defined below.
  
  1. v in VU if:
	1/ v has only one father;
	2/ v is the first son of this father and it can not be omitted.

		A symbol from VU is called as a unique symbol.

  2. v in VC if:
	1/ not v in VU;
	2/ v is a start symbol of at least one unique combination of 
	   terminal symbols;  A combination is called unique if:
		a/ it defines only one father of its start symbol;
		b/ the start symbol is the first son of this father and
		   it can not be omitted.
  3. v in VE if one of the following conditions is satisfied:
	1/ v is a last son or a list separator for all its fathers.
	2/ v has only one father and is not a first son of it.


		For the algorithm been described we need an error stack
	where we put all symbols reduced during look ahead move when
	an erroor is processed.  We define three working modes of the 
	parser machine which is supposed to use this algorithm.	 These 
	modes are: parser mode, first error mode, and second error 
	mode.

  1. Parser mode.
	
		In this mode the standard parsing algorithm is used
	except one change in it.  Before it decides that the symbol
	currently been processed is correct it takes a look at the
	lookup set of this symbol.  If this set is empty it reduces
	the current production and makes the same with the symbol it
	gets after reduction.  If the set is not empty it checks: does
	the symbol of the next token belong to this set?  If it 
	belongs, the algorithm continues to work as usual.  If not, it 
	signals an error and turns on the first error mode.  In the 
	case when the symbol of	the next token does not belong to the
	lookup set but the right brother can be omitted, the algorithm
	makes the same with the lookup set of this right brother.  
		The described change of the parsing algorithm allows
	to discover an error when the final state of the parser stack 
	for the last token could be easily rebuild.

  2. First error mode.

		In this mode the algorithm trys to make one symbol
	insertion, deletion, or	change so that parsing could be
	continued.  It makes this try only when the lookup set of the
	last symbol before an error consists of one symbol.  If this 
	try is not successful, it turns on the second error mode.  If 
	it is successful but the symbol to be deleted or changed is a 
	unique symbol, it checks: is the next symbol in the lookup set 
	of the unique symbol?  If yes, it turns on the second error 
	mode.  Otherwise it goes back to the parser mode.

  3. Second error mode.

		The algorithm for the second error mode has several
	steps which are described below.

    1/. This step defines: are we going to assume that the last symbol
      before an error is a correct symbol?  It gives a positive	answer
      if:
	a/. This is a unique symbol;
	b/. This is the last symbol of some unique combination (if the
	    parser stack's state allows to recognize the combination);
	c/. This symbol has an immediate left brother in the stack.
	    And this left brother has only one possible right brother
	    which is just equal to the last symbol.
      Otherwise the first step gives a negative answer.	 In all cases
      we go to the second step.

    2/. This step assigns the error symbol to be a current symbol of 
      the error mode.  Then it processes this current symbol in the 
      same way as the step 3 except that it makes look ahead for two
      symbols instead of one symbol.

    3/. This step defines if the current symbol is a suitable symbol
      for the step 4 or for the step 5.  It does that as follows.
	a/. If the current symbol is a unique symbol, the algorithm
	  checks out: is the next symbol in its lookup set?  If yes,
	  it goes to the step 4.  Otherwise it goes to the substep 3.d.
	b/. If the current symbol is in VC, the algorithm tries to 
	  discover the whole combination with scanning the input text. 
	  If it does not find any possible combination, it goes to the 
	  substep 3.d.  If it does, it checks out: is the next symbol 
	  in the lookup set of the combination last symbol.  If yes, it
	  goes to the step 4.  Otherwise it goes to the substep 3.d.
	c/. If the current symbol is in VE, the algorithm looks over
	  the parser stack for fathers of this symbol.  For each father
	  found it defines: is the next symbol (after the current 
	  symbol) a correct symbol to continue parsing?  If yes, the
	  algorithm includes this father into the set of possible
	  fathers.  If the set of possible fathers is empty, the
	  algorithm goes to the substep 3.d.  Otherwise it chooses the 
	  last father put onto the stack and goes to the step 5.
	d/. If the current symbol is not suitable for the steps 4 or 5,
	  the algorithm puts current symbol onto the error stack, then
	  takes the next symbol and assign it to be a current symbol.
	  At the end it goes to the step 3 again.

    4/. The algorithm performs this step when the current symbol is the
      first son of a single father.  The algorithm takes the following 
      actions.
	
	a/. An error slot is created on the top of the parser stack.
	  It is organized as usual parser procedure call but is marked
	  with an error mark.

	b/. The parser mode is called to process the father.

	c/. When parsing of the father is finished, the second error
	  mode is called again.

	d/. The algorithm removes the error slot from the stack, 
	  assigns the parsed symbol to be a current symbol, and goes 
	  to the step 3.

    5/. This step is performed if the current symbol is in VE, and its 
      single father is choosen from all its fathers in the parser 
      stack. Moreover parsing can be continued starting with this
      current symbol.
	This step cosists of the least cost algorithm that tries to
      find out a derivation subtree with the least cost to replace a
      string in the error stack.  If this string contains nonterminals
      the algorithm declines all subtrees which require to delete or
      change any nonterminal in it.  If there is no one subtree to
      replace the string the algorithm turns off semantic checking and
      calls the parse mode to continue parsing.  Otherwise the 
      algorithm	takes the least cost replacement, creates corresponding
      parse tree, and gives it next node by next node to the semantic
      routines.  Then it calls the parse mode.
	For many usual errors there will be one or two terminal symbols
      in the error stack, and the least cost algorithm will work fast.
   