   0. FOREWORD TO THE DRAFT
   ------------------------


   The language Pascal was designed by Professor Niklaus Wirth to satis
   two principal aims:

      a)  to make available a language suitable for teaching  programmi
	  as a systematic discipline;

      b)  to define a language whose implementations may be both reliab
	  and efficient on currently available computers.

   This standard is designed to promote the portability  of  Pascal  pr
   grams among a variety of data processing systems.



   1. SCOPE
   --------


      1.1 This International Standard specifies requirements for:

	 a)  the syntax of Pascal.

	 b)  the semantic rules for interpreting the meaning of a progr
	     written in Pascal.

	 c)  the form of input data to be processed by a program  writt
	     in Pascal.

	 d)  the form of output data resulting from the use of a  progr
	     written in Pascal.

      1.2 This standard does not specify:

	 a)  the size or complexity of a program and its data  that  wi
	     exceed  the  capacity of any specific data processing syst
	     or the capacity of a particular processor.

	 b)  the minimal requirements of a data processing system that
	     capable  of  supporting an implementation of a processor f
	     Pascal.

	 c)  the set of commands used to control the environment in whi
	     a Pascal program exists.

	 d)  the mechanism by which programs written in Pascal are tran
	     formed for use by a data processing system.



   2. REFERENCES
   -------------


   The title of the standards publication referred to  in  this  standa
   is:

   BS 3527 : Glossary of terms used in data processing.

   NOTE:  Published parts of BS 3527  are  identical  with  correspondi
   sections of ISO 2382 - Data processing - Vocabulary.



   3. DEFINITIONS
   --------------


   For the purposes of this standard the definitions of ISO 2382  and
   3527 apply together with the following.

      a)  error - A violation by a program of the  requirements  of  th
	  standard such that detection normally requires execution of t
	  program.

      b)  implementation-defined - Those parts of the language which  m
	  differ  between  processors,	but which will be defined for a
	  particular processor.

      c)  implementation-dependent - Those parts of  the  language  whi
	  may  differ between processors, and for which there need not
	  a definition for a particular processor.

      d)  processor (in a computer) - A functional unit  that  interpre
	  and executes instructions.

      e)  scope - The text for which the declaration or definition of
	  identifier or label is valid.

      f)  undefined - The value of a variable when the variable does  n
	  necessarily have assigned to it a value of its type.



   4. METALANGUAGE
   ---------------


   The metalanguage used in this standard to specify  the  constructs
   based  on  Backus-Naur  form.  The notation has been modified from t
   original to permit greater convenience of description and to allow f
   iterative  productions  to  replace recursive ones.	Table 1 lists t
   meanings of the various meta-symbols.


			  Table 1. Metalanguage Symbols
			  -----------------------------


	META-SYMBOL	     MEANING
	-----------	     -------

	=		     shall be defined to be
	|		     alternatively
	.		     end of definition
	[x]		     0 or 1 instance of x
	{x}		     0 or more repetitions of x
	(x|y|..|z)	     grouping: any one of x,y,..z
	"xyz"		     the terminal symbol xyz
	lower-case-name      a non-terminal symbol


   For increased readability, the lower case names are	hyphenated.   T
   juxtaposition of two meta-symbols in a production implies the concat
   nation of the text they represent.  Within 6.1 this	concatenation
   direct;   no  characters  may  intervene.   In all other parts of th
   standard the concatenation is in accordance with the rules set out
   6.1.

   The characters required to form Pascal programs are	those  implicit
   required to form the symbols and separators defined in 6.1.



   5. COMPLIANCE
   -------------


      5.1 PROCESSORS
      --------------


	 5.1.1 A processor complying with the requirements of this  sta
	 dard shall:

	    a)	accept all of the features of the language  specified
		clause 6 with the meanings defined in clause 6;

	    b)	be accompanied by a document which provides a  definiti
		of all implementation-defined features;

	    c)	process each occurrence of an error in one of the follo
		ing ways:

		1)  state in the aforementioned document that  the  err
		    is not detected;

		2)  issue a warning that an occurrence of that error  w
		    possible;

		3)  detect the error;

	    d)	be accompanied by a document which  separately	describ
		any  features  accepted  by  the  processor which are n
		specified in clause 6.	Such  extensions  shall  be  de
		cribed	as  being  'extensions	to  Pascal  specified
		BS....:  197-'.


	 5.1.2 A conforming processor should:

	    a)	be able to reject any program which  uses  extensions
		the language specified in clause 6;

	    b)	process programs whose interpretation is affected by  i
		plementation-dependent	features  in  a manner similar
		that specified for errors (see 5.1.1).


	 5.1.3 A conforming processor may include  additional  predefin
	 procedures and/or functions.



      5.2 PROGRAMS
      ------------


	 5.2.1 A program complying with the requirements of this standa
	 shall:

	    a)	use only those features  of  the  language  specified
		clause 6;

	    b)	not use any implementation-dependent feature.


	 5.2.2 The meaning of a conforming program shall not  be  alter
	 if  its  identifiers are distinguished only by their first eig
	 characters.



   6. REQUIREMENTS
   ---------------


      6.1 LEXICAL TOKENS
      ------------------


	 6.1.1 GENERAL
	 -------------


	 The lexical tokens used to construct Pascal  programs	shall
	 classified  into  special  symbols, identifiers, numbers, labe
	 and character-strings.

	 NOTE:	The syntax given in this sub-clause describes the  form
	 tion  of  these tokens from characters and their separation, a
	 therefore does not adhere to the same rules as the syntax in t
	 rest of this standard.


	 letter = "A"|"B"|"C"|"D"|"E"|"F"|"G"|"H"|"I"|"J"|"K"|
		  "L"|"M"|"N"|"O"|"P"|"Q"|"R"|"S"|"T"|"U"|"V"|
		  "W"|"X"|"Y"|"Z" .
		  "a"|"b"|"c"|"d"|"e"|"f"|"g"|"h"|"i"|"j"|"k"|
		  "l"|"m"|"n"|"o"|"p"|"q"|"r"|"s"|"t"|"u"|"v"|
		  "w"|"x"|"y"|"z" .

	 digit = "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9" .



	 6.1.2 SPECIAL SYMBOLS
	 ---------------------


	 The special symbols are tokens having a fixed meaning and  sha
	 be used to specify the syntactic structures of the language.


	   special-symbol = "+"|"-"|"*"|"/"|"="|
			    "<"|">"|"["|"]"|"."|
			    ","|":"|";"|"^"|"<>"|"<="|
			    ">="|":="|".."| word-symbol .

	   word-symbol = "AND"|"ARRAY"|"BEGIN"|"CASE"|"CONST"|"DIV"|
			 "DOWNTO"|"DO"|"ELSE"|"END"|"FILE"|"FOR"|
			 "FUNCTION"|"GOTO"|"IF"|"IN"|"LABEL"|"MOD"|
			 "NIL"|"NOT"|"OF"|"OR"|"PACKED"|"PROCEDURE"|
			 "PROGRAM"|"RECORD"|"REPEAT"|"SET"|"THEN"|
			 "TO"|"TYPE"|"UNTIL"|"VAR"|"WHILE"|"WITH" .



	 Matching upper and lower case letters	shall  be  equivalent
	 word-symbols.



	 6.1.3 IDENTIFIERS
	 -----------------


	 Identifiers shall serve to denote constants,  types,  variable
	 procedures, functions and programs, and fields and tag-fields
	 records.  Identifiers may be of any length.  Matching upper  a
	 lower case letters shall be equivalent in identifiers.


		     identifier = letter {(letter | digit)} .


	 Examples:

			    X	  Rome	   gcd	   SUM



	 6.1.4 DIRECTIVES
	 ----------------


	 Directives shall only occur immediately after procedure-headin
	 or function-headings.


		     directive = letter {(letter | digit)} .



	 6.1.5 NUMBERS
	 -------------


	 The usual decimal notation shall be used for numbers  which  a
	 the  constants of the data types integer and real (see 6.4.2.2
	 The letter E preceding the scale factor shall mean "times ten
	 the power of".


	 digit-sequence = digit {digit} .
	 unsigned-integer = digit-sequence .
	 unsigned-real =
	     unsigned-integer "." digit-sequence ["E" scale-factor] |
	     unsigned-integer "E" scale-factor .
	 unsigned-number = unsigned-integer | unsigned-real .
	 scale-factor = signed-integer .



	 sign = "+" | "-" .
	 signed-integer = [sign] unsigned-integer .
	 signed-number = [sign] unsigned-number .


	 Examples:

		    1	  +100	   -0.1     5E-3     87.35E+8



	 6.1.6 LABELS
	 ------------


	 Labels shall be unsigned integers and shall be distinguished
	 their apparent integral values.


			    label = unsigned-integer .


	 If a statement is prefixed by a label, a goto statement shall
	 permitted to refer to it.



	 6.1.7 CHARACTER-STRINGS
	 -----------------------


	 Character-strings shall be sequences of characters  enclosed
	 apostrophes.  Character-strings consisting of a single charact
	 shall be the constants of the standard type char (see	6.4.2.2
	 Character-strings consisting of n (>1) enclosed characters sha
	 be the constants of the type (see 6.4.3.2).


			   PACKED ARRAY [1..n] OF char


	 If the character-string is to contain an apostrophe, this  apo
	 trophe shall be written twice.


		character-string = "'" character {character} "'" .


	 Examples:

		   'A'		';'	     ''''
		   'Pascal'		     'THIS IS A STRING'



	 6.1.8 COMMENTS, SPACES, AND ENDS OF LINES
	 -----------------------------------------


	 The construct:


	    "{" any-sequence-of-symbols-not-containing-right-brace "}"


	 shall be called a comment.  The substitution of a  space  for
	 comment shall not alter the meaning of a program.

	 Comments, spaces, and ends of lines shall be  considered  to
	 token	separators.   An arbitrary number of separators may occ
	 between any two consecutive tokens, or before the first token
	 a  program  text.  There shall be at least one separator betwe
	 any  consecutive  pair  of  tokens  made  up	of   identifier
	 word-symbols, or numbers.  Apart from the use of the space cha
	 acter in character-strings, no separators shall occur within t
	 kens.



      6.2 BLOCKS, LOCALITY, AND SCOPE
      -------------------------------


	 6.2.1 BLOCK
	 -----------


	 A block shall	consist  of  the  definitions,	declarations  a
	 statement-part    which    together	form	a   part   of
	 procedure-declaration, a function-declaration or a program.  A
	 identifiers  and labels with a defining occurrence in a partic
	 lar block shall be local to that block.


	 block = [ label-declaration-part ]
		   [ constant-definition-part ]
		     [ type-definition-part ]
		       [ variable-declaration-part ]
			 [ procedure-and-function-declaration-part ]
			   statement-part .


	 The label-declaration-part shall specify all labels that mark
	 statement in the corresponding statement-part.  Each label sha
	 mark one and only one statement in the statement-part.  The  a
	 pearance  of  a label in a label-declaration shall be a defini
	 occurrence for the block in which the declaration occurs.





	     label-declaration-part = "LABEL" label {"," label} ";" .


	 The	 constant-definition-part     shall	 contain      a
	 constant-definitions local to the block.


	    constant-definition-part = "CONST" constant-definition ";"
					 {constant-definition ";"} .


	 The type-definition-part shall contain all type-definitions  l
	 cal to the block.


	      type-definition-part = "TYPE" type-definition ";"
				      {type-definition ";"} .


	 The	 variable-declaration-part     shall	 contain      a
	 variable-declarations local to the block.


	    variable-declaration-part = "VAR" variable-declaration ";"
					  {variable-declaration ";"} .


	 The procedure-and-function-declaration-part  shall  contain  a
	 procedure and function declarations local to the block.


	    procedure-and-function-declaration-part =
	       {(procedure-declaration | function-declaration) ";"} .


	 The statement-part shall specify the algorithmic actions  to
	 executed upon an activation of the block.


		      statement-part = compound-statement .


	 Local variables shall have values that are undefined at the  b
	 ginning of the statement-part.



	 6.2.2 SCOPE
	 -----------


	    6.2.2.1 Each identifier or label within the block of a Pasc
	    program shall have a defining oocurrence whose scope enclos
	    all corresponding occurrences of the identifier or	label
	    the program text.

	    This scope shall be the range for which the occurrence  is
	    defining  one,  and all ranges enclosed by that range subje
	    to the requirements of 6.2.2.2.


	    6.2.2.2 If an identifier or label which has a defining occu
	    rence  for range A has a further defining occurrence for so
	    range B enclosed by A, range B and all ranges enclosed  by
	    shall  be  excluded from the scope of the defining occurren
	    for range A.

	    An identifier that is a field-identifier may  be  used  as
	    field-identifier  within  a  field-designator in any range
	    which a variable of the corresponding record-type is access
	    ble.


	    6.2.2.3 The defining occurrence of	an  identifier	or  lab
	    shall  precede all corresponding occurrences of that identi
	    ier or label in the program text with one  exception,  name
	    that  a type-identifier T, that specifies the domain of a p
	    inter-type ^T, may have its defining occurrence  anywhere
	    the type-definition-part in which ^T occurs.

	    An identifier or label shall have not more than one  defini
	    occurrence for a particular range.



      6.3 CONSTANT-DEFINITIONS
      ------------------------


      A constant-definition shall introduce an	identifier  to	denote
      constant.


	 constant-definition = identifier "=" constant .
	 constant = [sign] (unsigned-number | constant-identifier)
		    | character string .
	 constant-identifier = identifier .


      The occurrence of  an  identifier  on  the  left	hand  side  of



      constant-definition   shall   be	 its  defining	occurrence  as
      constant-identifier for the block in which the  constant-definiti
      occurs.	The  scope of a constant-identifier shall not include i
      own definition.

      A constant-identifier following a sign shall denote a value of ty
      integer or real.



      6.4 TYPE-DEFINITIONS
      --------------------


	 6.4.1 GENERAL
	 -------------


	 A type shall determine the set of values which variables of th
	 type	assume	 and  the  operations  performed  upon	them.
	 type-definition shall associate an identifier with the type.


	    type-definition = identifier "=" type .
	    tupe = simple-type | structured-type | pointer-type .


	 The occurrence of an identifier on  the  left	hand  side  of
	 type-definition   shall   be	its   defining	occurrence  as
	 type-identifier for the block in which the  type-definition  o
	 curs.	 The scope of a type-identifier shall not include its o
	 definition, except for pointer-types (see 6.2.2.3).

	 A    type-identifier	 shall	   be	  considered	 as
	 simple-type-identifier,  a  structured-type-identifier, or a p
	 inter-type-identifier, according to the type that it denotes.


	      simple-type-identifier = type-identifier .
	      structured-type-identifier = type-identifier .
	      pointer-type-identifier = type-identifier .
	      type-identifier = identifier .



	 6.4.2 SIMPLE-TYPES
	 ------------------


	    6.4.2.1 GENERAL
	    ---------------


	    All the simple types shall define ordered sets of values.


		 simple-type = ordinal-type | real-type .
		 ordinal-type = enumerated-type | subrange-type |
				  ordinal-type-identifier .
		 ordinal-type-identifier = type-identifier .
		 real-type = real-type-identifier .
		 real-type-identifier = type-identifier .


	    An ordinal-type-identifier shall be defined to denote an  o
	    dinal-type.   A  real-type-identifier shall be defined to d
	    note a real-type.



	    6.4.2.2 STANDARD SIMPLE-TYPES
	    -----------------------------


	    A  standard  type  shall   be   denoted   by   a   predefin
	    type-identifier.   The  values  belonging  to a standard ty
	    shall be manipulated by means of predefined primitive  oper
	    tions.  The following types shall be standard:


	    integer  The values shall be a subset of the  whole  number
		     denoted  as  specified in 6.1.5.  The predefined i
		     teger constant maxint, whose value shall  be  impl
		     mentation-defined,  shall	define	the subset of t
		     integers available in any implementation over  whi
		     the integer operations are defined.  The range sha
		     be the set of values:

		     -maxint, -maxint+1, ... -1, 0, 1, ...maxint-1,  ma
		     int.



	    real     The values shall be an implementation-defined subs
		     of the real numbers denoted as specified by 6.1.5.



	    Boolean  The values shall be  truth  values  denoted  by  t
		     identifiers  false and true, such that false is le
		     than true.



	    char     The values shall be an implementation-defined set
		     characters  (for  the denotation of character valu
		     see 6.1.7).  The ordering properties  of  the  cha
		     acter values shall be defined by the ordering of t
		     (implementation-defined) ordinal values of the cha
		     acters, i.e.  the relationship between the charact
		     variables c1 and c2 shall be the same as  the  rel
		     tionship between ord(c1) and ord(c2).  In all Pasc
		     implementations the following relations shall hold

		    a)	The subset of character values	representing  t
			digits 0 to 9 shall be ordered and contiguous.

		    b)	The subset of character values	representing  t
			upper-case  letters  A	to Z shall be ordered b
			not necessarily contiguous.

		    c)	The subset of character values	representing  t
			lower-case letters a to z, if available, shall
			ordered but not necessarily contiguous.

	    Int Boolean and char shall be ordinal-types.  Real sha
	    be a real-type.

	    NOTE:  Operators applicable to standard types  are	specifi
	    in 6.7.



	    6.4.2.3 ENUMERATED-TYPES
	    ------------------------


	    An enumerated-type shall define an ordered set of  values
	    enumeration  of  the  identifiers  which denote these value
	    The ordering of these values shall be determined by  the  s
	    quence in which the constants are listed.


	       enumerated-type = "(" identifier-list ")" .
	       identifier-list = identifier { "," identifier } .


	    The occurrence of an identifier within the identifier-list
	    an enumerated-type shall be its defining occurrence as a co
	    stant for the block in which the enumerated-type occurs.



	    Examples:

		      (red,yellow,green,blue)
		      (club,diamond,heart,spade)
		      (married,divorced,widowed,single)



	    6.4.2.4 SUBRANGE-TYPES
	    ----------------------


	    The  definition  of  a  type  as  a   subrange   of   anoth
	    ordinal-type  called the host type, shall include identific
	    tion of the least and the largest value in the subrange.  T
	    first  constant  shall specify the lower bound which shall
	    less than or equal to the upper bound.


		      subrange-type = constant ".." constant .


	    Examples:

		      1..100
		      -10..+10
		      red..green


	    A variable of subrange-type shall possess all the  properti
	    of	variables of the host type, with the restriction that i
	    value shall be in the specified closed interval.



	 6.4.3 STRUCTURED-TYPES
	 ----------------------


	    6.4.3.1 GENERAL
	    ---------------


	    A structured-type shall be characterized by  the  type(s)
	    its  components and by its structuring method.  If the comp
	    nent type is itself structured, the resulting structured-ty
	    shall exhibit several levels of structuring.


	      structured-type = ["PACKED"] unpacked-structured-type |
				  structured-type-identifier .
	      unpacked-structured-type = array-type | set-type |
					 file type | record type .



	    The use of the prefix PACKED in the definition  of	a  stru
	    tured-type	shall  indicate  to  the  processor  that stora
	    should be economized, even if this causes an access to a co
	    ponent  of	a  variable  of  the type to be less efficient
	    terms of space or time.

	    An occurrence of the PACKED prefix shall only affect the  r
	    presentation of the level of the structured-type whose defi
	    ition it precedes.	If a component is itself structured,  t
	    component's representation shall be packed only if the PACK
	    prefix also occurs in the definition of its type.



	    6.4.3.2 ARRAY-TYPE
	    ------------------


	    An array-type shall be  a  structured-type	consisting  of
	    fixed  number  of components that are all of one type, call
	    the component-type.  The elements of the array shall  be  d
	    signated by indices, which are values of the index-type.  T
	    array type definition shall specify both the  index-type  a
	    the component-type.


	      array-type = "ARRAY" "[" index-type { "," index-type } "]
			   "OF" component-type .
	      index-type = ordinal-type .
	      component-type = type .


	    Examples:


		      ARRAY[1..100] OF real
		      ARRAY[Boolean] OF colour


	    If the component-type of an array-type is also an array-typ
	    an	abbreviated  form  of definition shall be permitted.  T
	    abbreviated form shall be equivalent in effect  to	the  fu
	    form and is illustrated by the following examples.


		 ARRAY[Boolean] OF
			 ARRAY[1..10] OF ARRAY[size] OF real


	    shall be equivalent to:


		 ARRAY[Boolean],1..10,size] OF real





	    and


		 PACKED ARRAY[1..10] OF
		       PACKED ARRAY[1..8] OF Boolean


	    shall be equivalent to:


		 PACKED ARRAY[1..10,1..8] OF Boolean


	    The term string type shall be a generic term used  to  desi
	    nate any type which is defined to be:


		 PACKED ARRAY[1..n] OF char



	    6.4.3.3 RECORD-TYPES
	    --------------------


	    A record-type shall be  a  structured-type	consisting  of
	    fixed number of components, possibly of different types.  T
	    record-type definition  shall  specify  for  each  componen
	    called  a  field, its type and an identifier that denotes i
	    The occurrence of an identifier as a tag-field or within  t
	    identifier-list of a record-section shall be its defining o
	    currence as a field-identifier for the record-type	in  whi
	    the tag-field or record-section occurs.

	    NOTE:  The syntax of a record-type permits the  specificati
	    of	a  variant-part.   This  enables different variables, a
	    though of identical type, to exhibit structures which  diff
	    in	 the  number  and/or  types  of  their	components.   T
	    variant-part provides for the specification  of  an  option
	    tag-field.

	    The value of the tag-field shall indicate  which  variant
	    assumed by the record-variable at a given time.  Each varia
	    shall be introduced  by  one  or  more  constants.	 All  t
	    case-constants   shall   be  distinct  and	shall  be  of
	    ordinal-type  that	is  compatible	with  the  tag-type  (s
	    6.4.5).

	    For a record with a tag-field a change of variant shall occ
	    only  when a value associated with a different variant is a
	    signed to the tag-field.  At that  moment  fields  associat



	    with the previous variant shall cease to exist, and those a
	    sociated with the new variant shall come into existence, wi
	    undefined values.  An error shall be caused if a reference
	    made to a field of a variant other than the current variant

	    For a variant record without a tag-field a change of  varia
	    shall  be  implied by reference to a field that is associat
	    with a new variant.  Fields associated with the previous va
	    iant  shall  cease to exist and those associated with the n
	    variant shall come into existence with undefined values.


	      record-type = "RECORD" [field-list [";"] ] "END" .
	      field-list = fixed-part [ ";" variant-part  ] |
			     variant-part .
	      fixed-part = record-section { ";" record-section } .
	      record-section = identifier-list ":" type .
	      variant-part = "CASE" [tag-field ":" ] tag-type "OF"
			       variant { ";" variant } .
	      tag-field = identifier .
	      variant = case-constant-list ":"
			  "(" [ field-list [";"] ] ")" .
	      tag-type = ordinal-type-identifier .
	      case-constant-list = case-constant { "," case-constant }
	      case-constant = constant .
	      field-identifier = identifier .


	    Examples:

	      RECORD
		year : integer;
		month : 1..12;
		day : 1..31
	      END

	      RECORD
		name, firstname : string;
		age : 0..99;
		CASE married : Boolean OF
		true : ( spousesname : string);
		false: ()
	      END

	      RECORD
		x,y : real;
		area : real;
		CASE s : shape OF
		triangle : ( side : real;
			     inclination, angle1, angle2 : angle);
		rectangle : (side1, side2 : real;
			     skew, angle3 : angle);
		circle : (diameter : real);



	      END



	    6.4.3.4 SET-TYPES
	    -----------------


	    A set-type shall define the range  of  values  which  is  t
	    powerset  of  its base-type.  The largest and smallest valu
	    permitted in the base-type of a set-type shall be implement
	    tion-defined.


		 set-type = "SET" "OF" base-type .
		 base-type = ordinal-type .


	    NOTE:  Operators applicable to  set-types  are  specified
	    section 6.7.2.4.



	    6.4.3.5 FILE-TYPES
	    ------------------


	    A file-type shall be a structured-type consisting  of  a  s
	    quence of components that are all of one type.  The number
	    components (i.e.  the length of the file) is not fixed by t
	    file-type  definition.   A file with zero components shall
	    designated empty.


			   file-type = "FILE" "OF" type .


	    A standard file-type shall be provided and shall  be  denot
	    by	the  predefined  type-identifier text.	Variables of ty
	    text shall	be  termed  'textfiles'.   Each  component  of
	    textfile  shall  be  of  type char, but the sequence of cha
	    acters represented as a textfile shall be substructured  in
	    lines  by line-markers.  All operations applicable to a var
	    able of type FILE OF char shall be applicable  to  textfile
	    but  certain  additional operations shall also be applicabl
	    as specified in 6.9.  The structure of textfiles shall be:


		   text-structure = { {character} line-marker } .


	    The line-marker shall not be a value  of  the  standard  ty
	    char.



	 6.4.4 POINTER-TYPES
	 -------------------


	 A pointer-type shall consist of an unbounded set of  values  p
	 inting  to  variables of a type.  No operators are specified r
	 garding pointers except the tests for equality and inequality.

	 Pointer values shall be created by the  standard  procedure  n
	 (see  6.6.5.3).  The pointer value NIL shall belong to every p
	 inter type and shall not point to a variable.


	  pointer-type = "^" type-identifier | pointer-type-identifier



	 6.4.5 IDENTICAL AND COMPATIBLE TYPES
	 ------------------------------------


	 Types which are designated at two or more  different  places
	 the  program text shall be designated identical if the same ty
	 identifier is used at these places, or if different  identifie
	 are  used which have been defined to be equivalent to each oth
	 by type definitions of the form T1 = T2.

	 Two types shall be designated compatible if they are  identica
	 or  if  one is a subrange of the other, or if both are subrang
	 of the same type, or if they are  string  types  with	the  sa
	 number  of  components,  or  if they are set-types of compatib
	 base-types.



	 6.4.6 ASSIGNMENT-COMPATIBILITY
	 ------------------------------


	 An   expression   E   of   type   T2	shall	 be    designat
	 assignment-compatible	with  a type T1 if any of the five stat
	 ments which follow is true.

	    a)	T1 and T2 are identical and neither is a file-type nor
		structured-type with a file component.

	    b)	T1 is a real-type and T2 is integer.

	    c)	T1 and T2 are compatible ordinal-types and the value of
		is in the closed interval specified by the type T1.

	    d)	T1 and T2 are compatible set-types and all the members
		the  value of the set E are in the closed interval spec



		fied by the base-type of T1.

	    e)	T1 and T2 are compatible string types.

	 At any place where the rule of assignment-compatibility is use

	    a)	If T1 and T2 are compatible ordinal-types and  the  val
		of  the expression E is not in the closed interval spec
		fied by the type T1, an error shall occur.

	    b)	If T1 and T2 are compatible  set-types	and  any  of  t
		members  of the set expression E is not in the closed i
		terval specified by the base-type of the type T1, an  e
		ror shall occur.



	 6.4.7 EXAMPLE OF A TYPE-DEFINITION-PART
	 ---------------------------------------


	   TYPE
	     count = integer;
	     range = integer;
	     colour = (red, yellow, green, blue);
	     sex = (male, female);
	     year = 1900..1999;
	     shape = (triangle, rectangle, circle);
	     card = ARRAY[1..80] OF char;
	     str = FILE OF char;
	     polar = RECORD r : real; theta : angle END;
	     person = ^persondetails;
	   persondetails = RECORD
	       name, firstname : str;
	       age : integer;
	       married : Boolean;
	       father, child, sibling : person;
	       CASE s : sex OF
	       male : (enlisted, bearded : Boolean);
	       female : (pregnant : Boolean)
	   END;
	   tape = FILE OF persondetails;
	   intfile = FILE OF integer;


	 NOTE:	In the above examples 'count', 'range' and 'integer'  d
	 note  identical  types.  The type 'year' is compatible with, b
	 not identical to, the types 'range', 'count' and 'integer'.



      6.5 DECLARATIONS AND DENOTATIONS OF VARIABLES
      ---------------------------------------------


	 6.5.1 VARIABLE-DECLARATIONS
	 ---------------------------


	 A variable declaration shall consist of a  list  of  identifie
	 denoting the new variables, followed by their type.


		variable-declaration = identifier-list ":" type .


	 The occurrence of an identifier within the identifier-list of
	 variable-declaration shall be its defining occurrence as a var
	 able-identifier for the block in which the  declaration  occur
	 A variable declared in a variable-declaration shall exist duri
	 the entire execution process of the block in  which  it  is  d
	 clared.


	 Examples:

	      x,y,z: real
	      i,j: integer
	      k: 0..9
	      p,q,r: Boolean
	      operator: (plus, minus, times)
	      a: ARRAY[0..63] OF real
	      c: colour
	      f: FILE OF char
	      hue1,hue2: SET OF colour
	      p1,p2: person
	      m,m1,m2 : ARRAY[1..10,1..10] OF real
	      coord : polar
	      pooltape : ARRAY[1..4] OF tape


	 A  denotation	of  a  variable   shall   designate   either
	 entire-variable, a component of a variable, or a variable refe
	 enced by a pointer (see 6.4.4).


	    variable = entire-variable | component-variable |
			referenced-variable .


	 NOTE:	Variables occurring in examples in the remainder of  th
	 standard should be assumed to have been declared as specified
	 6.5.1.



	 6.5.2 ENTIRE-VARIABLES
	 ----------------------


	 An entire-variable shall be denoted by its identifier.


	     entire-variable = variable-identifier .
	     variable-identifier = identifier .



	 6.5.3 COMPONENT-VARIABLES
	 -------------------------


	    6.5.3.1 GENERAL
	    ---------------


	    A component of a variable shall be denoted	by  the  variab
	    followed by a selector specifying the component.  The form
	    the selector shall depend on the structuring type of the va
	    iable.


	      component-variable = indexed-variable |
				    field-designator | file-buffer .



	    6.5.3.2 INDEXED-VARIABLES
	    -------------------------


	    A component of a variable of array-type shall be  denoted
	    the variable followed an index expression.


	      indexed-variable =
		   array-variable "[" expression { "," expression } "]"
	      array-variable = variable .


	    The index expression shall be assignment-compatible with  t
	    index-type specified in the definition of the array-type.


	    Examples:

		      a[12]
		      a[i+j]



	    If	the  component	of   an   array-variable   is	also
	    array-variable  an abbreviation may be used.  The abbreviat
	    form shall be considered equivalent to the full form  and
	    illustrated by the following example.


		      m[i][j]
		    is equivalent to
		      m[i,j]



	    6.5.3.3 FIELD-DESIGNATORS
	    -------------------------


	    A component of a variable of record-type shall be denoted
	    the  record-variable  followed by the field-identifier of t
	    component.


	      field-designator = record-variable "." field-identifier .
	      record-variable = variable .


	    Example:

		      p2^.pregnant
		      coord.theta



	    6.5.3.4 FILE-BUFFERS
	    --------------------


	    The existence of a file-variable f with components of type
	    shall  imply  the  existence  of a buffer variable of type
	    This buffer variable shall be denoted by f^ and  shall  ser
	    to	append	components  to the file during generation, and
	    access the file during  inspection	(see  6.6.5.2).   At  a
	    time, only the one component of a file variable determined
	    the current file position shall be directly accessible.  Th
	    component  shall be designated the current file component a
	    shall be represented by the file's buffer variable.


		file-buffer = file-variable "^" .
		file-variable = variable .



	 6.5.4 REFERENCED-VARIABLES
	 --------------------------


	    referenced-variable = pointer-variable "^" .
	    pointer-variable = variable .


	 A variable allocated by the standard procedure new (see 6.6.5.
	 shall	exist  until it is de-allocated by the standard procedu
	 dispose (see 6.6.5.3).  If p is  a  pointer  variable	which
	 bound	to a type T, p shall denote that variable and its point
	 value, whereas p^ shall denote the variable of type T referenc
	 by  p.   An error shall be caused if the pointer value is NIL
	 undefined at the time it is de-referenced.


	 Examples:

		   p1^.father
		   p1^.sibling^.bearded



      6.6 PROCEDURE AND FUNCTION DECLARATIONS
      ---------------------------------------


	 6.6.1 PROCEDURE-DECLARATIONS
	 ----------------------------


	 A procedure-declaration shall associate  an  identifier  with
	 part	of   a	 program  so  that  it	can  be  activated  by
	 procedure-statement.


	    procedure-declaration = procedure-heading ";"
				    (procedure-block | directive).
	    procedure-block = block.


	 The procedure-heading shall specify the  identifier  naming  t
	 procedure and the formal parameters (if any).	The appearance
	 an identifier in the procedure-heading of a procedure	shall
	 its  defining occurrence as a procedure-identifier for the blo
	 in which the procedure-declaration occurs.


	    procedure-heading = "PROCEDURE" identifier
				 [ formal-parameter-list ] .
	    procedure-identifier = identifier .



	 The algorithmic actions to be executed upon  activation  of  t
	 procedure  by	a  procedure-statement	shall be specified by t
	 statement-part  of  the  procedure-block.   The   use	 of   t
	 procedure-identifier  in a procedure-statement within the proc
	 dure-block shall imply recursive  execution  of  the  procedur
	 The  full  set of directives permitted after a procedure-headi
	 shall be implemenation-dependent.  However, to allow the call
	 a  procedure  to  precede textually its definition a forward d
	 claration shall be provided.  A forward declaration  shall  co
	 sist of a procedure-heading followed by the directive 'forward
	 In	 the	  subsequent	  procedure-declaration       t
	 formal-parameter-list shall be omitted.  The forward declarati
	 and the procedure-declaration shall be local to the same  bloc
	 The  forward  declaration  and  subsequent procedure-declarati
	 shall constitute a defining occurrence at the place of the  fo
	 ward declaration.

	 Examples of procedure declarations are:


	   PROCEDURE readinteger (VAR f: text; VAR x: integer) ;
	   VAR i,j: integer;
	   BEGIN WHILE f^ = ' ' DO get(f); i := 0;
	     WHILE f^ IN ['0'..'9'] DO
	       BEGIN j := ord(f^) - ord('0');
		 i := 10*i + j;
		 get(f)
	       END;
	     x := i
	   END

	   PROCEDURE bisect(FUNCTION f(x : real) : real;
			    a,b: real; VAR z: real);
	   VAR m: real;
	   BEGIN {assume f(a) < 0 and f(b) > 0 }
	     WHILE abs(a-b) > 1E-10*abs(a) DO
	     BEGIN m := (a+b)/2.0;
		     IF f(m) < 0 THEN a := m ELSE b :=m
	     END;
	     z := m
	   END

	   PROCEDURE append(VAR f : intfile);
	 {Enables items to be appended to a file regardless of its
	 current state}
	   VAR g : intfile;
	     PROCEDURE copy(VAR f,g : intfile);
	     BEGIN
	     reset(f); rewrite(g);
	     WHILE NOT eof(f) DO
	       BEGIN
	       g^ := f^;
	       put(g); get(f);



	       END;
	     END; { of copy }
	   BEGIN
	   copy(f,g);
	   copy(g,f);
	   END { of append }



	 6.6.2 FUNCTION-DECLARATIONS
	 ---------------------------


	 Function-declarations shall serve to define parts of the progr
	 that  compute	a  value  of  simple-type  or  a  pointer  valu
	 Functions  shall  be  activated   by	the   evaluation   of
	 function-designator  (see 6.7.3) that is a constituent of an e
	 pression.


	    function-declaration = function-heading ";"
				    (function-block | directive) .
	    function-block = block .


	 The function-heading shall specify  the  identifier  naming  t
	 function,  the  formal  parameters (if any), and the type of t
	 function result.  The appearance of an identifier in  the  fun
	 tion-heading of a function-declaration shall be its defining o
	 currence as a function-identifier for the  block  in  which  t
	 function-declaration occurs.


	   function-heading = "FUNCTION" identifier
			      [formal-parameter-list] ":" result-type .
	   function-identifier = identifier .
	   result-type = simple-type-identifier |
			 pointer-type-identifier .


	 The algorithmic actions to be executed upon  activation  of  t
	 function  by  a  function-designator  shall  be specified by t
	 statement-part of the function-block.	The function-block  sha
	 contain  at  least one assignment-statement that assigns a val
	 to the function-identifier.  The result of the function shall
	 the  last  value assigned.  If no assignment occurs the value
	 the   function   shall   be   undefined.    The   use	 of   t
	 function-identifier   in   a	function-designator   within  t
	 function-block shall imply recursive execution of the	functio
	 The  full  set  of directives permitted after a function-headi
	 shall be implementation-dependent.  However, to allow	the  ca
	 of a function to precede textually its definition, a forward d
	 claration shall be provided.  A forward declaration  shall  co



	 sist  of a function-heading followed by the directive 'forward
	 In the subsequent function-declaration the formal-parameter-li
	 and  the  result-type shall be omitted.  The forward declarati
	 and the function-declaration shall be local to the  same  bloc
	 The forward declaration and subsequent function-declaration co
	 stitute a defining occurrence at the place of	the  forward  d
	 claration.


	 Examples:

	   FUNCTION Sqrt(x:real): real;
	   VAR x0,x1: real;
	   BEGIN x1 := x; { x>1, Newton's method }
	     REPEAT x0 := x1; x1 := (x0+ x/x0)*0.5
	     UNTIL abs(x1-x0) < eps*x1 ;
	     Sqrt := x0
	   END

	   FUNCTION GCD(m,n : integer) : integer; forward;

	   FUNCTION max(a: vector; n: integer): real;
	   VAR x: real; i: integer;
	   BEGIN x := a[1];
	     FOR i := 2 TO n DO
	     BEGIN {x = max(a[1],....a[i-1])}
	     IF x < a[i] THEN x := a[i]
	     END;
	     {x = max(a[1],....a[n])}
	     max := x
	   END

	   FUNCTION GCD; {which has been forward declared}
	   BEGIN IF n=0 THEN GCD := m ELSE GCD := GCD(n,m MOD n)
	   END

	   FUNCTION Power(x: real;y: integer): real ; { y >= 0}
	   VAR w,z: real; i: integer;
	   BEGIN w := x; z := 1; i := y;
	     WHILE i > 0 DO
	     BEGIN {z*(w**i) = x ** y }
	       IF odd(i) THEN z := z*w;
	       i := i div 2;
	       w := sqr(w)
	     END;
	     {z = x**y }
	     Power := z
	   END



	 6.6.3 PARAMETERS
	 ----------------


	    6.6.3.1 GENERAL
	    ---------------


	    There shall be four kinds of parameters :  value  parameter
	    variable  parameters,  procedural  parameters  and function
	    parameters.  A parameter-group without a preceding	specifi
	    shall be a list of value parameters.


	      formal-parameter-list = "(" formal-parameter-section
				     {";" formal-parameter-section} ")"
	      formal-parameter-section =
		 ["VAR"] parameter-group |
		 procedure-heading |
		 function-heading .
	      parameter-group =
		 identifier-list ":" type .
	      parameter-identifier = identifier .


	    The occurrence of an identifier within the identifier-list
	    a  parameter-group shall be its defining occurrence as a pa
	    ameter-identifier for the formal-parameter-list in	which
	    occurs    and    any    corresponding    procedure-block
	    function-block.  The occurrence of	an  identifier	within
	    procedure-heading  in a formal-parameter-section shall be i
	    defining  occurrence  as  a  procedural  parameter	for   t
	    formal-parameter-list  in which it occurs and any correspon
	    ing procedure-block or function-block.  The occurrence of
	    identifier	   within     a     function-heading	 in
	    formal-parameter-section shall be its defining occurrence
	    a functional parameter for the formal-parameter-list in whi
	    it	occurs	 and   any   corresponding   procedure-block
	    function-block.   If  the  formal-parameter-list  is within
	    procedural parameter or a functional parameter,  there  sha
	    be no corresponding procedure-block or function-block.



	    6.6.3.2 VALUE PARAMETERS
	    ------------------------


	    The actual-parameter (see 6.7.3 and 6.8.2.3) shall be an  e
	    pression.  The formal parameter shall denote a variable loc
	    to the block.  The current value of the expression	shall
	    assigned  to  the variable upon activation of the block.  T
	    actual-parameter shall be assignment-compatible with the ty



	    of the formal parameter.



	    6.6.3.3 VARIABLE PARAMETERS
	    ---------------------------


	    The actual-parameters (see 6.7.3 and 6.8.2.3) shall be a va
	    iable.   The  formal parameter shall denote this actual var
	    able during the entire activation of the block.   Any  oper
	    tion  involving the formal parameter shall be performed imm
	    diately on the actual-parameter.  The type of the actual pa
	    ameter  shall  be  identical to that of the formal paramete
	    If the selection of this variable involves the indexing of
	    array,  or	the  de-referencing  of  a pointer, these actio
	    shall be executed before the activation of the block.

	    Components of variables of any packed type shall not be  us
	    as actual variable parameters.



	    6.6.3.4 PROCEDURAL PARAMETERS
	    -----------------------------


	    The actual-parameter (see 6.7.3 and 6.8.2.3) shall be a  pr
	    cedure-identifier.	 The formal parameter shall denote a pr
	    cedure that represents the actual procedure during the enti
	    activation	of  the block.	If the procedural parameter, up
	    activation, accesses any entity non-locally, the  entity  a
	    cessed shall be one that was accessible to the procedure wh
	    its procedure-identifier was passed as a  procedural  param
	    ter.   The	actual	procedure  and the formal procedure sha
	    have compatible formal-parameter-lists (see 6.6.3.6).



	    6.6.3.5 FUNCTIONAL PARAMETERS
	    -----------------------------


	    The actual-parameter (see 6.7.3 and 6.8.2.3) shall be a fun
	    tion-identifier.   The  formal parameter shall denote a fun
	    tion that represents the actual function  during  the  enti
	    activation	of  the block.	If the functional parameter, up
	    activation, accesses any entity non-locally, the  entity  a
	    cessed  shall be one that was accessible to the function wh
	    its function-identifier was passed as a functional paramete
	    The actual function and the formal function shall have comp
	    tible formal-parameter-lists (see 6.6.3.6) and identical  r
	    sult-types.



	    6.6.3.6 PARAMETER LIST COMPATIBILITY
	    ------------------------------------


	    Two formal-parameter-lists shall be compatible if  they  co
	    tain  the  same number of parameters and if the parameters
	    corresponding positions match.  Two  parameters  shall  mat
	    if:

		a)  they are both value parameters of identical type;

		b)  or they are both  variable	parameters  of	identic
		    type;

		c)  or they are both procedural parameters with  compat
		    ble parameter lists;

		d)  or they are both functional parameters with  compat
		    ble parameter lists and identical result-types.



	 6.6.4 STANDARD PROCEDURES AND FUNCTIONS
	 ---------------------------------------


	    6.6.4.1 GENERAL
	    ---------------


	    Standard procedures and functions shall  be  pre-declared
	    every  implementation of Pascal.  Any implementation may fe
	    ture additional pre-declared procedures and functions.

	    NOTE:  Since all pre-declared  entities  are  declared  in
	    range  surrounding the program, no conflict arises from a d
	    claration re-defining the same identifier within the  progr
	    block.

	    The standard procedures and functions shall be  as	specifi
	    in 6.6.5 and 6.6.6 respectively.



	 6.6.5 STANDARD PROCEDURES
	 -------------------------


	    6.6.5.1 GENERAL
	    ---------------


	    The effect of using standard procedures as procedural param



	    ters shall be implementation-dependent.



	    6.6.5.2 FILE HANDLING PROCEDURES
	    --------------------------------


	    put(f)	If the predicate eof(f) yields true prior to ex
			cution	of  put(f), the value of the buffer var
			able f^ shall be appended to the file  f,  eof(
			shall  remain  true and the value of f^ shall b
			come undefined.  If eof(f) does  not  yield  tr
			prior to execution an error shall occur.

	    get(f)	If the predicate eof(f) yields false prior to t
			execution  of  get(f),	the current file positi
			shall be advanced to the next component, and  t
			value  of this component shall be assigned to t
			buffer variable f^.  If no next component exist
			then  eof(f)  shall become true, and the value
			f^ shall become undefined.  If	eof(f)	does  n
			yield  false  prior  to execution, an error sha
			occur.

	    reset(f)	Shall reset the current file position to its  b
			ginning  and  shall assign to the buffer variab
			f^ the value of the first element of f .   eof(
			shall become false, if f is not empty;	otherwi
			the value of f^ shall be  undefined,  and  eof(
			shall be true.

			NOTE:  This is a necessary initializing operati
			prior to generating the file f.

	    rewrite(f)	Shall discard the current value of f  so  that
			new  file  may be generated.  eof(f) shall beco
			true.

			NOTE:  This is a necessary initializing operati
			prior to generating the file f.

	    If an activation of the procedure put(f) is not separated d
	    namically from a previous activation of get(f) or reset(f)
	    an activation of rewrite(f), the effect shall be  implement
	    tion-dependent.

	    An error shall be caused if the current file  position  of
	    file  f  is altered while the buffer variable f^ is an actu
	    variable parameter, or an element of the record-variable-li
	    of a with-statement, or both.

	    NOTE:  The standard procedures read, write, readln,  writel



	    and page are described in 6.9.



	    6.6.5.3 DYNAMIC ALLOCATION PROCEDURES
	    -------------------------------------


	    new(p)	shall allocate a new variable v and shall  assi
			a pointer to v to the pointer variable p.  If t
			type of v is a record  type  with  variants,  t
			form

	    new(p,t1,....tn)shall allocate a variable of the variant wi
			tag-field  values  t1,....,tn.	The tag-field v
			lues shall be listed contiguously and in the or
			er  of their declaration and shall not be chang
			during execution from the values indicated.   A
			trailing   tag-fields	may   be   omitted.   T
			tag-field  values  shall  not  be  given  to  t
			tag-fields by this procedure.

	    dispose(p)	shall indicate that storage occupied by the var
			able  p^ is no longer needed.  All pointer valu
			which referenced this variable shall become und
			fined.	If the second form of new was used to a
			locate the variable, the following form  of  di
			pose shall be used:

	    dispose(p,t1,....,tn), with identical tag-field values, sha
			indicate that storage occupied by this variant
			no longer needed.

	    An error shall be caused if the value of the pointer  param
	    ter of dispose is NIL or undefined.

	    An error shall be caused if a variable that is currently  e
	    ther  an  actual variable parameter, or an element of the r
	    cord-variable-list of a with-statement, or both, is  referr
	    to by the pointer parameter of dispose.

	    An error shall be caused if a referenced-variable created u
	    ing  the  second  form of new is used as an operand in an e
	    pression, or the variable in an assignment-statement or as
	    actual-parameter.



	    6.6.5.4 TRANSFER PROCEDURES
	    ---------------------------


	    Assume that the variables a and z are declared by:


		 a: ARRAY[m..n] OF T
		 z: PACKED ARRAY [u..v] OF T


	    where ord(n) - ord(m) >= ord(v) - ord(u)
	    and ord(m) <= ord(i) <= (ord(n) - ord(v) + ord(u))


	    The statement pack(a,i,z), given this assumption, shall mea


		 FOR j := u TO v DO z[j] := a[k]


	    and the statement unpack(z,a,i) shall mean:


		 FOR j := u TO v DO a[k] := z[j]


	    where j and k denote auxiliary variables not occurring  els
	    where  in  the  program  and  k is the result of applying t
	    function succ (ord(j)-ord(u)) times to i.



	 6.6.6 STANDARD FUNCTIONS
	 ------------------------


	    6.6.6.1 GENERAL
	    ---------------


	    The effect of using standard functions as  actual  function
	    parameters shall be implementation-dependent.



	    6.6.6.2 ARITHMETIC FUNCTIONS
	    ----------------------------


	    For the following arithmetic functions, the type of  the  e
	    pression  x  shall be either real or integer.  (For the fun
	    tions abs and sqr, the type of the result shall be	the  sa
	    as the type of the parameter, x.  For the remaining arithme
	    ic functions, the type of the result shall always be real):


	    abs(x)     shall compute the absolute value of x

	    sqr(x)     shall compute the square of x

	    sin(x)     shall compute the sine of x, where x is	in  rad
		       ans.

	    cos(x)     shall compute the cosine of x, where x is in rad
		       ans.

	    exp(x)     shall compute the value of the base of natural l
		       garithms raised to the power x.

	    ln(x)      shall compute the natural logarithm of x, if x
		       greater	than zero.  If x is not greater than ze
		       an error shall occur.

	    sqrt(x)    shall compute the positive square root of x, if
		       is  not negative.  If x is negative an error sha
		       occur.

	    arctan(x)  shall compute the principal value, in radians,
		       the arctangent of x.



	    6.6.6.3 TRANSFER FUNCTIONS
	    --------------------------


	    trunc(x)  From the real parameter x, this function shall  r
		      turn an integer result that is the integral part
		      x.  The absolute value of the result shall  not
		      greater  than  the absolute value of the paramete
		      An error shall occur if the result is not  a  val
		      of the type integer.  For example:


			   trunc(3.7) yields 3
			   trunc(-3.7) yields -3



	    round(x)  From the real parameter x, this function shall  r
		      turn  an	integer  result  that  is  the value of
		      rounded to the nearest integer.  If x  is  positi
		      or   zero,   round(x)   shall   be   equivalent
		      trunc(x+0.5), otherwise round(x) shall  be  equiv
		      lent  to trunc(x-0.5).  An error shall occur if t
		      result is not a value of the type integer.  For e
		      ample:


			   round(3.7) yields 4
			   round(-3.7) yields -4



	    6.6.6.4 ORDINAL FUNCTIONS
	    -------------------------


	    ord(x)   The  parameter  x	shall	be   an   expression
		     ordinal-type.   The result shall be of type intege
		     If the parameter is of type integer,  the	value
		     the  parameter  shall  be yielded as the result.
		     the parameter is type char, the result shall be  i
		     plementation-defined.   If  the  parameter is of a
		     other ordinal-type, the result shall be the  ordin
		     number  determined by mapping the values of the ty
		     on to consecutive non-negative integers starting
		     zero.


			  ord(false) shall yield 0.
			  ord(true) shall yield 1.


	    chr(x)   shall yield the character value whose ordinal numb
		     is  equal	to the value of the integer expression
		     if such a character value exists.	If such  a  cha
		     acter value does not exist, an error shall occur.

		     NOTE:  For any character value, ch, the following
		     true:


				      chr(ord(ch)) = ch


	    succ(x)  The  parameter  x	shall	be   an   expression
		     ordinal-type.  The result shall be of a type ident
		     cal to that of  the  expression  (see  6.7.1).   T
		     function shall yield a value whose ordinal number
		     one greater than that of the expression x, if such
		     value  exists.   If  such a value does not exist,



		     error shall occur.

	    pred(x)  The  parameter  x	shall	be   an   expression
		     ordinal-type.  The result shall be of a type ident
		     cal to that of  the  expression  (see  6.7.1).   T
		     function shall yield a value whose ordinal number
		     one less than that of the expression x,  if  such
		     value  exists.   If  such a value does not exist,
		     error shall occur.



	    6.6.6.5 PREDICATES
	    ------------------


	    odd(x)   shall yield true if the integer expression x is  o
		     otherwise it shall yield false.

	    eof(f)   shall indicate whether the associated  buffer  var
		     able  f^ is positioned at the end of the file f.
		     the actual-parameter-list is omitted,  the  functi
		     shall be applied to the standard file input.

	    eoln(f)  shall indicate whether the associated  buffer  var
		     able  f^  is  positioned at the end of a line in t
		     textfile	  f	(see	 6.9.1).      If      t
		     actual-parameter-list is omitted, the function sha
		     be applied to the standard file input.



      6.7 EXPRESSIONS
      ---------------


	 6.7.1 GENERAL
	 -------------


	 Expressions  shall  consist  of  operators  and  operands   i.
	 variables,  constants, and function designators.  An error sha
	 be caused if any variable or function used as an operand  in
	 expression  has  an  undefined  value	at  the  time of its us
	 Operator precedences shall be according to four classes of ope
	 ators	as follows.  The operator NOT shall have the highest pr
	 cedence,  followed  by  the  multiplying-operators,   then   t
	 adding-operators  and signs, and finally, with the lowest prec
	 dence, the relational-operators.  Sequences of two or more ope
	 ators	of  the  same  precedence  shall be executed from left
	 right.



	    unsigned-constant = unsigned-number | string |
				  constant-identifier | "NIL" .
	    factor = variable | unsigned-constant |
		     function-designator | set | "(" expression ")" |
		     "NOT" factor .
	    set = "[" [ element { "," element } ] "]" .
	    element = expression [ ".." expression ] .
	    term = factor { multiplying-operator factor } .
	    simple-expression = [ sign ] term { adding-operator term }
	    expression =
	       simple-expression
	       [ relational-operator simple-expression ] .


	 Any operand whose type is S, where S is a subrange of	T,  sha
	 be  treated  as  if  it  were of type T.  Similarly, any opera
	 whose type is SET OF S shall be treated as if it  were  of  ty
	 SET  OF T.  Consequently an expression that consists of a sing
	 operand of type S shall itself be of type T  and  an  expressi
	 that  consists of a single operand of type SET OF S shall itse
	 be of type SET OF T.  Expressions that  are  members  of  a  s
	 shall	be  of identical type which shall be the base-type of t
	 set.  [] shall denote	the  empty  set  that  belongs	to  eve
	 set-type.   The set [x..y] shall denote the set of all values
	 the base-type in the closed interval x to y.  If  x  is  great
	 than  y  then [x..y] shall denote the empty set.  An error sha
	 be caused if the value of an expression which is the member of
	 set  is outside the implementation-defined limits.  Examples a
	 as follows:


	   (a) Factors: 	     x
	       -------		     15
				     (x+y+z)
				     sin(x+y)
				     [red,c,green]
				     [1,5,10..19,23]
				     NOT p

	   (b) Terms:		     x*y
	       -----		     i/(1-i)
				     p OR q
				     (x <= y) AND (y < z)

	   (c) Simple expressions:   x+y
	       ------------------    -x
				     hue1 + hue2
				     i*j + 1

	   (d) Expressions:	     x = 1.5
	       -----------	     p <= q
				     p = q AND r
				     (i < j) = (j < k)



				     c IN hue1



	 6.7.2 OPERATORS
	 ---------------


	    6.7.2.1 SYNTAX
	    --------------


	       multiplying-operator = "*" | "/" | "DIV" | "MOD" | "AND"

	       adding-operator = "+" | "-" | "OR" .

	       relational-operator =
		  "=" | "<>" | "<" | ">" | "<=" | ">=" | "IN" .


	    The order of evaluation of the operands of a binary  operat
	    shall be implementation-dependent.



	    6.7.2.2 ARITHMETIC OPERATORS
	    ----------------------------


	    The types of operands and results for binary and unary oper
	    tions shall be as shown in tables 2 and 3 respectively.



			     Table 2. Binary Operations
			     ---------------------------


	    operator   operation       type of operands   type of resul
	    --------   ---------       ----------------   -------------

	       +       addition        integer or real	  integer or re

	       -       subtraction     integer or real	  integer or re

	       *       multiplication  integer or real	  integer or re

	       /       division        integer or real	  real

	      DIV      division with   integer		  integer
		       truncation

	      MOD      modulo	       integer		  integer



			      Table 3. Unary Operations
			      -------------------------


	    operator   operation       type of operand	 type of result
	    --------   ---------       ---------------	 --------------

	       +       identity        integer or real	 integer or rea

	       -       sign-inversion  integer or real	 integer or rea



	    NOTE:  The symbols +, - and * are also used as set	operato
	    (see 6.7.2.4).

	    If both the operands of the addition, subtraction  or  mult
	    plication operators are of the type integer, the result sha
	    be of the type integer but if otherwise, the result shall
	    of	the  type  real.   If  the  operand  of  the  identity
	    sign-inversion operators is of the type integer,  the  resu
	    shall  be  of  the	type integer but if otherwise, the resu
	    shall be of the type real.

	    The value of i DIV j shall be such that i-j < (i DIV j)*j
	    i,	if  i >= 0 and j > 0;  an error shall be caused if j =
	    otherwise the result shall be implementation-defined.

	    The value of i MOD j shall be equal to the value of  i  -
	    DIV j)*j.



	    The predefined constant maxint shall be of integer	type  a
	    shall have an implementation-defined value.  This value sha
	    satisfy the following conditions:

	       a)  All	integral  values  in  the  closed  interval  fr
		   -maxint  to +maxint shall be representable in the ty
		   integer.

	       b)  Any unary operation performed on an integer	value
		   this  interval  shall be correctly performed accordi
		   to the mathematical rules for integer arithmetic.

	       c)  Any binary integer operation on two integer values
		   this  same  interval  shall be correctly performed a
		   cording to the mathematical rules for  integer  arit
		   metic, provided that the result is also in this inte
		   val.  If the result is not in this interval	an  err
		   shall occur.

	       d)  Any relational operation on two integer values in th
		   same  interval  shall be correctly performed accordi
		   to the mathematical rules for integer arithmetic.



	    6.7.2.3 BOOLEAN OPERATORS
	    -------------------------


	    The types of operands  and	results  for  Boolean  operatio
	    shall be as shown in table 4.


			     Table 4. Boolean Operations
			     ---------------------------


	    operator  operation 	type of operands  type of resul
	    --------  --------- 	----------------  -------------

	      OR      logical "or"	   Boolean	     Boolean

	      AND     logical "and"	   Boolean	     Boolean

	      NOT     logical negation	   Boolean	     Boolean


	    Whether a Boolean expression is completely or partially  ev
	    luated  if	its value can be determined by partial evaluati
	    shall be implementation-dependent.



	    6.7.2.4 SET OPERATORS
	    ---------------------


	    The types of operands and results for set operations shall
	    as shown in table 5.


			       Table 5. Set Operations
			       -----------------------


	    operator  operation 	type of operands  type of resul
	    --------  --------- 	----------------  -------------

	       +      set union 	any set type T		 T

	       -      set difference	any set type T		 T

	       *      set intersection	any set type T		 T



	    6.7.2.5 RELATIONAL OPERATORS
	    ----------------------------


	    The types of operands and results for  relational  operatio
	    shall be as shown in table 6.



			   Table 6. Relational Operations
			   ------------------------------


	    operator  type of operands		 type of result
	    --------  ----------------		 --------------

	      = <>    any set, simple,		 Boolean
		      pointer or string type

	      < >     any simple or string type  Boolean

	      <= >=   any set, simple or	 Boolean
		      string type

	      IN      left operand:any ordinal	 Boolean
		      type T right operand: SET
		      OF T


	    Except when applied to sets, the operators <> , <= , >= sha
	    stand for 'not equal', 'less than or equal' and 'greater th
	    or equal' respectively.  The operands of =, <>, <, >, >=, a
	    <=	shall  be either of compatible type, or one operand sha
	    be real and the other shall be integer.  If u and v  are  s
	    operands,  u  <= v shall denote the inclusion of u in v and
	    >= v shall denote the inclusion of v in u.	If p  and  q  a
	    Boolean  operands, p = q shall denote their equivalence and
	    <= q shall denote the implication of q by p, because false
	    true.  When the relational operators = , <> , < , > , <= ,
	    are used to compare strings (see 6.4.3.2), they denote  lex
	    cographic ordering according to the ordering of the charact
	    set (see 6.4.2.2).	The operator IN  shall	yield  the  val
	    true  if the value of the operand of ordinal-type is a memb
	    of the set, otherwise it shall yield  the  value  false.
	    particular,  if  the  value of the operand of ordinal-type
	    outside the implementation-defined limits,	the  operator
	    shall yield false.



	 6.7.3 FUNCTION DESIGNATORS
	 --------------------------


	 A function-designator shall specify the activation of the  fun
	 tion	  denoted     by     the     function-identifier.     T
	 function-designator shall contain a (possibly empty) list of a
	 tual-parameters that shall be substituted in place of their co
	 responding	formal	   parameters	  defined     in      t
	 function-declaration.	 The  correspondence shall be establish
	 by the positions of the parameters in the lists  of  actual  a
	 formal parameters respectively.  The number of actual-paramete



	 shall be equal to the number of formal parameters.  The order
	 evaluation  and binding of the actual-parameters shall be impl
	 mentation-dependent.


	   function-designator = function identifier
				  [ actual-parameter-list ] .
	   function-identifier = identifier .
	   actual-parameter-list =
	      "(" actual-parameter { "," actual-parameter } ")" .
	   actual-parameter = ( expresssion | variable |
				procedure-identifier |
				function-identifier ) .


	 Examples:

			Sum(a,63)
			GCD(147,k)
			sin(x+y)
			eof(f)
			ord(f^)



      6.8 STATEMENTS
      --------------


	 6.8.1 GENERAL
	 -------------


	 Statements shall denote algorithmic actions, and shall  be  ex
	 cutable.   They  may  be prefixed by a label which can be refe
	 enced by goto statements.


	   statement = [ label ":" ] ( simple-statement |
				       structured-statement ) .
	   label = unsigned-integer .



	 6.8.2 SIMPLE STATEMENTS
	 -----------------------


	    6.8.2.1 GENERAL
	    ---------------


	    A simple-statement shall be a statement of which no part co



	    stitutes another statement.  An empty statement shall consi
	    of no symbols and shall denote no action.


	      simple-statement =
		  [ ( assignment-statement |
		  procedure-statement | goto-statement ) ] .



	    6.8.2.2 ASSIGNMENT-STATEMENTS
	    -----------------------------


	    The assignment-statement shall serve to replace  the  curre
	    value  of  a  variable by a new value specified as an expre
	    sion.


	      assignment-statement =
		    ( variable | function-identifier ) ":=" expression


	    The expression shall be assignment-compatible with	the  ty
	    of	the  variable or function.  If the selection of the var
	    able involves the indexing of an array or the  de-referenci
	    of	a  pointer, the decision whether these actions precede
	    follow the evaluation of the expression shall be  implement
	    tion-dependent.


	    Examples:

		      x := y+z
		      p := (1<=i) AND (i<100)
		      i := sqr(k) - (i*j)
		   hue1 := [blue,succ(c)]



	    6.8.2.3 PROCEDURE-STATEMENTS
	    ----------------------------


	    A procedure-statement shall serve to  execute  the	procedu
	    denoted by the procedure-identifier.  The procedure-stateme
	    shall contain a (possibly empty)  list  of	actual-paramete
	    that  are  substituted in place of their corresponding form
	    parameters	defined   in   the   procedure-declaration   (s
	    6.6.3.1).  The correspondence shall be established by the p
	    sitions of the parameters in the lists of actual  and  form
	    parameters	respectively.	The  number  of actual-paramete
	    shall be equal to the number of formal parameters.	The ord



	    of	evaluation  and binding of the actual parameters shall
	    implementation-dependent.


	       procedure-statement = procedure-identifier
				      [ actual-parameter-list ] .
	       procedure-identifier = identifier .


	    Examples:


		      printheading
		      transpose(a,n,m)
		      bisect(fct,-1.0,+1.0,x)



	    6.8.2.4 GOTO STATEMENTS
	    -----------------------


	    A goto-statement shall serve to indicate  that  further  pr
	    cessing  is  to continue at another part of the program tex
	    namely at the place of the label.


			   goto-statement = "GOTO" label .


	    The following restrictions shall govern the use of labels:

	       a)  A goto-statement leading to the label which prefixes
		   statement  S  shall	cause  an  error  unless the go
		   statement is activated either by S or by  a	stateme
		   in the statement-sequence (see 6.8.3.2 and 6.8.3.8)
		   which S is an immediate constituent.

	       b)  A goto-statement shall not refer to a case-constant.



	 6.8.3 STRUCTURED-STATEMENTS
	 ---------------------------


	    6.8.3.1 GENERAL
	    ---------------


	    Structured-statements shall be constructs composed	of  oth
	    statements	that have to be executed either in sequence (co
	    pound-statement), conditionally (conditional-statements), r



	    peatedly (repetitive-statements), or within an expanded sco
	    (with-statements).


	       structured-statement =
		   compound-statement | conditional-statement |
		   repetitive-statement | with-statement .



	    6.8.3.2 COMPOUND-STATEMENTS
	    ---------------------------


	    The  compound-statement  shall  specify  that  its	compone
	    statements	are  to  be executed in the same sequence as th
	    are written.  The symbols BEGIN and END shall act  as  stat
	    ment brackets.


	      compound-statement = "BEGIN" statement-sequence "END" .
	      statement-sequence = statement { ";" statement } .


	    Example:


		 BEGIN z := x ; x := y; y := z END



	    6.8.3.3 CONDITIONAL-STATEMENTS
	    ------------------------------


	    A conditional-statement shall select for execution	a  sing
	    one of its component statements.


	       conditional-statement = if-statement | case-statement .



	    6.8.3.4 IF-STATEMENTS
	    ---------------------


	      if-statement = "IF" Boolean-expression "THEN" statement
			     [ else-part ] .
	      else-part = "ELSE" statement .


	    If the Boolean-expression yields the value true,  the  stat



	    ment   following   the   THEN  shall  be  executed.   If  t
	    Boolean-expression yields false, the action shall  depend
	    the  existence  of an else-part;  if the else-part is prese
	    the statement following the ELSE shall be executed, otherwi
	    an empty statement shall be executed.


			  Boolean-expression = expression .


	    A Boolean-expression shall be an expression which produces
	    result of type Boolean.

	    The syntactic ambiguity arising from the construct:


			  IF e1 THEN IF e2 THEN s1 ELSE s2


	    shall be resolved by  interpreting	the  construct	as  bei
	    equivalent to:


		 IF e1 THEN
		   BEGIN
		   IF e2 THEN s1 ELSE s2
		   END


	    Examples:


		 IF x < 1.5 THEN z := x+y ELSE z := 1.5
		 IF p1 <> NIL THEN p1 := p1^.father



	    6.8.3.5 CASE-STATEMENTS
	    -----------------------


	    The case-statement shall consist of an expression  (the  ca
	    index)  and  a  list  of statements.  Each statement shall
	    preceded by one or more constants.	 All  the  case-constan
	    shall  be  distinct  and  shall be of an ordinal-type that
	    identical to the type of the case-index.  The  case-stateme
	    shall  specify execution of the statement whose case-consta
	    is equal to the current value  of  the  selector.	An  err
	    shall be caused if none of the case-constants is equal to t
	    current value of the selector.


	      case-statement =



		 "CASE" expression "OF"
		 case-list-element {";" case-list-element } [";"] "END"
	      case-list-element = case-constant-list ":" statement .


	    Examples:


	      CASE operator OF		CASE i OF
		plus:	x := x+y;	  1: x := sin(x);
		minus:	x := x-y;	  2: x := cos(x);
		times:	x := x*y	  3: x := exp(x);
	      END			  4: x := ln(x)
					END



	    6.8.3.6 REPETITIVE-STATEMENTS
	    -----------------------------


	    Repetitive-statements shall specify that  certain  statemen
	    are to be executed repeatedly.


	      repetitive-statement = while-statement |
				      repeat-statement | for-statement



	    6.8.3.7 REPEAT-STATEMENTS
	    -------------------------


	      repeat-statement = "REPEAT" statement-sequence
				 "UNTIL" Boolean-expression .


	    The sequence of statements between the symbols REPEAT and U
	    TIL shall be repeatedly executed until the Boolean-expressi
	    yields   the   value    true    on	  completion	of    t
	    statement-sequence.  The statement-sequence shall be execut
	    at least once, because the	Boolean-expression  is	evaluat
	    after execution of the statement-sequence.


	    Examples:

	      REPEAT k := i MOD j;
		i := j;
		j := k
	      UNTIL j = 0



	      REPEAT
		process(f^);
		get(f)
	      UNTIL eof(f)



	    6.8.3.8 WHILE STATEMENTS
	    ------------------------


	    while-statement = "WHILE" Boolean-expression "DO" statement


	    The statement shall be repeatedly executed while the  Boole
	    expression	yields	the value true.  If its value is false
	    the beginning, the statement shall not be executed.

	    The while-statement:


				   WHILE b DO body


	    shall be equivalent to:


		 IF b THEN
		   REPEAT
		   body
		   UNTIL NOT b


	    Examples:


		 WHILE a[i] <> x DO i := i+1

		 WHILE i>0 DO
		   BEGIN IF odd(i) THEN z := z*x;
		     i := i DIV 2;
		     x := sqr(x)
		   END

		 WHILE NOT eof(f) DO
		   BEGIN process(f^ ); get(f)
		   END



	    6.8.3.9 FOR-STATEMENTS
	    ----------------------


	    The for-statement shall indicate that a statement  is  to
	    repeatedly executed while a progression of values is assign
	    to a variable which is called  the	control-variable  of  t
	    for-statement.


	      for-statement = "FOR" control-variable ":=" initial-value
			      ( "TO | "DOWNTO" ) final-value
			      "DO" statement .
	      control-variable = entire-variable .
	      initial-value = expression .
	      final-value = expression .


	    The control-variable shall be an entire-variable that is  l
	    cal to the immediately enclosing block.  The control-variab
	    shall be of ordinal-type, and the  initial	and  final  val
	    shall be of a type compatible with this type.  An error sha
	    be caused if the control-variable is assigned to by the rep
	    ated  statement or altered by any procedure or function act
	    vated by the repeated statement.  After  a	for-statement
	    executed  (other  than being left by a goto statement leadi
	    out of it) the value of the control-variable  shall  be  le
	    undefined.	 Apart from the restrictions imposed by these r
	    quirements, the for-statement:


			      FOR v := e1 TO e2 DO body


	    shall be equivalent to:


	      BEGIN
	      temp1 := e1;
	      temp2 := e2;
	      IF temp1 <= temp2 THEN
		BEGIN
		v := temp1;
		body;
		WHILE v <> temp2 DO
		  BEGIN
		  v := succ(v);
		  body
		  END
		END
	      END



	    and the for-statement:


	      FOR v := e1 DOWNTO e2 DO body


	    shall be equivalent to:


	      BEGIN
	      temp1 := e1;
	      temp2 := e2;
	      IF temp1 >= temp2 THEN
		BEGIN
		v := temp1;
		body;
		WHILE v <> temp2 DO
		  BEGIN
		  v := pred(v);
		  body
		  END
		END
	      END


	    where temp1 and temp2 are auxiliary  variables  of	the  ho
	    type  of  the  variable  v that do not occur elsewhere in t
	    program.

	    Examples of for-statements are:


	      FOR i := 2 TO 63 DO
		IF a[i] > max THEN max := a[i]

	      FOR i := 1 TO n DO
		FOR j := 1 TO n DO
		  BEGIN
		  x := 0;
		  FOR k := 1 TO n DO
		    x := x + m1[i,k]*m2[k,j];
		  m[i,j] := x
		  END

	      FOR c := red TO blue DO q(c)



	    6.8.3.10 WITH-STATEMENTS
	    ------------------------


	      with-statement =



		   "WITH" record-variable-list "DO"
		   statement .
	      record-variable-list =
		   record-variable { "," record-variable } .
	      variable-identifier = field-identifier .


	    The    occurrence	 of    a    record-variable    in     t
	    record-variable-list  shall  be  a defining occurrence of i
	    field-identifiers	 as    variable-identifiers    for    t
	    with-statement in which the record-variable-list occurs.

	    The statement:


		 WITH v1,v2, ....vn DO s


	    shall be equivalent to:


		 WITH v1 DO
		   WITH v2 DO
		     ....
		       WITH vn DO s


	    Example:


		WITH date DO
		IF month = 12 THEN
		  BEGIN month := 1; year := year + 1
		  END
		ELSE month := month + 1


	    shall be equivalent to:


		IF date.month = 12 THEN
		  BEGIN date.month := 1; date.year := date.year+1
		  END
		ELSE date.month := date.month+1


	    If the selection of a variable  in	the  record-variable-li
	    involves  the indexing of an array or the de-referencing of
	    pointer, these actions shall be executed before the compone
	    statement is executed.



      6.9 INPUT AND OUTPUT
      --------------------


	 6.9.1 GENERAL
	 -------------


	 The basis of legible input and output shall consist of textfil
	 (see  6.4.3.5)  that  shall be passed as program-parameters (s
	 6.10) to a Pascal program and that in	the  environment  of  t
	 program  shall  represent  some input or output device such as
	 terminal, a card reader, a line printer or a file in the backi
	 store.

	 If, while reading a textfile f, the file position is advanced
	 a line-marker (i.e.  past the last character of a line), the v
	 lue of the buffer variable f^ shall become a space, and  eoln(
	 shall	yield true.  If the current file position is already at
	 line-marker, advancing the file position shall cause one of  t
	 following to occur:

	    a)	the current file position to be positioned at the end
		the file f

	    b)	or the current file position to be positioned at  anoth
		line-marker

	    c)	or the first character of the next line to be assigned
		the buffer variable f^.

	 NOTE:	Line-markers may be generated only by the standard proc
	 dure writeln.



	 6.9.2 THE PROCEDURE READ
	 ------------------------


	 The syntax of the parameter list of read shall be:


	   read-parameter-list =
	       "("[file-variable ","] variable {"," variable}")" .


	 The following requirements shall apply for the procedure read
	 denotes a textfile and v1..vn denote variables of the types ch
	 (or a subrange of char), integer (or a subrange of integer),
	 real):

	    a)	read(f,v1,...,vn) shall be equivalent to:



			 BEGIN read(f,v1); ... ; read(f,vn) END


	    b)	If v is a variable of type  char  or  subrange	of  cha
		read(f,v) shall be equivalent to:


				BEGIN v := f^; get(f) END


	    c)	If v is a variable of type integer (or subrange thereof
		read(f,v) shall imply the reading from f of a sequence
		characters that form a signed-integer  according  to  t
		syntax	of  6.1.5.  The value of the integer shall be a
		signed to v, if the value is  assignment-compatible  wi
		the  type  of v.  Preceding spaces and line-markers sha
		be skipped.  Reading shall cease  as  soon  as	the  fi
		buffer	variable  f^  contains	a character that does n
		form part of a signed-integer.	An error shall	occur
		the sequence of characters does not form a signed-integ
		as specified in 6.1.5.

	    d)	If v is a variable of type real,  read(f,v)  shall  imp
		the  reading from f of a sequence of characters that fo
		a signed-number according to the syntax  of  6.1.5.   T
		value  of the number shall be assigned to the variable
		Preceding  spaces  and	line-markers  shall  be  skippe
		Reading  shall	cease as soon as the file buffer variab
		f^ contains a character that does  not	form  part  of
		signed-number.	 An  error shall occur if the sequence
		characters does not form a signed-number as specified
		6.1.5.

	 The procedure read may also be used to read from a file  f  th
	 is  not  a textfile.  In this case read(f,x) shall be equivale
	 to:


			    BEGIN x := f^; get(f) END


	 If the file-variable is omitted, the procedure shall be  appli
	 to the standard file input.



	 6.9.3 THE PROCEDURE READLN
	 --------------------------


	 The syntax of the parameter list of readln shall be:



	   readln-parameter-list =
	      ["(" (file-variable | variable) {"," variable} ")"] .


	 readln(f,v1,...,vn) shall be equivalent to:


	     BEGIN read(f,v1,...,vn); readln(f) END


	 readln(f) shall be equivalent to:


	     BEGIN WHILE NOT eoln(f) DO get(f); get(f) END


	 Readln shall cause a skip to the beginning of the next line.

	 If the file-variable or parameter-list is omitted, the procedu
	 shall be applied to the standard file input.



	 6.9.4 THE PROCEDURE WRITE
	 -------------------------


	 The syntax of the parameter list of write shall be:


	   write-parameter-list =
	      "("[file-variable ","] write-parameter
	      {"," write-parameter}")" .
	   write-parameter =
	      expression [":" expression [":" expression ] ] .


	 The following requirements shall apply for the  procedure  wri
	 (f  denotes a textfile, p1,...,pn denote write-parameters, e d
	 notes	an  expression,  m  and  n  denote  expressions  of  ty
	 integer):

	    a)	write(f,p1,...,pn) shall be equivalent to:


			BEGIN write(f,p1); ... ; write(f,pn) END


	    b)	The write-parameters p shall have the following forms:


				       e:m e:m:n e



		where e is an expression whose value is to be written
		the  file  f  and may be numeric (integer or real), cha
		Boolean or a string, where m and n are	expressons  who
		integer  values are the field-width parameters, and whe
		their values are greater than zero.  Exactly m characte
		shall be written (with an appropriate number of spaces
		the left of the representation of e), except when e is
		numeric  value	that  requires more than m characters f
		its representation;  in such cases the	number	of  cha
		acters	written  shall	be as small as is consistent wi
		the representation of e (see requirements d and e).

		write(f,e) shall be equivalent to write(f,e:m),  using
		default  value	for m that depends on the type of e;  f
		integer, Boolean and real types this value shall  be  i
		plementation-defined.

		write(f,e:m:n) shall be applicable only if e is  of  ty
		real (see requirement e).

	    c)	If e is of type char, the default value for  m	shall
		one.

	    d)	If e is of type integer, the  decimal  representation
		the number e shall be written on the file f.  If p is t
		positive integer defined by:


		   IF e = 0
		     THEN p := 1
		     ELSE determine p such that 10**(p-1)
		       <= abs(e) < 10**p


		the representation shall consist of:


		   (1) if m >= p + 1
		   (m-p-1) spaces,
		   the sign character ('-' if e<0, otherwise a space),
		   p digits.

		   (2)If m<p+1, p characters shall be written if e>=0,
		   (p+1) if e<0.


	    e)	If e is of type real, a  decimal  representation  of  t
		number	e, rounded to the specified number of significa
		figures or decimal places, shall be written on	the  fi
		f.

		write(f,e:m) shall cause a floating-point  representati
		of  e  to  be written.	If d is an implementation-defin



		value (representing the number of digit characters  wri
		ten  in  an exponent), and the non-negative number er a
		the integer p are defined by:


		   IF e = 0.0
		     THEN BEGIN er := 0.0; p := 1 END
		     ELSE
		     BEGIN
		       er := abs(e);
		       determine p such that 10**(p-1) <= er < 10**p;
		       er := er + 0.5 * (10**(p-m+d+4));
		       er is truncated to (m-d-4) significant
			 decimal figures
		     END


		the representation shall consist of:


		   (1) if m >= d + 6:
		   the sign character
		      ('-' if e<0 and er<>0, otherwise a space),
		   the leading digit of er,
		   the character '.',
		   the next (m-d-5) digits of er,
		   the character 'E',
		   the sign of (p - 1) ('+' or '-'),
		   d digits for (p - 1)
		      (with leading zeros if necessary).

		   (2)If m<d+6, (d+6) characters shall be written,
		   including one digit after the decimal point.


		write(f, e:m:n) shall cause a fixed-point  representati
		of  e  to  be written.	If the non-negative number er a
		the positive integer p are defined by:


		   IF e = 0.0
		     THEN er := 0.0
		     ELSE
		     BEGIN
		       er := abs(e);
		       er := er + 0.5 * 10**(-n);
		       er is truncated to n decimal places
		     END;
		   IF trunc(er) = 0
		     THEN p := 1
		     ELSE determine p such that 10**(p-1)
		       <= trunc(er) < 10**p




		the representation shall consist of:


		   (1) if m >= p+n+2:
		   (m-p-n-2) spaces,
		   the sign character
		      ('-' if e<0 and er<>0, otherwise a space),
		   the first p digits of er,
		   the character '.',
		   the next n digits of er.

		   (2)If m<p+n+2, (p+n+2) characters shall be written.


	    f)	If e is of type Boolean, a  representation  of	the  wo
		true  or the word false (as appropriate) shall be writt
		on the file f.	This shall be equivalent to:


			write(f,'TRUE ':m) or write(f,'FALSE':m)


		as appropriate (see requirement g.).

	    g)	If e is of string type, the value of e shall  be  writt
		on  the file f preceded by (m-n) spaces if m>=n.  If m<
		characters one through m of the string shall be  writte
		If m is omitted, the default value shall be n.

	 The procedure write may also be used to write on  to  a  file
	 which is not a textfile.  In this case write(f,x) shall be equ
	 valent to:


			    BEGIN f^ := x; put(f) END


	 If the file-variable is omitted, the procedure shall be  appli
	 to the standard file output.



	 6.9.5 THE PROCEDURE WRITELN
	 ---------------------------


	 The syntax of the parameter list of writeln shall be:


	    writeln-parameter-list =
	       ["(" (file-variable | write-parameter)
	       {"," write-parameter}")"] .





	 writeln(f,p1,...,pn) shall be equivalent to:


	    BEGIN write(f,p1,...,pn); writeln(f) END


	 writeln(f) shall append a line-marker (see 6.4.3.5)
	 to the file f.


	 If the file-variable or the parameter-list is omitted, the  pr
	 cedure shall be applied to the standard file output.
	 6.9.6 THE PROCEDURE PAGE
	 ------------------------


	 page(f)  shall cause skipping to the top of a new page when  t
		  textfile f is printed.  If the actual-parameter-list
		  omitted the procedure shall be applied to the  standa
		  file output.



      6.10 PROGRAMS
      -------------


      A Pascal program shall have the form of a procedure declaration e
      cept for its heading.


	 program = program-heading ";" block "." .

	 program-heading =
	    "PROGRAM" identifier [ "(" program-parameters ")" ] .

	 program-parameters = identifier-list .


      The identifier following the symbol PROGRAM shall  be  the  progr
      name   which   has   no	significance  within  the  program.   T
      program-parameters shall denote entities	that  exist  outside  t
      program,	and  through  which communication with the environment
      the program is possible.	These entities (usually files)	shall
      external, and shall be declared in the variable-declaration-part
      the block which constitutes the program.	The  two  standard  fil
      input  and output shall not be declared explicitly (see 6.9.1), b
      shall be listed as parameters in the program-heading  if	they  a



      used  in	the program.  The appearance of the files input or outp
      as program-parameters shall cause them to be declared in	the  pr
      gram   block.    The   initializing   statements	reset(input)  a
      rewrite(output) shall be automatically generated if required.   T
      effect of an explicit use of reset or rewrite on the standard fil
      input or output shall be implementation-dependent.


      Examples:

	PROGRAM copy(f,g);
	VAR f,g: FILE OF real;
	BEGIN reset(f); rewrite(g);
	  WHILE NOT eof(f) DO
	    BEGIN g^ := f^; get(f); put(g)
	    END
	END.

	PROGRAM copytext(input,output);
	 {This program copies the characters and line-markers
	  of the textfile input to the textfile output.}
	VAR ch: char;
	BEGIN
	  WHILE NOT eof(input) DO
	  BEGIN
	    WHILE NOT eoln(input) DO
	      BEGIN read(ch); write(ch)
	      END;
	    readln; writeln
	  END
	END.



      6.11 HARDWARE REPRESENTATION
      ----------------------------


      The representation for tokens and separators given in  6.1  const
      tutes  a	reference  representation.  This reference representati
      shall be used for publication or interchange of programs written
      Pascal.	Only  in  cases where the character set is insufficient
      rich to permit the full use of the reference  language,  shall  t
      character  combinations given in table 7 be used as substitute sy
      bols.  In the case of symbols which are pairwise matching, such
      comment  markers,  the  pairwise	sets  shall be either both in t
      reference representation, or both in the substitute symbols.



			  Table 7. Substitute Symbols
			  ---------------------------


	     Reference Symbol	 Substitute Symbol
	     ----------------	 -----------------

		^		    @ or ^
		{   }		    (*	 *)
		[		    (.
		]		    .)
		:=		    .= or %=
		;		    .,
		:		    %
		>		    GT
		<		    LT
		>=		    GE
		<=		    LE
		<>		    NE+ Z?