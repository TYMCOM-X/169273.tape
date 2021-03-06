$TITLE SCNPAR - SCANNR Input Parser
$LENGTH 43

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         S C N P A R                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     MDSI, Company Confidential
     
     SYSTEM:  SCANNR Lexical Scanner Builder
     
     STARTED:  7 August 1978
     
     PURPOSE:  This  module  contains  the  parser  for  the  lexical
        scanner description language.  The actual parser is generated
        using the  SLR(1)  parser  generator  package.  The  language
        syntax is specified in file SCANNR.BNF.
     
     ENTRY POINTS:
     
        parser      is a public wrapper for the Parse function, which
                    does the actual work.
     
     EFFECTS:  Parser sets the following global values:
     
        Title will be set to a pointer to a  string  node  containing
        the title string for the scanner.
     
        InputMode  will be set to AsciiMode if 'ALPHABET IS ASCII' is
        specified,  to  NumericMode  if  'ALPHABET  IS  NUMERIC'   is
        specified,  to  SymbolicMode  if  'ALPHABET  IS <symbols>' is
        specified, and to UndefinedMode if no alphabet or  more  than
        one  alphabet  is  specified.  If  'ALPHABET IS <symbols>' is
        specified, then the individual symbols will have been defined
        as  alphabet  symbols  via  EnterSymbol,  so that they can be
        retrieved for output later.
     
        MinSymbol and MaxSymbol will be set to the numeric values  of
        the  smallest  and  largest  symbols  which  are  used in any
        regular expressions.
     
        Any  names  which  are  defined   in   '<name>   =   <regular
        expression>'  definitions  will  be  entered in the name list
        with EnterName.
     
        Any regular expressions  with  associated  actions  (possibly
        parameterized) will be recorded with NewScanner.
     
     ---------------------------------------------------------------- *)
$PAGE declarations

$INCLUDE scannr.typ
$INCLUDE scnlit.typ
$INCLUDE scntok.typ

$INCLUDE scnerr
$INCLUDE scnlex
$INCLUDE scnlst
$INCLUDE scnnam
$INCLUDE scnrea
$INCLUDE scnreu
$INCLUDE scnpat
$PAGE parse

const
    stk_max = 40;

type
    stk_node = sym_value;

$INCLUDE lrpars.inc

procedure parse_error;
begin
  err_token (invalue, 'Syntax error');
end;

var itemp1, itemp2, itemp3: number; (* Working variables. *)

$INCLUDE scannr.par
$PAGE parser

public procedure parser;

var result: sym_value;

begin
  if parse (result) then;
end.
    