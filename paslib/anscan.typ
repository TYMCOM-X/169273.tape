(********** lexical analyzer type definitions **********)

type options = (genlist,gentables,fortables,symnames);	(* compiler options *)

$include anal.sym

type  setofsymbols = set of symbols;

type tokenstring = string[64];			(* token chars *)
     tokendesc = record				(*token descriptor returned by scan*)
		   tokentext: tokenstring;
		   tokensym: symbols
		 end;

type error =
   (* lexical error messages *)
    ( badchar, noprime, badsym, toomany, nocarat,
   (* syntactic error messages *)
    synerror,					(*the catchall when there's nothing better*)
    expgramsy, exprule, expcolonequals, expsemicolon, expsymbol,
    exprightparen, exprightbracket, expsemcode, scalarnameexp,
   (* semantic error messages *)
      gnsoflo, gncodcnflct, gnredef, gnsymcnflct, gncodincomp,
      gnundefter, gnundefcod, gnundefcls);
