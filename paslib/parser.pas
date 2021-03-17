$WIDTH=100
$LENGTH=55
$TITLE PARSER.PAS, last modified 10/14/83, zw

PROGRAM parser;
(*this is an LL(k,j) parser interpreter*)

$SYSTEM PASUTL
$SYSTEM SYMUTL


FUNCTION startsymbol(gmr: gmrtyp): symtyp;

BEGIN
(*xxx*)
  startsymbol := NIL
END;

FUNCTION isterminal(sym: symtyp; gmr: gmrtyp): bin;

BEGIN
(*xxx*)
  isterminal := TRUE
END;

FUNCTION findproduction(sym: symtyp; gmr: gmrtyp; lst: lsttyp): protyp;

BEGIN
(*xxx*)
END;

PROCEDURE apply(opr: PROCEDURE(prosymtyp); lst: prolsttyp);

BEGIN
(*xxx*)
END;

PROCEDURE llkj(k, j: int; gmr: gmrtyp; lst: lsttyp; VAR tre: tretyp);

VAR
    currentlist: lsttyp;


  PROCEDURE advancelist;

  BEGIN
  (*xxx*)
  END;


  PROCEDURE firsterrorcheck;

  BEGIN
  (*xxx*)
  END;


  PROCEDURE lasterrorcheck;

  BEGIN
  (*xxx*)
  END;


  PROCEDURE parse(sym: symtyp; VAR tre: tretyp);


    PROCEDURE parseproduction(pro: protyp; VAR tre: tretyp);

    VAR
	tmptre: tretyp;


      PROCEDURE errorparse(prosym: prosymtyp);


	PROCEDURE errorcheck;

	BEGIN
	(*xxx*)
	END;

      BEGIN (*error parse*)
	errorcheck;
	WITH prosym DO BEGIN
	  IF tmptre = NIL THEN BEGIN
	    parse(sym, tmptre);
	    tre := tmptre
	  END
	  ELSE BEGIN
	    parse(sym, tmptre^.nxt);
	    tmptre := tmptre^.nxt
	  END
	END
      END;

    BEGIN (*parseproduction*)
      tmptre := NIL;
      apply(errorparse, pro.lst)
    END;


    PROCEDURE parsenonterminal(sym: symtyp; VAR tre: tretyp);

    BEGIN
      parseproduction(findproduction(sym, gmr, currentlist), tre)
    END;


    PROCEDURE parseterminal(sym: symtyp; VAR tre: tretyp);

    BEGIN
    (*assume that 'sym' matches current input symbol*)
      NEW(tre);
      tre^.ister := TRUE;
      tre^.sym := sym;
      tre^.nxt := NIL;
      tre^.lst := NIL;
      advancelist
    END;

  BEGIN (*parse*)
    IF isterminal(sym, gmr) THEN
      parseterminal(sym, tre)
    ELSE
      parsenonterminal(sym, tre)
  END;

BEGIN (*llkj*)
  currentlist := lst;
  firsterrorcheck;
  parse(startsymbol(gmr), tre);
  lasterrorcheck
END;

VAR
    lst: lsttyp;
    gmr: gmrtyp;
    tre: tretyp;
    k, j: int;
    optkeys: ARRAY[1 .. 2] OF wrdtyp := ('K', 'J');
    opts: ARRAY[1 .. 2] OF int := (0, 0);

BEGIN
  IF start('PARSER', '') THEN BEGIN
    getopt(optkeys, opts);
    optnum(k, 1, opts[1]);
    optnum(j, 1, opts[2]);
    scan('');
    rdgmr(gmr);
    rdlst(lst);
    wrgmr(gmr);
    wrlst(lst);
    llkj(k, j, gmr, lst, tre);
    wrtre(tre);
    tresym(tre)
  END
END.
