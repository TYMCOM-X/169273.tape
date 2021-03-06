$WIDTH=100
$LENGTH=55
$TITLE symbol.pas, last modified 9/20/83, zw
MODULE symbol;
(*symbolic manipulation utility*)

$SYSTEM pasutl

(*getlin -- try to get next INPUT line buffer*)
(*getch -- try to get next character from INPUT line buffer*)
(*scan -- scan next symbol from INPUT line buffer*)
(*lkpsym -- lookup symbol name in table*)
(*ldsym -- load symbol from INPUT*)
(*stsym -- store symbol to OUTPUT*)
(*newexp -- create new expression cell*)
(*delexp -- delete entire expression*)
(*ldexp -- load expression from INPUT*)
(*stexp -- store expression to OUTPUT*)

$PAGE external declaration examples

(*EXTERNAL FUNCTION getlin: BOOLEAN;*)
(*EXTERNAL FUNCTION getch: BOOLEAN;*)
(*EXTERNAL PROCEDURE scan(VAR sym_nam_typ);*)
(*EXTERNAL FUNCTION lkpsym(sym_nam_typ): sym_typ;*)
(*EXTERNAL PROCEDURE ldsym(VAR sym_typ);*)
(*EXTERNAL PROCEDURE stsym(sym_typ);*)
(*EXTERNAL FUNCTION newexp(exp_typ, exp_typ): exp_typ;*)
(*EXTERNAL PROCEDURE delexp(exp_typ);*)
(*EXTERNAL PROCEDURE ldexp(VAR exp_typ);*)
(*EXTERNAL PROCEDURE stexp(exp_typ);*)

$PAGE constants, types and variables

$PAGE public declarations

PUBLIC FUNCTION getlin: BOOLEAN; FORWARD;
PUBLIC FUNCTION getch: BOOLEAN; FORWARD;
PUBLIC PROCEDURE scan(VAR s: sym_nam_typ); FORWARD;
PUBLIC FUNCTION lkpsym(s: sym_nam_typ): sym_typ; FORWARD;
PUBLIC PROCEDURE ldsym(VAR s: sym_typ); FORWARD;
PUBLIC PROCEDURE stsym(s: sym_typ); FORWARD;
PUBLIC FUNCTION newexp(l, r: exp_typ): exp_typ; FORWARD;
PUBLIC PROCEDURE delexp(e: exp_typ); FORWARD;
PUBLIC PROCEDURE ldexp(VAR e: exp_typ); FORWARD;
PUBLIC PROCEDURE stexp(e: exp_typ); FORWARD;

$PAGE getlin, getch

(*getlin -- try to get next INPUT line buffer*)
FUNCTION getlin: BOOLEAN;
BEGIN
END;

(*getch -- try to get next character from INPUT line buffer*)
FUNCTION getch: BOOLEAN;
BEGIN
END;

$PAGE scan

(*scan -- scan next symbol from INPUT line buffer*)
PROCEDURE scan(VAR s: sym_nam_typ);
BEGIN
END;

$PAGE lkpsym

(*lkpsym -- lookup symbol name in table*)
FUNCTION lkpsym(s: sym_nam_typ): sym_typ;
BEGIN
END;

$PAGE ldsym, stsym

(*ldsym -- load symbol from INPUT*)
PROCEDURE ldsym(VAR s: sym_typ);
BEGIN
END;

(*stsym -- store symbol to OUTPUT*)
PROCEDURE stsym(s: sym_typ);
BEGIN
END;

$PAGE newexp, delexp

(*newexp -- create new expression cell*)
FUNCTION newexp(l, r: exp_typ): exp_typ;
BEGIN
END;

(*delexp -- delete entire expression*)
PROCEDURE delexp(e: exp_typ);
BEGIN
END;

$PAGE ldexp, stexp

(*ldexp -- load expression from INPUT*)
PROCEDURE ldexp(VAR e: exp_typ);
VAR s: sym_typ;
BEGIN
  e := NIL;
  IF nxtsym <> lpar_sym THEN BEGIN
    ldsym(s); e := symexp(s)
  END
  ELSE BEGIN
    ldsym(s);
    WHILE nxtsym <> rpar_sym DO BEGIN
      ldexp(e1); ldexp(e2); e := newexp(e1, e2)
    END
  END;
END;

(*stexp -- store expression to OUTPUT*)
PROCEDURE stexp(e: exp_typ);
VAR ee: exp_typ;
BEGIN
  IF e = NIL THEN stsym(nil_sym)
  ELSE IF e^.is_sym THEN stsym(e^.sym)
  ELSE BEGIN
    ee := e;
    wrstr('(');
    WHILE ee := NIL DO BEGIN
      IF ee^.left = NIL THEN stsym(nil_sym)
      ELSE IF ee^.left^.is_sym THEN stsym(ee^.sym)
      ELSE stexp(ee);
      ee := ee^.right;
      IF ee <> NIL THEN BEGIN
        IF ee^.is_sym THEN BEGIN
	  stsym(dot_sym); stsym(ee^.sym); ee := NIL
	END
      END
    END;
    wrstr(')');
  END;
  wrlin('')
END.
 