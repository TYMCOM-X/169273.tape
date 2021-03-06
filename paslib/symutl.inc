(*SYMUTL.INC, last modified 10/18/83, zw*)
$IFNOT symutlinc

$SYSTEM PASUTL.TYP
$SYSTEM SYMUTL.TYP

EXTERNAL PROCEDURE namtosym(namtyp; VAR symtyp);

EXTERNAL PROCEDURE symtonam(symtyp; VAR namtyp);

EXTERNAL PROCEDURE wrsym(symtyp);

EXTERNAL PROCEDURE rdsym(VAR symtyp);

EXTERNAL PROCEDURE wrset(settyp);

EXTERNAL PROCEDURE rdset(VAR settyp);

EXTERNAL PROCEDURE wrlst(lsttyp);

EXTERNAL PROCEDURE rdlst(VAR lsttyp);

EXTERNAL PROCEDURE wrprolst(prolsttyp);

EXTERNAL PROCEDURE rdprolst(VAR prolsttyp);

EXTERNAL PROCEDURE wrpro(protyp);

EXTERNAL PROCEDURE rdpro(VAR protyp);

EXTERNAL PROCEDURE wrproset(prosettyp);

EXTERNAL PROCEDURE rdproset(VAR prosettyp);

EXTERNAL PROCEDURE wrgmr(gmrtyp);

EXTERNAL PROCEDURE rdgmr(VAR gmrtyp);

EXTERNAL PROCEDURE wrtre(tretyp);

EXTERNAL PROCEDURE rdtre(VAR tretyp);

EXTERNAL PROCEDURE tresym(tretyp);

EXTERNAL PROCEDURE fmtsym;

$ENABLE symutlinc
$ENDIF
  