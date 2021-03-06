$WIDTH=100
$LENGTH=55
$TITLE calc.pas, last modified 6/12/83, zw
PROGRAM calc;
  (*a simple calculator*)

EXTERNAL FUNCTION star(prognam, hlpfil: STRING[*]): BOOLEAN;

FUNCTION readlin(VAR fil: TEXT; VAR lin: STRING[*]): BOOLEAN;
  (*read a line of input, return FALSE if EOF*)
  BEGIN
    readlin := NOT EOF(fil); IF NOT readlin THEN RETURN;
    IF fil = TTY THEN BEGIN
      BREAK(TTYOUTPUT); READLN(TTY); READ(TTY, lin);
      readlin := NOT ((lin <> '') ANDIF (lin[1] = '.') ANDIF
        (UPPERCASE(SUBSTR(lin, 2)) = 'END'))
      END
    ELSE READLN(fil, lin)
    END;

VAR lin: STRING[80]; (*input line*)

TYPE (*scan structure*)
  scntabtyp = ARRAY [scnstate, char] OF scnaction
  scnid = (identifier, number, opchar);
  scnrcd = RECORD next: ^scnrcd; CASE tkntyp: tknid OF
    identifier: (idval: STRING[20]);
    number: (numval: REAL);
    opchar: (opval: CHAR)
    END;
VAR scnlst: ^scnrcd := NIL;
CONST scntab: scntabtyp := ();
  


TYPE (*parse structure*)
  partabtyp = ARRAY [parstate, scnid] OF paraction;
  parid = (variable, constant, plus, minus, multiply, divide, assign, enter);
  parrcd = RECORD CASE tkntyp: tknid OF
    variable: (varval: ^varrcd);
    constant: (conval: REAL);
    plus: (add1, add2: ^parrcd);
    minus: (sub1, sub2: ^parrcd);
    multiply: (mul1, mul2: ^parrcd);
    divide: (div1, div2: ^parrcd);
    assign: (assvar: ^varrcd; assexp: ^parrcd);
    enter: ()
    END;
VAR partre: ^parrcd;
CONST partab: partabtyp := ();

TYPE (*execution structure*)
(*exercd*)
(*the stack*)
VAR exelst: ^exercd;

PROCEDURE scan;
  (*scan input line to get scan list*)
  BEGIN
    EXCEPTION
      ALLCONDITIONS: WRITELN('% Scan error.')
    END;

PROCEDURE parse;
  (*parse scan list to get parse tree*)
  BEGIN
    EXCEPTION
      ALLCONDITIONS: WRITELN('% Parse error.')
    END;

PROCEDURE translate;
  (*translate the parse tree into an execution list*)
  PROCEDURE flatten(partre: ^parrcd; VAR exelst: ^exercd);
  (*recursive procedure to flatten parse tree to get execution list*)
    BEGIN
      END;
  BEGIN
    flatten(partre, exelst)
    EXCEPTION
      ALLCONDITIONS: WRITELN('% Translation error.')
    END;

PROCEDURE execute;
  (*execute instructions from execution list*)
  VAR exeptr: ^exercd;
  BEGIN
    exeptr := exelst;
    WHILE exeptr <> NIL DO BEGIN
      CASE exeptr^.parid OF
        constant: push(exeptr^.conval);
        variable: push(exeptr^.varval^.number);
        assign: exeptr^.assvar^.number := pop;
        display: WRITELN(pop);
        enter: push(topstk);
        plus: push(pop + pop);
        minus: push(-pop + pop);
        multiply: push(pop * pop);
        divide: push(1/pop * pop)
        END
      END
    EXCEPTION
      ALLCONDITIONS: WRITELN('% Execution error.')
    END;

BEGIN
  WHILE star('CALC', 'CALC.HLP') DO BEGIN
    clear;
    WHILE readlin(lin) DO BEGIN
      scan; parse; translate; execute
      END
    END
  END.

            