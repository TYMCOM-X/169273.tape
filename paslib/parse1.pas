$WIDTH=100
$LENGTH=55
$TITLE parser.pas, last modified 8/18/83, zw

PROGRAM parser;
(*LL(1) parser for regular, context-free grammars*)

(*by Zon Williamson, (C) 1983, TYMSHARE Inc., Cupertino, CA*)

(*
This simple implementation is to be used as a guide to the algorithms
used in the TYM-PASCAL compiler.
*)

(*
input grammar and symbol string from file "PARSER.DAT"
output diagnostics and parse tree to "TTY:"
*)

(*
The format of an input grammar is as follows:
<grammar> ::= ( <non-term set> <term set> <prod set> <start sym> )
<non-term set> ::= ( <sym list> )
<term set> ::= ( <sym list> )
<prod set> ::= ( <prod list> )
<start sym> ::= <sym name>
<sym list> ::= <null> | <sym name> <sym list>
<prod list> ::= <null> | <prod> <prod list>
<prod> ::= ( <sym name> <str list> )
<str list> ::= <null> | <sym str> <str list>
<sym str> ::= ( <sym list> )
*)

(*
The format of an input string is as follows:
<sym str> ::= ( <sym list> )
<sym list> ::= <null> | <sym name> <sym list>
*)

$PAGE description of data structures and terminology

(*
Symbols are the things which are manipulated by this program.  Think of them
as words or numbers.  A grammar consists of four things -- the terminal set,
the non-terminal set, the production set and the start symbol.  The set of all
symbols is divided into two subsets -- the terminals and the non-terminals.
For each non-terminal symbol there is a corresponding element in the
production set.  An element of the production set is a list of strings of
symbols.  A production defines a non-terminal to be represented by any one of
its production strings.
*)

$PAGE description of parsing algorithm

(*
Parse the start symbol of the grammar.  To parse a terminal symbol, check
to see if it matches the current token, if so, advance to the next token
and set parser-ok flag, otherwise, reset parser-ok flag.  A non-terminal
symbol is parsed by successfully parsing all of the symbols in one of its
production strings.  The production strings are tried one after another
until either the parse-ok flag is set or the list is exausted.
*)

$PAGE constant parameters

CONST

  (*input and output file names*)
  in_fil = 'PARSER.DAT'; out_fil = 'TTY:';

  (*token backup limit -- size of token stack*)
  bkp_lim = 10;

  (*total number of symbols -- terminals plus non-terminals*)
  num_syms = 10;

  (*maximum symbol name length*)
  nam_len = 20;

  (*end-of-string symbol*)
  eos_nam = 'EOS'; eos_sym = 1;

  (*maximum input and output line lengths*)
  ibuf_siz = 60; obuf_siz = 60;

  (*end-of-file marker for INPUT -- set by get_ch*)
  eof_ch = '#';

  (*character sets used by input scanner*)
  delim = [' ']; rmrkr = ['(', '[', '{']; lmrkr = [')', ']', '}'];

$PAGE data type definitions

TYPE

  (*symbols represented internally as numbers, externally as names*)
  sym_typ = 1 .. num_syms; nam_typ = STRING[nam_len];

  (*symbol table -- cross-refrence of symbol names and numbers*)
  sym_tab_typ = ARRAY[sym_typ] OF nam_typ;

  (*symbol sets have TRUE entries if indexing symbols are members*)
  sym_set_typ = ARRAY[sym_typ] OF BOOLEAN;

  (*symbol string are linked lists of symbol*)
  str_typ = ^str_rcd;
  str_rcd = RECORD sym: sym_typ; next: str_typ END;

  (*productions are linked lists of symbol strings*)
  prod_typ = ^prod_rcd;
  prod_rcd = RECORD str: str_typ; next: prod_typ END;

  (*production sets have production lists for each non-terminal symbol*)
  prod_set_typ = ARRAY[sym_typ] OF prod_typ;

  (*context-free grammars follow standard mathematical form*)
  grmr_typ = RECORD
    non_terms: sym_set_typ; terms: sym_set_typ;
    prods: prod_set_typ; start: sym_typ
  END;

  (*parse tree nodes are lists of symbols -- multi-forked trees*)
  par_tre_typ = ^par_tre_rcd;
  par_tre_rcd = RECORD sym: sym_typ; next: par_tre_typ; prod: par_tre_typ END;

$PAGE global data structures

VAR

  (*debug flag set TRUE if debugging*)
  dbg: BOOLEAN := FALSE;

  (*debug indentation counter*)
  dbg_ind: INTEGER := 0;

  (*error flag set TRUE when error signaled*)
  err: BOOLEAN := FALSE;

  (*input and output buffers used with standard INPUT and OUTPUT files*)
  ibuf: RECORD buf: STRING[ibuf_siz]; csr: 1 .. ibuf_siz + 1; ch: CHAR END;
  obuf: RECORD buf: STRING[obuf_siz] END;

  (*current token used by input scanner*)
  tkn: nam_typ;

  (*global symbol table -- valid symbols have non-null names*)
  sym_nam: sym_tab_typ;

  (*null set used as constant, all entries are FALSE*)
  nul_set: sym_set_typ;

  (*backup token stack used by parser*)
  tkn_stk: RECORD stk: ARRAY[1 .. bkp_lim] OF sym_typ; csr: 0 .. bkp_lim END;

  (*current and next token symbols used by parser*)
  crnt_tkn: sym_typ; next_tkn: sym_typ;

  (*parse-ok flag, set while parse succeeding, reset if parse failing*)
  parse_ok: BOOLEAN;

  (*string, grammar and parse tree -- inputs and output of parser*)
  str: str_typ; grmr: grmr_typ; par_tre: par_tre_typ;

$PAGE buffered INPUT and OUTPUT

PROCEDURE put_buf;
BEGIN (*write output buffer to OUTPUT file*)
  WITH obuf DO BEGIN
    WRITELN(OUTPUT, buf); BREAK(OUTPUT); buf := ''
  END
END;

PROCEDURE put_str(str: STRING);
BEGIN (*put string to output buffer*)
  IF LENGTH(obuf.buf) + LENGTH(str) > obuf_siz THEN put_buf;
  obuf.buf := obuf.buf || str
END;

PROCEDURE get_buf;
BEGIN (*read input buffer from INPUT file*)
  WITH ibuf DO BEGIN
    IF EOF THEN buf := '' ELSE READLN(INPUT, buf);
    csr := MINIMUM(csr)
  END
END;
  
PROCEDURE get_ch;
BEGIN (*get next character from input buffer*)
  WITH ibuf DO BEGIN
    IF csr > LENGTH(buf) THEN get_buf;
    IF buf = '' THEN ch := eof_ch ELSE ch := buf[csr];
    csr := SUCC(csr)
  END
END;

$PAGE debugging routines and error signaling

PROCEDURE d(s: STRING[*]);
BEGIN (*debug print string*)
  IF dbg THEN BEGIN WRITELN(OUTPUT, ' ': dbg_ind * 2, s); BREAK(OUTPUT) END
END;

PROCEDURE b(s: STRING[*]);
BEGIN (*begin block, print string*)
  IF dbg THEN BEGIN d(s); dbg_ind := SUCC(dbg_ind) END
END;

PROCEDURE e(s: STRING[*]);
BEGIN (*end block, print string*)
  IF dbg THEN BEGIN dbg_ind := PRED(dbg_ind); d(s) END
END;

PROCEDURE sgnl_err(msg: STRING);
BEGIN (*signal error -- display message and set error flag*)
  put_buf; put_str('error: ' || msg); put_buf; err := TRUE
END;

$PAGE input scanner
       
PROCEDURE scan;
BEGIN (*scan token: '', '(', ')' or name from input buffer*)
  WHILE ibuf.ch IN delim DO get_ch;
  IF ibuf.ch = eof_ch THEN tkn := ''
  ELSE IF ibuf.ch IN rmrkr THEN BEGIN tkn := '('; get_ch END
  ELSE IF ibuf.ch IN lmrkr THEN BEGIN tkn := ')'; get_ch END
  ELSE BEGIN
    tkn := ibuf.ch; get_ch;
    WHILE NOT (ibuf.ch IN delim + rmrkr + lmrkr) DO BEGIN
      tkn := tkn || ibuf.ch; get_ch
    END
  END;
  tkn := UPPERCASE(tkn)
END;

$PAGE symbol table lookup and insertion

FUNCTION lkp_sym(nam: nam_typ; VAR sym: sym_typ): BOOLEAN;
VAR tmp_sym: sym_typ;
BEGIN (*lookup symbol given name, return TRUE if found*)
  lkp_sym := FALSE;
  FOR tmp_sym := MINIMUM(tmp_sym) TO MAXIMUM(tmp_sym)
  DO EXIT IF sym_nam[tmp_sym] = nam DO BEGIN
    sym := tmp_sym; lkp_sym := TRUE
  END
END;

PROCEDURE mak_sym(nam: nam_typ);
VAR sym: sym_typ;
BEGIN (*make symbol -- insert name into symbol table*)
  FOR sym := MINIMUM(sym) TO MAXIMUM(sym) DO BEGIN
    EXIT IF sym_nam[sym] = '' DO sym_nam[sym] := nam;
    EXIT IF sym_nam[sym] = nam DO sgnl_err('already defined: ' || nam);
    EXIT IF sym = MAXIMUM(sym) DO sgnl_err('symbol table full')
  END
END;

$PAGE data structure load routines

PROCEDURE ld_prod_str(VAR str: str_typ);
VAR tmp_str: str_typ; sym: sym_typ;
BEGIN (*load production string from INPUT file*)
  IF tkn <> '(' THEN sgnl_err('looking for "(": ' || tkn)
  ELSE BEGIN
    scan;
    WHILE NOT err AND (tkn <> '') AND (tkn <> ')') DO BEGIN
      IF NOT lkp_sym(tkn, sym) THEN sgnl_err('undefined: ' || tkn)
      ELSE BEGIN
        IF str = NIL THEN BEGIN NEW(tmp_str); str := tmp_str END
        ELSE BEGIN NEW(tmp_str^.next); tmp_str := tmp_str^.next END;
        tmp_str^.sym := sym; tmp_str^.next := NIL; scan
      END
    END;
    IF NOT err THEN BEGIN
      IF tkn <> ')' THEN sgnl_err('looking for ")": ' || tkn) ELSE scan
    END
  END;
  IF err THEN sgnl_err('while loading production string')
END;

PROCEDURE ld_prod(VAR prod_set: prod_set_typ);
VAR prod: prod_typ; sym: sym_typ;
BEGIN (*load production from INPUT file*)
  IF tkn <> '(' THEN sgnl_err('looking for "(": ' || tkn)
  ELSE BEGIN
    scan;
    IF NOT lkp_sym(tkn, sym) THEN sgnl_err('undefined: ' || tkn)
    ELSE IF NOT grmr.non_terms[sym] THEN sgnl_err('not non-terminal: ' || tkn)
    ELSE IF prod_set[sym] <> NIL THEN sgnl_err('already defined: ' || tkn)
    ELSE BEGIN
      scan; NEW(prod); prod_set[sym] := prod; prod^.next := NIL;
      prod^.str := NIL; ld_prod_str(prod^.str);
      WHILE NOT err AND (tkn <> '') AND (tkn <> ')') DO BEGIN
        NEW(prod^.next); prod := prod^.next; prod^.next := NIL;
        prod^.str := NIL; ld_prod_str(prod^.str);
      END;
      IF NOT err THEN BEGIN
        IF tkn <> ')' THEN sgnl_err('looking for ")": ' || tkn) ELSE scan
      END
    END
  END;
  IF err THEN sgnl_err('while loading production')
END;

$PAGE

PROCEDURE ld_non_terms(VAR sym_set: sym_set_typ);
VAR sym: sym_typ;
BEGIN (*load non-terminal symbol set from INPUT file*)
  IF tkn <> '(' THEN sgnl_err('looking for "(": ' || tkn)
  ELSE BEGIN
    scan;
    WHILE NOT err AND (tkn <> '') AND (tkn <> ')') DO BEGIN
      IF lkp_sym(tkn, sym) THEN sgnl_err('already defined: ' || tkn)
      ELSE BEGIN
        mak_sym(tkn);
        IF lkp_sym(tkn, sym) THEN BEGIN sym_set[sym] := TRUE; scan END
      END;
    END;
    IF NOT err THEN BEGIN
      IF tkn <> ')' THEN sgnl_err('looking for ")": ' || tkn) ELSE scan
    END
  END;
  IF err THEN sgnl_err('while loading non-terminal symbol set')
END;

PROCEDURE ld_terms(VAR sym_set: sym_set_typ);
VAR sym: sym_typ;
BEGIN (*load terminal symbol set from INPUT file*)
  IF tkn <> '(' THEN sgnl_err('looking for "(": ' || tkn)
  ELSE BEGIN
    scan;
    WHILE NOT err AND (tkn <> '') AND (tkn <> ')') DO BEGIN
      IF lkp_sym(tkn, sym) THEN sgnl_err('already defined: ' || tkn)
      ELSE BEGIN
        mak_sym(tkn);
        IF lkp_sym(tkn, sym) THEN BEGIN sym_set[sym] := TRUE; scan END
      END
    END;
    IF NOT err THEN BEGIN
      IF tkn <> ')' THEN sgnl_err('looking for ")": ' || tkn) ELSE scan
    END
  END;
  IF err THEN sgnl_err('while loading terminal symbol set')
END;

$PAGE

PROCEDURE ld_prods(VAR prod_set: prod_set_typ);
BEGIN (*load production set from INPUT file*)
  IF tkn <> '(' THEN sgnl_err('looking for "(": ' || tkn)
  ELSE BEGIN
    scan;
    WHILE NOT err AND (tkn <> '') AND (tkn <> ')') DO ld_prod(prod_set);
    IF NOT err THEN BEGIN
      IF tkn <> ')' THEN sgnl_err('looking for ")": ' || tkn) ELSE scan
    END
  END;
  IF err THEN sgnl_err('while loading production set')
END;

PROCEDURE ld_start(VAR sym: sym_typ);
BEGIN (*load start symbol from INPUT file*)
  IF NOT lkp_sym(tkn, sym) THEN sgnl_err('undefined: ' || tkn)
  ELSE IF NOT grmr.non_terms[sym] THEN sgnl_err('not non-terminal: ' || tkn)
  ELSE scan;
  IF err THEN sgnl_err('while loading start symbol')
END;

PROCEDURE ld_grmr;
VAR sym: sym_typ;
BEGIN (*load grammar structure from INPUT file*)
  WITH grmr DO BEGIN
    terms := nul_set; non_terms := nul_set;
    FOR sym := MINIMUM(sym) TO MAXIMUM(sym) DO prods[sym] := NIL;
    start := eos_sym; terms[eos_sym] := TRUE
  END;
  IF tkn <> '(' THEN sgnl_err('looking for "(": ' || tkn)
  ELSE BEGIN
    scan;
    IF NOT err THEN ld_non_terms(grmr.non_terms);
    IF NOT err THEN ld_terms(grmr.terms);
    IF NOT err THEN ld_prods(grmr.prods);
    IF NOT err THEN ld_start(grmr.start);
    IF NOT err THEN BEGIN
      IF tkn <> ')' THEN sgnl_err('looking for ")": ' || tkn) ELSE scan
    END
  END;
  IF err THEN sgnl_err('while loading grammar')
END;

$PAGE

PROCEDURE ld_str;
VAR tmp_str: str_typ; sym: sym_typ;
BEGIN (*load global string structure from INPUT file*)
  str := NIL;
  IF tkn <> '(' THEN sgnl_err('looking for "(": ' || tkn)
  ELSE BEGIN
    scan;
    WHILE NOT err AND (tkn <> '') AND (tkn <> ')') DO BEGIN
      IF NOT lkp_sym(tkn, sym) THEN sgnl_err('undefined: ' || tkn)
      ELSE IF NOT grmr.terms[sym] THEN sgnl_err('not terminal: ' || tkn)
      ELSE BEGIN
        IF str = NIL THEN BEGIN NEW(tmp_str); str := tmp_str END
        ELSE BEGIN NEW(tmp_str^.next); tmp_str := tmp_str^.next END;
        tmp_str^.sym := sym; tmp_str^.next := NIL; scan
      END
    END;
    IF NOT err THEN BEGIN
      IF tkn <> ')' THEN sgnl_err('looking for ")": ' || tkn) ELSE scan
    END
  END;
  IF err THEN sgnl_err('while loading string')
END;

$PAGE data structure display routines

PROCEDURE dpy_prod_str(str: str_typ);
VAR tmp_str: str_typ;
BEGIN (*display production string*)
  put_str('( '); tmp_str := str;
  WHILE tmp_str <> NIL DO BEGIN
    put_str(sym_nam[tmp_str^.sym] || ' '); tmp_str := tmp_str^.next
  END;
  put_str(') ')
END;

PROCEDURE dpy_prod(sym: sym_typ; prod: prod_typ);
VAR tmp_prod: prod_typ;
BEGIN (*display production*)
  put_str('( '); put_str(sym_nam[sym] || ' '); tmp_prod := prod;
  WHILE tmp_prod <> NIL DO BEGIN
    dpy_prod_str(tmp_prod^.str); tmp_prod := tmp_prod^.next
  END;
  put_str(') ')
END;

PROCEDURE dpy_sym_set(sym_set: sym_set_typ);
VAR sym: sym_typ;
BEGIN (*display symbol set*)
  put_str('( ');
  FOR sym := MINIMUM(sym) TO MAXIMUM(sym) DO BEGIN
    IF sym_set[sym] THEN put_str(sym_nam[sym] || ' ')
  END;
  put_str(') ')
END;

PROCEDURE dpy_prod_set(prod_set: prod_set_typ);
VAR sym: sym_typ;
BEGIN (*display production set*)
  put_str('( ');
  FOR sym := MINIMUM(sym) TO MAXIMUM(sym) DO BEGIN
    IF prod_set[sym] <> NIL THEN dpy_prod(sym, prod_set[sym])
  END;
  put_str(') ')
END;

PROCEDURE dpy_grmr;
BEGIN (*display global grammar structure*)
  put_buf; put_str('grammar:'); put_buf; put_str('( ');
  WITH grmr DO BEGIN
    dpy_sym_set(non_terms); dpy_sym_set(terms);
    dpy_prod_set(prods); put_str(sym_nam[start] || ' ')
  END;
  put_str(') '); put_buf
END;

$PAGE

PROCEDURE dpy_str;
VAR tmp_str: str_typ;
BEGIN (*display global string structure*)
  put_buf; put_str('string:'); put_buf; put_str('( '); tmp_str := str;
  WHILE tmp_str <> NIL DO BEGIN
    put_str(sym_nam[tmp_str^.sym] || ' '); tmp_str := tmp_str^.next
  END;
  put_str(') '); put_buf
END;

PROCEDURE dpy_nod(nod: par_tre_typ);
VAR tmp_tre: par_tre_typ;
BEGIN (*display node of parse tree*)
  put_str('( '); tmp_tre := nod;
  WHILE tmp_tre <> NIL DO BEGIN
    IF grmr.non_terms[tmp_tre^.sym] THEN BEGIN
      put_str(sym_nam[tmp_tre^.sym] || ' '); dpy_nod(tmp_tre^.prod);
    END
    ELSE put_str(sym_nam[tmp_tre^.sym] || ' ');
    tmp_tre := tmp_tre^.next
  END;
  put_str(') ')
END;

PROCEDURE dpy_par_tre;
BEGIN (*display global parse tree structure*)
  put_buf; put_str('parse tree:'); put_buf;
  dpy_nod(par_tre); put_buf
END;

PROCEDURE dpy_set(sym_set: sym_set_typ);
VAR sym: sym_typ;
BEGIN (*display symbol set*)
  put_buf; put_str('( ');
  FOR sym := MINIMUM(sym) TO MAXIMUM(sym) DO BEGIN
    IF sym_set[sym] THEN put_str(sym_nam[sym] || ' ')
  END;
  put_str(') '); put_buf;
END;

$PAGE generate terminate and use sets

PROCEDURE gen_trm_set(VAR sym_set: sym_set_typ);
VAR done, is_trm: BOOLEAN; sym: sym_typ; prod: prod_typ; str: str_typ;
BEGIN (*generate set of all symbols which terminate*)
  sym_set := grmr.terms;
  REPEAT
    done := TRUE;
    FOR sym := MINIMUM(sym) TO MAXIMUM(sym) DO BEGIN
      prod := grmr.prods[sym];
      IF NOT sym_set[sym] THEN WHILE prod <> NIL DO BEGIN
        str := prod^.str; is_trm := TRUE;
        WHILE str <> NIL DO BEGIN
          EXIT IF NOT sym_set[str^.sym] DO is_trm := FALSE;
          str := str^.next
        END;
        EXIT IF is_trm DO IF NOT sym_set[sym] THEN BEGIN
          sym_set[sym] := TRUE; done := FALSE
        END;
        prod := prod^.next
      END
    END
  UNTIL done;
  IF dbg THEN BEGIN d('terminate set: '); dpy_set(sym_set) END
END;

PROCEDURE gen_use_set(VAR sym_set: sym_set_typ);
VAR done: BOOLEAN; sym: sym_typ; prod: prod_typ; str: str_typ;
BEGIN (*generate set all symbols which can be used*)
  sym_set := nul_set; sym_set[grmr.start] := TRUE;
  REPEAT
    done := TRUE;
    FOR sym := MINIMUM(sym) TO MAXIMUM(sym)
    DO IF grmr.non_terms[sym] THEN BEGIN
      prod := grmr.prods[sym];
      WHILE prod <> NIL DO BEGIN
        str := prod^.str;
        WHILE str <> NIL DO BEGIN
          IF NOT sym_set[str^.sym] THEN BEGIN
            sym_set[str^.sym] := TRUE; done := FALSE
          END;
          str := str^.next
        END;
        prod := prod^.next
      END
    END
  UNTIL done;
  IF dbg THEN BEGIN d('used set: '); dpy_set(sym_set) END
END;

$PAGE grammar and string validation

PROCEDURE chk_grmr;
VAR sym: sym_typ; sym_set: sym_set_typ;
BEGIN (*check grammar for validity*)
  FOR sym := MINIMUM(sym) TO MAXIMUM(sym)
  DO IF grmr.non_terms[sym] ANDIF (grmr.prods[sym] = NIL)
  THEN sgnl_err('non-terminal has no production: ' || sym_nam[sym]);
  gen_trm_set(sym_set);
  FOR sym := MINIMUM(sym) TO MAXIMUM(sym)
  DO IF (sym_nam[sym] <> '') AND NOT sym_set[sym]
  THEN sgnl_err('symbol does not terminate: ' || sym_nam[sym]);
  gen_use_set(sym_set);
  FOR sym := MINIMUM(sym) TO MAXIMUM(sym)
  DO IF (sym_nam[sym] <> '') AND NOT sym_set[sym]
  THEN sgnl_err('symbol not used: ' || sym_nam[sym])
END;

PROCEDURE chk_str;
BEGIN (*check string for validity*)
END;

$PAGE token manipulation for parser

PROCEDURE get_tkn(VAR tkn: sym_typ);
VAR tmp_str: str_typ;
BEGIN (*get next token from symbol string*)
  IF str = NIL THEN tkn := eos_sym
  ELSE BEGIN
    tmp_str := str; tkn := str^.sym; str := str^.next; DISPOSE(tmp_str)
  END;
END;

PROCEDURE pop_tkn;
BEGIN (*pop current/next token from token stack/string*)
  WITH tkn_stk DO BEGIN
    crnt_tkn := next_tkn;
    IF csr > MINIMUM(csr) THEN BEGIN  next_tkn := stk[csr]; csr := PRED(csr)
    END
    ELSE get_tkn(next_tkn)
  END
END;

PROCEDURE push_tkn;
BEGIN (*push current/next token onto token stack*)
  WITH tkn_stk DO BEGIN
    IF csr < bkp_lim THEN BEGIN
      csr := SUCC(csr); stk[csr] := next_tkn; next_tkn := crnt_tkn;
    END
    ELSE sgnl_err('backup limit exceeded: ' || sym_nam[crnt_tkn])
  END
END;

PROCEDURE put_back(VAR tre: par_tre_typ);
BEGIN (*put back tokens from parse tree*)
  IF tre <> NIL THEN BEGIN
    IF tre^.next <> NIL THEN put_back(tre^.next)
    ELSE IF grmr.non_terms[tre^.sym] THEN put_back(tre^.prod)
    ELSE BEGIN push_tkn; crnt_tkn := tre^.sym END;
    DISPOSE(tre); tre := NIL
  END
END;

$PAGE parsing algorithm

PROCEDURE parse(sym: sym_typ; VAR tre: par_tre_typ);
VAR tmp_tre: par_tre_typ; crnt_str: str_typ; crnt_prod: prod_typ;
BEGIN (*try to parse symbol from string, build parse tree*)
  b('parse: ' || sym_nam[sym]);
  NEW(tre); tre^.sym := sym; tre^.next := NIL;
  IF grmr.terms[sym] THEN BEGIN
    d('terminal');
    parse_ok := (crnt_tkn = sym); IF parse_ok THEN pop_tkn
  END
  ELSE BEGIN
    d('non-terminal');
    tre^.prod := NIL; crnt_prod := grmr.prods[sym];
    parse_ok := (crnt_prod <> NIL);
    IF NOT parse_ok THEN sgnl_err('grammar broken: ' || sym_nam[sym])
    ELSE REPEAT
      d('begin production');
      crnt_str := crnt_prod^.str; parse_ok := (crnt_str = NIL);
      IF parse_ok THEN d('null production');
      IF NOT parse_ok THEN REPEAT
        IF tre^.prod = NIL THEN BEGIN
          parse(crnt_str^.sym, tre^.prod); tmp_tre := tre^.prod
        END
        ELSE BEGIN
          parse(crnt_str^.sym, tmp_tre^.next); tmp_tre := tmp_tre^.next
        END;
        IF parse_ok THEN crnt_str := crnt_str^.next
      UNTIL err OR NOT parse_ok OR (crnt_str = NIL);
      IF NOT parse_ok AND NOT err THEN BEGIN
        d('production failed');
        put_back(tre^.prod); crnt_prod := crnt_prod^.next
      END
    UNTIL err OR parse_ok OR (crnt_prod = NIL)
  END;
  IF NOT parse_ok AND NOT err THEN BEGIN DISPOSE(tre); tre := NIL END;
  IF err THEN sgnl_err('while parsing: ' || sym_nam[sym]);
  IF parse_ok THEN e('parse ok') ELSE e('parse not ok')
END;

$PAGE initial set up and main block

PROCEDURE set_up;
VAR sym: sym_typ; resp: STRING[3];
BEGIN (*set up initial global structures*)
  OPEN(TTY); REWRITE(TTY); REWRITE(OUTPUT, out_fil); RESET(INPUT, in_fil);
  WRITELN(TTY, 'Parser, Version 1');
  WRITELN(TTY, 'INPUT = ', FILENAME(INPUT));
  WRITELN(TTY, 'OUTPUT = ', FILENAME(OUTPUT));
  WRITE(TTY, 'debug (Y/N)? '); BREAK(TTY); READLN(TTY); READ(TTY, resp);
  dbg := UPPERCASE(resp) = 'Y'; d('--debugging--');
  obuf.buf := ''; get_buf; get_ch; scan;
  FOR sym := MINIMUM(sym) TO MAXIMUM(sym) DO sym_nam[sym] := '';
  FOR sym := MINIMUM(sym) TO MAXIMUM(sym) DO nul_set[sym] := FALSE;
  sym_nam[eos_sym] := eos_nam; tkn_stk.csr := MINIMUM(tkn_stk.csr);
  IF NOT err THEN ld_grmr;
  IF NOT err THEN BEGIN dpy_grmr; chk_grmr END;
  IF NOT err THEN ld_str;
  IF NOT err THEN BEGIN dpy_str; chk_str END;
  IF NOT err THEN BEGIN pop_tkn; pop_tkn END
END;

BEGIN (*main block for parser program*)
  set_up;
  IF NOT err THEN BEGIN
    parse(grmr.start, par_tre);
    IF parse_ok THEN dpy_par_tre ELSE sgnl_err('parse fails')
  END
END.
 \ A