MODULE sym;
(*symbol manipulation -- strings and sets*)

$SYSTEM err
$SYSTEM bufio
$SYSTEM scan

$INCLUDE sym.typ

PUBLIC PROCEDURE mak_sym(nam: nam_typ); FORWARD;
(*make symbol -- insert name into symbol table*)

PUBLIC FUNCTION lkp_sym(nam: nam_typ; VAR sym: sym_typ): BOOLEAN; FORWARD;
(*lookup symbol given name, return TRUE if found*)

PUBLIC VAR sym_nam: sym_tab_typ := nul_sym_tab;
(*global symbol table -- valid symbols have non-null names*)

PUBLIC PROCEDURE ld_sym(VAR sym: sym_typ); FORWARD;
(*load symbol from INPUT file*)

PUBLIC PROCEDURE st_sym(sym: sym_typ); FORWARD;
(*store symbol to OUTPUT file*)

PUBLIC PROCEDURE ld_str(VAR sym_str: sym_str_typ); FORWARD;
(*load symbol string from INPUT file*)

PUBLIC PROCEDURE st_str(sym_str: sym_str_typ); FORWARD;
(*store symbol string to OUTPUT file*)

PUBLIC PROCEDURE ld_set(VAR sym_set: sym_set_typ); FORWARD;
(*load symbol set from INPUT file*)

PUBLIC PROCEDURE st_set(sym_set: sym_set_typ); FORWARD;
(*store symbol set to OUTPUT file*)

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
    EXIT IF sym_nam[sym] = nam;
    EXIT IF sym_nam[sym] = '' DO sym_nam[sym] := nam;
    EXIT IF sym = MAXIMUM(sym) DO sgnl_err('symbol table full: ' || nam)
  END
END;

PROCEDURE ld_sym(VAR sym: sym_typ);
BEGIN (*load symbol from INPUT file*)
  IF tkn = new_tkn THEN scan;
  IF tkn = end_tkn THEN sym := end_sym;
  mak_sym(UPPERCASE(tkn));
  IF NOT lkp_sym(UPPERCASE(tkn), sym) THEN BEGIN
    sgnl_err('symbol table broken: ' || tkn); sym := end_sym
  END;
  scan
END;

PROCEDURE st_sym(sym: sym_typ);
BEGIN (*store symbol to OUTPUT file*)
  put_str(sym_nam[sym] || ' ')
END;

PROCEDURE ld_str(VAR sym_str: sym_str_typ);
VAR tmp_str: sym_str_typ; sym: sym_typ;
BEGIN (*load symbol string from INPUT file*)
  IF tkn = new_tkn THEN scan;
  sym_str := nul_sym_str;
  IF tkn <> '(' THEN sgnl_err('looking for "(": ' || tkn)
  ELSE BEGIN
    scan;
    WHILE (tkn <> '') AND (tkn <> ')') DO BEGIN
      ld_sym(sym);
      IF sym_str = NIL THEN BEGIN NEW(tmp_str); sym_str := tmp_str END
      ELSE BEGIN NEW(tmp_str^.next); tmp_str := tmp_str^.next END;
      tmp_str^.sym := sym; tmp_str^.next := NIL
    END;
    IF tkn <> ')' THEN sgnl_err('looking for ")": ' || tkn) ELSE scan
  END;
  IF err THEN sgnl_err('while loading string')
END;

PROCEDURE st_str(sym_str: sym_str_typ);
VAR tmp_str: sym_str_typ;
BEGIN (*store symbol string to OUTPUT file*)
  put_buf; put_str('( '); tmp_str := sym_str;
  WHILE tmp_str <> NIL DO BEGIN
    st_sym(tmp_str^.sym); tmp_str := tmp_str^.next
  END;
  put_str(') ')
END;

PROCEDURE ld_set(VAR sym_set: sym_set_typ);
VAR sym: sym_typ;
BEGIN (*load symbol string from INPUT file*)
  IF tkn = new_tkn THEN scan;
  sym_set := nul_sym_set;
  IF tkn <> '(' THEN sgnl_err('looking for "(": ' || tkn)
  ELSE BEGIN
    scan;
    WHILE (tkn <> '') AND (tkn <> ')') DO BEGIN
      ld_sym(sym); sym_set[sym] := TRUE
    END;
    IF tkn <> ')' THEN sgnl_err('looking for ")": ' || tkn) ELSE scan
  END;
  IF err THEN sgnl_err('while loading set')
END;

PROCEDURE st_set(sym_set: sym_set_typ);
VAR sym: sym_typ;
BEGIN (*store symbol set to OUTPUT file*)
  put_buf; put_str('( ');
  FOR sym := MINIMUM(sym) TO MAXIMUM(sym)
  DO IF sym_set[sym] THEN st_sym(sym);
  put_str(') ')
END.
   