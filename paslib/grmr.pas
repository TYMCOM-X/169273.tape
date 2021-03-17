MODULE grmr;
(*grammar manipulation*)

$SYSTEM err
$SYSTEM bufio
$SYSTEM scan
$SYSTEM sym

$INCLUDE grmr.typ

PUBLIC PROCEDURE ld_grmr(VAR grmr: grmr_typ); FORWARD;
(*load grammar from INPUT*)

PUBLIC PROCEDURE st_grmr(grmr: grmr_typ); FORWARD;
(*store grammar to OUTPUT*)

PROCEDURE ld_prod(VAR prod_set: prod_set_typ);
VAR prod: prod_typ; sym: sym_typ;
BEGIN (*load production from INPUT file*)
  IF tkn = new_tkn THEN scan;
  IF tkn <> '(' THEN sgnl_err('looking for "(": ' || tkn)
  ELSE BEGIN
    scan; ld_sym(sym);
    WHILE NOT err AND (tkn <> '') AND (tkn <> ')') DO BEGIN
      IF prod_set[sym] = nul_prod THEN BEGIN
        NEW(prod); prod_set[sym] := prod
      END
      ELSE BEGIN NEW(prod^.next); prod := prod^.next END;
      prod^.next := nul_prod; ld_str(prod^.str)
    END;
    IF tkn <> ')' THEN sgnl_err('looking for ")": ' || tkn) ELSE scan
  END;
  IF err THEN sgnl_err('while loading production')
END;

PROCEDURE ld_prod_set(VAR prod_set: prod_set_typ);
BEGIN (*load production set from INPUT file*)
  IF tkn = new_tkn THEN scan;
  IF tkn <> '(' THEN sgnl_err('looking for "(": ' || tkn)
  ELSE BEGIN
    scan;
    WHILE (tkn <> '') AND (tkn <> ')') DO ld_prod(prod_set);
    IF tkn <> ')' THEN sgnl_err('looking for ")": ' || tkn) ELSE scan
  END;
  IF err THEN sgnl_err('while loading production set')
END;

PROCEDURE ld_grmr(VAR grmr: grmr_typ);
VAR sym: sym_typ;
BEGIN (*load grammar from INPUT file*)
  IF tkn = new_tkn THEN scan;
  WITH grmr DO BEGIN
    terms := nul_sym_set; non_terms := nul_sym_set;
    FOR sym := MINIMUM(sym) TO MAXIMUM(sym) DO prods[sym] := nul_prod;
    start := end_sym; terms[end_sym] := TRUE
  END;
  IF tkn <> '(' THEN sgnl_err('looking for "(": ' || tkn)
  ELSE BEGIN
    scan;
    ld_set(grmr.non_terms); ld_set(grmr.terms);
    ld_prod_set(grmr.prods); ld_sym(grmr.start);
    IF tkn <> ')' THEN sgnl_err('looking for ")": ' || tkn) ELSE scan
  END;
  IF err THEN sgnl_err('while loading grammar')
END;

PROCEDURE st_prod(sym: sym_typ; prod: prod_typ);
VAR tmp_prod: prod_typ;
BEGIN (*store production to OUTPUT file*)
  put_buf; put_str('( '); st_sym(sym); tmp_prod := prod;
  WHILE tmp_prod <> nul_prod DO BEGIN
    st_str(tmp_prod^.str); tmp_prod := tmp_prod^.next
  END;
  put_str(') ')
END;

PROCEDURE st_prod_set(prod_set: prod_set_typ);
VAR sym: sym_typ;
BEGIN (*store production set to OUTPUT file*)
  put_buf; put_str('( ');
  FOR sym := MINIMUM(sym) TO MAXIMUM(sym) DO BEGIN
    IF prod_set[sym] <> nul_prod THEN st_prod(sym, prod_set[sym])
  END;
  put_str(') ')
END;

PROCEDURE st_grmr(grmr: grmr_typ);
BEGIN (*store grammar to OUTPUT file*)
  put_buf; put_str('( ');
  WITH grmr DO BEGIN
    st_set(non_terms); st_set(terms); st_prod_set(prods);
    put_buf; st_sym(start);
  END;
  put_str(') ')
END.
