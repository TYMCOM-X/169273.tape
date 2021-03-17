$WIDTH=100
$LENGTH=55
$TITLE PASS4.PAS, last modified 12/27/83, zw
PROGRAM pass4 OPTIONS STORAGE(8000), SPECIAL(WORD);
(*TYM-Pascal compiler -- code generation driver*)

$PAGE system modules

$SYSTEM PASCAL.INC
$SYSTEM PTMCON.INC
$SYSTEM PASIST.INC
$SYSTEM PASFIL.INC
$SYSTEM PASLOG.INC
$SYSTEM PASPT.TYP
$SYSTEM PASIF.TYP
$SYSTEM PASIFU.INC
$SYSTEM PASTAL.INC
$SYSTEM PTMGEN.INC
$SYSTEM PASCV.INC
$SYSTEM INFPAC.INC

$PAGE pass4 - main program

VAR
start_time: INTEGER;
segstuff: segrecd;
code_size, const_size, static_size: unit_range;

BEGIN
  start_time := RUNTIME;
  unchain;
  IF finish and prog_options.code_opt THEN BEGIN
    ch_open(TRUE, FALSE);
    tal_init;
    gen_code(code_size, const_size, static_size);
    ch_close
  END;
  IF prog_options.statistics_opt THEN BEGIN
    REWRITE(TTY); SEGINFO(segstuff);
    WRITELN(TTY, '[Pass 4: ', (runtime - start_time) / 1000.0:8:3,
      ' seconds, ', (segstuff.lowlen + 511) DIV 512:3, '+',
      (segstuff.highlen + 511) DIV 512:3, 'P]');
    IF finish AND prog_options.code_opt THEN BEGIN
      WRITELN(TTY, '[Code area:      ', cv_radix (code_size, adr_width),
        ' words (', cv_int(code_size), ' decimal)]');
      WRITELN(TTY, '[Constant area:  ', cv_radix (const_size, adr_width),
        ' words (', cv_int(const_size), ' decimal)]');
      WRITELN(TTY, '[Static area:    ', cv_radix (static_size, adr_width),
        ' words (', cv_int(static_size), ' decimal)]');
      WRITELN(TTY)
    END;
    CLOSE(TTYOUTPUT);
  END;
  IF prog_options.overlay_opt THEN BEGIN
    LOG_record.lowseg_size := const_size + static_size;
    log_record.highseg_size := code_size;
  END
  ELSE BEGIN
    log_record.lowseg_size := static_size;
    log_record.highseg_size := code_size + const_size;
  END;
  log_write;
  chain('PASCAL')
end.
