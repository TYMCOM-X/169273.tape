$TITLE PASGEN.PAS, last modified 5/14/84, zw
PROGRAM pasgen options special(word), storage(8000);
(*TYM-Pascal compiler Code Generation Pass*)
$PAGE includes
$SYSTEM pascal
$SYSTEM ptmcon
$SYSTEM pasist
$SYSTEM pasfil
$SYSTEM paslog
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM pasifu
$SYSTEM pasenv
$SYSTEM pastal
$SYSTEM ptmgen
$SYSTEM pascv
$SYSTEM passw
$SYSTEM prgdir
$SYSTEM infpac
$SYSTEM RUNUTL.INC
$SYSTEM tmpnam
$PAGE pass4 - main program
var start_time: integer;
    segstuff: segrecd;
    code_size, const_size, static_size: unit_range;
begin
  start_time := runtime;
  if not rdpas(tempname ('PAS'), true) then begin
    rewrite (tty);
    writeln (tty, '?Compiler temporary file PAS lost');
    stop;
  end;
  rewrite (tty);
  if finish and prog_options.code_opt then begin
    ch_open (true, false);
    tal_init;
    gen_code (code_size, const_size, static_size);
    ch_close;
  end;
  if prog_options.statistics_opt then begin
    seginfo (segstuff);
    writeln (tty, '[Pass 4: ', (runtime - start_time) / 1000.0:8:3, ' seconds, ',
                  (segstuff.lowlen+511) div 512:3, '+',
                  (segstuff.highlen+511) div 512:3, 'P]');
    if finish and prog_options.code_opt then begin
      writeln (tty, '[Code area:      ', cv_radix (code_size, adr_width), ' words (',
                    cv_int (code_size), ' decimal)]');
      writeln (tty, '[Constant area:  ', cv_radix (const_size, adr_width), ' words (',
                    cv_int (const_size), ' decimal)]');
      writeln (tty, '[Static area:    ', cv_radix (static_size, adr_width), ' words (',
                    cv_int (static_size), ' decimal)]');
      writeln (tty);
    end;
  end;
  close (ttyoutput);
  if runoff > 0 then begin
    if prog_options.overlay_opt then begin
      log_record.lowseg_size := const_size + static_size;
      log_record.highseg_size := code_size;
    end
    else begin
      log_record.lowseg_size := static_size;
      log_record.highseg_size := code_size + const_size;
    end;
    log_write;
    IF NOT runprg('PASCAL' || prgm_dir (), 1) THEN BEGIN
      rewrite (tty);
      writeln (tty, '?Unable to run PAS')
    END
  end;
end.
