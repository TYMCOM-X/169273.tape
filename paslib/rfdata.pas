$include rfdata.typ

public
$include rfdata.var

public procedure rf_data_init;
 begin
  cur_state := alignmode;
  left_indentation := 0;
  right_indentation := 0;
  par_indentation := 0;
  line_spacing := 0;
  dashes := false;
  device := stdtty;
  dounder := true;
  dodecap := false;

  with page do begin
    length := 66;
    width := 85;
    top_margin := 6;
    bottom_margin := 6;
    left_margin := 5;
    right_margin := 10;
    number := 0;
  end;
  next_page := page;
 end.
 