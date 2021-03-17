module dcsecs;

$HEADER dcsecs.hdr

$SYSTEM idtime.typ

public function dc_secs ( time: time_int ): seconds
  options special (word);

const
  secs_per_day: seconds = 86400;

var temp: machine_word;
begin
  temp := time.t * secs_per_day;
  temp := temp + 400000b;
  temp := temp div (2**18);
  dc_secs := temp;
end.
