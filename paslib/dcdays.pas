module dcdays;

$HEADER dcdays.hdr

$SYSTEM idtime.typ

public function dc_days ( date: date_int ): days;

begin
  dc_days := date.d;
end.
