program dag005;

var r: record
	f1, f2: integer;
	case tag: integer of
	    1: ( v11, v12: integer );
	    2: ( v21, v22: integer )
    end;

var i, j, k: integer;

begin  with r do begin
  f1 := 1;
  f2 := 2;
  i := f1;
  i := f2;

  v11 := 11;
  v12 := 12;
  v21 := 21;
  i := v11;
  i := v12;
  i := v21;

  tag := j;
  i := v11;
  i := v12;
  i := v21;

  i := f1;
  i := f2;
end  end.
   