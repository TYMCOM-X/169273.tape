program prog options special, nocheck;
type
  map_array = array[1..67] of packed array[1..79] of char;
const
  cmap: map_array := (
'+-----------+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----------+',
'|           |     |     |     |     |     |     |     |     |     |           |',
'|           |     |     |     |     |     |     |     |     |     |           |',
'|           |     |     |     |     |     |     |     |     |     |           |',
'|           |     |     |     |     |     |     |     |     |     |           |',
'|           |     |     |     |     |     |     |     |     |     |           |',
'+-----------+     |     |     |     |     |     |     |     |     +-----------+',
'|           |     |     |     |     |     |     |     |     |     |           |',
'|           |     |     |     |     |     |     |     |     |     |           |',
'|           |     |     |     |     |     |     |     |     |     |           |',
'|           |     |     |     |     |     |     |     |     |     |           |',
'|           |     |     |     |     |     |     |     |     |     |           |',
'+-----------+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----------+',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'+-----------+                                                     +-----------+',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'+-----------+                                                     +-----------+',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'+-----------+                                                     +-----------+',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'+-----------+                                                     +-----------+',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'+-----------+                                                     +-----------+',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'+-----------+                                                     +-----------+',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'|           |                                                     |           |',
'+-----------+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----------+',
'|           |     |     |     |     |     |     |     |     |     |           |',
'|           |     |     |     |     |     |     |     |     |     |           |',
'|           |     |     |     |     |     |     |     |     |     |           |',
'|           |     |     |     |     |     |     |     |     |     |           |',
'|           |     |     |     |     |     |     |     |     |     |           |',
'+-----------+     |     |     |     |     |     |     |     |     +-----------+',
'|           |     |     |     |     |     |     |     |     |     |           |',
'|           |     |     |     |     |     |     |     |     |     |           |',
'|           |     |     |     |     |     |     |     |     |     |           |',
'|           |     |     |     |     |     |     |     |     |     |           |',
'|           |     |     |     |     |     |     |     |     |     |           |',
'+-----------+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----------+');

type place_array = array[1..40] of string[20];
var
  i, j, k: integer;
  s: string;
  w1, w2: place_array;
  map: map_array;

procedure horizontal (i, j, k: integer);
begin
  if w2[i] = '' then
    map[j,k:length(w1[i])] := w1[i]
  else begin
    map[j-1,k:length(w1[i])] := w1[i];
    map[j+1,k:length(w2[i])] := w2[i];
  end;
end;
procedure vertical (i, j, k: integer);
var ix: integer;
begin
  if w2[i] = '' then begin
    for ix := 1 to length (w1[i]) do
      map[k+ix-1,j:1] := substr (w1[i],ix,1)
  end
  else begin
    for ix := 1 to length (w1[i]) do
      map[k+ix-1,j-1:1] := substr(w1[i],ix,1);
    for ix := 1 to length (w2[i]) do
      map[k+ix-1,j+1:1] := substr (w2[i],ix,1);
  end;
end;
begin
  map := cmap;
  open (input,'board.txt');
  for i := 1 to 40 do begin
    readln;
    read (s);
    j := index (s,' ');
    if j <> 0 then begin
      w1[i] := substr (s, 1, min(j-1, 10));
      w2[i] := substr (s, j+1, min (10, length (s) - j));
    end
    else begin
      w1[i] := substr (s, 1, min (10, length (s)));
      w2[i] := '';
    end;
  end;
  close (input);
  for i := 1 to 9 do begin
    vertical (i, 64 - (i-1)*6, 57);
    vertical (30-i, 64 - (i-1)*6, 3);
  end;
  for i := 10 to 20 do begin
    horizontal (i, 68 - (4 + (i-10)*6), 3);
    horizontal (50-i, 68 - (4+(i-10)*6), 69);
  end;
  rewrite (output,'board.tmp');
  for i := 1 to 67 do
    writeln (map[i]);
  close (output);
end.
    