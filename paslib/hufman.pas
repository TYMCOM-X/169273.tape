program huffman;

var char_desc: array [char] of record
	index: 1..128;
	weight: integer;
	length: 0..31;
	encoding: packed array [1..31] of 0..1;
    end;

var table: array [0..255] of record
	name: char;
	weight: integer;
	parent: 0..255;
	code: 0..1;
    end;

var char_count, bit_count: integer;

var cc: char;
    iter: 1..127;
    a_bot, a_top, q_bot, q_top, i, j, k: 0 .. 256;
    code: 0..1;

var input_name: string [40];


procedure select (var index: 0..256);
begin
  table[index].code := code;
  table[index].parent := q_top + 1;
  index := index + 1;
  table[q_top+1].weight := table[q_top+1].weight + table[index].weight;
end;

begin
  rewrite (tty);
  open (tty);
  writeln (tty, 'Huffman Tree Construction');
  write (tty, 'Source File? ');
  break;
  readln (tty);
  read (tty, input_name);
  reset (input, input_name, [ascii]);
  if iostatus <> io_ok then begin
    writeln (tty, 'Bad input file');
    stop;
  end;
  for cc := minimum (char) to maximum (char) do
    char_desc[cc].weight := 0;
  char_count := 0;
  while not eof (input) do begin
    char_count := char_count + 1;
    with char_desc[input^] do
      weight := weight + 1;
    get (input);
  end;
  writeln (tty, char_count, ' characters; ', (char_count+4) div 5, ' words');
  if char_count = 0 then stop;
  a_top := 0;
  for cc := minimum (char) to maximum (char) do begin
    if char_desc[cc].weight <> 0 then begin
      a_top := a_top + 1;
      table[a_top].name := cc;
      table[a_top].weight := char_desc[cc].weight;
      table[a_top].parent := 0;
    end;
  end;
  writeln (tty, a_top, ' significant characters');
  for i := a_top - 1 downto 1 do begin
    if table[i].weight > table[i+1].weight then begin
      table[0] := table[i];
      j := i + 1;
      repeat
	table[j-1] := table[j];
	j := j + 1;
      until (j > a_top) orif (table[j].weight > table[0].weight);
      table[j-1] := table[0];
    end;
  end;
  for i := 1 to a_top do
    char_desc[table[i].name].index := i;
  a_bot := 1;
  q_top := a_top;
  q_bot := q_top + 1;
  for iter := 1 to a_top - 1 do begin
    table[q_top+1].parent := 0;
    table[q_top+1].weight := 0;
    for code := 0 to 1 do begin
      if q_bot > q_top then
	select (a_bot)
      else if a_bot > a_top then
	select (q_bot)
      else if table[a_bot].weight > table[q_bot].weight then
	select (q_bot)
      else
	select (a_bot);
    end;
    q_top := q_top + 1;
  end;
  for cc := minimum (char) to maximum (char) do begin
    with char_desc[cc] do begin
      if weight <> 0 then begin
	i := index;
	j := 0;
	while i <> q_top do begin
	  k := table[i].parent;
	  table[i].parent := j;
	  j := i;
	  i := k;
	end;
	length := 0;
	while j <> 0 do begin
	  length := length + 1;
	  encoding[length] := table[j].code;
	  k := table[j].parent;
	  table[j].parent := i;
	  i := j;
	  j := k;
	end;
      end;
    end;
  end;
  bit_count := 0;
  for i := a_top downto 1 do begin
    cc := table[i].name;
    if cc in [succ(' ')..pred(chr(177b))]
      then write (tty, '  ', cc)
      else write (tty, ord(cc):3:o);
    write (tty, ' ', char_desc[cc].weight:6);
    write (tty, '  ');
    for j := 1 to char_desc[cc].length do
      write (tty, char_desc[cc].encoding[j]:1);
    writeln (tty);
    bit_count := bit_count + char_desc[cc].weight * char_desc[cc].length;
  end;
  writeln (tty, bit_count, ' bits; ', (bit_count+35) div 36, ' words');
end.
