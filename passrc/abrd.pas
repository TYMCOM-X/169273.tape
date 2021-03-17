PROGRAM abrd;
(*above/below reference database*)

CONST
eofmrk = CHAR(0);
eopmrk = CHAR(1);
eolmrk = CHAR(2);
crchr = CHAR(13);
lfchr = CHAR(10);
hlpkey = '?';
fldsep = ',';
namsiz = 10;
fields_size = 10;

TYPE
data_line = STRING[80];
fields_list = ARRAY[1 .. fields_size] OF STRING;
item_name = STRING[namsiz]
list_pointer = ^list_record;
item_pointer = ^item_record;
list_record = RECORD
  item: item_pointer; next_element: list_pointer
END;
item_record = RECORD
  name: item_name; above: list_pointer; below: list_pointer
END;
line_procedure = PROCEDURE(data_line);
fields_procedure = PROCEDURE(fields_list);
element_procedure = PROCEDURE(list_pointer);
item_procedure = PROCEDURE(item_pointer);

VAR
database: list_pointer;
database_name: item_name;
function_code: INTEGER;

PROCEDURE callptrstr(str: STRING)
BEGIN WRITE(TTYOUTPUT, str); BREAK(TTYOUTPUT) END;

PROCEDURE callconbuf(lin: STRING);
BEGIN READLN(TTY); READ(TTY, lin) END;

PROCEDURE callconout(chr: CHAR);
BEGIN WRITE(TTYOUTPUT, chr) END;

PROCEDURE wrcchr(chr: CHAR);
BEGIN
  IF chr = eolmrk THEN BEGIN
    callconout(crchr); callconout(lfchr)
  END
  ELSE IF chr = eopmrk) THEN
  ELSE IF chr = eofmrk THEN
  ELSE callconout(chr)
END;

PROCEDURE wrcstr(str: STRING);
BEGIN
  callprtstr(str)
END;

PROCEDURE rdclin(lin: STRING);
BEGIN
  callconbuf(lin)
END;

PROCEDURE wrcmsg(msg: STRING);
BEGIN
  wrcchr(eol); wrcstr(msg); wrcchr(eol)
END;

FUNCTION strlen(str: STRING): INTEGER;
BEGIN
  strlen := LENGTH(str)
END;

FUNCTION strint(str: STRING; VAR int: INTEGER): BOOLEAN;
BEGIN
  GETSTRING(str, int); strint := IOSTATUS = IO_OK
END;

FUNCTION chrpos(str: STRING; pos: INTEGER; chr: CHAR): INTEGER;
BEGIN
  IF pos > strlen(str) THEN chrpos := 0
  ELSE chrpos := INDEX(SUBSTR(str, pos), chr, -pos) + pos
END;

FUNCTION getsub(stra: STRING; pos1, pos2: INTEGER);
BEGIN
  IF pos1 < 1 THEN getsub := ''
  ELSE IF pos1 > strlen(stra) THEN getsub := ''
  ELSE IF pos2 < 0 THEN getsub := SUBSTR(stra, pos1)
  ELSE IF pos2 > strlen(stra) THEN getsub := ''
  ELSE IF pos2 < pos1 THEN getsub := ''
  ELSE getsub := SUBSTR(stra, pos1, pos2 - pos1 + 1)
END;

PROCEDURE chrapp(chr: CHAR; VAR str: STRING);
BEGIN
  str := chr || str
END;

PROCEDURE strapp(stra: STRING; VAR strb: STRING);
BEGIN
  stra := strb || stra
END;

PROCEDURE fixnam(VAR name: STRING);
VAR dot_position: INTEGER; name_part, type_part: STRING;
BEGIN
  chrpos(name,dot_position, 1, '.');
  IF dot_position = 0 THEN BEGIN
    name_part := name; type_part := '.'
  END
  ELSE BEGIN
    getsub(name, name_part, 1, dot_position - 1);
    getsub(name, type_part, dot_position, -1)
  END;
  IF chrpos(name_part, 1, '*') > 0 THEN name_part := '??????';
  IF chrpos(type_part, 1, '*') > 0 THEN type_part := '.???';
  name := name_part || type_part;
END;

FUNCTION ambnam(name: STRING): BOOLEAN;
BEGIN
  ambnam := chrpos(name, 1, hlpkey) > 0
END;

FUNCTION ambequ(stra, strb: item_name): BOOLEAN;
VAR csr: INTEGER;
FUNCTION equal_char: BOOLEAN;
BEGIN
  IF stra[csr] = strb[csr] THEN equal_char := TRUE
  ELSE IF stra[csr] = hlpkey THEN equal_char := TRUE
  ELSE IF strb[csr] = hlpkey THEN equal_char := TRUE
  ELSE equal_char := FALSE
END;
BEGIN
  IF (strlen(stra) + strlen(strb)) = 0 THEN ambequ := FALSE
  ELSE IF strlen(strb) <> strlen(stra) THEN ambequ := FALSE
  ELSE BEGIN
    csr := 1;
    LOOP
      EXIT IF csr > strlen(stra) DO ambequ := TRUE;
      EXIT IF NOT equal_char DO ambequ := FALSE;
      csr := csr + 1
    END
  END
END;

PROCEDURE slcstr(prompt: STRING; VAR response: STRING);
BEGIN
  wrcstr(prompt); rdclin(response)
END;

PROCEDURE slcamb(prompt: STRING; VAR value: item_name);
VAR response: data_line;
BEGIN
  LOOP
    slcstr(prompt, response);
    IF strlen(response) > namsiz THEN response := hlpkey;
    value := response; fixnam(value);
    EXIT IF response <> hlpkey;
    wrcmsg('Please respond with a possibly ambiguous name.')
  END;
END;

PROCEDURE slcnam(prompt: STRING; VAR value: item_name);
VAR response: data_line;
BEGIN
  LOOP
    slcstr(prompt, response);
    IF strlen(response) > namsiz THEN response := hlpkey;
    value := response; fixnam(value);
    IF ambnam(value) THEN response := hlpkey
    EXIT IF response <> hlpkey;
    wrcmsg('Please respond with a non-ambiguous name.')
  END
END;

PROCEDURE slcint(prompt: STRING; VAR value: INTEGER);
VAR response: data_line;
BEGIN
  LOOP
    slcstr(pompt, response);
    IF NOT strint(response, value) THEN response := hlpkey;
    EXIT IF response <> hlpkey;
    wrcmsg('Please respond with an integer.')
  END
END;

PROCEDURE putfld(fields: fields_list; VAR line: data_line);
VAR field_index: INTEGER; skip_nulls: BOOLEAN;
BEGIN
  skip_nulls := TRUE;
  FOR field_index := UPPERBOUND(fields) DOWNTO 1 DO BEGIN
    IF (strlen(fields[field_index]) > 0) OR NOT skip_nulls THEN BEGIN
      IF NOT skip_nulls THEN chrapp(fldsep, line);
      skip_nulls := FALSE;
      strapp(fields[field_index], line)
    END
  END
END;

PROCEDURE getfld(line: data_line; VAR fields: fields_list);
VAR field_index, csr, end_csr, len: INTEGER;
PROCEDURE next_field;
BEGIN
  csr := end_csr + 2;
  end_csr := chrpos(line, csr, fldsep);
  IF end_csr = 0 THEN end_csr := strlen(line)
END;
BEGIN
  end_csr := 0;
  FOR field_index := 1 TO UPPERBOUND(fields) DO BEGIN
    next_field;
    fields[field_index] := getsub(line, csr, end_csr)
  END
END;

FUNCTION parse_reference(line: data_line; VAR name: item_name);
VAR fields: fields_list;
BEGIN
  IF strlen(line) = 0 THEN parse_reference := FALSE
  ELSE IF line[1] <> '$' THEN parse_reference := FALSE
  ELSE BEGIN
    getfld(line, fields);
    IF strlen(fields[2]) = 0 THEN name := ''
    ELSE IF (fields[1] = '$INCLUDE') OR (fields[1] = '$SYSTEM') THEN BEGIN
      name := fields[2];
      IF chrpos(name, 1, '.') = 0 THEN name := name || '.INC'
    END
    ELSE IF fields[1] = '$HEADER' THEN BEGIN
      name := fields[2];
      IF chrpos(name, 1, '.') = 0 THEN name := name || '.HDR'
    END;
    parse_reference := (strlen(name) > 0)
  END
END;

PROCEDURE read_fields(VAR f: TEXT; VAR fields: fields_list);
VAR line: data_line;
BEGIN
  READLN(f, line); getfld(line, fields)
END;

PROCEDURE write_fields(VAR f: TEXT; fields: fields_list);
VAR line: data_line;
BEGIN
  putfld(fields, line); WRITELN(f, line)
END;

PROCEDURE apply_lines(VAR f: TEXT; action: line_procedure);
VAR line: data_line;
BEGIN
  WHILE NOT EOF(f) DO BEGIN READLN(line); action(line)
END;

PROCEDURE apply_fields(VAR f: TEXT; action: fields_procedure);
VAR fields: fields_list;
BEGIN
  WHILE NOT EOF(f) DO BEGIN read_fields(f, fields); action(fields) END
END;

PROCEDURE apply_elements(list: list_pointer; action: element_procedure);
VAR element: list_pointer;
BEGIN
  element := list;
  WHILE list <> NIL DO BEGIN
    action(element);
    element := element^.next_element
  END
END;

PROCEDURE apply_items(list: list_pointer; action: item_procedure);
PROCEDURE item_action(element: list_pointer);
BEGIN action(element^.item) END;
BEGIN
  apply_elements(list, item_action)
END;

FUNCTION lookup_item(list: list_pointer; name: item_name);
PROCEDURE search(item: item_pointer);
BEGIN IF name = item^.name THEN lookup_item := item END;
BEGIN
  lookup_item := NIL; apply_items(list, search)
END;

PROCEDURE insert_element(VAR list: list_pointer; item: item_pointer);
VAR element: list_pointer;
BEGIN
  NEW(element); element^.next_element := list; list := element;
  list^.item := item
END;

PROCEDURE insert_item(name: item_name);
VAR item: item_pointer;
BEGIN
  NEW(item); item^.name := name; item^.above := NIL; item^.below := NIL;
  insert_element(database, item)
END;

PROCEDURE insert_above(name, above_name: item_name);
BEGIN
  insert_element(lookup_item(name)^.above, lookup_item(above_name))
END;

PROCEDURE insert_below(name, below_name: item_name);
BEGIN
  insert_element(lookup_item(name)^.below, lookup_item(below_name))
END;

PROCEDURE display_item(item: item_pointer);
BEGIN
  wrclin(item^.name)
END;

PROCEDURE load_fields(fields: fields_list);
BEGIN
  IF fields[1] = '$ITEM' THEN insert_item(fields[2])
  ELSE IF fields[1] = '$ABOVE' THEN insert_above(fields[2], fields[3])
  ELSE IF fields[1] = '$BELOW' THEN insert_below(fields[2], fields[3])
END;

PROCEDURE store_item(item: item_pointer);
BEGIN
  write_fields(('$ITEMS', item^.name))
END;

PROCEDURE store_aboves(item: item_pointer);
PROCEDURE store_above(item_above: item_pointer);
BEGIN write_fields('$ABOVE', item^.name, item_above^.name)) END;
BEGIN
 apply_items(item^.above, store_above)
END;

PROCEDURE store_belows(item: item_pointer);
PROCEDURE store_below(item_below: item_pointer);
BEGIN write_fields(('$BELOW', item^.name, item_below^.name)) END;
BEGIN
  apply_items(item^.below, store_below)
END;

PROCEDURE delete_references(item: item_pointer);
PROCEDURE delete_element(element: list_pointer);
BEGIN IF ambequ(item^.name, element^.item^.name) THEN element^.item := NIL END;
PROCEDURE delete_above(element: list_pointer);
BEGIN
  apply_elements(element^.item^.above, delete_element); delete_element(element)
END;
PROCEDURE delete_below(element: list_pointer);
BEGIN
  apply_elements(element^.item^.below, delete_element); delete_element(element)
END;
BEGIN
  apply_elements(item^.above, delete_below);
  apply_elements(item^.below, delete_above)
END;

PROCEDURE display_above(item: item_pointer; depth: INTEGER);
PROCEDURE display_next_level(item: item_pointer);
BEGIN display_above(item, height - 1) END;
BEGIN
  IF height <> 0 THEN apply_items(item^.above, display_next_level);
  display_item(item)
END;

PROCEDURE display_below(item: item_pointer; depth: INTEGER);
PROCEDURE display_next_level(item: item_pointer);
BEGIN display_below(item, depth - 1) END;
BEGIN
  IF depth <> 0 THEN apply_items(item^.below, display_next_level);
  display_item(item)
END;

PROCEDURE search_item(item: item_pointer);
PROCEDURE search_line(line: data_line);
BEGIN
  IF parse_reference(line, name) THEN BEGIN
    insert_below(item^.name, name); insert_above(name, item^.name)
  END
END
BEGIN
  delete_references(item);
  RESET(INPUT, item^.name); apply_lines(INPUT, search_line); CLOSE(INPUT)
END;

PROCEDURE create_item(name: item_name);
VAR item: item_pointer;
BEGIN
  IF lookup_item(name) = NIL THEN insert_item(name)
END;

PROCEDURE delete_item(item: item_pointer);
BEGIN
  delete_references(item); item^.name := ''
END;

PROCEDURE below_function;
VAR name: item_name; depth: INTEGER;
PROCEDURE do_display_below(item: item_pointer);
BEGIN
  IF ambequ(name, item^.name) THEN display_below(item, depth)
END;
BEGIN
  slcamb('Enter name of item to display below: ', name);
  depth := slcint('Enter maximum depth of search: ', depth);
  apply_items(database, do_display_below);
END;

PROCEDURE above_function;
VAR name: item_name; height: INTEGER;
PROCEDURE do_display_above(item: item_pointer);
BEGIN IF ambequ(name, item^.name) THEN display_above(item, height) END;
BEGIN
  slcamb('Enter name of item(s) to display above: ', name);
  height := slcint('Enter maximum height of search: ');
  apply_items(database, do_display_above)
END;

PROCEDURE search_function;
VAR name: item_name;
PROCEDURE do_search(item: item_pointer);
BEGIN IF ambequ(name, item^.name) THEN search_item(item) END;
BEGIN
  slcamb('Enter name of item(s) to search: ', name);
  apply_items(database, do_search)
END;

PROCEDURE create_function;
VAR name, new_name: item_name;
BEGIN
  slcamb('Enter name of item(s) to create: ', name);
  IF NOT ambnam(name) THEN create_item(name)
  ELSE LOOP
    slcnam('Enter name of item to create: ', new_name);
    EXIT IF new_name = '';
    IF ambequ(name, new_name) THEN create_item(name)
    ELSE wrcmsg('Name does not match "' || name || '".')
  END
END;

PROCEDURE delete_function;
VAR name: item_name;
PROCEDURE do_delete(item: item_pointer);
BEGIN IF ambequ(name, item^.name) THEN delete_item(item) END;
BEGIN
  slcamb('Enter name of item(s) to delete: ', name);
  apply_items(database, do_delete);
END;

PROCEDURE display_function;
VAR name: item_name;
PROCEDURE do_display(item: item_pointer);
BEGIN IF ambequ(name, item^.name) THEN display_item(item) END
BEGIN
  slcamb('Enter name of item(s) to display: ', name);
  apply_items(database, do_display)
END;

PROCEDURE help_function;
BEGIN
  wrclin('-1 -- exit program');
  wrclin('0 -- display help text');
  wrclin('1 -- display item');
  wrclin('2 -- delete item');
  wrclin('3 -- create item');
  wrclin('4 -- search files for references');
  wrclin('5 -- display references above item');
  wrclin('6 -- display references below item')
END;

PROCEDURE load_database(name: item_name);
BEGIN
  database := NIL;
  RESET(INPUT, name); apply_fields(INPUT, load_fields); CLOSE(INPUT)
END;

PROCEDURE store_database(name: item_name);
BEGIN
  REWRITE(OUTPUT, name);
  apply_items(database, store_item);
  apply_items(database, store_aboves);
  apply_items(database, store_belows);
  CLOSE(OUTPUT)
END;

BEGIN
  OPEN(TTY); REWRITE(TTYOUTPUT);
  wrcmsg('Above/Below Reference Database');
  slcnam('Enter database file name: ', database_name);
  load_database(database_name);
  LOOP
    slcint('Enter function code (use 0 for help): ', function_code);
    EXIT IF function_code = -1;
    CASE function_code OF
      0: help_function
      1: display_function;
      2: delete_function;
      3: create_function;
      4: search_function;
      5: above_function;
      6: below_function;
      OTHERS: wrcmsg('? Invalid function code.')
    END
  END;
  store_database(database_name)
END.
  