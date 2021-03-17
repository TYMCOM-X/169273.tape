$TITLE SRC.PAS, last modified 4/30/84, zw
PROGRAM src;
$SYSTEM VERSIO.INC

CONST
name_size = 10;
type_size = 4;
field_size = 20;
line_size = 80;

TYPE
line_string = STRING[line_size];
field_string = STRING[field_size];
field_list = ARRAY [1 .. *] OF field_string;
name_string = STRING[name_size];
type_string = STRING[type_size];
list_pointer = ^list_record;
item_pointer = ^item_record;
list_record = RECORD (*item=NIL signifies deleted element record*)
  item: item_pointer;
  next_element: list_pointer
END;
item_record = RECORD (*name='' signifies deleted item record*)
  name: name_string;
  above, below: list_pointer;
  has_been_visited: BOOLEAN;
END;
line_procedure = PROCEDURE(line_string);
fields_procedure =  PROCEDURE(field_list);
element_procedure =  PROCEDURE(list_pointer);
item_procedure = PROCEDURE(item_pointer);

VAR
debug_flag: BOOLEAN;
database: list_pointer;
database_name: name_string;
command_fields: ARRAY [1 .. 3] OF field_string;
left_margin: INTEGER := 1;

PROCEDURE prepare_left_margin;
BEGIN
  WRITE(TTYOUTPUT, ' ': left_margin)
END;

PROCEDURE indent;
BEGIN
  left_margin := left_margin + 2
END;

PROCEDURE unindent;
BEGIN
  left_margin := left_margin - 2
END;

PROCEDURE read_console_line(VAR line: STRING[*]);
BEGIN
  BREAK(TTYOUTPUT);
  READLN(TTY);
  READ(TTY, line)
END;

PROCEDURE display_prompt(prompt: STRING[*]);
BEGIN
  prepare_left_margin;
  WRITE(TTYOUTPUT, prompt);
  BREAK(TTYOUTPUT)
END;

PROCEDURE display_line(line: STRING[*]);
BEGIN
  prepare_left_margin;
  WRITELN(TTYOUTPUT, line);
  BREAK(TTYOUTPUT)
END;

PROCEDURE display_number(number: INTEGER);
BEGIN
  prepare_left_margin;
  WRITELN(TTYOUTPUT, number);
  BREAK(TTYOUTPUT)
END;

PROCEDURE display_message(message: STRING[*]);
BEGIN
  display_line('');
  display_line(message)
END;

PROCEDURE debug_message(message: STRING[*]);
BEGIN
  IF debug_flag THEN display_message('DEBUG: ' || message)
END;

FUNCTION find_character (string0: STRING[*]; position: INTEGER;
  character: CHAR): INTEGER;
VAR
x: INTEGER;
BEGIN
  IF position < 0 THEN x := LENGTH(string0) + position + 1
  ELSE x := position;
  IF (x < 1) ORIF (x > LENGTH(string0)) THEN find_character := 0
  ELSE find_character := INDEX(SUBSTR(string0, x), character, 1 - x) + x - 1
END;

PROCEDURE substring (VAR string1: STRING[*]; string2: STRING[*];
  position1, position2: INTEGER);
VAR
x, y: INTEGER;
BEGIN
  IF position1 < 0 THEN x := LENGTH(string2) + position1 + 1
  ELSE x := position1;
  IF position2 < 0 THEN y := LENGTH(string2) + position2 + 1
  ELSE y := position2;
  IF (x < 1) OR (x > LENGTH(string2)) THEN string1 := ''
  ELSE IF (y < 1) OR (y > LENGTH(string2)) THEN string1 := ''
  ELSE IF y > x THEN string1 := SUBSTR(string2, x, MIN(UPPERBOUND( string1)
    , y - x + 1))
  ELSE string1 := SUBSTR(string2, y, MIN(UPPERBOUND(string1) , x - y + 1))
END;

PROCEDURE append_character(VAR string0: STRING[*]; character: CHAR);
BEGIN
  IF LENGTH(string0) < UPPERBOUND(string0)
    THEN string0 := string0 || character
END;

PROCEDURE append_string(VAR string2: STRING[*]; string1: STRING[*]);
BEGIN
  string2 := string2 || SUBSTR(string1, 1, MIN(UPPERBOUND(string2)
    , LENGTH(string2) + LENGTH(string1)) - LENGTH(string2))
END;

PROCEDURE right_trim(VAR string0: STRING[*]);
VAR
position: INTEGER;
BEGIN
  position := LENGTH(string0);
  WHILE (position > 0) ANDIF (string0[position] = ' ')
    DO position := position - 1;
  substring(string0, string0, 1, position)
END;

PROCEDURE prepare_name (VAR item_name: name_string; default_type: type_string)
  ;
VAR
dot_position: INTEGER;
item_type: type_string;
BEGIN
  dot_position := find_character(item_name, 1, '.');
  IF dot_position = 0 THEN item_type := default_type
  ELSE BEGIN
    substring(item_type, item_name, dot_position, -1);
    substring(item_name, item_name, 1, dot_position - 1);
  END;
  IF find_character(item_name, 1, '*') > 0 THEN item_name := '??????'
  ELSE append_string(item_name, '      ');
  IF find_character(item_type, 1, '*') > 0 THEN item_type := '.???'
  ELSE append_string(item_type, '   ');
  substring(item_name, item_name, 1, 6);
  substring(item_type, item_type, 1, 4);
  append_string(item_name, item_type);
  item_name := UPPERCASE(item_name)
END;

FUNCTION unprepared_name(item_name: name_string): name_string;
VAR
dot_position: INTEGER;
item_type: type_string;
BEGIN
  unprepared_name := item_name;
  prepare_name(unprepared_name, '.  ');
  dot_position := find_character(unprepared_name, 1, '.');
  substring(item_type, unprepared_name, dot_position, -1);
  substring(unprepared_name, unprepared_name, 1, dot_position - 1);
  right_trim(unprepared_name);
  right_trim(item_type);
  IF LENGTH(item_type) > 1 THEN append_string(unprepared_name, item_type)
END;

FUNCTION ambiguous_name(item_name: name_string): BOOLEAN;
BEGIN
  ambiguous_name := find_character(item_name, 1, '?') > 0
END;

FUNCTION ambiguous_match(name1, name2: name_string): BOOLEAN;
VAR
position: INTEGER;

  FUNCTION matched(character1, character2: CHAR): BOOLEAN;
  BEGIN
    IF character1 = character2 THEN matched := TRUE
    ELSE IF character1 = '?' THEN matched := TRUE
    ELSE IF character2 = '?' THEN matched := TRUE
    ELSE matched := FALSE
  END;
BEGIN
  IF (LENGTH(name1) + LENGTH(name2)) = 0 THEN ambiguous_match := FALSE
  ELSE BEGIN
    ambiguous_match := TRUE;
    FOR position := 1 TO MIN(LENGTH(name1), LENGTH(name2))
      DO EXIT IF NOT matched(name1[position], name2[position])
      DO ambiguous_match := FALSE;
    IF ambiguous_match THEN BEGIN
      IF LENGTH(name1) < LENGTH(name2) THEN BEGIN
	FOR position := LENGTH(name1) + 1 TO LENGTH(name2)
	  DO EXIT IF NOT matched(name2[position], ' ')
	  DO ambiguous_match := FALSE
      END
      ELSE BEGIN
	FOR position := LENGTH(name2) + 1 TO LENGTH(name1)
	  DO EXIT IF NOT matched(name1[position], ' ')
	  DO ambiguous_match := FALSE
      END
    END
  END
END;

PROCEDURE fields_to_line(fields: field_list; VAR line: line_string);
VAR
field_index: INTEGER;
BEGIN
  line := '';
  FOR field_index := 1 TO UPPERBOUND(fields) DO BEGIN
    IF field_index > 1 THEN append_character(line, ' ');
    append_string(line, fields[field_index])
  END
END;

PROCEDURE line_to_fields(line: line_string; VAR fields: field_list);
VAR
field_index, position, end_position: INTEGER;

  PROCEDURE next_field;
  BEGIN
    position := end_position + 1;
    WHILE find_character(line, position, ' ')
      = position DO position := position + 1;
    end_position := find_character(line, position, ' ') - 1;
    IF end_position < position THEN end_position := LENGTH(line)
  END;
BEGIN
  end_position := 0;
  FOR field_index := 1 TO UPPERBOUND(fields) DO BEGIN
    next_field;
    substring(fields[field_index], line, position, end_position)
  END
END;

FUNCTION string_to_integer(str: STRING[*]; VAR value: INTEGER) : BOOLEAN;
BEGIN
  GETSTRING(str, value);
  string_to_integer := iostatus = IO_OK
END;

FUNCTION string_to_name (str: STRING[*]; VAR item_name: name_string;
  default_type: type_string): BOOLEAN;
BEGIN
  string_to_name := (LENGTH(str) > 0) AND (LENGTH(str)
    <= UPPERBOUND(item_name));
  IF string_to_name THEN BEGIN
    item_name := str;
    prepare_name(item_name, default_type)
  END
END;

FUNCTION solicit_string(prompt: STRING[*]; VAR response: STRING[*]) : BOOLEAN;
BEGIN
  display_line('');
  display_prompt(prompt);
  read_console_line(response);
  solicit_string := response <> ''
END;

FUNCTION solicit_fields(prompt: STRING[*]; VAR fields: field_list) : BOOLEAN;
VAR
response: line_string;
BEGIN
  REPEAT
    solicit_fields := solicit_string(prompt, response);
    IF solicit_fields THEN BEGIN
      line_to_fields(response, fields);
      IF response = '?' THEN display_message(
	'Please respond with field value(s).')
    END
  UNTIL NOT solicit_fields OR (response <> '?')
END;

FUNCTION solicit_name (prompt: STRING; VAR value: name_string;
  default_type: type_string): BOOLEAN;
VAR
response: line_string;
BEGIN
  REPEAT
    solicit_name := solicit_string(prompt, response);
    IF solicit_name THEN BEGIN
      IF NOT string_to_name(response, value, default_type)
	THEN response := '?';
      IF response = '?' THEN display_message( 'Please respond with a name.')
    END
  UNTIL NOT solicit_name OR (response <> '?')
END;

FUNCTION solicit_integer(prompt: STRING[*]; VAR value: INTEGER) : BOOLEAN;
VAR
response: line_string;
BEGIN
  REPEAT
    solicit_integer := solicit_string(prompt, response);
    IF solicit_integer THEN BEGIN
      IF NOT string_to_integer(response, value) THEN response := '?';
      IF response = '?' THEN display_message(
	'Please respond with an integer.')
    END
  UNTIL NOT solicit_integer OR (response <> '?')
END;

FUNCTION scan_reference (line: line_string; VAR item_name: name_string)
  : BOOLEAN;
VAR
fields: ARRAY[1 .. 2] OF field_string;
prefix: STRING[2];
default_type: type_string;
BEGIN
  IF LENGTH(line) = 0 THEN scan_reference := FALSE
  ELSE IF find_character('$(', 1, line[1]) = 0 THEN scan_reference := FALSE
  ELSE BEGIN
    line_to_fields(line, fields);
    substring(prefix, fields[1], 1, 2);
    IF prefix = '(*' THEN BEGIN
      substring(fields[1], fields[1], 2, -1);
      substring(fields[2], fields[2], 1, -2);
    END
    ELSE IF line[1] = '$' THEN substring(fields[1], fields[1], 2, -1)
    ELSE fields[1] := '';
    IF fields[1] = 'SYSTEM' THEN default_type := '.INC'
    ELSE IF fields[1] = 'INCLUDE' THEN default_type := '.INC'
    ELSE IF fields[1] = 'HEADER' THEN default_type := '.HDR'
    ELSE fields[2] := '';
    scan_reference := string_to_name(fields[2], item_name, default_type)
  END
END;

PROCEDURE display_item(item: item_pointer);
BEGIN
  IF item = NIL THEN RETURN;
  IF LENGTH(item^.name) = 0 THEN RETURN;
  display_line(unprepared_name(item^.name))
END;

PROCEDURE write_fields(fields: field_list);
VAR
line: line_string;
BEGIN
  fields_to_line(fields, line);
  WRITELN(line)
END;

PROCEDURE apply_to_file(item_name: name_string; action: line_procedure);
VAR
line: line_string;
BEGIN
  RESET(INPUT, item_name);
  IF iostatus <> IO_OK THEN display_message('Can not access: ' ||
    unprepared_name(item_name))
  ELSE WHILE NOT EOF(INPUT) DO BEGIN
    READLN(line);
    action(line)
  END;
  CLOSE(INPUT)
END;

PROCEDURE apply_to_elements(list: list_pointer; action: element_procedure);
VAR
element: list_pointer;
BEGIN
  element := list;
  WHILE element <> NIL DO BEGIN
    action(element);
    element := element^.next_element
  END
END;

PROCEDURE apply_to_items(list: list_pointer; action: item_procedure);

  PROCEDURE item_action(element: list_pointer);
  BEGIN
    IF element = NIL THEN RETURN;
    IF element^.item <> NIL THEN action(element^.item)
  END;
BEGIN
  apply_to_elements(list, item_action)
END;

FUNCTION lookup_element(list: list_pointer; item: item_pointer)
  : list_pointer;

  PROCEDURE SEARCH(element: list_pointer);
  BEGIN
    IF element = NIL THEN RETURN;
    IF element^.item = item THEN lookup_element := element
  END;
BEGIN
  lookup_element := NIL;
  apply_to_elements(list, SEARCH)
END;

FUNCTION lookup_item(list: list_pointer; item_name: name_string)
  : item_pointer;

  PROCEDURE SEARCH(item: item_pointer);
  BEGIN
    IF item = NIL THEN RETURN;
    IF item^.name = item_name THEN lookup_item := item
  END;
BEGIN
  lookup_item := NIL;
  apply_to_items(list, SEARCH)
END;

PROCEDURE unvisit;

  PROCEDURE not_visited(item: item_pointer);
  BEGIN
    IF item <> NIL THEN item^.has_been_visited := FALSE
  END;
BEGIN
  apply_to_items(database, not_visited)
END;

FUNCTION valid_item(item: item_pointer): BOOLEAN;
BEGIN
  valid_item := (item <> NIL) ANDIF (LENGTH(item^.name) > 0) ANDIF
    NOT item^.has_been_visited
END;

FUNCTION valid_element(element: list_pointer): BOOLEAN;
BEGIN
  valid_element := (element <> NIL) ANDIF valid_item(element^.item)
END;

PROCEDURE insert_element(VAR list: list_pointer; item: item_pointer);
VAR
element: list_pointer;
BEGIN
  element := lookup_element(list, NIL);
  IF element = NIL THEN BEGIN
    NEW(element);
    element^.next_element := list;
    list := element;
  END;
  element^.item := item
END;

PROCEDURE insert_item(item_name: name_string);
VAR
item: item_pointer;
BEGIN
  debug_message('Inserting '||item_name);
  IF lookup_item(database, item_name) = NIL THEN BEGIN
    item := lookup_item(database, '');
    IF item = NIL THEN BEGIN
      debug_message('Item record allocated.');
      NEW(item);
      item^.above := NIL;
      item^.below := NIL;
      insert_element(database, item)
    END;
    item^.name := item_name;
    item^.has_been_visited := FALSE
  END
  ELSE debug_message('Item already inserted.')
END;

PROCEDURE insert_above(item, above_item: item_pointer);
BEGIN
  IF NOT (valid_item(item) ANDIF valid_item(above_item)) THEN RETURN;
  debug_message('Inserting ' || above_item^.name || ' above ' || item^ .name);
  IF lookup_item(item^.above, above_item^.name)
    = NIL THEN insert_element(item^.above, above_item)
  ELSE debug_message('Above item already inserted.')
END;

PROCEDURE insert_below(item, below_item: item_pointer);
BEGIN
  IF NOT (valid_item(item) ANDIF valid_item(below_item)) THEN RETURN;
  debug_message('Inserting ' || below_item^.name || ' below ' || item^ .name);
  IF lookup_item(item^.below, below_item^.name)
    = NIL THEN insert_element(item^.below, below_item)
  ELSE debug_message('Below item already inserted.')
END;

PROCEDURE reference(item: item_pointer; reference_name: name_string);
VAR
referenced_item: item_pointer;
source_name: name_string;
BEGIN
  debug_message(item^.name || ' references ' || reference_name);
  referenced_item := lookup_item(database, reference_name);
  IF referenced_item = NIL THEN BEGIN
    insert_item(reference_name);
    referenced_item := lookup_item(database, reference_name);
    display_item(referenced_item)
  END;
  insert_above(referenced_item, item);
  insert_below(item, referenced_item);
  substring(source_name, reference_name, find_character(reference_name , 1,
    '.'), -1);
  IF source_name = '.INC' THEN BEGIN
    substring(source_name, reference_name, 1, find_character( source_name, 1,
      '.'));
    append_string(source_name, 'PAS');
    reference(referenced_item, source_name)
  END
END;

PROCEDURE load_line(line: line_string);
VAR
fields: ARRAY [1 .. 3] OF field_string;
name1, name2: name_string;
BEGIN
  line_to_fields(line, fields);
  fields[1] := UPPERCASE(fields[1]);
  IF fields[1] = 'ITEM' THEN BEGIN
    IF string_to_name(fields[2], name1, '.PAS') THEN insert_item(name1)
  END
  ELSE IF fields[1] = 'ABOVE' THEN BEGIN
    IF string_to_name(fields[2], name1, '.PAS') ANDIF
      string_to_name(fields[3], name2, '.PAS')
      THEN insert_above(lookup_item(database, name1)
      , lookup_item(database, name2))
  END
  ELSE IF fields[1] = 'BELOW' THEN BEGIN
    IF string_to_name(fields[2], name1, '.PAS') ANDIF
      string_to_name(fields[3], name2, '.PAS')
      THEN insert_below(lookup_item(database, name1)
      , lookup_item(database, name2))
  END
END;

PROCEDURE store_item(item: item_pointer);
BEGIN
  IF valid_item(item) THEN write_fields(('ITEM', unprepared_name(item^.name)))
END;

PROCEDURE store_aboves(item: item_pointer);

  PROCEDURE store_above(item_above: item_pointer);
  BEGIN
    IF valid_item(item_above)
      THEN write_fields(('ABOVE', unprepared_name(item^.name)
      , unprepared_name(item_above^.name)))
  END;
BEGIN
  IF valid_item(item) THEN apply_to_items(item^.above, store_above)
END;

PROCEDURE store_belows(item: item_pointer);

  PROCEDURE store_below(item_below: item_pointer);
  BEGIN
    IF valid_item(item_below)
      THEN write_fields(('BELOW', unprepared_name(item^.name)
      , unprepared_name(item_below^.name)))
  END;
BEGIN
  IF valid_item(item) THEN apply_to_items(item^.below, store_below)
END;

PROCEDURE delete_references(item: item_pointer);

  PROCEDURE delete_element(element: list_pointer);
  BEGIN
    IF valid_element(element) ANDIF
      ambiguous_match(item^.name, element^.item^.name)
      THEN element^.item := NIL
  END;

  PROCEDURE delete_above(element: list_pointer);
  BEGIN
    IF valid_element(element) THEN BEGIN
      apply_to_elements(element^.item^.above, delete_element);
      delete_element(element)
    END
  END;

  PROCEDURE delete_below(element: list_pointer);
  BEGIN
    IF valid_element(element) THEN BEGIN
      apply_to_elements(element^.item^.below, delete_element);
      delete_element(element)
    END
  END;
BEGIN
  IF NOT valid_item(item) THEN RETURN;
  debug_message('Deleting references to ' || item^.name);
  apply_to_elements(item^.above, delete_below);
  apply_to_elements(item^.below, delete_above)
END;

PROCEDURE display_above(item: item_pointer; height: INTEGER);

  PROCEDURE display_next_level(item: item_pointer);
  BEGIN
    IF valid_item(item) THEN display_above(item, height - 1)
  END;
BEGIN
  IF NOT valid_item(item) THEN RETURN;
  IF height <> 0 THEN BEGIN
    display_item(item);
    indent;
    apply_to_items(item^.above, display_next_level);
    unindent
  END
END;

PROCEDURE display_below(item: item_pointer; depth: INTEGER);

  PROCEDURE display_next_level(item: item_pointer);
  BEGIN
    IF valid_item(item) THEN display_below(item, depth - 1)
  END;
BEGIN
  IF NOT valid_item(item) THEN RETURN;
  IF depth <> 0 THEN BEGIN
    display_item(item);
    indent;
    apply_to_items(item^.below, display_next_level);
    unindent
  END
END;

PROCEDURE search_item(item: item_pointer);

  PROCEDURE search_line(line: line_string);
  VAR
  item_name, source_name: name_string;
  item_type: type_string;
  BEGIN
    IF LENGTH(line) = 0 THEN RETURN;
    IF scan_reference(line, item_name) THEN reference(item, item_name) ;
  END;
BEGIN
  IF NOT valid_item(item) THEN RETURN;
  delete_references(item);
  display_item(i
  indent;
  apply_to_file(item^.name, search_line);
  unindent
END;

PROCEDURE create_item(item_name: name_string);
BEGIN
  IF (LENGTH(item_name) = 0) OR ambiguous_name(item_name) THEN RETURN;
  IF NOT valid_item(lookup_item(database, item_name)) THEN BEGIN
    indent;
    insert_item(item_name);
    display_line('');
    display_item(lookup_item(database, item_name));
    unindent
  END
  ELSE display_message('Item already exists.')
END;

PROCEDURE delete_item(item: item_pointer);
BEGIN
  IF NOT valid_item(item) THEN RETURN;
  delete_references(item);
  display_item(item);
  item^.name := ''
END;

FUNCTION measure_height(item: item_pointer): INTEGER;

  PROCEDURE accumulate_height(item: item_pointer);
  BEGIN
    measure_height := MAX(measure_height, measure_height(item))
  END;
BEGIN
  measure_height := 0;
  IF NOT valid_item(item) THEN RETURN;
  apply_to_items(item^.above, accumulate_height);
  measure_height := measure_height + 1
END;

FUNCTION measure_depth(item: item_pointer): INTEGER;

  PROCEDURE accumulate_depth(item: item_pointer);
  BEGIN
    measure_depth := MAX(measure_depth, measure_depth(item))
  END;
BEGIN
  measure_depth := 0;
  IF NOT valid_item(item) THEN RETURN;
  apply_to_items(item^.below, accumulate_depth);
  measure_depth := measure_depth + 1
END;

PROCEDURE item_is_found(VAR item_found: BOOLEAN);
BEGIN
  IF NOT item_found THEN display_line('');
  item_found := TRUE
END;

PROCEDURE was_item_found(item_found: BOOLEAN);
BEGIN
  IF NOT item_found THEN display_message('Item not found.')
END;

PROCEDURE below_function;
VAR
item_name: name_string;
depth: INTEGER;
item_found: BOOLEAN;

  PROCEDURE do_display_below(item: item_pointer);
  BEGIN
    IF NOT valid_item(item) THEN RETURN;
    IF ambiguous_match(item_name, item^.name) THEN BEGIN
      item_is_found(item_found);
      display_below(item, depth)
    END
  END;
BEGIN
  IF string_to_name(command_fields[2], item_name, '.PAS') ORIF
    solicit_name('Enter name of item to display below: ', item_name, '.PAS')
    THEN BEGIN
    IF NOT string_to_integer(command_fields[3], depth) ANDIF
      NOT solicit_integer('Enter maximum depth of search: ', depth)
      THEN depth := -1;
    IF depth < 1 THEN display_message('Unlimited depth.');
    item_found := FALSE;
    indent;
    apply_to_items(database, do_display_below);
    unindent;
    was_item_found(item_found)
  END
END;

PROCEDURE above_function;
VAR
item_name: name_string;
height: INTEGER;
item_found: BOOLEAN;

  PROCEDURE do_display_above(item: item_pointer);
  BEGIN
    IF NOT valid_item(item) THEN RETURN;
    IF ambiguous_match(item_name, item^.name) THEN BEGIN
      item_is_found(item_found);
      display_above(item, height)
    END
  END;
BEGIN
  IF string_to_name(command_fields[2], item_name, '.PAS') ORIF
    solicit_name('Enter name of item(s) to display above: ', item_name ,
    '.PAS') THEN BEGIN
    IF NOT string_to_integer(command_fields[3], height) ANDIF
      NOT solicit_integer('Enter maximum height of search: ', height)
      THEN height := -1;
    IF height < 1 THEN display_message('Unlimited height.');
    item_found := FALSE;
    indent;
    apply_to_items(database, do_display_above);
    unindent;
    was_item_found(item_found)
  END
END;

PROCEDURE search_function;
VAR
item_name: name_string;
item_found: BOOLEAN;
item_count: INTEGER;

  PROCEDURE do_search(item: item_pointer);
  BEGIN
    IF NOT valid_item(item) THEN RETURN;
    IF ambiguous_match(item_name, item^.name) THEN BEGIN
      item_is_found(item_found);
      search_item(item);
      item^.has_been_visited := TRUE;
      item_count := item_count + 1
    END
  END;
BEGIN
  IF string_to_name(command_fields[2], item_name, '.PAS') ORIF
    solicit_name('Enter name of item(s) to search: ', item_name, '.PAS')
    THEN BEGIN
    item_found := FALSE;
    unvisit;
    indent;
    REPEAT
      item_count := 0;
      apply_to_items(database, do_search)
    UNTIL item_count = 0;
    unindent;
    unvisit;
    was_item_found(item_found);
  END
END;

PROCEDURE create_function;
VAR
item_name, new_name: name_string;
BEGIN
  IF string_to_name(command_fields[2], item_name, '.PAS') ORIF
    solicit_name('Enter name of item(s) to create: ', item_name, '.PAS')
    THEN BEGIN
    IF NOT ambiguous_name(item_name) THEN create_item(item_name)
    ELSE WHILE solicit_name('Enter name of item to create: ', new_name ,
      '.PAS') DO BEGIN
      IF ambiguous_name(new_name)
	THEN display_message('Name can not be ambiguous.')
      ELSE IF ambiguous_match(item_name, new_name) THEN create_item(new_name)
      ELSE display_message('Name does not match.')
    END
  END
END;

PROCEDURE delete_function;
VAR
item_name: name_string;
item_found: BOOLEAN;

  PROCEDURE do_delete(item: item_pointer);
  BEGIN
    IF NOT valid_item(item) THEN RETURN;
    IF ambiguous_match(item_name, item^.name) THEN BEGIN
      item_is_found(item_found);
      delete_item(item)
    END
  END;
BEGIN
  IF string_to_name(command_fields[2], item_name, '.PAS') ORIF
    solicit_name('Enter name of item(s) to delete: ', item_name, '.PAS')
    THEN BEGIN
    item_found := FALSE;
    indent;
    apply_to_items(database, do_delete);
    unindent;
    was_item_found(item_found)
  END
END;

PROCEDURE display_function;
VAR
item_name: name_string;
item_found: BOOLEAN;

  PROCEDURE do_display(item: item_pointer);
  BEGIN
    IF NOT valid_item(item) THEN RETURN;
    IF ambiguous_match(item_name, item^.name) THEN BEGIN
      item_is_found(item_found);
      display_item(item)
    END
  END;
BEGIN
  IF string_to_name(command_fields[2], item_name, '.PAS') ORIF
    solicit_name('Enter name of item(s) to display: ', item_name, '.PAS')
    THEN BEGIN
    item_found := FALSE;
    indent;
    apply_to_items(database, do_display);
    unindent;
    was_item_found(item_found)
  END
END;

PROCEDURE missing_function;
VAR
item_name: name_string;
item_found: BOOLEAN;
missing_count: INTEGER;

  PROCEDURE do_missing(item: item_pointer);
  BEGIN
    IF NOT valid_item(item) THEN RETURN;
    IF ambiguous_match(item_name, item^.name) THEN BEGIN
      item_is_found(item_found);
      RESET(INPUT, item^.name);
      IF iostatus <> IO_OK THEN BEGIN
	missing_count := missing_count + 1;
	display_item(item)
      END;
      CLOSE(INPUT)
    END
  END;
BEGIN
  IF string_to_name(command_fields[2], item_name, '.PAS') ORIF
    solicit_name('Enter name of missing item(s) to display: ', item_name,
    '.PAS') THEN BEGIN
    missing_count := 0;
    item_found := FALSE;
    indent;
    apply_to_items(database, do_missing);
    unindent;
    IF missing_count = 0 THEN display_line('None missing.');
    was_item_found(item_found)
  END
END;

PROCEDURE height_function;
VAR
item_name: name_string;
maximum_height: INTEGER;
item_found: BOOLEAN;

  PROCEDURE do_display_height(item: item_pointer);
  VAR
  height: INTEGER;
  BEGIN
    IF NOT valid_item(item) THEN RETURN;
    IF ambiguous_match(item_name, item^.name) THEN BEGIN
      item_is_found(item_found);
      height := measure_height(item);
      IF (maximum_height < 0) ORIF (height <= maximum_height) THEN BEGIN
	display_item(item);
	indent;
	display_number(height);
	unindent
      END
    END
  END;
BEGIN
  IF string_to_name(command_fields[2], item_name, '.PAS') ORIF
    solicit_name('Enter name of item(s) to display above: ', item_name ,
    '.PAS') THEN BEGIN
    IF NOT string_to_integer(command_fields[3], maximum_height) ANDIF
      NOT solicit_integer('Enter maximum height of search: ', maximum_height)
      THEN maximum_height := -1;
    IF maximum_height < 1 THEN display_message('Unlimited height.');
    item_found := FALSE;
    indent;
    apply_to_items(database, do_display_height);
    unindent;
    was_item_found(item_found)
  END
END;

PROCEDURE depth_function;
VAR
item_name: name_string;
maximum_depth: INTEGER;
item_found: BOOLEAN;

  PROCEDURE do_display_depth(item: item_pointer);
  VAR
  depth: INTEGER;
  BEGIN
    IF NOT valid_item(item) THEN RETURN;
    IF ambiguous_match(item_name, item^.name) THEN BEGIN
      item_is_found(item_found);
      depth := measure_depth(item);
      IF (maximum_depth < 0) ORIF (depth <= maximum_depth) THEN BEGIN
	display_item(item);
	indent;
	display_number(depth);
	unindent
      END
    END
  END;
BEGIN
  IF string_to_name(command_fields[2], item_name, '.PAS') ORIF
    solicit_name('Enter name of item(s) to display above: ', item_name ,
    '.PAS') THEN BEGIN
    IF NOT string_to_integer(command_fields[3], maximum_depth) ANDIF
      NOT solicit_integer('Enter maximum depth of search: ', maximum_depth)
      THEN maximum_depth := -1;
    IF maximum_depth < 1 THEN display_message('Unlimited depth.');
    item_found := FALSE;
    indent;
    apply_to_items(database, do_display_depth);
    unindent;
    was_item_found(item_found)
  END
END;

PROCEDURE help_function;
BEGIN
  display_message('Above/Below Source Reference Database');
  display_line( 'EXIT <database item_name> -- exit program, store database');
  display_line('HELP -- display help text');
  display_line('DISPLAY <item> -- display item(s)');
  display_line('DELETE <item> -- delete item(s)');
  display_line('CREATE <item> -- create item(s)');
  display_line('SEARCH <item> -- search item(s) for references');
  display_line( 'ABOVE <item> <height> -- display references above item(s)');
  display_line('     note: height < 0 signifies no limit');
  display_line( 'BELOW <item> <depth> -- display references below item(s)');
  display_line('     note: depth < 0 signifies no limit');
  display_line('HEIGHT <item> <maximum> -- display heights of item(s)' );
  display_line('     note: maximum < 0 signifies no limit');
  display_line('DEPTH <item> <maximum> -- display depth of items(s)');
  display_line('     note: maximum < 0 signifies no limit');
  display_line( 'MISSING <item> -- display any missing (unaccessable) item(s)'
    )
END;

PROCEDURE load_database(item_name: name_string);
BEGIN
  display_message('Loading database: ' || unprepared_name(item_name));
  apply_to_file(item_name, load_line);
END;

PROCEDURE store_database(item_name: name_string);
BEGIN
  display_message('Storing database: ' || unprepared_name(item_name));
  REWRITE(OUTPUT, item_name);
  apply_to_items(database, store_item);
  apply_to_items(database, store_aboves);
  apply_to_items(database, store_belows);
  CLOSE(OUTPUT)
END;

BEGIN
  OPEN(TTY);
  REWRITE(TTYOUTPUT);
  WRITELN(TTYOUTPUT, 'TYM-Pascal Source File Database, Version ', version());
  WRITELN(TTYOUTPUT);
  database := NIL;
  database_name := '';
  IF solicit_name('Enter database name: ', database_name, '.SRC')
    THEN load_database(database_name);
  LOOP
    REPEAT
    UNTIL solicit_fields('Enter function: ', command_fields);
    command_fields[1] := UPPERCASE(command_fields[1]);
    EXIT IF command_fields[1] = 'EXIT';
    IF command_fields[1] = 'DISPLAY' THEN display_function
    ELSE IF command_fields[1] = 'DELETE' THEN delete_function
    ELSE IF command_fields[1] = 'CREATE' THEN create_function
    ELSE IF command_fields[1] = 'SEARCH' THEN search_function
    ELSE IF command_fields[1] = 'ABOVE' THEN above_function
    ELSE IF command_fields[1] = 'BELOW' THEN below_function
    ELSE IF command_fields[1] = 'HEIGHT' THEN height_function
    ELSE IF command_fields[1] = 'DEPTH' THEN depth_function
    ELSE IF command_fields[1] = 'MISSING' THEN missing_function
    ELSE IF command_fields[1] = 'DEBUG' THEN debug_flag := NOT debug_flag
    ELSE help_function
  END;
  IF string_to_name(command_fields[2], database_name, '.SRC') ORIF
    (LENGTH(database_name) > 0) ORIF
    solicit_name('Enter database name: ', database_name, '.SRC')
    THEN store_database(database_name)
  ELSE display_message('Database not stored.')
END.
    xu“