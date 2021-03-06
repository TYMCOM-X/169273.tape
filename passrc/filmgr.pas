PROGRAM file_manager;
(*File Management Database*)

CONST
version = '1.0';
data_file = 'FILMGR.DAT';
file_definitions_flag = 'FILE DEFINITIONS';
relation_definitions_flag = 'RELATION DEFINITIONS';
file_relations_flag = 'FILE RELATIONS';
end_flag = 'END';
maximum_number_of_file_definitions = 2000;
maximum_number_of_relation_definitions = 100;
maximum_number_of_file_relations = 10000;
name_length = 6;
extension_length = 3;
description_length = 70;

TYPE
file_definition = RECORD
  name: STRING[name_length];
  extension: STRING[extension_length];
  description: STRING[description_length];
END;
relation_definition = RECORD
  name: STRING[name_length];
  description: STRING[description_length]
END;
file_relation = RECORD
  related_from, related_to, relation: INTEGER
END;

VAR
file_definitions: ARRAY [1 .. maximum_number_of_file_definitions]
  OF file_definition;
relation_definitions: ARRAY [1 .. maximum_number_of_relation_definitions]
  OF relation_definition;
file_relations: ARRAY [1 .. maximum_number_of_file_relations] OF file_relation;
number_of_file_definitions: INTEGER;
number_of_relation_definitions: INTEGER;
number_of_file_relations: INTEGER;
file_number: INTEGER;
relation_number: INTEGER;
line: STRING;
name: STRING;
extension: STRING;
description: STRING;
related_to: INTEGER;
related_from: INTEGER;
relation: INTEGER;
done: BOOLEAN;

EXTERNAL PROCEDURE start(STRING);
EXTERNAL FUNCTION asktab(STRING; ARRAY[1 .. *] OF STRING): INTEGER;
EXTERNAL PROCEDURE assume(BOOLEAN; STRING);
EXTERNAL PROCEDURE openio(STRING; STRING);
EXTERNAL VAR err: BOOLEAN;
EXTERNAL FUNCTION rdlin(VAR STRING): BOOLEAN;
EXTERNAL PROCEDURE wrstr(STRING);
EXTERNAL PROCEDURE wrlin(STRING);
EXTERNAL PROCEDURE dpylin(STRING);

PROCEDURE lookup_file_definition;
VAR i: INTEGER;
BEGIN (*lookup a file definition given a name and extension*)
  file_number := 0;
  FOR i := 1 TO number_of_file_definitions DO
    IF (file_definitions[i].name = name)
      AND (file_definitions[i].extension = extension)
        THEN file_number := i
END;

PROCEDURE lookup_relation_definition;
VAR i: INTEGER;
BEGIN (*lookup a relation definition given a name*)
  relation_number := 0;
  FOR i := 1 TO number_of_relation_definitions DO
    IF relation_definitions[i].name = name
      THEN relation_number := i
END;

PROCEDURE add_file_definition;
BEGIN (*add a file definition to the database*)
  dpylin('file definition added');
  number_of_file_definitions := number_of_file_definitions + 1;
  file_definitions[number_of_file_definitions].name := name;
  file_definitions[number_of_file_definitions].extension := extension;
  file_definitions[number_of_file_definitions].description := description
END;

PROCEDURE add_relation_definition;
BEGIN (*add a relation definition to the database*)
  dpylin('relation definition added');
  number_of_relation_definitions := number_of_relation_definitions + 1;
  relation_definitions[number_of_relation_definitions].name := name;
  relation_definitions[number_of_relation_definitions].description 
    := description
END;

PROCEDURE add_file_relation;
BEGIN (*add a file relation to the database*)
  dpylin('file relation added');
  number_of_file_relations := number_of_file_relations + 1;
  file_relations[number_of_file_relations].related_from := related_from;
  file_relations[number_of_file_relations].related_to := related_to;
  file_relations[number_of_file_relations].relation := relation
END;

PROCEDURE load_database;
VAR dot_pos, semi_pos: INTEGER;
BEGIN (*load database from external file*)
  openio(data_file, ''); line := '';
  assume(NOT err, 'can not read data file');
  number_of_file_definitions := 0;
  WHILE rdlin(line) DO IF line = file_definitions_flag THEN BEGIN
    dpylin('loading file definitions');
    WHILE rdlin(line) DO BEGIN
      EXIT IF line = end_flag;
      dot_pos := INDEX(line, '.'); semi_pos := INDEX(line, ';');
      name := SUBSTR(line, 1, dot_pos - 1);
      extension := SUBSTR(line, dot_pos + 1, semi_pos - dot_pos);
      description := SUBSTR(line, semi_pos + 1);
      add_file_definition
    END
  END;
  number_of_relation_definitions := 0;
  WHILE rdlin(line) DO IF line = relation_definitions_flag THEN BEGIN
    dpylin('loading relation definitions');
    WHILE rdlin(line) DO BEGIN
      EXIT IF line = end_flag;
      name := SUBSTR(line, 1, INDEX(line, ';'));
      description := SUBSTR(line, INDEX(line, ';') + 1);
      add_relation_definition;
    END
  END;
  number_of_file_relations := 0;
  WHILE rdlin(line) DO IF line = file_relations_flag THEN BEGIN
    dpylin('loading file relations');
    WHILE rdlin(line) DO BEGIN
      EXIT IF line = end_flag;
      dot_pos := INDEX(line, '.');
      semi_pos := INDEX(line, ';');
      name := SUBSTR(line, dot_pos - 1);
      extension := SUBSTR(line, dot_pos + 1, semi_pos - dot_pos);
      lookup_file_definition;
      related_from := file_number;
      dot_pos := INDEX(SUBSTR(line, semi_pos), '.');
      name := SUBSTR(line, semi_pos + 1, dot_pos - semi_pos - 1);
      semi_pos := INDEX(SUBSTR(line, dot_pos), ';');
      extension := SUBSTR(line, dot_pos + 1, semi_pos - dot_pos - 1);
      lookup_file_definition;
      related_to := file_number;
      name := SUBSTR(line, semi_pos + 1);
      lookup_relation_definition;
      relation := relation_number;
      add_file_relation
    END
  END
END;

PROCEDURE store_database;
BEGIN (*store database to external file*)
  openio('', data_file);
  assume(NOT err, 'can not write data file');
  wrlin(file_definitions_flag);
  dpylin('storing file definitions');
  FOR file_number := 1 TO number_of_file_definitions DO BEGIN
    WITH file_definitions[file_number] DO BEGIN
      wrstr(name); wrstr('.'); wrstr(extension); wrstr(';'); wrstr(description)
    END;
    wrlin('')
  END;
  wrlin('END');
  wrlin(relation_definitions_flag);
  dpylin('storing relation definitions');
  FOR relation_number := 1 TO number_of_relation_definitions DO BEGIN
    WITH relation_definitions[relation_number] DO BEGIN
      wrstr(name); wrstr(';'); wrstr(description)
    END;
    wrlin('')
  END;
  wrlin('END');
  wrlin(file_relations_flag);
  dpylin('storing file relations');
  FOR relation_number := 1 TO number_of_file_relations DO BEGIN
    WITH file_relations[relation_number] DO BEGIN
      WITH file_definitions[related_from] DO BEGIN
        wrstr(name); wrstr('.'); wrstr(extension)
      END;
      wrstr(';');
      WITH file_definitions[related_to] DO BEGIN
        wrstr(name); wrstr('.'); wrstr(extension)
      END;
      wrstr(';');
      WITH relation_definitions[relation] DO BEGIN
        wrstr(name)
      END;
      wrlin('')
    END
  END;
  wrlin('END')
END;

PROCEDURE verify_database;
BEGIN (*verify the consistancy of the database*)
  dpylin('not implemented yet')
END;

PROCEDURE modify_database;
BEGIN (*make changes to the database*)
  dpylin('not implemented yet')
END;

PROCEDURE rebuild_program;
BEGIN (*generate command file to rebuild a program*)
  dpylin('not implemented yet')
END;

PROCEDURE query_database;
BEGIN (*retrieve information from database*)
  dpylin('not implemented yet')
END;

BEGIN (*top level command dispatch*)
  start('File Manager ' || version);
  load_database;
  done := FALSE;
  REPEAT
    CASE asktab('|', ('EXIT', 'VERIFY', 'MODIFY', 'REBUILD', 'QUERY')) OF
      1: done := TRUE;
      2: verify_database;
      3: modify_database;
      4: rebuild_program;
      5: query_database
    END
  UNTIL done;
  store_database
END.
 