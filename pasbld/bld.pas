$TITLE BLD.PAS, last modified 4/26/84, zw
PROGRAM bld OPTIONS storage(4000);
(*TYM-Pascal utility to simplify compiling and linking.*)
$SYSTEM JOBNUM.INC
$SYSTEM TIMUTL.INC
$SYSTEM PASDIR.TYP
$SYSTEM PASDIR.INC
$SYSTEM INFPAC.INC
$SYSTEM RUNUTL.INC
$SYSTEM CMDUTL.INC
$SYSTEM VERSIO.INC

CONST
max_progs := 64;
max_mods := 256;
e := 62;
s := 63; (* build table module switches *)
max_sw := 7;
max_defaults := 5;
bld_file_name := 'BLD.DAT';

TYPE
str = STRING;
name_list = ^name_node;
passes = 0 .. max_progs;
modules = 0 .. max_mods;
name_node = RECORD
  name: FILE_NAME;
  codes: SET OF passes;
  link: name_list
END;
prog_list = RECORD (* program name data structure *)
  name: FILE_NAME;
  number: INTEGER;
  load: BOOLEAN;
  opt: ARRAY[1 .. max_sw] OF STRING;
  link: ^prog_list
END;
mod_list = RECORD (* module name data structure *)
  name: FILE_NAME;
  lit: BOOLEAN;
  number: INTEGER;
  comp: BOOLEAN;
  include: BOOLEAN;
  select: BOOLEAN;
  opt: ARRAY[1 .. max_sw] OF STRING;
  link: ^mod_list
END;

VAR
build_file: TEXT;
network: ARRAY[modules, passes] OF BOOLEAN;
files_list: name_list;
prog_ptr, prog_tail: ^prog_list;
mod_ptr, mod_tail: ^mod_list;
compiler: FILE_NAME;
loader: FILE_NAME;
compile_on: BOOLEAN;
link_on: BOOLEAN;
select_on: BOOLEAN;
select_now: BOOLEAN;
include_on: BOOLEAN;
include_now: BOOLEAN;
queued: BOOLEAN;
n_defaults: 0 .. max_defaults;
default_ppns: ARRAY[1 .. max_defaults] OF FILE_NAME;
(*
  PROCEDURE ERR (error message regurgitator)
  Calling sequence
   err(msg)
  Where:
   msg is str error message to be displayed at the terminal
									*)

PROCEDURE err(msg: str);
BEGIN
  WRITELN(TTY,'ERROR:  ' || msg);
  STOP;
END;
(*
  FUNCTION CONVERT_PROG (associate number with program name)
  Calling sequence:
   prog_num := convert_prog(name)
  Where:
   name is str program name
  Return:
   prog_num is integer program number associated with name
									*)

FUNCTION convert_prog(name: FILE_NAME): INTEGER;
VAR
value,INDEX: INTEGER;
found,num: BOOLEAN;
prog_temp: ^prog_list;
BEGIN
  INDEX := 1;
  found := FALSE;
  num := cmd_number(name,INDEX,FALSE,value);
  IF num THEN BEGIN (* found legal value *)
    prog_temp := prog_ptr;
    WHILE (NOT found) AND (prog_temp <> prog_tail)
      DO IF prog_temp^.number = value THEN found := TRUE
    ELSE prog_temp := prog_temp^.link;
    IF NOT found THEN err('undefined program number');
    convert_prog := value
  END
  ELSE IF LOWERCASE(name) = 'e' THEN (* "e" switch found *)
  convert_prog := e
  ELSE IF LOWERCASE(name) = 's' THEN (* "s" switch found*)
  convert_prog := s
  ELSE BEGIN
    prog_temp := prog_ptr; (* start search at top of program list *)
    WHILE (NOT found) AND (prog_temp <> prog_tail)
      DO IF prog_temp^.name = name THEN BEGIN
      convert_prog := prog_temp^.number;
      found := TRUE
    END
    ELSE prog_temp := prog_temp^.link;
    IF NOT found THEN err('undefined program name "' || name || '"')
  END
END (* convert_prog *);
(*
  PROCEDURE GET_PROG_RANGE (interpret i-j lists)
  Calling swquence:
   get_prog_range(list,lower,upper)
  Where:
   list is str sequence of program numbers to be interpreted -- (i or i-j)
  Return:
   lower is integer value of lowest value in expression
   upper is integer value of highest value in expression
									  *)

PROCEDURE get_prog_range(list: str; VAR lower:INTEGER; VAR upper:INTEGER);
VAR
range: INTEGER;
BEGIN
  range := SEARCH(list,['-']);
  IF range > 0 THEN BEGIN (* found  i-j expression *)
    lower := convert_prog(SUBSTR(list,1,range-1));
    upper := convert_prog(SUBSTR(list,range+1))
  END
  ELSE BEGIN
    lower := convert_prog(list);
    upper := lower
  END
END (* get_prog_range *);
(*
  PROCEDURE DO_MODULE  ( add module to global data structure )
  Calling sequence:
   do_module(work_line,next)
  Where:
   work_line is str line from build_file containing module information
   next is integer index
									*)

PROCEDURE do_module(VAR work_line: str; VAR next:INTEGER);
VAR
list: str;
i,lower,upper,range,quote: INTEGER;
done: BOOLEAN;
mod_temp: ^mod_list;
BEGIN
  NEW(mod_temp);
  work_line := UPPERCASE (work_line);
  next := 1;
  IF cmd_check_punct (work_line, next, '"') THEN BEGIN
    IF NOT cmd_string (work_line, next, '"', mod_temp^.name)
      THEN err('unbalanced literal ' || work_line);
    mod_temp^.lit := TRUE;
  END
  ELSE BEGIN
    IF NOT cmd_file_name (work_line, next, TRUE, mod_temp^.name)
      THEN err('bad module name ' || work_line);
    mod_temp^.lit := FALSE;
  END;
  mod_temp^.comp := FALSE;
  mod_temp^.include := FALSE;
  mod_temp^.select := FALSE;
  IF mod_ptr = mod_tail THEN BEGIN
    mod_ptr := mod_temp;
    mod_temp^.number := 1;
    mod_temp^.link := mod_tail;
    mod_tail^.link := mod_temp;
  END
  ELSE BEGIN
    mod_temp^.link := mod_tail;
    mod_temp^.number := mod_tail^.link^.number + 1;
    mod_tail^.link^.link := mod_temp;
    mod_tail^.link := mod_temp;
  END;
  FOR i := 1 TO max_sw DO mod_temp^.opt[i] := '';
  IF NOT cmd_check_punct (work_line, next, '/')
    THEN err('module specification must be followed by attribute list');
  work_line := SUBSTR(work_line,next);
  done := FALSE;
  WHILE NOT done DO BEGIN
    next := SEARCH(work_line,[',',';'],LENGTH(work_line)+1);
    list := SUBSTR(work_line,1,next-1);
    get_prog_range(list,lower,upper);
    FOR i := lower TO upper DO network[mod_temp^.number,i] := TRUE; (* update network data structure *)
    IF (next > LENGTH(work_line)) ORIF (work_line[next] = ';') THEN BEGIN
      work_line := '';
      done := TRUE
    END
    ELSE work_line := SUBSTR(work_line,next+1)
  END
END (* do_module *);
(*
  FUNCTION CONVRT (pad integers on the left with zeroes)
  Calling sequence:
   value := convrt(num,pad)
  Where:
   num is integer number to put in character form
   pad is integer number of places fill up with num
  Return:
   value is str character representation of num with appropriate
    number of zeroes added on te left
									*)

FUNCTION convrt(num: INTEGER; pad:INTEGER):str;
CONST
zeropad := '0000000000000000';
VAR
value: str;
BEGIN
  PUTSTRING(value,num: 0);
  IF LENGTH(value) < pad THEN value := SUBSTR(zeropad,1,pad-LENGTH(value))
    || value;
  convrt := value
END (* convrt *);
(*
  PROCEDURE GET_FULL_NAME
  Calling sequence:
   get_full_name(out_file,mod_ptr);
  Where:
   out_file is text file used for link commands
   mod_ptr is pointer to current module
								   *)

PROCEDURE get_full_name(VAR out_file:TEXT; mod_ptr : ^mod_list);
VAR
mod_file: FILE OF INTEGER;
defn: INTEGER;
full_name: str;
BEGIN
  full_name := mod_ptr^.name;
  IF NOT mod_ptr^.lit THEN BEGIN
    RESET(mod_file,'.REL ' || mod_ptr^.name); (* try default module file names *)
    defn := 0;
    WHILE (EOF(mod_file)) AND (defn < n_defaults) DO BEGIN
      CLOSE(mod_file);
      defn := defn+1;
      full_name := mod_ptr^.name || default_ppns[defn]; (* add default *)
      RESET(mod_file,'.REL ' || full_name);
    END;
    CLOSE(mod_file);
  END;
  WRITE(out_file,full_name);
  IF network[mod_ptr^.number,s] THEN (* /SEARCH switch in build_file *)
  WRITE(out_file,'/S');
  WRITELN(out_file);
END (* get_full_name *);
(*
  PROCEDURE ADD_LINK_OPT (add linker options to linker input file)
  Calling sequence:
   add_link_opt(out_file,prog_ptr);
  Where
   out_file is text file for linker input
   prog_ptr is pointer to current program
								       *)

PROCEDURE add_link_opt(VAR out_file:TEXT; prog_ptr : ^prog_list);
VAR
i: INTEGER;
BEGIN
  FOR i := 1 TO max_sw DO IF prog_ptr^.opt[i] = 'SYMSEG: HIGH' THEN WRITELN(
    out_file,'/SYMSEG: HIGH');
  FOR i := 1 TO max_sw DO IF prog_ptr^.opt[i] = 'SYS: LNKDDT' THEN BEGIN
    WRITELN(out_file,'SYS: LNKDDT');
    WRITELN(out_file,'SYS: JOBDAT');
  END;
END (* add_link_opt *);
(*
  PROCEDURE LINK_CHOICES (setup link file for requested programs)
  Calling sequence:
   link_choices(file_name,multiple);
  where:
   file_name is text file to write link commands to
   multiple is BOOLEAN flag -- true if more than one program may be linked;
    false otherwise
  Notes:
   If more than one program is to be linked, link files are chained
    together via the last line in each link file, so that when the
    loader is run, execution will be transferred to each link file
    in succession.
									*)

PROCEDURE link_choices(FILE_NAME:str; multiple : BOOLEAN);
VAR
i,num,tnum: INTEGER;
full_name: str;
prog_next,prog_temp: ^prog_list;
mod_temp: ^mod_list;
next,first: BOOLEAN;
out_link: TEXT;
mod_file: FILE OF INTEGER;
defn: 0..max_defaults;
BEGIN
  num := 0;
  prog_temp := prog_ptr;
  WHILE prog_temp <> prog_tail DO BEGIN
    IF prog_temp^.load THEN BEGIN (* program is to be compiled *)
      first := TRUE;
      mod_temp := mod_ptr;
      WHILE mod_temp <> mod_tail DO BEGIN (* see which moodules are in this program *)
	IF network[mod_temp^.number,prog_temp^.number] THEN BEGIN
	  IF first THEN BEGIN (* first line in link file? *)
	    IF num = 0 THEN (* first link file? *)
	    REWRITE(out_link,jobnum || FILE_NAME)
	    ELSE REWRITE(out_link,jobnum || 'L' || convrt(num,2) || '.TMP');
	    WRITE(out_link,prog_temp^.name || '[,]/SSAVE');
	    FOR i := 1 TO max_sw DO IF (prog_temp^.opt[i]='MAP') OR
	      (prog_temp^.opt[i]='NOMAP')
	      THEN WRITE(out_link,'/' || prog_temp^.opt[i]);
	    WRITELN(out_link,'=');
	    add_link_opt(out_link,prog_temp);
	    first := FALSE
	  END;
	  (* now put module name in file *)
	  get_full_name(out_link,mod_temp);
	END (* if *);
	mod_temp := mod_temp^.link;
      END (* while *);
      (* look ahead to see if linker will be run again *)
      prog_next := prog_temp^.link;
      next := FALSE;
      WHILE (prog_next <> prog_tail) AND (NOT next)
	DO IF prog_next^.load THEN next := TRUE
      ELSE prog_next := prog_next^.link;
      IF next THEN BEGIN (* more files to follow *)
	IF NOT multiple THEN err('only one program may be linked');
	tnum := num + 1;
	WRITE(out_link,'/RUN:' || loader || '/RUNOFFSET:1/TMPFIL:LNK:"@');
	WRITE(out_link,jobnum || 'L' || convrt(tnum,2) || '.TMP"');
      END;
      WRITELN(out_link,'/GO');
      CLOSE(out_link);
      num := num + 1;
    END (* if *);
    prog_temp := prog_temp^.link
  END (* while *)
END (*link_choices *);
(*
  PROCEDURE INIT_COMP_FILE ( write header info to compile file )
  Calling sequence:
   init_comp_file(out_file);
  Where:
   out_file is text file used as compilation file
								      *)

PROCEDURE init_comp_file(VAR out_file: TEXT);
VAR
i: INTEGER;
BEGIN
  FOR i := 1 TO n_defaults DO BEGIN
    IF i = 1 THEN WRITE(out_file,'/SEARCH(')
    ELSE WRITE(out_file,',');
    WRITE(out_file,default_ppns[i])
  END;
  IF i <> 0 THEN WRITE(out_file,')');
  WRITELN(out_file,'/SPECIAL(WORD)/NOCHECK/NAMES');
END (* init_comp_file *);
(*
  PROCEDURE FILL_COMP_FILE ( add the body into the compilation file )
  Calling sequence:
   fill_comp_file(out_file);
  Where:
   out_file is text file to be used for compilation
									    *)

PROCEDURE fill_comp_file(VAR out_file: TEXT);
VAR
mod_temp: ^mod_list;
i: INTEGER;
BEGIN
  mod_temp := mod_ptr;
  WHILE mod_temp <> mod_tail DO BEGIN
    IF mod_temp^.comp THEN BEGIN
      WRITE(out_file,mod_temp^.name);
      IF NOT mod_temp^.lit THEN BEGIN
	FOR i := 1 TO max_sw DO (* append switches *)
	IF mod_temp^.opt[i] <> '' THEN WRITE(out_file,'/' || mod_temp^.opt[i])
	  ;
      END;
      WRITELN(out_file);
    END;
    mod_temp := mod_temp^.link;
  END (* while *);
END (* fill_comp_file *);
(*
  PROCEDURE COMPILE_CHOICES (setup compile file for requested modules)
  Calling sequence:
   compile_choices;
  Notes:
   The compiler may also run the loader, since if programs are to be
    both compiled and loaded, a mechanism is needed to transfer control
    from the compiler to the loader, which can not be done directly
    from any single calling program.
									*)

PROCEDURE compile_choices;
VAR
comp_file: TEXT;
BEGIN
  REWRITE(comp_file,jobnum || 'PAS.TMP'); (* default compiler input file *)
  init_comp_file(comp_file);
  fill_comp_file(comp_file);
  IF link_on THEN BEGIN (* add a line to run the loader *)
    WRITELN(comp_file,'/RUN:',loader,'/RUNOFFSET');
    link_choices('LNK.TMP',TRUE);
  END;
  CLOSE(comp_file);
END (* compile choices *);
(*
  PROCEDURE SELECT_CHOICES (dump selected module names to temporary file)
  Calling sequence:
   select_choices;
  Note:
   A temporary file of the form xxxSEL.TMP is used, where xxx is the
    three digit job number.
								       *)

PROCEDURE select_choices;
VAR
i: INTEGER;
mod_temp: ^mod_list;
select_file: TEXT;
mod_file: FILE OF INTEGER;
defn: 0..max_defaults;
full_name: str;
BEGIN
  REWRITE(select_file,jobnum || 'SEL.TMP');
  WRITELN(select_file,'MODULES SELECTED:  ');
  mod_temp := mod_ptr;
  WHILE mod_temp <> mod_tail DO BEGIN
    IF mod_temp^.select THEN BEGIN
      full_name := mod_temp^.name;
      IF NOT mod_temp^.lit THEN BEGIN
	RESET(mod_file,'.REL ' || mod_temp^.name); (* try default file extension *)
	defn := 0;
	WHILE (EOF(mod_file)) AND (defn < n_defaults) DO BEGIN
	  CLOSE(mod_file);
	  defn := defn+1;
	  full_name := mod_temp^.name || default_ppns[defn];
	  RESET(mod_file,'.REL ' || full_name);
	END;
	CLOSE(mod_file);
      END;
      WRITE(select_file,full_name);
      FOR i := 1 TO max_sw DO IF mod_temp^.opt[i] <> '' THEN WRITE(select_file
	,'/' || mod_temp^.opt[i]);
      WRITELN(select_file);
    END;
    mod_temp := mod_temp^.link;
  END (* while *);
  WRITELN(TTY,jobnum || 'SEL.TMP created');
  BREAK;
END (* select_choices *);

PROCEDURE include_lists;
TYPE
augmented_passes = MINIMUM (passes)..MAXIMUM (passes) + 1;
linkage = RECORD
  thefile: ^file_record;
  next: ^linkage
END;
file_record = RECORD
  fname: FILE_NAME;
  creation: dtime_int;
  pass_set: SET OF augmented_passes;
  num_passes, first_pass: passes;
  usage: str;
  exploded: BOOLEAN;
  owner: ^file_record;
  num_owned: 0..4;
  owns: ARRAY [1..4] OF ^file_record;
  first_child, last_child: ^linkage
END;
module_list = RECORD
  first_mod, last_mod: ^linkage
END;
CONST
day_label: ARRAY [week_day] OF STRING[4] := ('Sun ', 'Mon ', 'Tue ', 'Wed ',
  'Thu ', 'Fri ', 'Sat ');
pass_codes: SET OF passes = [MINIMUM (passes)..MAXIMUM (passes)];
VAR
incl_list: module_list;
lines_on_page: INTEGER;
listing: BOOLEAN;

  PROCEDURE put_on_list (VAR list: module_list; file_ptr: ^file_record);
  BEGIN
    IF list.first_mod = NIL THEN BEGIN
      NEW (list.first_mod);
      list.last_mod := list.first_mod
    END
    ELSE BEGIN
      NEW (list.last_mod^.next);
      list.last_mod := list.last_mod^.next
    END;
    list.last_mod^.thefile := file_ptr;
    list.last_mod^.next := NIL
  END;

  PROCEDURE new_file (VAR file_ptr: ^file_record; name: FILE_NAME;
    set_of_passes: SET OF passes);
  BEGIN
    NEW (file_ptr);
    WITH file_ptr^ DO BEGIN
      fname := name;
      pass_set := set_of_passes;
      exploded := FALSE;
      owner := NIL;
      num_owned := 0;
      first_child := NIL;
      last_child := NIL
    END
  END;

  PROCEDURE count_passes (VAR file_ptr: ^file_record);
  VAR
  temp_index: passes;
  temp_index_char: str;
  err: dir_errors;
  info: dir_attrs;
  BEGIN
    WITH file_ptr^ DO BEGIN
      dir_attr (err, fname, info);
      creation := info.creation;
      usage := '';
      num_passes := 0;
      FOR temp_index := MINIMUM (passes) TO MAXIMUM (passes)
	DO IF temp_index IN pass_set THEN BEGIN
	PUTSTRING(temp_index_char,temp_index: 0);
	usage := usage || ',' || temp_index_char;
	num_passes := num_passes + 1;
	IF num_passes = 1 THEN first_pass := temp_index
      END;
      usage := usage || '>';
      usage [1] := '<';
      IF (num_passes > 3) AND ((owner = NIL) ORIF (MAXIMUM (augmented_passes)
	IN owner^.pass_set))
	THEN pass_set := pass_set + [MAXIMUM (augmented_passes)]
    END
  END;

  PROCEDURE find (VAR name: FILE_NAME);
  VAR
  defn: 0..max_defaults;
  BEGIN
    defn := 0;
    RESET (OUTPUT, name); (* try current account first *)
    WHILE EOF (OUTPUT) AND (defn < n_defaults) DO BEGIN
      CLOSE (OUTPUT);
      defn := defn + 1;
      RESET (OUTPUT, name || default_ppns [defn])
    END;
    IF EOF (OUTPUT) THEN BEGIN
      WRITELN (TTYOUTPUT, '?unable to find ', name);
      STOP
    END;
    CLOSE (OUTPUT);
    IF defn > 0 THEN (* had to follow default path? *)
    name := name || default_ppns [defn]
  END;

  PROCEDURE new_include (new_name: FILE_NAME; VAR file_ptr: ^file_record);
  VAR
  inc_rec_ptr: ^linkage;
  BEGIN
    inc_rec_ptr := incl_list.first_mod;
    WHILE (inc_rec_ptr <> NIL) ANDIF (inc_rec_ptr^.thefile^.fname <> new_name)
      DO inc_rec_ptr := inc_rec_ptr^.next;
    IF inc_rec_ptr = NIL THEN BEGIN (* new file not on list *)
      new_file (file_ptr, new_name, []);
      put_on_list (incl_list, file_ptr)
    END
    ELSE file_ptr := inc_rec_ptr^.thefile (* already on list *)
  END;

  PROCEDURE explode (VAR modptr: ^linkage);
  VAR
  new_name: FILE_NAME;
  line: str;
  i: INTEGER;
  temp_ptr: ^linkage;
  BEGIN
    IF NOT modptr^.thefile^.exploded THEN BEGIN
      modptr^.thefile^.exploded := TRUE;
      RESET (INPUT, modptr^.thefile^.fname);
      WHILE NOT EOF (INPUT) DO IF INPUT^ <> '$' THEN READLN (INPUT)
      ELSE BEGIN
	line := '';
	WHILE NOT EOLN (INPUT) AND (LENGTH (line) < UPPERBOUND (line))
	  DO BEGIN
	  line := line || INPUT^;
	  GET (INPUT)
	END;
	READLN (INPUT);
	line := UPPERCASE (line);
	IF (LENGTH (line) >= 8) ANDIF ((line [2: 7] = 'INCLUDE') OR
	  (line [2:6] = 'SYSTEM')) THEN BEGIN
	  i := 8;
	  IF (line[i] <> ' ') AND (line[i] <> CHR (11b)) THEN i := i+1;
	  REPEAT
	    i := i+1;
	  UNTIL (line [i] <> ' ') AND (line [i] <> CHR (11b));
	  IF cmd_file_name (line, i, TRUE, new_name)
	    THEN WITH modptr^.thefile^ DO BEGIN
	    IF INDEX (new_name, '.') = 0 THEN BEGIN
	      i := INDEX (new_name, '[');
	      IF i = 0 THEN new_n= new_name || '.INC'
	      ELSE new_name := SUBSTR (new_name, 1, i-1)
		|| '.INC' || SUBSTR (new_name, i)
	    END;
	    find (new_name);
	    IF first_child = NIL THEN BEGIN
	      NEW (first_child);
	      last_child := first_child
	    END
	    ELSE BEGIN
	      NEW (last_child^.next);
	      last_child := last_child^.next
	    END;
	    last_child^.next := NIL;
	    new_include (new_name, modptr^.thefile^.last_child^.thefile)
	  END
	END
      END;
      CLOSE (INPUT);
    END;
    temp_ptr := modptr^.thefile^.first_child;
    WHILE temp_ptr <> NIL DO BEGIN
      temp_ptr^.thefile^.pass_set := temp_ptr^.thefile^.pass_set + modptr^.
	thefile^.pass_set;
      explode (temp_ptr);
      temp_ptr := temp_ptr^.next
    END
  END (* explode *);

  FUNCTION format (fname: FILE_NAME; offset: INTEGER; creation: dtime_int)
    : str;
  CONST
  blank: str := '                                        ';
  VAR
  i: INTEGER;
  BEGIN
    format := fname;
    i := INDEX (format, '[');
    IF listing AND (i > 0) THEN format := SUBSTR (format, 1, i-1)
      || blank [1:12-i] || SUBSTR (format, i);
    format := format || blank [1:(28+offset-LENGTH (format))];
    format := ' ' || format || day_label [day_of_week (extr_date (creation))
      ] || SUBSTR (dc_ext (creation), 1, 15)
  END (* format *);

  PROCEDURE print_formatted (list: module_list; pass_index: augmented_passes);
  TYPE
  line_rec_ptr = ^line_rec;
  line_rec = RECORD
    next_line: line_rec_ptr;
    line: str
  END;
  VAR
  list_ptr: ^linkage;
  i: 1..4;
  fileref: ^file_record;
  line_list_head, last_line, last_primary_entry, new_line: ^line_rec;
  temp_str: str;

    PROCEDURE insert (VAR last: line_rec_ptr);
    BEGIN
      new_line^.next_line := last^.next_line;
      last^.next_line := new_line;
      last := new_line
    END;
  BEGIN
    NEW (line_list_head);
    line_list_head^.line := ' ';
    last_primary_entry := line_list_head;
    NEW (line_list_head^.next_line);
    last_line := line_list_head^.next_line;
    last_line^.next_line := NIL;
    last_line^.line := ' ';
    list_ptr := list.first_mod;
    WHILE list_ptr <> NIL DO WITH list_ptr^.thefile^ DO BEGIN
      IF (pass_index IN pass_set) ANDIF ((owner = NIL) ORIF
	(NOT (pass_index IN owner^.pass_set))) THEN BEGIN
	NEW (new_line);
	new_line^.line := format (fname, 3, creation);
	IF (pass_index > MAXIMUM (passes)) ORIF (((num_passes = 2) OR
	  (num_passes = 3)) AND (first_pass = pass_index))
	  THEN new_line^.line := new_line^.line || '   ' || usage
	ELSE IF num_passes <> 1 THEN BEGIN
	  new_line^.line [1] := '(';
	  IF owner = NIL THEN fileref := list_ptr^.thefile
	  ELSE fileref := owner;
	  IF fileref^.num_passes <= 3 THEN BEGIN
	    PUTSTRING(temp_str,fileref^.first_pass);
	    new_line^.line := new_line^.line || '  see pass' || temp_str ||
	      ')'
	  END
	  ELSE new_line^.line := new_line^.line || '  see common)'
	END;
	IF new_line^.line [1] = ' ' THEN BEGIN (* primary entry for this file *)
	  insert (last_primary_entry);
	  FOR i := 1 TO num_owned DO BEGIN
	    NEW (new_line);
	    new_line^.line := format (owns[i]^.fname, 0, owns[i]^.creation);
	    IF listing THEN new_line^.line := '   ' || new_line^.line;
	    IF (num_passes <> 1) OR (pass_set <> owns[i]^.pass_set)
	      THEN new_line^.line := new_line^.line || '   ' || owns[i]^.usage
	      ;
	    insert (last_primary_entry);
	  END
	END
	ELSE IF listing THEN BEGIN (* a cross referencing entry *)
	  insert (last_line);
	  FOR i := 1 TO num_owned DO IF pass_index IN owns[i]^.pass_set THEN
	    BEGIN
	    NEW (new_line);
	    new_line^.line := '   ' || format (owns[i]^.fname, 0, owns[i]^.
	      creation);
	    IF (num_passes = 2) OR (num_passes = 3) THEN BEGIN
	      PUTSTRING(temp_str,first_pass);
	      new_line^.line := new_line^.line || '  see pass' || temp_str
	    END
	    ELSE new_line^.line := new_line^.line || '  see common';
	    insert (last_line)
	  END
	END
      END;
      list_ptr := list_ptr^.next
    END;
    new_line := line_list_head;
    WHILE new_line <> NIL DO BEGIN
      IF listing THEN BEGIN
	IF lines_on_page > 44 THEN BEGIN
	  page (OUTPUT);
	  lines_on_page := 0
	END;
	WRITELN (OUTPUT, '   ', new_line^.line);
	lines_on_page := lines_on_page + 1
      END
      ELSE IF new_line^.line <> ' ' THEN BEGIN
	new_line^.line := SUBSTR (new_line^.line, 2);
	WRITELN (OUTPUT, SUBSTR (new_line^.line, 1, INDEX (new_line^.line, ' '
	  ) - 1))
      END;
      new_line := new_line^.next_line
    END
  END;

  PROCEDURE print_exploded (file_ptr: ^file_record; level: INTEGER);
  VAR
  child: ^linkage;
  BEGIN
    IF listing THEN WRITELN (OUTPUT, ' ': level*3, file_ptr^.fname)
    ELSE WRITELN (OUTPUT, file_ptr^.fname);
    child := file_ptr^.first_child;
    WHILE child <> NIL DO BEGIN
      print_exploded (child^.thefile, level+1);
      child := child^.next
    END
  END;
VAR
jobstuff: jobrec;
incl_name, time_stamp: str;
machine_indep, wild_on_pass: BOOLEAN;
match_level, temp_level: INTEGER;
pass_union: SET OF augmented_passes;
out_name: FILE_NAME;
incl_rec_ptr, best_match, mod_rec_ptr: ^linkage;
pass_index: augmented_passes;
namelist_ptr: name_list;
file_ptr: ^file_record;
mod_list: module_list;
BEGIN
(* initialze lists *)
  mod_list.first_mod := NIL;
  mod_list.last_mod := NIL;
  incl_list.first_mod := NIL;
  incl_list.last_mod := NIL;
  pass_union := [];
  (* put all marked files on module list and explode them *)
  namelist_ptr := files_list;
  WHILE namelist_ptr <> NIL DO BEGIN
    new_file (file_ptr, namelist_ptr^.name, namelist_ptr^.codes);
    put_on_list (mod_list, file_ptr);
    WITH mod_list.last_mod^.thefile^ DO BEGIN
      fname := fname || '.PAS';
      find (fname)
    END;
    explode (mod_list.last_mod);
    count_passes (file_ptr); (* fill in some fields for later printing *)
    pass_union := pass_union + file_ptr^.pass_set;
    namelist_ptr := namelist_ptr^.link
  END;
  IF mod_list.first_mod = NIL THEN RETURN;
  (* process all included files, attempting to match to corresponding modules *)
  incl_rec_ptr := incl_list.first_mod;
  WHILE incl_rec_ptr <> NIL DO BEGIN
    incl_name := incl_rec_ptr^.thefile^.fname;
    incl_name := SUBSTR (incl_name, 1, INDEX (incl_name, '.', LENGTH (
      incl_name)));
    machine_indep := incl_name[1:3] = 'PTM';
    wild_on_pass := incl_name[1:3] = 'PAS';
    mod_rec_ptr := mod_list.first_mod;
    best_match := NIL;
    match_level := 10;
    WHILE mod_rec_ptr <> NIL DO WITH mod_rec_ptr^.thefile^ DO BEGIN
      EXIT IF (SUBSTR (fname, 1, LENGTH (incl_name)) = incl_name)
	DO best_match := mod_rec_ptr;
      IF machine_indep ANDIF (SUBSTR (fname, 4, LENGTH (incl_name)-3)
	= SUBSTR (incl_name, 4)) THEN BEGIN
	IF fname [1: 3] = 'P10' THEN temp_level := 0
	ELSE IF fname [1: 3] = 'Q10' THEN temp_level := 1
	ELSE temp_level := 2;
	IF temp_level < match_level THEN BEGIN
	  best_match := mod_rec_ptr;
	  match_level := temp_level
	END
      END;
      IF wild_on_pass ANDIF ((fname[1: 2] = 'PA') AND
	(fname [3] IN ['0'..'9'])) ANDIF (SUBSTR (fname, 4, LENGTH (incl_name)
	-3) = SUBSTR (incl_name, 4)) THEN IF (ORD (fname [3]) - ORD ('0'))
	< match_level THEN BEGIN
	match_level := ORD (fname [3]) - ORD ('0');
	best_match := mod_rec_ptr
      END;
      mod_rec_ptr := mod_rec_ptr^.next
    END;
    IF best_match <> NIL THEN BEGIN
      assert (incl_rec_ptr^.thefile^.owner = NIL);
      incl_rec_ptr^.thefile^.owner := best_match^.thefile;
      assert (best_match^.thefile^.num_owned < 4);
      best_match^.thefile^.num_owned := best_match^.thefile^.num_owned + 1;
      best_match^.thefile^.owns [best_match^.thefile^.num_owned] :=
	incl_rec_ptr^.thefile
    END;
    count_passes (incl_rec_ptr^.thefile); (* fill in some fields for later printing *)
    incl_rec_ptr := incl_rec_ptr^.next
  END;
  (* query user *)
  WRITE (TTYOUTPUT, 'output filename-- ');
  BREAK (TTYOUTPUT);
  READLN (TTY);
  out_name := '';
  WHILE NOT EOLN (TTY) DO BEGIN
    out_name := out_name || TTY^;
    GET (TTY)
  END;
  REWRITE (OUTPUT, out_name);
  WRITE (TTYOUTPUT, 'form of output ("command file" or "listing")-- ');
  BREAK (TTYOUTPUT);
  READLN (TTY);
  listing := UPPERCASE (TTY^) = 'L';
  (* produce desired listing *)
  jobinfo (jobstuff);
  time_stamp := 'From ' || jobstuff.lsegppn || '   ' || SUBSTR (dc_ext (
    daytime), 1, 15);
  IF mod_list.first_mod = mod_list.last_mod THEN BEGIN
    IF listing THEN WRITELN (OUTPUT, time_stamp: 80);
    print_exploded (mod_list.first_mod^.thefile, 0)
  END
  ELSE FOR pass_index := MINIMUM (passes) TO MAXIMUM (augmented_passes)
    DO IF pass_index IN pass_union THEN BEGIN
    IF listing THEN BEGIN
      page (OUTPUT);
      IF pass_index <= MAXIMUM (passes)
	THEN WRITELN (OUTPUT, 'Pass', pass_index, ' modules', time_stamp: 66)
      ELSE WRITELN (OUTPUT, 'Modules common to many passes', time_stamp: 51);
      lines_on_page := 1
    END;
    print_formatted (mod_list, pass_index);
    IF listing THEN BEGIN
      IF lines_on_page > 43 THEN BEGIN
	page (OUTPUT);
	lines_on_page := 0;
      END;
      WRITELN (OUTPUT);
      IF pass_index <= MAXIMUM (passes)
	THEN WRITELN (OUTPUT, 'Pass', pass_index,
	' included files not corresponding to a module in Pass', pass_index)
      ELSE WRITELN (OUTPUT,
	'Common included files not corresponding to any module');
      lines_on_page := lines_on_page + 2
    END;
    print_formatted (incl_list, pass_index)
  END;
  CLOSE
END;
(*
  PROCEDURE INCLUDE_CHOICES (interface to include lists)
  Calling sequence:
   include_choices;
								     *)

PROCEDURE include_choices;
VAR
i: INTEGER;
mod_temp: ^mod_list;
name_ptr,name_tail: name_list;
BEGIN
  mod_temp := mod_ptr;
  name_tail := NIL;
  WHILE mod_temp <> mod_tail DO BEGIN
    IF (mod_temp^.include) AND (NOT mod_temp^.lit) THEN BEGIN
      NEW(name_ptr);
      name_ptr^.name := mod_temp^.name;
      name_ptr^.codes := [];
      name_ptr^.link := NIL;
      FOR i := 0 TO e-1 DO IF network[mod_temp^.number,i] THEN name_ptr^.codes
	:= name_ptr^.codes + [i];
      IF name_tail = NIL THEN files_list := name_ptr
      ELSE name_tail^.link := name_ptr;
      name_tail := name_ptr;
    END;
    mod_temp := mod_temp^.link;
  END;
  include_lists;
END (* include_choices *);
(*
  FUNCTION COMPARE_SWITCHES
  Calling sequence:
   merge_switch := compare_switches(old_switch,new_switch);
  Where:
   old_switch is array [1..max_sw] of str array of global switches set
    outside of "(mod1,...,modn)/old_switches" expression
   new_switch is array [1..max_sw] of str array of overriding switches
    set within "(mod1,...,modi/new_switches,...,modn)" expression.
  Return:
   merge_switch is array [1..max_sw] of str array of merged switches
								     *)

FUNCTION compare_switches(switch: ARRAY [1..max_sw] OF str;
  new_sw: ARRAY [1..max_sw] OF str): ARRAY [1..max_sw] OF str;
VAR
update_array: ARRAY [1..max_sw] OF str;
i,j: INTEGER;
found: BOOLEAN;
current_switch: str;
BEGIN
  i := 1;
  update_array := switch;
  WHILE new_sw[i] <> '' DO BEGIN
    IF SUBSTR(new_sw[i],1,2) = 'NO' THEN current_switch := SUBSTR(new_sw[i],3)
    ELSE current_switch := new_sw[i];
    j := 1;
    WHILE (update_array [j] <> '') AND (NOT found)
      DO IF (update_array [j] = current_switch) OR (SUBSTR(update_array [j],3)
      = current_switch) THEN BEGIN
      update_array [j] := new_sw[i];
      found := TRUE
    END
    ELSE j := j+1;
    (* end while *)
    IF NOT found THEN update_array [j] := new_sw[i];
    i := i+1
  END (* while *);
  compare_switches := update_array
END (* compare_switches *);
(*
  FUNCTION VALID_SWITCH (determine validity of user specified switch)
  Calling sequence:
   ok := valid_switch(switch,full_switch,switch_type);
  Where:
   switch is str user specified switch
   switch_type is str -- either 'COMP' or 'LINK' specifying which type of
    switch is legal in the current context.
  Return:
   full_switch is str complete switch specification (e.g. 'GLOBAL' for 'X')
   ok is BOOLEAN -- true if switch is valid; false otherwise
									   *)

FUNCTION valid_switch(switch_str: str;VAR full_sw:str;sw_type:str):BOOLEAN;
CONST
valid_comp_switches:ARRAY [1..max_sw] OF str := ('BLOCKS','DEBUG','OPTIMIZE',
  'QUICK','SOURCE','XREF','');
actual_comp_switches:ARRAY [1..max_sw] OF str := ('QBLOCKS','DEBUG','OPTIMIZE'
  ,'QUICK','SOURCE','GLOBAL','');
valid_link_switches:ARRAY [1..max_sw] OF str := ('MAP','DEBUG','SYMSEG: HIGH',
  '','','','');
actual_link_switches:ARRAY [1..max_sw] OF str := ('MAP','SYS: LNKDDT',
  'SYMSEG:HIGH','','','','');
VAR
switch: str;
valid_switch_names,actual_switch: ARRAY [1..max_sw] OF str;
found,next,i: INTEGER;
BEGIN
  IF sw_type = 'COMP' THEN BEGIN
    valid_switch_names := valid_comp_switches;
    actual_switch := actual_comp_switches
  END
  ELSE BEGIN
    valid_switch_names := valid_link_switches;
    actual_switch := actual_link_switches
  END;
  next := 1;
  valid_switch := FALSE;
  full_sw := '';
  IF (cmd_eol(switch_str,next)) THEN RETURN;
  switch := SUBSTR(switch_str,next);
  IF switch[1] = '-' THEN BEGIN
    full_sw := 'NO';
    switch := SUBSTR(switch,2)
  END
  ELSE IF (LENGTH(switch) >= 2) ANDIF (SUBSTR(switch,1,2) = 'NO') THEN BEGIN
    full_sw := 'NO';
    switch := SUBSTR(switch,3)
  END;
  IF (full_sw = 'NO') AND (sw_type = 'LINK')
    THEN err('LINK switches may not be negated');
  IF cmd_eol(switch,next) THEN RETURN;
  next := SEARCH(switch,[' '],LENGTH(switch)+1);
  switch := SUBSTR(switch,1,next-1);
  found := 0;
  FOR i := 1 TO max_sw DO BEGIN
    IF switch = SUBSTR(valid_switch_names[i],1,LENGTH(switch))
      THEN found := i;
    EXIT IF found > 0
  END;
  IF found = 0 THEN RETURN
  ELSE BEGIN
    full_sw := full_sw || actual_switch[found];
    valid_switch := TRUE
  END;
END (*valid switch *);
(*
  FUNCTION SET_SWITCHES (parse switches from command string)
  Calling sequence:
   switch_array := set_switches(switch_str,switch_type);
  Where:
   switch_str is str user specified set of switches
   switch_type is str type of switches to expect -- either 'COMP' or 'LINK'
  Return:
   switch_array is array [1..max_sw] of str array of switches found
									  *)

FUNCTION set_switches(switch_str:str;sw_type : str):ARRAY [1..max_sw] OF str;
VAR
i,next: INTEGER;
list,full_sw: str;
BEGIN
  FOR i := 1 TO max_sw DO set_switches[i] := '';
  IF switch_str <> '' THEN BEGIN
    i := 0;
    list := SUBSTR(switch_str,2); (* get past leading "/" *)
    WHILE list <> '' DO BEGIN
      i := i+1;
      next := SEARCH(list,['/'],LENGTH(list)+1);
      IF valid_switch(SUBSTR(list,1,next-1),full_sw,sw_type)
	THEN set_switches[i] := full_sw
      ELSE err('invalid switch:  "' || SUBSTR(list,1,next-1) || '"');
      list := SUBSTR(list,MIN(next+1,LENGTH(list)+1))
    END (*while *);
    IF i=0 THEN err('expecting switch specification');
  END (* if *);
END (* set_switches *);
(*
  PROCEDURE LINK_ALL_COMPILED ( set link flags)
  Calling sequence:
   link_all_compiled(link_switches);
  Where:
   link_switches is array [1..max_sw] of str array of valid switches
  Note:
   All programs containing modules which have been slated for compilation
    are flagged for linking.
									 *)

PROCEDURE link_all_compiled(link_switches: ARRAY [1..max_sw] OF str);
VAR
prog_temp: ^prog_list;
mod_temp: ^mod_list;
i: passes;
found,any: BOOLEAN;
BEGIN
  any := FALSE;
  mod_temp := mod_ptr;
  WHILE mod_temp <> mod_tail DO BEGIN
    IF mod_temp^.comp THEN BEGIN
      FOR i := MINIMUM (passes) TO MAXIMUM (passes) DO BEGIN
	IF (network[mod_temp^.number,i]) THEN BEGIN
	  prog_temp := prog_ptr;
	  found := FALSE;
	  WHILE NOT found DO IF prog_temp^.number = i THEN found := TRUE
	  ELSE prog_temp := prog_temp^.link;
	  (* end while *)
	  prog_temp^.load := TRUE;
	  any := TRUE;
	  prog_temp^.opt := link_switches
	END
      END
    END;
    mod_temp := mod_temp^.link
  END;
  IF NOT any THEN err('no modules compiled; therefore none to link');
END (* link_all_compiled *);
(*
  PROCEDURE LINK_RANGE (set link flag for range of programs)
  Calling sequence:
   link_range(lower,upper,link_switches);
  Where:
   lower is integer lower limit of program range to be linked
   upper is integer higher limit of program range to be linked
   link_switches is array [1..max_sw] of str array of user
    spedified link switches
							       *)

PROCEDURE link_range(lower: INTEGER; upper:INTEGER;
  link_switches: ARRAY [1..max_sw] OF str);
VAR
prog_temp: ^prog_list;
BEGIN
  prog_temp := prog_ptr;
  WHILE prog_temp <> prog_tail DO BEGIN
    IF (prog_temp^.number >= lower) AND (prog_temp^.number <= upper)
      THEN BEGIN
      prog_temp^.load := TRUE;
      prog_temp^.opt := link_switches
    END;
    prog_temp := prog_temp^.link
  END
END (* link_range *);
(*
  PROCEDURE LINK_PIECE (parse link command)
  Calling sequence:
   link_piece(cmd);
  Where:
   cmd is str body of user specified link command (everything after "L")
									*)

PROCEDURE link_piece(cmdstr: str);
VAR
link_str: str;
cmd: str;
i, next: INTEGER;
lower,upper: INTEGER;
link_switches: ARRAY [1..max_sw] OF str;
BEGIN
  next := SEARCH(cmdstr,['/'],LENGTH(cmdstr)+1); (* pull off link switches *)
  link_switches := set_switches(SUBSTR(cmdstr,next),'LINK');
  cmd := SUBSTR(cmdstr,1,next-1);
  cmd := SUBSTR(cmd,VERIFY(cmd,[' '],LENGTH(cmd)+1));
  IF cmd = '' THEN err('null link command');
  IF cmd[1] = '*' THEN (* link all *)
  link_range(MINIMUM (passes),MAXIMUM (passes),link_switches)
  ELSE IF (UPPERCASE(cmd[1]) = 'C') AND (SUBSTR(cmd,2) = ' ')
    THEN link_all_compiled(link_switches)
  ELSE BEGIN
    get_prog_range(cmd,lower,upper);
    link_range(lower,upper,link_switches)
  END;
  link_on := TRUE
END (* link piece *);
(*
  PROCEDURE PICK_MODS (get modules from user specified list)
  Calling sequence:
   pick_mods(cmd,switch,except);
  Where:
   cmd is str module specification list -- of the form:
    "*" -- operate on all modules in system
    "(mod1,...,modn)" -- operate on specified modules
    "prog#" or "prog#-prog#" -- operate on modules included in any of
     the programs specified
   switch is array [1..max_sw] of str array of user specified switches
   except is BOOLEAN -- if true then do not compile modules specified
								     *)

PROCEDURE pick_mods(cmdstr: str;switch:ARRAY [1..max_sw] OF str;
  except:BOOLEAN);
VAR
cmd,mod_and_new_sw,md,new_sw: STRING;
i,INDEX,next,mod_num,lower,upper: INTEGER;
temp_mod: ^mod_list;
delim: CHAR;
new_sw_array: ARRAY [1..max_sw] OF str;
BEGIN
  cmd := cmdstr;
  temp_mod := mod_ptr;
  CASE cmd[1] OF
    '*': (* compile all modules in system *)
    WHILE temp_mod <> mod_tail DO BEGIN
      IF (include_now) AND (NOT network[temp_mod^.number,e]) AND
	(NOT network[temp_mod^.number,s]) THEN temp_mod^.include := TRUE
      ELSE IF select_now THEN temp_mod^.select := TRUE
      ELSE IF (NOT network[temp_mod^.number,e]) AND
	(NOT network[temp_mod^.number,s]) THEN BEGIN
	temp_mod^.comp := NOT except ;
	temp_mod^.opt := switch
      END;
      temp_mod := temp_mod^.link;
    END;
    '(': (* compile individual modules *)
    BEGIN
      cmd := SUBSTR(cmd,2);
      delim := ',';
      WHILE delim <> ')' DO BEGIN (* balanced parans already checked for *)
	next := SEARCH(cmd,[',',')']);
	delim := cmd[next];
	new_sw := '';
	mod_and_new_sw := SUBSTR(cmd,1,next-1);
	INDEX := SEARCH(mod_and_new_sw,['/']); (* separate modifier and switches *)
	IF INDEX > 0 THEN BEGIN
	  md := SUBSTR(mod_and_new_sw,1,INDEX-1);
	  new_sw_array := set_switches(SUBSTR(mod_and_new_sw,INDEX),'COMP')
	END
	ELSE md := mod_and_new_sw;
	new_sw_array := compare_switches(switch,new_sw_array);
	temp_mod := mod_ptr;
	WHILE (temp_mod <> mod_tail) AND (temp_mod^.name <> UPPERCASE(md))
	  DO temp_mod := temp_mod^.link;
	IF (temp_mod = mod_tail)
	  THEN err('undefined module name:  "' || md || '"')
	ELSE IF (include_now) AND (NOT network[temp_mod^.number,e]) AND
	  (NOT network[temp_mod^.number,s]) THEN temp_mod^.include := TRUE
	ELSE IF select_now THEN temp_mod^.select := TRUE
	ELSE IF (NOT network[temp_mod^.number,e]) AND
	  (NOT network[temp_mod^.number,s])
	  THEN temp_mod^.comp := NOT except ;
	temp_mod^.opt := new_sw_array;
	cmd := SUBSTR(cmd,next+1)
      END (* while *);
    END;
    OTHERS: (* check for range of program values *)
    BEGIN
      get_prog_range(cmd,lower,upper);
      WHILE temp_mod <> mod_tail DO BEGIN
	FOR i:= lower TO upper DO BEGIN
	  IF (network[temp_mod^.number,i]) THEN BEGIN
	    IF (include_now) AND (NOT network[temp_mod^.number,e]) AND
	      (NOT network[temp_mod^.number,s]) THEN temp_mod^.include := TRUE
	    ELSE IF select_now THEN temp_mod^.select := TRUE
	    ELSE IF (NOT network[temp_mod^.number,e]) AND
	      (NOT network[temp_mod^.number,s])
	      THEN temp_mod^.comp := NOT except;
	    temp_mod^.opt := switch
	  END
	END;
	temp_mod := temp_mod^.link
      END
    END
  END
END;

PROCEDURE examine_piece(cmdstr: STRING);
(*Process compile request.*)
VAR
cmd, switch_buf: STRING;
comp_switches: ARRAY[1 .. max_sw] OF STRING;
INDEX, next: INTEGER;
except: BOOLEAN;
BEGIN
  except := FALSE;
  cmd := SUBSTR(cmdstr, VERIFY(cmdstr, [' '], LENGTH(cmdstr) + 1));
  switch_buf := '';
  comp_switches := set_switches(switch_buf, 'COMP');
  IF cmd[1] = '#' THEN BEGIN (*"#" is a don't compile switch*)
    except := TRUE;
    cmd := SUBSTR(cmd, 2)
  END;
  IF cmd = '' THEN err('Invalid command:  "' || cmdstr || '"');
  IF cmd[1] = '(' THEN BEGIN
    INDEX := SEARCH(cmd, [')']);
    switch_buf := SUBSTR(cmd, INDEX +1);
    IF switch_buf <> '' THEN switch_buf := SUBSTR(switch_buf, VERIFY(
      switch_buf, [' ']));
    IF (switch_buf <> '') ANDIF (switch_buf[1] <> '/')
      THEN err('Expecting switches "' || cmdstr || '"');
    cmd := SUBSTR(cmd, 1, INDEX)
  END
  ELSE BEGIN
    INDEX := SEARCH(cmd, ['/'], LENGTH(cmd) + 1);
    switch_buf := SUBSTR(cmd, INDEX);
    cmd := SUBSTR(cmd, 1, INDEX - 1)
  END;
  comp_switches := set_switches(switch_buf, 'COMP');
  pick_mods(cmd, comp_switches, except);
  IF (NOT select_now) AND (NOT include_now) THEN compile_on := TRUE
END;

PROCEDURE separate_command(cmdstr: STRING; operate: PROCEDURE(STRING));
(*Seperate and process command operations.*)
VAR
cmd, rest: STRING;
next: INTEGER;
BEGIN
  cmd := SUBSTR(cmdstr, VERIFY(cmdstr, [' '], LENGTH(cmdstr) + 1));
  WHILE cmd <> '' DO BEGIN
    IF cmd[1] = '(' THEN BEGIN
      next := SEARCH(cmd, [')']);
      IF next = 0 THEN err('Unbalanced paranthesis ' || cmd)
      ELSE BEGIN
	rest := SUBSTR(cmd, next + 1);
	next := next + SEARCH(rest, [','], LENGTH(rest) + 1)
      END
    END
    ELSE next := SEARCH(cmd, [','], LENGTH(cmd) + 1);
    operate(SUBSTR(cmd, 1,next - 1));
    cmd := SUBSTR(cmd, MIN(LENGTH(cmd)+ 1, next + 1));
  END
END;

PROCEDURE queue_f;
(*Create XXXBLD.CMD command file for manual processing.*)
VAR
cmd_file: TEXT;
BEGIN
  IF compile_on THEN BEGIN
    REWRITE(cmd_file, jobnum || 'BLD.CMD');
    init_comp_file(cmd_file);
    fill_comp_file(cmd_file);
    CLOSE(cmd_file);
    IF link_on THEN err('"QF" can not both compile and link');
  END
  ELSE IF link_on THEN link_choices('BLD.CMD', FALSE)
  ELSE err('"QF" inappropriate -- nothing to compile or link');
  WRITELN(TTYOUTPUT, jobnum || 'BLD.CMD created');
  BREAK(TTYOUTPUT)
END;

PROCEDURE queue_b;
(*Create XXXBLD.CTL control file for batch processing.*)
VAR
ctl_file: TEXT;
prog_temp: ^prog_list;
mod_temp: ^mod_list;
i: INTEGER;
first: BOOLEAN;
BEGIN
  REWRITE(ctl_file, jobnum || 'BLD.CTL');
  WRITELN(ctl_file, ':' || jobnum || 'BLD.LOG');
  WRITELN(ctl_file, ':TIME 300');
  IF compile_on THEN BEGIN
    WRITELN(ctl_file, ':DEFINE A="?"');
    WRITELN(ctl_file, ':FIND A');
    WRITELN(ctl_file, 'R ' || compiler);
    init_comp_file(ctl_file);
    fill_comp_file(ctl_file);
    WRITELN(ctl_file);
    WRITELN(ctl_file, ':IF A THEN QUIT');
  END;
  IF link_on THEN BEGIN
    prog_temp := prog_ptr;
    WHILE prog_temp <> prog_tail DO BEGIN
      IF prog_temp^.load THEN BEGIN
	first := TRUE;
	mod_temp := mod_ptr;
	WHILE mod_temp <> mod_tail DO BEGIN
	  IF network[mod_temp^.number, prog_temp^.number] THEN BEGIN
	    IF first THEN BEGIN
	      WRITELN(ctl_file, 'R ' || loader);
	      WRITE(ctl_file, prog_temp^.name || '[,]/SSAVE');
	      FOR i := 1 TO max_sw DO IF (prog_temp^.opt[i] = 'MAP') OR
		(prog_temp^.opt[i] = 'NOMAP')
		THEN WRITE(ctl_file, '/' || prog_temp^.opt[i]);
	      WRITELN(ctl_file, '=');
	      add_link_opt(ctl_file, prog_temp);
	      first := FALSE
	    END;
	    get_full_name(ctl_file, mod_temp)
	  END;
	  mod_temp := mod_temp^.link
	END;
	WRITELN(ctl_file, '/GO');
      END;
      prog_temp := prog_temp^.link
    END
  END;
  CLOSE(ctl_file);
  WRITELN(TTYOUTPUT, jobnum || 'BLD.CTL created');
  BREAK(TTYOUTPUT);
END;

PROCEDURE queue_n;
BEGIN
  IF compile_on THEN compile_choices
  ELSE IF link_on THEN link_choices('LNK.TMP', TRUE);
  IF compile_on THEN IF NOT runprg(compiler, 1) THEN BEGIN
    REWRITE(TTYOUTPUT);
    WRITELN(TTYOUTPUT, '?Can not run compiler.');
    STOP
  END;
  IF link_on THEN IF NOT runprg(loader, 1) THEN BEGIN
    REWRITE(TTYOUTPUT);
    WRITELN(TTYOUTPUT, '?Can not run linker.');
    STOP
  END;
END;

PROCEDURE queue_command(cmd: STRING);
(*Process queue request.*)
VAR
prog_temp: ^prog_list;
mod_temp: ^mod_list;
BEGIN
  IF SUBSTR(cmd, 2) <> '' THEN err('Extra queue command ' || cmd);
  IF select_on THEN select_choices;
  IF include_on THEN include_choices;
  CASE cmd[1] OF
    'N': queue_n;
    'F': queue_f;
    'B': queue_b;
    OTHERS: err('Invalid queue command "' || cmd || '"');
  END;
  prog_temp := prog_ptr;
  WHILE prog_temp <> prog_tail DO BEGIN
    prog_temp^.load := FALSE;
    prog_temp := prog_temp^.link;
  END;
  mod_temp := mod_ptr;
  WHILE mod_temp <> mod_tail DO BEGIN
    mod_temp^.comp := FALSE;
    mod_temp^.include := FALSE;
    mod_temp^.select := FALSE;
    mod_temp := mod_temp^.link;
  END;
  queued := TRUE
END;

PROCEDURE include_piece(cmd: STRING);
(*Process include request.*)
BEGIN
  include_on := TRUE;
  include_now := TRUE;
  examine_piece(cmd);
END;

PROCEDURE select_piece(cmd: STRING);
(*Process select request.*)
BEGIN
  select_on := TRUE;
  select_now := TRUE;
  examine_piece(cmd);
END;

PROCEDURE process_command(cmd: STRING);
(*Process a user command.*)
BEGIN
  IF cmd <> '' THEN BEGIN
    select_now := FALSE;
    include_now := FALSE;
    queued := FALSE;
    CASE cmd[1] OF
      'C': separate_command(SUBSTR(cmd, 2), examine_piece);
      'L': separate_command(SUBSTR(cmd, 2), link_piece);
      'S': separate_command(SUBSTR(cmd, 2), select_piece);
      'I': separate_command(SUBSTR(cmd, 2), include_piece);
      'Q': queue_command(SUBSTR(cmd, 2));
      OTHERS: err('Invalid command "' || cmd[1] || '"');
    END
  END
END;

PROCEDURE read_command;
(*Read and process user commands.*)
VAR
cmdline: STRING;
next: INTEGER;
done: BOOLEAN;
BEGIN
  REPEAT
    done := TRUE;
    WRITE(TTY, '*');
    BREAK(TTYOUTPUT);
    READLN(TTY);
    READ(TTY, cmdline);
    cmdline := UPPERCASE(cmdline);
    IF cmdline = '?' THEN BEGIN
      WRITELN(TTYOUTPUT, 'Commands are:');
      WRITELN(TTYOUTPUT, 'C -- compile');
      WRITELN(TTYOUTPUT, 'L -- link');
      WRITELN(TTYOUTPUT, 'S -- ???');
      WRITELN(TTYOUTPUT, 'I -- ???');
      WRITELN(TTYOUTPUT, 'Q -- queue, QN=now, QB=batch, QF=file');
      WRITELN(TTYOUTPUT, 'Seperate multiple commands with ";".');
      done := FALSE
    END
    ELSE WHILE cmdline <> '' DO BEGIN
      next := SEARCH(cmdline, [';'], LENGTH(cmdline) + 1);
      process_command(SUBSTR(cmdline, 1, next - 1));
      IF (next <= LENGTH(cmdline)) ANDIF (SUBSTR(cmdline, next + 1) = '')
	THEN done := FALSE;
      cmdline := SUBSTR(cmdline, MIN(next + 1, LENGTH(cmdline) + 1))
    END
  UNTIL done
END;

PROCEDURE do_program (VAR work_line: STRING; VAR next: INTEGER;
  value: INTEGER);
(*Add a program to the network.*)
VAR
prog_temp: ^prog_list;
BEGIN
  cmd_skip_blanks(work_line, next);
  IF next > LENGTH(work_line) THEN err('Null program name?');
  work_line := SUBSTR(work_line, next);
  next := SEARCH(work_line, [' ', ';'], LENGTH(work_line) + 1);
  NEW(prog_temp);
  prog_temp^.name := UPPERCASE(SUBSTR(work_line, 1, next - 1));
  prog_temp^.number := value;
  IF prog_ptr = prog_tail THEN BEGIN
    prog_ptr := prog_temp;
    prog_temp^.link := prog_tail;
    prog_tail^.link := prog_temp;
  END
  ELSE BEGIN
    prog_temp^.link := prog_tail;
    prog_tail^.link^.link := prog_temp;
    prog_tail^.link := prog_temp;
  END;
  work_line := SUBSTR(work_line, next)
END;

PROCEDURE add_network(line: STRING);
(*Add program or module to the network.*)
VAR
work_line: STRING;
next, value: INTEGER;
prog: BOOLEAN;
BEGIN
  work_line := line;
  next := 1;
  prog := cmd_number(work_line, next, FALSE, value);
  work_line := SUBSTR(work_line, next);
  next := 1;
  IF prog THEN do_program(work_line,next,value)
  ELSE do_module(work_line, next);
  IF work_line <> '' THEN BEGIN
    cmd_skip_blanks(work_line, next);
    IF (next <= LENGTH(work_line)) ANDIF (work_line[next] <> ';')
      THEN err('Extra characters in line "' || line || '"')
  END
END;

PROCEDURE read_name(VAR name: FILE_NAME);
(*Read name of compiler/loader.*)
BEGIN
  name := '';
  WHILE NOT EOLN(build_file) ANDIF NOT (build_file^ IN ['/',';']) DO BEGIN
    IF build_file^ <> ' ' THEN name := name || UPPERCASE(build_file^);
    GET(build_file)
  END;
  IF name = '' THEN err('Name expected in ' || bld_file_name);
END;

PROCEDURE process_build_file;
(*Read the build file, create data structures.*)
VAR
line: STRING[256];
i: INTEGER;
BEGIN
  line := '';
  n_defaults := 0;
  LOOP
    RESET(build_file, bld_file_name || line);
    IF iostatus <> IO_OK THEN err('Cannot read ' || bld_file_name || line);
    EXIT IF build_file^ <> '@';
    GET(build_file);
    READLN(build_file, line);
    n_defaults := n_defaults + 1;
    default_ppns[n_defaults] := line
  END;
  read_name(compiler);
  READLN(build_file);
  read_name(loader);
  READLN(build_file);
  READLN(build_file, line);
  i := 1;
  WHILE (n_defaults <> max_defaults) ANDIF NOT cmd_eol(line, i) ANDIF
    cmd_file_name(line, i, FALSE, default_ppns[n_defaults + 1])
    DO n_defaults := n_defaults + 1;
  WHILE NOT EOF(build_file) DO BEGIN
    READLN(build_file, line);
    add_network(line)
  END
END;

PROCEDURE set_up_structures;
VAR
i: modules;
j: passes;
BEGIN
  NEW(prog_tail);
  NEW(mod_tail);
  prog_ptr := prog_tail;
  mod_ptr := mod_tail;
  mod_tail^.number := 0;
  compile_on := FALSE;
  link_on := FALSE;
  include_on := FALSE;
  select_on := FALSE;
  FOR i := MINIMUM(modules) TO MAXIMUM(modules) DO FOR j := MINIMUM(passes)
    TO MAXIMUM(passes) DO network[i, j] := FALSE;
END;

BEGIN
  OPEN(TTY);
  REWRITE(TTYOUTPUT);
  WRITELN(TTYOUTPUT,'TYM-Pascal Program Builder, Version ', version());
  WRITELN(TTYOUTPUT);
  set_up_structures;
  process_build_file;
  read_command;
  IF NOT queued THEN queue_command('N');
END.
    T