(*
  BUILD
  Utility program to simplify compiling and linking in large systems where
  procedure modules may be used in more than one program.
  
  Version 2.0
                                                                          *)

program build
  options storage (4000);

$SYSTEM jobnum
$SYSTEM dtime.typ
$SYSTEM dtime
$SYSTEM pasdir.typ
$SYSTEM pasdir
$SYSTEM infpac
$SYSTEM run
$SYSTEM cmdutl
const
    max_progs := 64;
    max_mods := 256;
    e := 62;
    s := 63; (* build table module switches *)
    max_sw := 7;
    max_defaults := 5;
    bld_file_name := 'PASCAL.BLD' (* hard coded build file name *);
 
type
    str = string;
    str15 = string[15];
    name_list = ^name_node;
    passes = 0..max_progs;
    modules = 0..max_mods;
  
    name_node = record
      name: str;
      codes: set of passes;
      link: name_list
    end;

    prog_list = record (* program name data structure *)
      name: str15;
      number: integer;
      load: boolean;
      opt: array [1..max_sw] of str;
      link: ^prog_list
    end;
    
    mod_list = record (* module name data structure *) 
      name: str;
      lit: boolean;
      number: integer;
      comp: boolean;
      include: boolean;
      select: boolean;
      opt: array [1..max_sw] of str;
      link: ^mod_list
    end;

var
    build_file: text ;
    network: array [modules, passes] of boolean;
    files_list: name_list;
    prog_ptr,prog_tail: ^prog_list;
    mod_ptr,mod_tail: ^mod_list;
    compiler: str;
    loader: str;
    compile_on: boolean;
    link_on: boolean;
    select_on: boolean;
    select_now: boolean;
    include_on: boolean;
    include_now: boolean;
    queued: boolean;
    n_defaults: 0..max_defaults;
    default_ppns: array [1..max_defaults] of str;

$PAGE err
(*
  PROCEDURE ERR (error message regurgitator)
  Calling sequence
   err(msg)
  Where: 
   msg is str error message to be displayed at the terminal
                                                                        *)

procedure err(msg: str);
begin
writeln(tty,'ERROR:  ' || msg);
stop;
end;
$PAGE convert_prog
(*
  FUNCTION CONVERT_PROG (associate number with program name)
  Calling sequence: 
   prog_num := convert_prog(name)
  Where: 
   name is str program name
  Return: 
   prog_num is integer program number associated with name
                                                                        *)

function convert_prog(name: str15):integer;
var
    value,index: integer;
    found,num: boolean;
    prog_temp: ^prog_list;

begin
index := 1;
found := false;
num := cmd_number(name,index,false,value);
if num then begin (* found legal value *)
  prog_temp := prog_ptr;
  while (not found) and (prog_temp <> prog_tail) do 
    if prog_temp^.number = value then
      found := true
    else
      prog_temp := prog_temp^.link;
  if not found then err('undefined program number');
  convert_prog := value
end
else if lowercase(name) = 'e' then (* "e" switch found *)
  convert_prog := e
else if lowercase(name) = 's' then (* "s" switch found*)
  convert_prog := s
else begin
  prog_temp := prog_ptr;  (* start search at top of program list *)
  while (not found) and (prog_temp <> prog_tail) do
    if prog_temp^.name = name then begin
      convert_prog := prog_temp^.number;
      found := true
    end
    else
      prog_temp := prog_temp^.link;
    if not found then
      err('undefined program name "' || name || '"')
end
end (* convert_prog *);
$PAGE get_prog_range
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

procedure get_prog_range(list: str; var lower:integer; var upper:integer);
var
    range: integer;

begin
range := search(list,['-']);
if range > 0 then begin (* found  i-j expression *)
  lower := convert_prog(substr(list,1,range-1));
  upper := convert_prog(substr(list,range+1))
end
else begin
  lower := convert_prog(list);
  upper := lower
end
end (* get_prog_range *);
$PAGE do_module
(*
  PROCEDURE DO_MODULE  ( add module to global data structure )
  Calling sequence: 
   do_module(work_line,next)
  Where: 
   work_line is str line from build_file containing module information
   next is integer index
                                                                        *)

procedure do_module(var work_line: str; var next:integer);
var
    list: str;
    i,lower,upper,range,quote: integer;
    done: boolean;
    mod_temp: ^mod_list;

begin
new(mod_temp);
work_line := uppercase (work_line);
next := 1;
if cmd_check_punct (work_line, next, '"') then begin
  if not cmd_string (work_line, next, '"', mod_temp^.name) then
    err('unbalanced literal ' || work_line);
  mod_temp^.lit := true;
end
else begin
  if not cmd_file_name (work_line, next, true, mod_temp^.name) then
    err('bad module name ' || work_line);
  mod_temp^.lit := false;
end;
mod_temp^.comp := false;
mod_temp^.include := false;
mod_temp^.select := false;
if mod_ptr = mod_tail then begin
  mod_ptr := mod_temp;
  mod_temp^.number := 1;
  mod_temp^.link := mod_tail;
  mod_tail^.link := mod_temp;
end
else begin
  mod_temp^.link := mod_tail;
  mod_temp^.number := mod_tail^.link^.number + 1;
  mod_tail^.link^.link := mod_temp;
  mod_tail^.link := mod_temp;
end;
for i := 1 to max_sw do
  mod_temp^.opt[i] := '';
if not cmd_check_punct (work_line, next, '/') then
  err('module specification must be followed by attribute list');
work_line := substr(work_line,next);
done := false;
while not done do begin
  next := search(work_line,[',',';'],length(work_line)+1);
  list := substr(work_line,1,next-1);
  get_prog_range(list,lower,upper);
  for i := lower to upper do
    network[mod_temp^.number,i] := true; (* update network data structure *)
  if (next > length(work_line)) orif (work_line[next] = ';') then begin
    work_line := '';
    done := true
  end
  else
    work_line := substr(work_line,next+1)
end
end (* do_module *);
$PAGE convrt
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

function convrt(num: integer; pad:integer):str;
const
    zeropad := '0000000000000000';

var
    value: str;

begin
putstring(value,num: 0);
if length(value) < pad then
  value := substr(zeropad,1,pad-length(value)) || value;
convrt := value
end (* convrt *);
$PAGE get_full_name (determine directory for module name)
(*
  PROCEDURE GET_FULL_NAME
  Calling sequence: 
   get_full_name(out_file,mod_ptr);
  Where: 
   out_file is text file used for link commands
   mod_ptr is pointer to current module
                                                                   *)

procedure get_full_name(var out_file:text; mod_ptr : ^mod_list);
var
    mod_file: file of integer;
    defn: integer;
    full_name: str;

begin
full_name := mod_ptr^.name;
if not mod_ptr^.lit then begin
  reset(mod_file,'.REL ' || mod_ptr^.name); (* try default module file names *)
  defn := 0;
  while (eof(mod_file)) and (defn < n_defaults) do begin
    close(mod_file);
    defn := defn+1;
    full_name := mod_ptr^.name || default_ppns[defn]; (* add default *)
    reset(mod_file,'.REL ' || full_name);
  end;
  close(mod_file);
end;
write(out_file,full_name);
if network[mod_ptr^.number,s] then (* /SEARCH switch in build_file *)
  write(out_file,'/S');
writeln(out_file);
end (* get_full_name *);
$PAGE add_link_opt
(*
  PROCEDURE ADD_LINK_OPT (add linker options to linker input file)
  Calling sequence: 
   add_link_opt(out_file,prog_ptr);
  Where
   out_file is text file for linker input
   prog_ptr is pointer to current program
                                                                       *)

procedure add_link_opt(var out_file:text; prog_ptr : ^prog_list);
var
    i: integer;

begin
for i := 1 to max_sw do
  if prog_ptr^.opt[i] = 'SYMSEG: HIGH' then
    writeln(out_file,'/SYMSEG: HIGH');
for i := 1 to max_sw do
  if prog_ptr^.opt[i] = 'SYS: LNKDDT' then begin
    writeln(out_file,'SYS: LNKDDT');
    writeln(out_file,'SYS: JOBDAT');
  end;
end (* add_link_opt *);
$PAGE link_choices
(*
  PROCEDURE LINK_CHOICES (setup link file for requested programs)
  Calling sequence: 
   link_choices(file_name,multiple);
  where: 
   file_name is text file to write link commands to
   multiple is boolean flag -- true if more than one program may be linked;
    false otherwise
  Notes: 
   If more than one program is to be linked, link files are chained
    together via the last line in each link file, so that when the
    loader is run, execution will be transferred to each link file
    in succession.
                                                                        *)

procedure link_choices(file_name:str; multiple : boolean);
var
    i,num,tnum: integer;
    full_name: str;
    prog_next,prog_temp: ^prog_list;
    mod_temp: ^mod_list;
    next,first: boolean;
    out_link: text;
    mod_file: file of integer;
    defn: 0..max_defaults;

begin
num := 0;
prog_temp := prog_ptr;
while prog_temp <> prog_tail do begin
  if prog_temp^.load then begin (* program is to be compiled *)
    first := true;
    mod_temp := mod_ptr;
    while mod_temp <> mod_tail do begin (* see which moodules are in this program *)
      if network[mod_temp^.number,prog_temp^.number] then begin
        if first then begin (* first line in link file? *)
          if num = 0 then (* first link file? *)
            rewrite(out_link,jobnum || file_name)
          else
            rewrite(out_link,jobnum || 'L' || convrt(num,2) || '.TMP');
          write(out_link,prog_temp^.name || '[,]/SSAVE');
          for i := 1 to max_sw do
            if (prog_temp^.opt[i]='MAP') or (prog_temp^.opt[i]='NOMAP') then
              write(out_link,'/' || prog_temp^.opt[i]);
          writeln(out_link,'=');
	  add_link_opt(out_link,prog_temp);
          first := false
        end;
        (* now put module name in file *)
        get_full_name(out_link,mod_temp);
      end (* if *);
      mod_temp := mod_temp^.link;
    end (* while *);
    (* look ahead to see if linker will be run again *)
    prog_next := prog_temp^.link;
    next := false;
    while  (prog_next <> prog_tail) and (not next) do
      if prog_next^.load then
        next := true
      else
        prog_next := prog_next^.link;
    if next then begin (* more files to follow *)
      if not multiple then
        err('only one program may be linked');
      tnum := num + 1;
      write(out_link,'/RUN:' || loader || '/RUNOFFSET:1/TMPFIL:LNK:"@');
      write(out_link,jobnum || 'L' || convrt(tnum,2) || '.TMP"');
    end;
    writeln(out_link,'/GO');
    close(out_link);
    num := num + 1;
  end (* if *);
  prog_temp := prog_temp^.link
end (* while *)
end (*link_choices *);
$PAGE init_comp_file
(*
  PROCEDURE INIT_COMP_FILE ( write header info to compile file )
  Calling sequence: 
   init_comp_file(out_file);
  Where: 
   out_file is text file used as compilation file
                                                                      *)

procedure init_comp_file(var out_file: text);
var
    i: integer;

begin
for i := 1 to n_defaults do begin
  if i = 1 then 
    write(out_file,'/SEARCH(')
  else
    write(out_file,',');
  write(out_file,default_ppns[i])
end;
if i <> 0 then
  write(out_file,')');
writeln(out_file,'/SPECIAL(WORD)/NOCHECK/NAMES');
end (* init_comp_file *);
$PAGE fill_comp_file
(*
  PROCEDURE FILL_COMP_FILE ( add the body into the compilation file )
  Calling sequence: 
   fill_comp_file(out_file);
  Where: 
   out_file is text file to be used for compilation
                                                                            *)

procedure fill_comp_file(var out_file: text);
var
    mod_temp: ^mod_list;
    i: integer;

begin
mod_temp := mod_ptr;
while mod_temp <> mod_tail do begin
  if mod_temp^.comp then begin
    write(out_file,mod_temp^.name);
    if not mod_temp^.lit then begin
      for i := 1 to max_sw do (* append switches *)
	if mod_temp^.opt[i] <> '' then
	  write(out_file,'/' || mod_temp^.opt[i]);
    end;
    writeln(out_file);
  end;
  mod_temp := mod_temp^.link;
end (* while *);
end (* fill_comp_file *);
$PAGE compile_choices
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

procedure compile_choices;
var
    comp_file: text;

begin
rewrite(comp_file,jobnum || 'PAS.TMP'); (* default compiler input file *)
init_comp_file(comp_file);
fill_comp_file(comp_file);
if link_on then begin (* add a line to run the loader *)
  writeln(comp_file,'/RUN:',loader,'/RUNOFFSET');
  link_choices('LNK.TMP',true);
end;
close(comp_file);
end (* compile choices *);
$PAGE select_choices
(*
  PROCEDURE SELECT_CHOICES (dump selected module names to temporary file)
  Calling sequence: 
   select_choices;
  Note: 
   A temporary file of the form xxxSEL.TMP is used, where xxx is the
    three digit job number.
                                                                       *)

procedure select_choices;
var
    i: integer;
    mod_temp: ^mod_list;
    select_file: text;
    mod_file: file of integer;
    defn: 0..max_defaults;
    full_name: str;

begin
rewrite(select_file,jobnum || 'SEL.TMP');
writeln(select_file,'MODULES SELECTED:  ');

mod_temp := mod_ptr;
while mod_temp <> mod_tail do begin
  if mod_temp^.select then begin
    full_name := mod_temp^.name;
    if not mod_temp^.lit then begin
      reset(mod_file,'.REL ' || mod_temp^.name); (* try default file extension *)
      defn := 0;
      while (eof(mod_file)) and (defn < n_defaults) do begin
	close(mod_file);
	defn := defn+1;
	full_name := mod_temp^.name || default_ppns[defn];
	reset(mod_file,'.REL ' || full_name);
      end;
      close(mod_file);
    end;
    write(select_file,full_name);
    for i := 1 to max_sw do
      if mod_temp^.opt[i] <> '' then
        write(select_file,'/' || mod_temp^.opt[i]);
      writeln(select_file);
  end;
  mod_temp := mod_temp^.link;
end (* while *);
writeln(tty,jobnum || 'SEL.TMP created');
break;
end (* select_choices *);
$PAGE include_lists - declarations
procedure include_lists;
  
type
  augmented_passes = minimum (passes)..maximum (passes) + 1;
  
  linkage = record
    thefile: ^file_record;
    next: ^linkage
  end;
  
  file_record = record
    fname: file_name;
    creation: dtime_int;
    pass_set: set of augmented_passes;
    num_passes,
    first_pass: passes;
    usage: str;
    exploded: boolean;
    owner: ^file_record;
    num_owned: 0..4;
    owns: array [1..4] of ^file_record;
    first_child,
    last_child: ^linkage
  end;
  
  module_list = record
    first_mod,
    last_mod: ^linkage
  end;
  
const
  day_label: array [week_day] of string[4] :=
			  ('Sun ', 'Mon ', 'Tue ', 'Wed ', 'Thu ', 'Fri ', 'Sat ');
  
  pass_codes: set of passes = [minimum (passes)..maximum (passes)];

var
  incl_list: module_list;
  lines_on_page: integer;
  listing: boolean;
$PAGE include_lists - utilities
procedure put_on_list (var list: module_list; file_ptr: ^file_record);
  begin
    if list.first_mod = nil then begin
      new (list.first_mod);
      list.last_mod := list.first_mod
    end
    else begin
      new (list.last_mod^.next);
      list.last_mod := list.last_mod^.next
    end;
    list.last_mod^.thefile := file_ptr;
    list.last_mod^.next := nil
  end;
  
  
  
procedure new_file (var file_ptr: ^file_record; name: file_name; set_of_passes: set of passes);
  begin
    new (file_ptr);
    with file_ptr^ do begin
      fname := name;
      pass_set := set_of_passes;
      exploded := false;
      owner := nil;
      num_owned := 0;
      first_child := nil;
      last_child := nil
    end
  end;
  
  

procedure count_passes (var file_ptr: ^file_record);
  var
    temp_index: passes;
    temp_index_char: str;
    err: dir_errors;
    info: dir_attrs;
  begin
    with file_ptr^ do begin
      dir_attr (err, fname, info);
      creation := info.creation;
      usage := '';
      num_passes := 0;
      for temp_index := minimum (passes) to maximum (passes) do
	if temp_index in pass_set then begin
          putstring(temp_index_char,temp_index: 0);
	  usage := usage || ',' || temp_index_char;
	  num_passes := num_passes + 1;
	  if num_passes = 1 then
	    first_pass := temp_index
	end;
      usage := usage || '>';
      usage [1] := '<';
      if (num_passes > 3) and
	 ((owner = nil) orif (maximum (augmented_passes) in owner^.pass_set)) then
	pass_set := pass_set + [maximum (augmented_passes)]
    end
  end;
  
  
  
procedure find (var name: file_name);
  var
    defn: 0..max_defaults;
  begin
    defn := 0;
    reset (output, name); (* try current account first *)
    while eof (output) and (defn < n_defaults) do begin
      close (output);
      defn := defn + 1;
      reset (output, name || default_ppns [defn])
    end;
    if eof (output) then begin
      writeln (ttyoutput, '?unable to find ', name);
      stop
  ;
    close (output);
    if defn > 0 then  (* had to follow default path? *)
      name := name || default_ppns [defn]
  end;
  
  
  
procedure new_include (new_name: file_name; var file_ptr: ^file_record);
  var
    inc_rec_ptr: ^linkage;
  begin
    inc_rec_ptr := incl_list.first_mod;
    while (inc_rec_ptr <> nil) andif (inc_rec_ptr^.thefile^.fname <> new_name) do
      inc_rec_ptr := inc_rec_ptr^.next;
    if inc_rec_ptr = nil then begin (* new file not on list *)
      new_file (file_ptr, new_name, []);
      put_on_list (incl_list, file_ptr)
    end
    else
      file_ptr := inc_rec_ptr^.thefile  (* already on list *)
  end;
  
  
  
procedure explode (var modptr: ^linkage);
  var
    new_name: file_name;
    line: str;
    i: integer;
    temp_ptr: ^linkage;
  begin
    if not modptr^.thefile^.exploded then begin
      modptr^.thefile^.exploded := true;
      reset (input, modptr^.thefile^.fname);
      while not eof (input) do
	if input^ <> '$' then
	  readln (input)
	else begin
	  line := '';
	  while not eoln (input) and (length (line) < upperbound (line)) do begin
	    line := line || input^;
	    get (input)
	  end;
	  readln (input);
	  line := uppercase (line);
	  if (length (line) >= 8) andif
		((line [2: 7] = 'INCLUDE') or (line [2:6] = 'SYSTEM')) then begin
	    i := 8;
	    if (line[i] <> ' ') and (line[i] <> chr (11b)) then
	      i := i+1;
	    repeat
	      i := i+1;
	    until (line [i] <> ' ') and (line [i] <> chr (11b));
	    if cmd_file_name (line, i, true, new_name) then with modptr^.thefile^ do begin
	      if index (new_name, '.') = 0 then begin
		i := index (new_name, '[');
		if i = 0 then
		  new_name := new_name || '.INC'
		else
		  new_name := substr (new_name, 1, i-1) || '.INC' || substr (new_name, i)
	      end;
	      find (new_name);
	      if first_child = nil then begin
		new (first_child);
		last_child := first_child
	      end
	      else begin
		new (last_child^.next);
		last_child := last_child^.next
	      end;
	      last_child^.next := nil;
	      new_include (new_name, modptr^.thefile^.last_child^.thefile)
	    end
	  end
	end;
      close (input);
    end;

    temp_ptr := modptr^.thefile^.first_child;
    while temp_ptr <> nil do begin
      temp_ptr^.thefile^.pass_set := temp_ptr^.thefile^.pass_set + modptr^.thefile^.pass_set;
      explode (temp_ptr);
      temp_ptr := temp_ptr^.next
    end
  end (* explode *);
  
  
  
function format (fname: file_name; offset: integer; creation: dtime_int): str;
  
  const
    blank: str := '                                        ';
  var
    i: integer;
  begin
    format := fname;
    i := index (format, '[');
    if listing and (i > 0) then
      format := substr (format, 1, i-1) || blank [1:12-i] || substr (format, i);
    format := format || blank [1:(28+offset-length (format))];
    format := ' ' || format || day_label [day_of_week (extr_date (creation))] ||
			       substr (dc_ext (creation), 1, 15)
  end (* format *);
$PAGE print_formatted, print_exploded   in include_lists
procedure print_formatted (list: module_list; pass_index: augmented_passes);
  
  type
    line_rec_ptr = ^line_rec;
    line_rec = record
      next_line: line_rec_ptr;
      line: str
    end;
  var
    list_ptr: ^linkage;
    i: 1..4;
    fileref: ^file_record;
    line_list_head, last_line, last_primary_entry, new_line: ^line_rec;
    temp_str: str;
  
  procedure insert (var last: line_rec_ptr);
    begin
      new_line^.next_line := last^.next_line;
      last^.next_line := new_line;
      last := new_line
    end;
  
  begin
    new (line_list_head);
    line_list_head^.line := ' ';
    last_primary_entry := line_list_head;
    new (line_list_head^.next_line);
    last_line := line_list_head^.next_line;
    last_line^.next_line := nil;
    last_line^.line := ' ';
  
    list_ptr := list.first_mod;
    while list_ptr <> nil do with list_ptr^.thefile^ do begin
      if (pass_index in pass_set) andif ((owner = nil) orif (not (pass_index in owner^.pass_set))) then begin
	new (new_line);
	new_line^.line := format (fname, 3, creation);
	if (pass_index > maximum (passes)) orif
	      (((num_passes = 2) or (num_passes = 3)) and (first_pass = pass_index)) then
	  new_line^.line := new_line^.line || '   ' || usage
	else if num_passes <> 1 then begin
	  new_line^.line [1] := '(';
	  if owner = nil then
	    fileref := list_ptr^.thefile
	  else
	    fileref := owner;
	  if fileref^.num_passes <= 3 then begin
            putstring(temp_str,fileref^.first_pass);
	    new_line^.line := new_line^.line || '  see pass' || temp_str || ')'
          end
	  else
	    new_line^.line := new_line^.line || '  see common)'
	end;
	if new_line^.line [1] = ' ' then begin  (* primary entry for this file *)
	  insert (last_primary_entry);
	  for i := 1 to num_owned do begin
	    new (new_line);
	    new_line^.line := format (owns[i]^.fname, 0, owns[i]^.creation);
	    if listing then
	      new_line^.line := '   ' || new_line^.line;
	    if (num_passes <> 1) or (pass_set <> owns[i]^.pass_set) then
	      new_line^.line := new_line^.line || '   ' || owns[i]^.usage;
	    insert (last_primary_entry);
	  end
	end
	else if listing then begin (* a cross referencing entry *)
	  insert (last_line);
	  for i := 1 to num_owned do
	    if pass_index in owns[i]^.pass_set then begin
	      new (new_line);
	      new_line^.line := '   ' || format (owns[i]^.fname, 0, owns[i]^.creation);
	      if (num_passes = 2) or (num_passes = 3) then begin
                putstring(temp_str,first_pass);
		new_line^.line := new_line^.line || '  see pass' || temp_str
              end
	      else
		new_line^.line := new_line^.line || '  see common';
	      insert (last_line)
	    end
	end
      end;
      list_ptr := list_ptr^.next
    end;
  
    new_line := line_list_head;
    while new_line <> nil do begin
      if listing then begin
	if lines_on_page > 44 then begin
	  page (output);
	  lines_on_page := 0
	end;
	writeln (output, '   ', new_line^.line);
	lines_on_page := lines_on_page + 1
      end
      else if new_line^.line <> ' ' then begin
	new_line^.line := substr (new_line^.line, 2);
        writeln (output, substr (new_line^.line, 1, index (new_line^.line, ' ') - 1))
      end;
      new_line := new_line^.next_line
    end
  end;
  
  
procedure print_exploded (file_ptr: ^file_record; level: integer);
  var
    child: ^linkage;
  begin
    if listing then
      writeln (output, ' ': level*3, file_ptr^.fname)
    else
      writeln (output, file_ptr^.fname);
    child := file_ptr^.first_child;
    while child <> nil do begin
      print_exploded (child^.thefile, level+1);
      child := child^.next
    end
  end;
$PAGE include_lists - body
  
var
  jobstuff: jobrec;
  incl_name,
  time_stamp: str;
  machine_indep,
  wild_on_pass: boolean;
  match_level,
  temp_level: integer;
  pass_union: set of augmented_passes;
  out_name: file_name;
  incl_rec_ptr,
  best_match,
  mod_rec_ptr: ^linkage;
  pass_index: augmented_passes;
  namelist_ptr: name_list;
  file_ptr: ^file_record;
  mod_list: module_list;
  
begin
  
  (* initialze lists *)
  
  mod_list.first_mod := nil; 
  mod_list.last_mod := nil;
  incl_list.first_mod := nil;
  incl_list.last_mod := nil;
  pass_union := [];
  
  (* put all marked files on module list and explode them *)
  
  namelist_ptr := files_list;
  while namelist_ptr <> nil do begin
    new_file (file_ptr, namelist_ptr^.name, namelist_ptr^.codes);
    put_on_list (mod_list, file_ptr);
    with mod_list.last_mod^.thefile^ do begin
      fname := fname || '.PAS';
      find (fname)
    end;
    explode (mod_list.last_mod);
    count_passes (file_ptr); (* fill in some fields for later printing *)
    pass_union := pass_union + file_ptr^.pass_set;
    namelist_ptr := namelist_ptr^.link
  end;
  if mod_list.first_mod = nil then return;
  
  (* process all included files, attempting to match to corresponding modules *)
  
  incl_rec_ptr := incl_list.first_mod;
  while incl_rec_ptr <> nil do begin
    incl_name := incl_rec_ptr^.thefile^.fname;
    incl_name := substr (incl_name, 1, index (incl_name, '.', length (incl_name)));
    machine_indep := incl_name[1:3] = 'PTM';
    wild_on_pass := incl_name[1:3] = 'PAS';
    mod_rec_ptr := mod_list.first_mod;
    best_match := nil;
    match_level := 10;
    while mod_rec_ptr <> nil do with mod_rec_ptr^.thefile^ do begin
     exit if (substr (fname, 1, length (incl_name)) = incl_name) do best_match := mod_rec_ptr;
      if machine_indep andif
	      (substr (fname, 4, length (incl_name)-3) = substr (incl_name, 4)) then begin
	if fname [1: 3] = 'P10' then
	  temp_level := 0
	else if fname [1: 3] = 'Q10' then
	  temp_level := 1
	else
	  temp_level := 2;
	if temp_level < match_level then begin
	  best_match := mod_rec_ptr;
	  match_level := temp_level
	end
      end;
      if wild_on_pass andif ((fname[1: 2] = 'PA') and (fname [3] in ['0'..'9'])) andif
	      (substr (fname, 4, length (incl_name)-3) = substr (incl_name, 4)) then
	if (ord (fname [3]) - ord ('0')) < match_level then begin
	  match_level := ord (fname [3]) - ord ('0');
	  best_match := mod_rec_ptr
	end;
      mod_rec_ptr := mod_rec_ptr^.next
    end;
    if best_match <> nil then begin
      assert (incl_rec_ptr^.thefile^.owner = nil);
      incl_rec_ptr^.thefile^.owner := best_match^.thefile;
      assert (best_match^.thefile^.num_owned < 4);
      best_match^.thefile^.num_owned := best_match^.thefile^.num_owned + 1;
      best_match^.thefile^.owns [best_match^.thefile^.num_owned] := incl_rec_ptr^.thefile
    end;
    count_passes (incl_rec_ptr^.thefile);  (* fill in some fields for later printing *)
    incl_rec_ptr := incl_rec_ptr^.next
  end;
  
  (* query user *)
  
  write (ttyoutput, 'output filename-- ');
  break (ttyoutput);
  readln (tty);
  out_name := '';
  while not eoln (tty) do begin
    out_name := out_name || tty^;
    get (tty)
  end;
  rewrite (output, out_name);
  write (ttyoutput, 'form of output ("command file" or "listing")-- ');
  break (ttyoutput);
  readln (tty);
  listing := uppercase (tty^) = 'L';
  
  (* produce desired listing *)
  
  jobinfo (jobstuff);
  time_stamp := 'From ' || jobstuff.lsegppn || '   ' || substr (dc_ext (daytime), 1, 15);
  if mod_list.first_mod = mod_list.last_mod then begin
    if listing then
      writeln (output, time_stamp: 80);
    print_exploded (mod_list.first_mod^.thefile, 0)
  end
  else
    for pass_index := minimum (passes) to maximum (augmented_passes) do
      if pass_index in pass_union then begin
	if listing then begin
	  page (output);
	  if pass_index <= maximum (passes) then
	    writeln (output, 'Pass', pass_index, ' modules', time_stamp: 66)
	  else
	    writeln (output, 'Modules common to many passes', time_stamp: 51);
	  lines_on_page := 1
	end;
	print_formatted (mod_list, pass_index);
	if listing then begin
	  if lines_on_page > 43 then begin
	    page (output);
	    lines_on_page := 0;
	  end;
	  writeln (output);
	  if pass_index <= maximum (passes) then
	    writeln (output, 'Pass', pass_index,
			     ' included files not corresponding to a module in Pass', pass_index)
	  else
	    writeln (output, 'Common included files not corresponding to any module');
	  lines_on_page := lines_on_page + 2
	end;
	print_formatted (incl_list, pass_index)
      end;
  close
end;
$PAGE include_choices
(*
  PROCEDURE INCLUDE_CHOICES (interface to include lists)
  Calling sequence: 
   include_choices;
                                                                     *)

procedure include_choices;
var
    i: integer;
    mod_temp: ^mod_list;
    name_ptr,name_tail: name_list;

begin
mod_temp := mod_ptr;
name_tail := nil;
while mod_temp <> mod_tail do begin
  if (mod_temp^.include) and (not mod_temp^.lit) then begin
    new(name_ptr);
    name_ptr^.name := mod_temp^.name;
    name_ptr^.codes := [];
    name_ptr^.link := nil;
    for i := 0 to e-1 do
      if network[mod_temp^.number,i] then
        name_ptr^.codes := name_ptr^.codes + [i];
    if name_tail = nil then
      files_list := name_ptr
    else
      name_tail^.link := name_ptr;
    name_tail := name_ptr;
  end;
  mod_temp := mod_temp^.link;
end;
include_lists;
end (* include_choices *);
$PAGE compare_switches
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

function compare_switches(switch: array [1..max_sw] of str;
                          new_sw: array [1..max_sw] of str):
                         array [1..max_sw] of str;
var
    update_array: array [1..max_sw] of str;
    i,j: integer;
    found: boolean;
    current_switch: str;

begin
i := 1;
update_array := switch;
while new_sw[i] <> '' do begin
  if substr(new_sw[i],1,2) = 'NO' then
    current_switch := substr(new_sw[i],3)
  else
    current_switch := new_sw[i];
  j := 1;
  while (update_array [j] <> '') and (not found) do
    if (update_array [j] = current_switch) or 
        (substr(update_array [j],3) = current_switch) then begin
      update_array [j] := new_sw[i];
      found := true
    end
    else
      j := j+1;
  (* end while *)
  if not found then
    update_array [j] := new_sw[i];
  i := i+1
end (* while *) ;
compare_switches := update_array
end (* compare_switches *) ;
$PAGE valid_switch
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
   ok is boolean -- true if switch is valid; false otherwise
                                                                           *)

function valid_switch(switch_str: str;var full_sw:str;sw_type:str):boolean;
const
    valid_comp_switches:array [1..max_sw] of str :=
      ('BLOCKS','DEBUG','OPTIMIZE','QUICK','SOURCE','XREF','');
    actual_comp_switches:array [1..max_sw] of str :=
      ('QBLOCKS','DEBUG','OPTIMIZE','QUICK','SOURCE','GLOBAL','');
    valid_link_switches:array [1..max_sw] of str :=
      ('MAP','DEBUG','SYMSEG: HIGH','','','','');
    actual_link_switches:array [1..max_sw] of str :=
      ('MAP','SYS: LNKDDT','SYMSEG:HIGH','','','','');
var
    switch: str;
    valid_switch_names,actual_switch: array [1..max_sw] of str;
    found,next,i: integer;

begin
if sw_type = 'COMP' then begin
  valid_switch_names := valid_comp_switches;
  actual_switch := actual_comp_switches
end
else begin
  valid_switch_names := valid_link_switches;
  actual_switch := actual_link_switches
end;
next := 1;
valid_switch := false;
full_sw := '';
if (cmd_eol(switch_str,next)) then
  return;
switch := substr(switch_str,next);
if switch[1] = '-' then begin
  full_sw := 'NO';
  switch := substr(switch,2)
end
else if (length(switch) >= 2) andif (substr(switch,1,2) = 'NO') then begin
  full_sw := 'NO';
  switch := substr(switch,3)
end;
if (full_sw = 'NO') and (sw_type = 'LINK') then
  err('LINK switches may not be negated');
if cmd_eol(switch,next) then 
  return;
next := search(switch,[' '],length(switch)+1);
switch := substr(switch,1,next-1);
found := 0;
for i := 1 to max_sw do begin
  if switch = substr(valid_switch_names[i],1,length(switch)) then
    found := i;
  exit if found > 0
end;
if found = 0 then
  return
else begin
  full_sw := full_sw || actual_switch[found];
  valid_switch := true
end;
end (*valid switch *);
$PAGE set_switches
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

function set_switches(switch_str:str;sw_type : str):array [1..max_sw] of str;
var
    i,next: integer;
    list,full_sw: str;

begin
for i := 1 to max_sw do
  set_switches[i] := '';
if switch_str <> '' then begin
  i := 0;
  list := substr(switch_str,2); (* get past leading "/" *)
  while list <> '' do begin
    i := i+1;
    next := search(list,['/'],length(list)+1);
    if valid_switch(substr(list,1,next-1),full_sw,sw_type) then
      set_switches[i] := full_sw
    else
      err('invalid switch:  "' || substr(list,1,next-1) || '"');
    list := substr(list,min(next+1,length(list)+1))
  end (*while *);
  if i=0 then
    err('expecting switch specification');
end (* if *) ;
end (* set_switches *);
$PAGE link_all_compiled
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

procedure link_all_compiled(link_switches: array [1..max_sw] of str);
var
    prog_temp: ^prog_list;
    mod_temp: ^mod_list;
    i: passes;
    found,any: boolean;
 
begin
any := false;
mod_temp := mod_ptr;
while mod_temp <> mod_tail do begin
  if mod_temp^.comp then begin
    for i := minimum (passes) to maximum (passes) do begin
      if (network[mod_temp^.number,i]) then begin
        prog_temp := prog_ptr;
        found := false;
        while not found do
          if prog_temp^.number = i then
           found := true
          else
            prog_temp := prog_temp^.link;
        (* end while *)
        prog_temp^.load := true;
        any := true;
        prog_temp^.opt := link_switches
      end
    end
  end;
  mod_temp := mod_temp^.link
end;
if not any then
  err('no modules compiled; therefore none to link');
end (* link_all_compiled *);
$PAGE link_range
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

procedure link_range(lower: integer; upper:integer; 
                     link_switches: array [1..max_sw] of str);
var
    prog_temp: ^prog_list;

begin
prog_temp := prog_ptr;
while prog_temp <> prog_tail do begin
  if (prog_temp^.number >= lower) and (prog_temp^.number <= upper) then begin
    prog_temp^.load := true;
    prog_temp^.opt := link_switches
  end;
  prog_temp := prog_temp^.link
end
end (* link_range *);
$PAGE link_piece
(*
  PROCEDURE LINK_PIECE (parse link command)
  Calling sequence: 
   link_piece(cmd);
  Where: 
   cmd is str body of user specified link command (everything after "L")
                                                                        *)

procedure link_piece(cmdstr: str);
var
    link_str: str;
    cmd: str;
    i, next: integer;
    lower,upper: integer;
    link_switches: array [1..max_sw] of str;

begin
next := search(cmdstr,['/'],length(cmdstr)+1); (* pull off link switches *)
link_switches := set_switches(substr(cmdstr,next),'LINK');
cmd := substr(cmdstr,1,next-1);
cmd := substr(cmd,verify(cmd,[' '],length(cmd)+1));
if cmd = '' then
  err('null link command');
if cmd[1] = '*' then (* link all *)
  link_range(minimum (passes),maximum (passes),link_switches)
else if (uppercase(cmd[1]) = 'C') and (substr(cmd,2) = ' ') then
  link_all_compiled(link_switches)
else begin
  get_prog_range(cmd,lower,upper);
  link_range(lower,upper,link_switches)
end;
link_on := true
end (* link piece *);
$PAGE pick_mods
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
   except is boolean -- if true then do not compile modules specified
                                                                     *)

procedure pick_mods(cmdstr: str;switch:array [1..max_sw] of str;except:boolean);
var
    cmd,mod_and_new_sw,md,new_sw: string;
    i,index,next,mod_num,lower,upper: integer;
    temp_mod: ^mod_list;
    delim: char;
    new_sw_array: array [1..max_sw] of str;

begin
cmd := cmdstr;
temp_mod := mod_ptr;
case cmd[1] of 
  '*':  (* compile all modules in system *)
    while temp_mod <> mod_tail do begin
      if (include_now) and
       (not network[temp_mod^.number,e]) and
       (not network[temp_mod^.number,s]) then
        temp_mod^.include := true 
      else if select_now then
        temp_mod^.select := true
      else if (not network[temp_mod^.number,e]) and (not network[temp_mod^.number,s]) then begin
	temp_mod^.comp := not except ;
	temp_mod^.opt := switch
      end;
      temp_mod := temp_mod^.link;
  end;
  '(': (* compile individual modules *)
    begin
    cmd := substr(cmd,2);
    delim := ',';
    while delim <> ')' do begin (* balanced parans already checked for *)
      next := search(cmd,[',',')']);
      delim := cmd[next];
      new_sw := '';
      mod_and_new_sw := substr(cmd,1,next-1);
      index := search(mod_and_new_sw,['/']); (* separate modifier and switches *)
      if index > 0 then begin
        md := substr(mod_and_new_sw,1,index-1);
        new_sw_array := set_switches(substr(mod_and_new_sw,index),'COMP')
      end
      else
        md := mod_and_new_sw;
      new_sw_array := compare_switches(switch,new_sw_array);
      temp_mod := mod_ptr;
      while (temp_mod <> mod_tail) and (temp_mod^.name <> uppercase(md)) do
        temp_mod := temp_mod^.link;
      if (temp_mod = mod_tail) then
        err('undefined module name:  "' || md || '"')
      else if (include_now) and
       (not network[temp_mod^.number,e]) and
       (not network[temp_mod^.number,s]) then
	temp_mod^.include := true 
      else if select_now then
	temp_mod^.select := true
      else if (not network[temp_mod^.number,e]) and
               (not network[temp_mod^.number,s]) then
	  temp_mod^.comp := not except ;
      temp_mod^.opt := new_sw_array;
      cmd := substr(cmd,next+1)
    end (* while *) ;
  end;
  others:  (* check for range of program values *)
    begin
    get_prog_range(cmd,lower,upper);
    while temp_mod <> mod_tail do begin
      for i:= lower to upper do begin
        if (network[temp_mod^.number,i]) then begin 
          if (include_now) and
           (not network[temp_mod^.number,e]) and
           (not network[temp_mod^.number,s]) then
	    temp_mod^.include := true 
	  else if select_now then
	    temp_mod^.select := true
          else if (not network[temp_mod^.number,e]) and
           (not network[temp_mod^.number,s]) then
	    temp_mod^.comp := not except ;
          temp_mod^.opt := switch
        end
      end;
      temp_mod := temp_mod^.link
      end
    end;
  end
end;
$PAGE examine_piece
(*
  PROCEDURE EXAMINE_PIECE (parse user specified compile list)
  Calling sequence: 
   examine_piece(cmd);
  Where: 
   cmd is str compile command (everything after the "C")
  Note: 
   This routine is also called to parse the body of select or include
    commands, and sets appropriate flags in each case.
                                                                     *)

procedure examine_piece(cmdstr: str);
var
  cmd,switch_buf: str;
  comp_switches: array [1..max_sw] of str;
  index,next: integer;
  except: boolean;

begin
except := false;
cmd := substr(cmdstr,verify(cmdstr,[' '],length(cmdstr)+1));
switch_buf := '';
comp_switches := set_switches(switch_buf,'COMP');
if cmd[1] = '#' then begin (* "#" is a don't compile switch *)
  except := true;
  cmd := substr(cmd,2)
end;
if cmd = '' then
  err('invalid command:  "' || cmdstr || '"');
if cmd[1] = '(' then begin (* find global switches first for modules in list *)
  index := search(cmd,[')']);
  switch_buf := substr(cmd,index+1);
  if switch_buf <> '' then
    switch_buf := substr(switch_buf,verify(switch_buf,[' ']));
  if (switch_buf <> '') andif (switch_buf[1] <> '/') then
    err('extraneous input -- expecting switches: "' || cmdstr || '"');
  cmd := substr(cmd,1,index)
end
else begin (* pick off switches from other types of lists *)
  index := search(cmd,['/'],length(cmd)+1);
  switch_buf := substr(cmd,index);
  cmd := substr(cmd,1,index-1)
end;
comp_switches := set_switches(switch_buf,'COMP') ;
pick_mods(cmd,comp_switches,except);
if (not select_now) and (not include_now) then
  compile_on := true;
end (*examine_piece*) ;
$PAGE separate_command
(*
  PROCEDURE SEPARATE_COMMAND (gather tokens between commas)
  Calling sequence: 
   separate_command(cmd,operate);
  Where: 
   cmd is str expression with tokens separated by commas
   operate is a procedure used to process each token
                                                                  *)

procedure separate_command(cmdstr: str; operate:procedure (str));
var
    cmd: str;
    rest: str;
    next: integer;

begin
cmd := substr(cmdstr,verify(cmdstr,[' '],length(cmdstr)+1));
while cmd <> '' do begin
  if cmd[1] = '(' then begin
    next := search(cmd,[')']);
    if next = 0 then
      err('unbalanced  paranthesis:  ' || cmd)
    else begin
      rest := substr(cmd,next+1);
      next := next + search(rest,[','],length(rest)+1)
    end;
  end
  else
    next := search(cmd,[','],length(cmd)+1);
  operate(substr(cmd,1,next-1));
  cmd := substr(cmd,min(length(cmd)+1,next+1));
end
end (* separate_command *);
$PAGE queue_f
(*
  PROCEDURE QUEUE_F (process "QF" requests)
  Calling sequence: 
   queue_f;
                                                                    *)

procedure queue_f;
var 
    cmd_file: text;

begin
if compile_on then begin
  rewrite(cmd_file,jobnum || 'BLD.CMD');
  init_comp_file(cmd_file);
  fill_comp_file(cmd_file);
  close(cmd_file);
  if link_on then
    err('"QF" not to be used for both compilation and linking');
end
else if link_on then begin
  link_choices('BLD.CMD',false);
end
else
  err('"QF" inappropriate -- nothing to compile or link');
writeln(tty,jobnum || 'BLD.CMD created');
break;
end (* queue_f *);
$PAGE queue_d_or_i;
(*
  PROCEDURE QUEUE_D_OR_I (process "QD" or "QI" requests)
  Calling sequence: 
   queue_d_or_i;
  NOte: 
   This routine creates file xxxBLD.CTL for batch processing
                                                                    *)

procedure queue_d_or_i;
var
    ctl_file: text;
    prog_temp: ^prog_list;
    mod_temp: ^mod_list;
    i: integer;
    first: boolean;

begin
rewrite(ctl_file,jobnum || 'BLD.CTL');
writeln(ctl_file,':' || jobnum || 'BLD.LOG');
writeln(ctl_file,':TIME 300');
if compile_on then begin
  writeln(ctl_file,':DEFINE A="?"');
  writeln(ctl_file,':FIND A');
  writeln(ctl_file,'R ' || compiler);
  init_comp_file(ctl_file);
  fill_comp_file(ctl_file);
  writeln(ctl_file);
  writeln(ctl_file,':IF A THEN QUIT');
end;
if link_on then begin
  writeln(ctl_file,'R ' || loader);
  prog_temp := prog_ptr;
  while prog_temp <> prog_tail do begin
    if prog_temp^.load then begin
      first := true;
      mod_temp := mod_ptr;
      while mod_temp <> mod_tail do begin
        if network[mod_temp^.number,prog_temp^.number] then begin
          if first then begin
	    write(ctl_file,prog_temp^.name || '[,]/SSAVE');
	    for i := 1 to max_sw do
	      if (prog_temp^.opt[i]='MAP') or (prog_temp^.opt[i]='NOMAP') then
		write(ctl_file,'/' || prog_temp^.opt[i]);
	    writeln(ctl_file,'=');
	    add_link_opt(ctl_file,prog_temp);
            first := false;
          end;
          get_full_name(ctl_file,mod_temp);
        end;
        mod_temp := mod_temp^.link;
      end;
      writeln(ctl_file,'/GO');
    end;
    prog_temp := prog_temp^.link;
  end;
end;
close(ctl_file);
writeln(tty,jobnum || 'BLD.CTL created');
break;
end (* queue_d_or_i *);
$PAGE queue_command
(*
  PROCEDURE QUEUE_COMMAND (process "Q" requests)
  Calling sequence: 
   queue_command(cmd);
  Where: 
   cmd is str user specified queue command (everything after the "Q")
    "F" -- set up control file for a single run
    "I" -- create batch file and submit for immediate execution
    "M" -- create MIC file (COM on Tymshare) and run immediately
    "D" -- create batch file and submit to batch queue with /AFTER switch
    "N" -- execute now; run compiler/loader immediately
  Note: 
   "QN" is the default mode if no queue commands are given
                                                                           *)

procedure queue_command(cmd_str: str);
var
    cmd: str;
    prog_temp: ^prog_list;
    mod_temp: ^mod_list;

begin
cmd := cmd_str;
if substr(cmd,2) <> '' then
  err('extraneous queue command:  ' || cmd_str);
if select_on then
  select_choices;
if include_on then
  include_choices;
case cmd[1] of
  'N': begin
    if compile_on then
      compile_choices
    else if link_on then
      link_choices('LNK.TMP',true);
    if compile_on then
      run(compiler,true);
    if link_on then
      run(loader,true);
    end;
  'F': begin
      queue_f;
    end;
  'M': begin 
    err('QM not currently supported');
    end;
  'D': begin
    queue_d_or_i;
    end;
  'I': begin
    queue_d_or_i;
    end;
  others: begin
    err('queue command "' || cmd_str || '" is invalid');
    end;
end;
prog_temp := prog_ptr;
while prog_temp <> prog_tail do begin
  prog_temp^.load := false;
  prog_temp := prog_temp^.link;
end;
mod_temp := mod_ptr;
while mod_temp <> mod_tail do begin
  mod_temp^.comp := false;
  mod_temp^.include := false;
  mod_temp^.select := false;
  mod_temp := mod_temp^.link;
end;
queued := true
end (* queue_command *);
$PAGE include_piece
(*
  PROCEDURE INCLUDE_PIECE (parse include command)
  Calling sequence: 
   include_piece(cmd);
  Where: 
   cmd is str user specified include request (everything after the "I")
                                                                       *)

procedure include_piece(cmdstr: str);
var
    cmd: str;

begin
cmd := cmdstr;
include_on := true;
include_now := true;
examine_piece(cmd);
end (* include_piece *);
$PAGE select_piece
(*
  PROCEDURE SELECT_PIECE (parse select command)
  Calling sequence: 
   select_piece(cmd);
  Where: 
   cmd is str user specified select request (everything after the "S")
                                                                      *)

procedure select_piece(cmdstr: str);
var
    cmd: str;

begin
cmd := cmdstr;
select_on := true;
select_now := true;
examine_piece(cmd);
end (* select_piece *);
$PAGE process_command
(*
  PROCEDURE PROCESS_COMMAND (examine user specified command)
  Calling sequence: 
   process_command(cmd);
  Where: 
   cmd is str user specified command -- the first non-blank character
    is found and control transferred to the appropriate procedure
                                                                    *)

procedure process_command(cmdstr: str);
var
    cmd: str;
    next: integer;
    done: boolean;

begin
next := 1;
cmd := uppercase(cmdstr);
done := cmd_eol(cmd,next);
if not done then begin
  cmd := substr(cmd,next);
  select_now := false;
  include_now := false;
  queued := false;
  case cmd[1] of
    'C': separate_command(substr(cmd,2),examine_piece);
    'L': separate_command(substr(cmd,2),link_piece);
    'S': separate_command(substr(cmd,2),select_piece);
    'I': separate_command(substr(cmd,2),include_piece);
    'Q': queue_command(substr(cmd,2));
    others:err('invalid command:  "' || cmd[1] || '"');
  end
end
end (* process_command *) ;
$PAGE read_command
(*
  PROCEDURE READ_COMMAND (prompt user for commands)
  Calling sequence: 
   read_command;
  Note: 
   Users are prompted with "BUILD:  ".  User commands are separated
    with semicolons.  Any line ending with "&" signals continuation.
                                                                   *)

procedure read_command;
var
    cmdline: str;
    i,next: integer;
    done: boolean;
    delim: char;

begin
done := false;
while not done do begin
  done := true;
  write(tty,'BUILD:  ');
  break ;
  readln(tty);
  read(tty,cmdline);
  while cmdline <> '' do begin
    next := search(cmdline,[';'],length(cmdline)+1);
    process_command(substr(cmdline,1,next-1));
    if (next <= length(cmdline)) andif (substr(cmdline,next+1) = '') then
        done := false;
    cmdline := substr(cmdline,min(next+1,length(cmdline)+1))
  end
end
end (*read_command *);
$PAGE do_program
(*
  PROCEDURE DO_PROGRAM (build global data structure from build file)
  Calling sequence: 
   do_program(line,index,prog_num)
  Where: 
   line is str program line from build file
   index is integer pointer into line
   value is integer program number (previously found)
                                                                       *)

procedure do_program(var work_line: str; var next:integer; 
                     value: integer);
var
    prog_temp: ^prog_list;
 
begin
cmd_skip_blanks(work_line,next);
if next > length(work_line) then
  err('null program name');
work_line := substr(work_line,next);
next := search(work_line,[' ',';'],length(work_line)+1);
new(prog_temp);
prog_temp^.name := uppercase(substr(work_line,1,next-1));
prog_temp^.number := value;
if prog_ptr = prog_tail then begin
  prog_ptr := prog_temp;
  prog_temp^.link := prog_tail;
  prog_tail^.link := prog_temp;
end
else begin
  prog_temp^.link := prog_tail;
  prog_tail^.link^.link := prog_temp;
  prog_tail^.link := prog_temp;
end;
work_line := substr(work_line,next)
end (* do_program *) ;

$PAGE add_network
(*
  PROCEDURE ADD_NETWORK (parse line from build file)
  Calling sequence: 
   add_network(line);
  Where: 
   line is str line from build file. Line is examined to determine if
    it is a program line or module line, and control is transferred
    to the appropriate procedure.
                                                                     *)

procedure add_network(line: str);
var
    work_line: str;
    next, value: integer;
    prog: boolean;

begin
work_line := line;
next := 1;
prog := cmd_number(work_line,next,false,value);
work_line := substr(work_line,next);
next := 1;
if prog then (* number found at beginning of line in build file *)
  do_program(work_line,next,value)
else
  do_module(work_line,next);
if work_line <> '' then begin
  cmd_skip_blanks(work_line,next);
  if (next <= length(work_line)) andif (work_line[next] <> ';') then (* check comment *)
    err('extraneous characters in build file line:  "' || line || '"')
end
end (* add_network *) ;
$PAGE read_name
(*
  PROCEDURE READ_NAME (find compiler/loader name in build_file)
  Calling sequence: 
   read_name(name);
  Return: 
   name is str compiler/loader name encountered next in build file
                                                                  *)

procedure read_name(var name: str);
begin
name := '';
while not eoln(build_file) andif not (build_file^ in ['/',';']) do begin
  if build_file^ <> ' ' then
    name := name || uppercase(build_file^);
  get(build_file);
end;
if name = '' then
  err('missing name in ' || bld_file_name);
end (* read_name *);
$PAGE process_build_file
(*  PROCESS_BUILD_FILE reads the build file, creating the
    appropriate internal data structures.  *)

procedure process_build_file;

var line : string [256];
    i : integer;

begin
  line := '';
  n_defaults := 0;
  loop
    reset (build_file, bld_file_name || line);
    if iostatus <> io_ok then
      err ('Cannot read ' || bld_file_name || line);
  exit if build_file^ <> '@';
    get (build_file);
    readln (build_file, line);
    n_defaults := n_defaults + 1;
    default_ppns[n_defaults] := line;
  end;

  read_name (compiler);
  readln (build_file);

  read_name (loader);
  readln (build_file);

  readln (build_file, line);
  i := 1;
  while (n_defaults <> max_defaults) andif
	not cmd_eol (line, i) andif
	cmd_file_name (line, i, false, default_ppns[n_defaults+1]) do
    n_defaults := n_defaults + 1;
end (* process_build_file *);
$PAGE main
var
    i: modules;
    j: passes;
    cmdline: str;
    done: boolean;
    line: str;
 
begin
open (tty);
rewrite (tty);
writeln(tty,'** Build  Version  2.0 **');
new(prog_tail);
new(mod_tail);
mod_tail^.number := 0;
prog_ptr := prog_tail;
mod_ptr := mod_tail;
compile_on := false;
link_on := false;
include_on := false;
select_on := false;
for i:= minimum (modules) to maximum (modules) do
  for j:= minimum (passes) to maximum (passes) do
    network[i,j] := false;

process_build_file;
while not eof(build_file) do begin
  readln(build_file,line);
  add_network(line)
end;
(* now set up terminal for I/O *)
read_command;
if not queued then
  queue_command('N');
end.
 @