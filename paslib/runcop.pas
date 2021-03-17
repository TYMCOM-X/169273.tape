$TITLE runcop - Tymshare hack to copy files to the printer "queue"
  
program runcop
    options special (word);
  
const
  listings_account: string [30] := '[31024,314532]';
  
var
  i:			integer;
  opt:			string [3];
  negative:		boolean;
  
  list_filename,
  label_filename,
  source_filename,
  initials:		string [30];
  
  line,
  source_command,
  label_line:		string [200];
  sourcefile,
  listfile:		text;
  
  binary_sourcefile,
  binary_listfile:	file of machine_word;
  chosen_options,
  default_options:	set of (header, macro, autoff);
  
$INCLUDE (pasdev2)dtime.typ
$INCLUDE (pasdev2)dtime.inc
$INCLUDE (pasdev2)pasdir.typ
$INCLUDE (pasdev2)pasdir.inc
$PAGE character representations

type rep_array = array [' '..'_'] of packed array [1..35] of char;

const rep: rep_array =
     (  '                                   ',	(*   *) 
	' $$   $$   $$   $$        $$   $$  ',	(* ! *) 
	' $ $  $ $                          ',	(* " *) 
	'      $ $ $$$$$ $ $ $$$$$ $ $      ',	(* # *) 
	'  $   $$$ $ $   $$$   $ $ $$$   $  ',	(* $ *) 
	'$$   $$  $   $   $   $   $  $$   $$',	(* percent sign *) 
	' $   $ $  $ $   $  $$ $ $$  $  $$ $',	(* & *) 
	'   $   $                           ',	(* ' *) 
	'    $   $   $    $    $     $     $',	(* ( *) 
	'$     $     $    $    $   $   $    ',	(* ) *) 
	'     $ $ $ $$$ $$$$$ $$$ $ $ $     ',	(* * *) 
	'       $    $  $$$$$  $    $       ',	(* + *) 
	'                $$   $$    $    $  ',	(* , *) 
	'               $$$$$               ',	(* - *) 
	'                          $$   $$  ',	(* . *) 
	'         $   $   $   $   $         ',	(* / *) 
	'  $$  $  $ $  $ $  $ $  $ $  $  $$ ',	(* 0 *) 
	'  $   $$    $    $    $    $   $$$ ',	(* 1 *) 
	' $$$ $   $    $ $$$ $    $    $$$$$',	(* 2 *) 
	' $$$ $   $    $  $$     $$   $ $$$ ',	(* 3 *) 
	'   $   $$  $ $ $  $ $$$$$   $    $ ',	(* 4 *) 
	'$$$$$$    $$$$     $    $$   $ $$$ ',	(* 5 *) 
	'  $$  $   $    $ $$ $$  $$   $ $$$ ',	(* 6 *) 
	'$$$$$    $   $   $   $   $    $    ',	(* 7 *) 
	' $$$ $   $$   $ $$$ $   $$   $ $$$ ',	(* 8 *) 
	' $$$ $   $$  $$ $$ $    $   $  $$  ',	(* 9 *) 
	'      $$   $$        $$   $$       ',	(* : *) 
	' $$   $$        $$   $$    $   $   ',	(* ; *) 
	'    $   $   $   $     $     $     $',	(* < *) 
	'          $$$$$     $$$$$          ',	(* = *) 
	'$     $     $     $   $   $   $    ',	(* > *) 
	' $$$ $   $   $   $    $         $  ',	(* ? *) 
	' $$$ $   $    $ $$ $$ $ $$ $$  $$  ',	(*  *) 
	' $$$ $   $$   $$$$$$$   $$   $$   $',	(* a *) 
	'$$$$  $  $ $  $ $$$  $  $ $  $$$$$ ',	(* b *) 
	' $$$ $   $$    $    $    $   $ $$$ ',	(* c *) 
	'$$$$  $  $ $  $ $  $ $  $ $  $$$$$ ',	(* d *) 
	'$$$$$$    $    $$$  $    $    $$$$$',	(* e *) 
	'$$$$$$    $    $$$  $    $    $    ',	(* f *) 
	' $$$$$    $    $ $$$$   $$   $ $$$ ',	(* g *) 
	'$   $$   $$   $$$$$$$   $$   $$   $',	(* h *) 
	' $$$   $    $    $    $    $   $$$ ',	(* i *) 
	'    $    $    $    $    $$   $ $$$ ',	(* j *) 
	'$   $$  $ $ $  $$   $ $  $  $ $   $',	(* k *) 
	'$    $    $    $    $    $    $$$$$',	(* l *) 
	'$   $$$ $$$ $ $$ $ $$   $$   $$   $',	(* m *) 
	'$   $$$  $$ $ $$  $$$   $$   $$   $',	(* n *) 
	' $$$ $   $$   $$   $$   $$   $ $$$ ',	(* o *) 
	'$$$$ $   $$   $$$$$ $    $    $    ',	(* p *) 
	' $$$ $   $$   $$   $$ $ $$  $  $$ $',	(* q *) 
	'$$$$ $   $$   $$$$$ $ $  $  $ $   $',	(* r *) 
	' $$$ $   $$     $$$     $$   $ $$$ ',	(* s *) 
	'$$$$$  $    $    $    $    $    $  ',	(* t *) 
	'$   $$   $$   $$   $$   $$   $ $$$ ',	(* u *) 
	'$   $$   $$   $ $ $  $ $   $    $  ',	(* v *) 
	'$   $$   $$   $$ $ $$ $ $$ $ $ $ $ ',	(* w *) 
	'$   $$   $ $ $   $   $ $ $   $$   $',	(* x *) 
	'$   $$   $ $ $   $    $    $    $  ',	(* y *) 
	'$$$$$    $   $   $   $   $    $$$$$',	(* z *) 
	'  $$$  $    $    $    $    $    $$$',	(* [ *) 
	'     $     $     $     $     $     ',	(* back slash *) 
	'$$$    $    $    $    $    $  $$$  ',	(* ] *) 
	'  $   $ $ $   $                    ',	(* ^ *) 
	'                              $$$$$'  );   (* _ *) 
$PAGE print_row -- put out one row of characters
  
procedure print_row (name_string: file_name; creation_date_time: string[22]);

  var
    ind,
    hfill,
    len,
    i:		integer;
    r:		file_name;
    name_and_creation: string[80];

  begin
    r := name_string;  (* make copy I can change *)
    i := search (r, [':']);  (* look for device: *)
    if i > 0 then
      r := substr (r, i+1);  (* remove the device specification *)
    len := length(r);
    i := search (r, ['[']);
    if i > 0 then
      len := i - 1;		(* don't try to print ppn *)
    if len = 0 then return;
    hfill := (108 - (5 + 3)*len + 3) div 2;
    ind := 0;
    while ind <> 35 do begin
      write (listfile,' ':hfill);
      for i := 1 to len do begin
	write (listfile, substr (rep[r[i]], ind+1,5));
	if i <> len then write (listfile,' ':3)
      end;
      writeln (listfile);
      ind := ind + 5
    end;
    writeln (listfile);
    writeln (listfile);
    name_and_creation := name_string || '    ' || creation_date_time;
    writeln (listfile, ' ':(108 - length (name_and_creation)) div 2, name_and_creation)
  end;
$PAGE utilities for printing header and banner pages
  
procedure skip_lines (start_line, end_line: integer);
  
  var j: integer;
  
  begin
    for j := start_line to end_line do
      writeln (listfile)
  end;
  
  
procedure star_lines (start_line, end_line: integer);
  
  var j: integer;
  
  begin
    for j := start_line to end_line do
      writeln (listfile, '#$#$#$#$#$#$#$#$#$#$#$ RUNCOP Version 1.1 #$#$#$#$#$#$',
                         '#$#$#$#$#$#$ RUNCOP Version 1.1 #$#$#$#$#$#$#$#$#$#$#$')
  end;
$PAGE banner_pages
  
procedure banner_pages (name_string: file_name);
  
  const
    day_label: array [week_day] of string[4] = ('Sun ', 'Mon ', 'Tue ', 'Wed ', 'Thu ', 'Fri ', 'Sat ');
  
  var
    err: dir_errors;
    info: dir_attrs;
    creation_date_time: string[22];
  
  begin
    dir_attr (err, name_string, info);
    creation_date_time := day_label [day_of_week (extr_date (info.creation))]
			       || dc_ext (info.creation)[1:15];
  
    rewrite (listfile, list_filename, [preserve]);
    star_lines (1, 3);
    skip_lines (4, 12);
    print_row (name_string, creation_date_time);		(* ten lines *)
    skip_lines (23, 44);
    star_lines (45, 52);
    skip_lines (5, 12);
    print_row (name_string, creation_date_time);		(* ten lines *)
    skip_lines (23, 47);
    star_lines (48, 51);
    if not (macro in chosen_options) then
      page (listfile);
    close (listfile)
  end;
$PAGE mainline
  
begin
  
  rewrite (ttyoutput, 'tty:');
  open (tty, 'tty:');
  
  writeln (ttyoutput);
  write (ttyoutput, 'enter your initials-- ');
  break (ttyoutput);
  readln (tty);
  read (tty, initials);
  initials := substr (initials, 1, min (length (initials), 3));
  
  list_filename := initials || '.LST' || listings_account;
  label_filename := 'LABEL.' || initials || listings_account;
  
  reset (listfile, list_filename);
  if eof (listfile) then begin		(* file doesn't already exist *)
    reset (input, label_filename);
    if eof (input) then begin
      writeln (ttyoutput, '?cannot find ', label_filename);
      stop
    end;
    rewrite (listfile, list_filename || '<007>');
    page (listfile);
    star_lines (1, 3);
    skip_lines (4, 26);
    i := 26;
    while not eof (input) do begin	(* copy labelling *)
      readln (input, label_line);
      i := i + 1;
      writeln (listfile, label_line)
    end;
    skip_lines (i + 1, 44);
    star_lines (45, 52); 	(* run right across perforation *)
    skip_lines (5, 26);
    i := 26;
    reset (input, label_filename);
    while not eof (input) do begin	(* copy labelling again *)
      readln (input, label_line);
      i := i + 1;
      writeln (listfile, label_line)
    end;
    close (input);
    skip_lines (i + 1, 47);
    star_lines (48, 51);		(* just down to perforation this time *)
    page (listfile)
  end;
  close (listfile);
  
  writeln (ttyoutput);
  default_options := [header];
  loop
    write (ttyoutput, 'input filename, or "done"-- ');
    break (ttyoutput);
    readln (tty);
    read (tty, source_command);
    source_command := uppercase (source_command);
  exit if (source_command = ' ') or (source_command = 'DONE');
    i := search (source_command, ['/']);
    if i > 0 then
      source_filename := '.PAS ' || substr (source_command, 1, i-1)
    else
      source_filename := '.PAS ' || source_command;
    chosen_options := default_options;
    while i > 0 do begin
      source_command := substr (source_command, i+1);
      negative := (length (source_command) >= 2) andif (substr (source_command, 1, 2) = 'NO');
      if negative then
	source_command := substr (source_command, 3);
      opt := substr (source_command, 1, min (3, length (source_command)));
      if (opt = 'HEA') or (opt = 'BAN') then
	if negative then
	  chosen_options := chosen_options - [header]
	else
	  chosen_options := chosen_options + [header]
      else if opt = 'MAC' then
	if negative then
	  chosen_options := chosen_options - [macro]
	else
	  chosen_options := chosen_options + [macro]
      else if opt = 'AUT' then
	if negative then
	  chosen_options := chosen_options - [autoff]
	else
	  chosen_options := chosen_options + [autoff]
      else
        writeln (ttyoutput, '?only valid options are [NO]HEAD, [NO]MACRO and [NO]AUTOFF');
      i := search (source_command, ['/', ','])
    end;
  
    if source_filename = '.PAS ' then (* no filename ==> new default options *)
      default_options := chosen_options
  
    else if (macro in chosen_options) or (autoff in chosen_options) then begin
      reset (sourcefile, source_filename, [ascii]);
      if eof (sourcefile) then
        writeln (ttyoutput, '?file null or does not exist.')
      else begin
        if header in chosen_options then
          banner_pages (filename (sourcefile));
        rewrite (listfile, list_filename, [preserve]);
	i := 0;
	while not eof (sourcefile) do begin
	  read (sourcefile, line);
	  if (macro in chosen_options) andif
	      (length (line) > 0) andif (line[1] = chr (11b (* tab *))) then
	    line[1] := chr (0 (* null *));
	  writeln (listfile, line);
          i := i + 1;
	  if eopage (sourcefile)  or  ((autoff in chosen_options) and (i >= 45)) then begin
	    page (listfile);
	    i := 0
	  end;
	  readln (sourcefile)
	end;
	page (listfile);
	close (listfile)
      end;
      close (sourcefile)
    end
  
    else begin
      reset (binary_sourcefile, source_filename);
      if eof (binary_sourcefile) then
	writeln (ttyoutput, '?file null or does not exist.')
      else begin
        if header in chosen_options then
          banner_pages (filename (binary_sourcefile));
	rewrite (binary_listfile, list_filename, [preserve]);
	while not eof (binary_sourcefile) do begin
	  binary_listfile^ := binary_sourcefile^;
	  put (binary_listfile);
	  get (binary_sourcefile)
	end;
	binary_listfile^ := 060000000000b (* ff *);
	put (binary_listfile);
	close (binary_listfile)
      end;
      close (binary_sourcefile)
    end
  end (* loop *);
  
end.
