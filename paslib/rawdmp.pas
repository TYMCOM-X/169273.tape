$PAGE rawdmp
program rawdmp;
type
   one_byte = 0..255;
   fake_word = packed array [1..4] of one_byte;

var
   tty_flag : boolean;
   record_number : integer;
   input_buffer_1 : fake_word;
   input_buffer_2 : fake_word;
   input_buffer_3 : fake_word;
   input_buffer_4 : fake_word;
   input_buffer_5 : fake_word;
   input_buffer_6 : fake_word;
   input_buffer_7 : fake_word;
   input_buffer_8 : fake_word;
   input_file : file of fake_word;
   output_file : text;
$PAGE initialize
procedure initialize;
begin
   open(tty);
   rewrite(tty);
   tty_flag:=false;
   record_number:=1;
end; (* initialize *)
$PAGE get_file_names
procedure get_file_names;
var
   input_file_name : file_name;
   output_file_name : file_name;
begin
   write(tty,'Input file: ');
   break(tty);
   readln(tty);
   read(tty,input_file_name);
   reset(input_file,'.RO '||input_file_name,[retry]);
   if iostatus(input_file)<>io_ok then
   begin
      writeln(tty,'Bad input file.');
      stop;
   end;
   write(tty,'Output file: ');
   break(tty);
   readln(tty);
   read(tty,output_file_name);
   tty_flag:=false;
   if output_file_name=' ' then
      tty_flag:=true
   else
   begin
      rewrite(output_file,'.LS '||output_file_name,[retry]);
      if iostatus(output_file)<>io_ok then
      begin
	 writeln(tty,'Bad output file.');
	 stop;
      end;
   end;
end; (* get_file_names *)
$PAGE read_eight_words
procedure read_eight_words;
begin
   read(input_file,input_buffer_1);
   read(input_file,input_buffer_2);
   read(input_file,input_buffer_3);
   read(input_file,input_buffer_4);
   read(input_file,input_buffer_5);
   read(input_file,input_buffer_6);
   read(input_file,input_buffer_7);
   read(input_file,input_buffer_8);
   exception
      io_error : begin
                    writeln(tty,'*** I/O Error ***');
                    close;
                    stop;
                 end;
end; (* read_eight_words *)
$PAGE write_eight_words_to_tty
procedure write_eight_words_to_tty;

   procedure dump_hex (input_buffer : fake_word);
   var
      local_byte_ptr : integer;
   begin
      for local_byte_ptr:=1 to 4 do
         write(tty,input_buffer[local_byte_ptr]:2:h);
      write(tty,' ');
   end; (* dump_hex *)

   procedure dump_chars (input_buffer : fake_word);
   var
      local_byte_ptr : integer;
   begin
      write(tty,' ');
      for local_byte_ptr:=1 to 4 do
         if chr(input_buffer[local_byte_ptr]) in [' '..'}'] then
            write(tty,' ',chr(input_buffer[local_byte_ptr]))
         else
            write(tty,'  ');
   end; (* dump_chars *)

begin
   writeln(tty);
   write(tty,record_number:4,':  ');
   dump_chars(input_buffer_1);
   dump_chars(input_buffer_2);
   dump_chars(input_buffer_3);
   dump_chars(input_buffer_4);
   dump_chars(input_buffer_5);
   dump_chars(input_buffer_6);
   dump_chars(input_buffer_7);
   dump_chars(input_buffer_8);
   writeln(tty);
   write(tty,'        ');
   dump_hex(input_buffer_1);
   dump_hex(input_buffer_2);
   dump_hex(input_buffer_3);
   dump_hex(input_buffer_4);
   dump_hex(input_buffer_5);
   dump_hex(input_buffer_6);
   dump_hex(input_buffer_7);
   dump_hex(input_buffer_8);
   writeln(tty);
end; (* write_eight_words_to_tty *)
$PAGE write_eight_words_to_output_file
procedure write_eight_words_to_output_file;

   procedure dump_hex (input_buffer : fake_word);
   var
      local_byte_ptr : integer;
   begin
      for local_byte_ptr:=1 to 4 do
         write(output_file,input_buffer[local_byte_ptr]:2:h);
      write(output_file,' ');
   end; (* dump_hex *)

   procedure dump_chars (input_buffer : fake_word);
   var
      local_byte_ptr : integer;
   begin
      write(output_file,' ');
      for local_byte_ptr:=1 to 4 do
         if chr(input_buffer[local_byte_ptr]) in [' '..'}'] then
            write(output_file,' ',chr(input_buffer[local_byte_ptr]))
         else
            write(output_file,'  ');
   end; (* dump_chars *)

begin
   writeln(output_file);
   write(output_file,record_number:4,':  ');
   dump_chars(input_buffer_1);
   dump_chars(input_buffer_2);
   dump_chars(input_buffer_3);
   dump_chars(input_buffer_4);
   dump_chars(input_buffer_5);
   dump_chars(input_buffer_6);
   dump_chars(input_buffer_7);
   dump_chars(input_buffer_8);
   writeln(output_file);
   write(output_file,'        ');
   dump_hex(input_buffer_1);
   dump_hex(input_buffer_2);
   dump_hex(input_buffer_3);
   dump_hex(input_buffer_4);
   dump_hex(input_buffer_5);
   dump_hex(input_buffer_6);
   dump_hex(input_buffer_7);
   dump_hex(input_buffer_8);
   writeln(output_file);
end; (* write_eight_words_to_output_file *)
$PAGE main
begin
   initialize;
   get_file_names;
   read_eight_words;
   while not eof(input_file) do
   begin
      if tty_flag then
	 write_eight_words_to_tty
      else
	 write_eight_words_to_output_file;
      record_number:=record_number+8;
      read_eight_words;
   end;
   if tty_flag then
      write_eight_words_to_tty
   else
      write_eight_words_to_output_file;
   close;
end.
