$source
program cursor_bug options assembly;

(* This program is intended to induce a DEC 10 runtime bug involving
   the cursor function on a text file. *)

const
   status_text : array [io_status] of string
      := ('IO_OK', 'IO_NOVF', 'IO_POVF', 'IO_DGIT', 'IO_GOVF', 'IO_INTR',
          'IO_REWR', 'IO_EOF', 'IO_OUTF', 'IO_INPF', 'IO_SEEK', 'IO_ILLC',
          'IO_NEPF', 'IO_OPNF');

var
   test_file : text;
   test_string : string [20];
   integer_var : integer;
   test_file_name : file_name;

procedure write_info (message : string);
begin
writeln (ttyoutput,
   message,
   ', cursor = ',
   cursor (test_file));
end;

begin
open (tty);
rewrite (ttyoutput);

rewrite (test_file, 'test.tmp');
test_file_name := filename (test_file);

writeln (test_file);
write (test_file, 123);
write (test_file, 0);
write (test_file, -456);
writeln (test_file);
close (test_file);

reset (test_file);
readln (test_file);
read (test_file, test_string);
writeln (ttyoutput, '               123456789012345');
writeln (ttyoutput, 'File contents: ', test_string);
writeln (ttyoutput);

reset (test_file);
write_info ('After reset');

read (test_file, integer_var);
write_info ('After first integer read');

read (test_file, integer_var);
write_info ('After second integer read');

read (test_file, integer_var);
write_info ('After third integer read');
end.
