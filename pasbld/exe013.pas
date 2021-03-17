$Title Formatted I/O Executable test program

program exe013;
(* EXE013 - Formatted and unformatted I/O *)

  const test_name:file_name := 'exe013.tmp';

  var
     fix_string   :packed array[1..20] of char;
     test_string  :string[20];
     test_integer1:integer;
     test_integer2:integer;
     test_integer3:integer;
     test_real1   :minimum(real)..maximum(real) prec 4;  
     test_real2   :minimum(real)..maximum(real) prec 11;  
     test_char    :char;
     test_bool    :boolean;

     test_file    :text;

     fname:file_name;

  procedure error (error_number: 0..1000);
    begin
    writeln(tty, 'Error ',error_number);
    break(tty);
    end;

$PAGE 'UNFORMMATED WRITES'
begin
rewrite(tty);
writeln(tty,'Begin EXE013');

test_integer1 := -12345;
test_integer2 := 333333;
test_real1    := .00000001;
test_real2    := .00000002;  (* NEGATIVE INTEGER *)

rewrite(test_file,test_name); 

fname := filename(test_file);

writeln(test_file,test_integer1);
writeln(test_file,test_integer2);
writeln(test_file,test_integer2);
writeln(test_file,'ABC');     
writeln(test_file,'999999999999');

writeln(test_file,test_real1);
writeln(test_file,test_real2);
writeln(test_file,-237.813905);

writeln(test_file,'abcde');
writeln(test_file,'vwxyz');

writeln(test_file,'-4,,030,,-3.26');
writeln(test_file,'11');

close(test_file);

$PAGE

reset(test_file,fname);

readln(test_file,test_string);
if not(( test_string = '-12345') and (length(test_string) = 6))
   then error ( 1 );

readln(test_file,test_string);
if not(( test_string = ' 333333') and (length(test_string) = 7))
   then error ( 2 );  (* POSITIVE INTEGER *)
 
readln(test_file,test_real2);
if test_real2  <> 333333.00000  
   then error ( 3 );  (* INTEGER READ AS REAL *)

read(test_file,test_integer1);
if not(( test_integer1 = 0) and (iostatus(test_file) = io_dgit))
   then error ( 4 );  (* ALPHA READ AS INTEGER *)
readln ( test_file );

read(test_file,test_integer2);
if not(( test_integer2 = 333333) and (iostatus(test_file) = io_novf))
   then error ( 5 );  (* TOO LARGE OF AN INTEGER *)
readln ( test_file );

read(test_file,test_string);
if not( test_string = ' 1.000E-08' )
   then error ( 6 );  (* CHECK PRECISION OF REALS *)
readln ( test_file );

read(test_file,test_string);
if not( test_string = ' 2.0000000000E-08' )
   then error ( 7 );
readln ( test_file );

read(test_file,test_string);
if not(( test_string = '-237.813905') and (length(test_string) = 11))
   then error ( 8 );  (* NEGATIVE REAL *)
readln ( test_file );

read(test_file,fix_string);
if not( fix_string = 'abcde               ') 
   then error ( 9 );  (* FIXED STRING READ *)
readln ( test_file );

read(test_file,test_string);
if not(( test_string = 'vwxyz') and (length(test_string) = 5))
   then error ( 10 );  (* VARIABLE STRING READ *)
readln ( test_file );

readln ( test_file,test_integer1,test_integer2,test_integer3,
       test_real1,test_real2);
if test_integer1 <> -4
   then error ( 11 );
if test_integer2 <> 0
   then error ( 12 );  (* DEFAULT TO ZERO *)
if test_integer3 <> 30
   then error ( 13 );  (* SKIP LEADING ZEROS *)
if test_real1 <> 0
   then error ( 14 );  (* DEFAULT TO ZERO *)
if test_real2 <> -3.2600000000
   then error ( 15 );

read(test_file,test_integer1);
if test_integer1 <> 11
   then error ( 16 );

close(test_file);

$PAGE

rewrite(test_file,fname);

writeln(test_file,'127');
writeln(test_file,'ABC');
writeln(test_file,'78');
writeln(test_file,'ABCDEFG');
writeln(test_file,'123   ');

close(test_file);

$PAGE 'FORMATED READS'

reset(test_file,fname);

read(test_file,test_integer1:3:O);
if not(( test_integer1 = 87) and (iostatus(test_file) <> io_dgit))
   then error ( 17 );  (* READ VALID OCTAL *)
readln ( test_file );
 
read(test_file,test_integer2:3:H);
if not(( test_integer2 = 2748) and (iostatus(test_file) <> io_dgit))
   then error ( 18 );  (* READ VALID HEX *)
readln ( test_file );

read(test_file,test_integer1:2:O);
if not(( test_integer1 = 0) and (iostatus(test_file) = io_dgit))
   then error ( 19 );  (* READ INVALID OCATAL *)
readln ( test_file );
 
read(test_file,test_integer2:7:H);
if not(( test_integer2 = 0) and (iostatus(test_file) = io_dgit))
   then error ( 20 );  (* READ INVALID HEX *)
readln ( test_file );
 
read(test_file,test_integer2:6);
if test_integer2 <> 123  
   then error ( 21 );  (* IGNORE TRAILING BLANKS *)
readln ( test_file );

close(test_file);

$PAGE 'FORMATTED WRITES'

rewrite(test_file,fname);

test_string := 'Format';
test_char   := 'X';
test_integer1 := 751;
test_integer2 := -1234;
test_bool   := false;
test_real1  := 234.567;
test_real2  := -219.431257;

writeln(test_file,test_string:20);
writeln(test_file,test_string:4);
writeln(test_file,test_string:20:L);

writeln(test_file,test_char:3);
writeln(test_file,test_char:2:L);

writeln(test_file,test_bool:4);
writeln(test_file,test_bool:6);

writeln(test_file,test_integer1:5);
writeln(test_file,test_integer2:5);
writeln(test_file,test_integer2:8);
writeln(test_file,test_integer1:0);
writeln(test_file,test_integer1:1:O);
writeln(test_file,test_integer1:4:O);
writeln(test_file,test_integer1:8:O);
writeln(test_file,test_integer2:16:O);
writeln(test_file,test_integer1:1:H);
writeln(test_file,test_integer1:3:H);
writeln(test_file,test_integer1:7:H);
writeln(test_file,test_integer2:16:H);

writeln(test_file,test_real2:14);
writeln(test_file,test_real2:4);
writeln(test_file,test_real1:12:5);
writeln(test_file,test_real1:4:3);
writeln(test_file,test_real2:7:2:E);
writeln(test_file,test_real1:0:5:E);

close(test_file);

$PAGE

reset(test_file,fname);

read(test_file,test_string);
if test_string <> '              Format'
   then error ( 22 );  (* WIDE FORMATTED STRING *)
readln ( test_file );

read(test_file,test_string);
if test_string <> 'Form'
   then error ( 23 );  (* NARROW FORMATTED STRING *)
readln ( test_file );

read(test_file,test_string);
if test_string <> 'Format              '
   then error ( 24 );  (* WIDE LEFT ALIGNED STRING *)
readln ( test_file );

read(test_file,test_string);
if test_string <> '  X'
   then error ( 25 );  (* CHARACTER *)
readln ( test_file );

read(test_file,test_string);
if test_string <> 'X '
   then error ( 26 );  (* LEFT ALIGN CHARACTER *)
readln ( test_file );

read(test_file,test_string);
if test_string <> 'FALS'
   then error ( 27 );  (* NARRROW FEILD WITH BOOLEAN *)
readln ( test_file );

read(test_file,test_string);
if test_string <> ' FALSE'
   then error ( 28 );  (* WIDE FEILD WITH BOOLEAN *)
readln ( test_file );

read(test_file,test_string);
if test_string <> '  751'
   then error ( 29 );  (* FEILD WITH POSITIVE INTEGER *)
readln ( test_file );

read(test_file,test_string);
if test_string <> '-1234'
   then error ( 30 );  (* FEILD WITH NEGATIVE INTEGER *)
readln ( test_file );

read(test_file,test_string);
if test_string <> '   -1234'
   then error ( 31 );  (* WIDE FEILD WITH INTEGER *)
readln ( test_file );

read(test_file,test_string);
if test_string <> '751'
   then error ( 32 );  (* NARROW FEILD WITH INTEGER *)
readln ( test_file );

read(test_file,test_string);
if test_string <> '7'
   then error ( 33 );  (* RIGHT MOST DIGIT ONLY *)
readln ( test_file );

read(test_file,test_string);
if test_string <> '1357'
   then error ( 34 );  (* OCTAL READ *)
readln ( test_file );

read(test_file,test_string);
if test_string <> '00001357'
   then error ( 35 );  (* OCTAL PADDED WITH LEADING ZEROS *)
readln ( test_file );

read(test_file,test_string);
if test_string <> '7777777777775456'
   then error ( 36 );  (* NEGATIVE OCTAL READ *)
readln ( test_file );

read(test_file,test_string);
if test_string <> 'F'
   then error ( 37 );  (* RIGHT MOST HEX DIGIT *)
readln ( test_file );

read(test_file,test_string);
if test_string <> '2EF'
   then error ( 38 );  (* HEX READ *)
readln ( test_file );

read(test_file,test_string);
if test_string <> '00002EF'
   then error ( 39 );  (* HEX READ PADDED WITH LEADING ZEROS *)
readln ( test_file );

read(test_file,test_string);
if test_string <> 'FFFFFFFFFFFFFB2E'
   then error ( 40 );  (* READ NEGATIVE HEX *)
readln ( test_file );

read(test_file,test_string);
if test_string <> '-2.1943126E+02'
   then error ( 41 );  (* WIDE FEILD FOR REAL READ *)
readln ( test_file );

read(test_file,test_string);
if test_string <> '-2.2E+02'
   then error ( 42 );  (* NARROW FEILD FOR REAL *)
readln ( test_file );

read(test_file,test_string);
if test_string <> '   234.56700'
   then error ( 43 );  (* FIELD AND DECIMAL WIDTH *)
readln ( test_file );

read(test_file,test_string);
if test_string <> '2.34567E+02'
   then error ( 44 );  (* FEILD WIDTH TOO SMALL *)
readln ( test_file );

read(test_file,test_string);
if test_string <> '-2.2E+02'
   then error ( 45 );  (* EXPONTENTIAL NOTATION *)
readln ( test_file );

read(test_file,test_string);
if test_string <> '2.3457E+02'
   then error ( 46 );  (* EXPONTENTIAL NOTATION WITH TOO SMALL OF FEILD *)
readln ( test_file );

scratch(test_file);

writeln(tty,'End EXE013');
end.
 