$TITLE BINARY I/O

program exe011;
(* EXE011 - binary io file reads, writes, opening and closing *)

  const
    test_name:file_name :=
$IF p10      'exe011.tst';
$IF vax      'exe011.tst';
$IF m68      'exe011.bn';

    const_1: integer = 1;
    const_2: integer = 2;
    const_3: integer = 3;

  type
     days = (mon,tue,wed,thurs,fri,sat,sun);
     week = set of days;

  var
     wtest_array:array[0..9] of integer := (0,1,2,3,4,5,6,7,8,9);
     wtest_string1:string[10];
     wtest_string2:string[20];
     wtest_real:real;
     wtest_char:char;
     wtest_bool:boolean;

     rtest_array :array[0..9] of integer:= (0,0,0,0,0,0,0,0,0,0);
     rtest_string1:string[10];
     rtest_string2:string[20];
     rtest_char:char;
     rtest_real:real;
     rtest_integer1:integer;
     rtest_integer2:integer;
     rtest_integer3:integer;

     test_file: file of *;

     total_size:integer;
     count:integer;
     i,j: integer;
     w,somedays :week;

     fname:file_name;

$PAGE "SUBROUNTINES"

  procedure error ( error_number: 0..1000 );
    begin
      writeln(tty, 'Error ', error_number);
      break(tty);
    end;


  function check_array : boolean;
    var
       index:integer;
  begin
  check_array := true;
  for index := 1 to 5 do
    check_array := (rtest_array[index] <> index) and check_array;
  end;

$PAGE "START OF BODY"

begin
somedays := [mon,sun,fri,sat];
wtest_string1 := '1234512345';
wtest_string2 := 'abcdefghij0123456789';
wtest_real :=  9876.54321;
wtest_char := 'A';
wtest_bool := true;

rewrite(tty);
writeln(tty,'Begin EXE011');

$PAGE "SEQUENTIAL I/O"

rewrite(test_file,test_name,[retry]);
  if iostatus(test_file) <> io_ok    (* io status  ok *)
     then error ( 1 );
  if cursor  (test_file) <> 1        (* cursor position equal 1 *) 
     then error ( 2 );
  if not eof(test_file)            (* end of file *)
     then error ( 3 );
  if extent(test_file) <> 0          (* file empty *)
     then error ( 4 );

   fname := filename(test_file);


  write(test_file,wtest_string1,wtest_char);
  write(test_file,wtest_real,wtest_string2);
  write(test_file,wtest_array[1],wtest_array[5]);
  i := 5;
  j := size(w);
  w := somedays;
  write(test_file,wtest_array:(i* size(i)), w:j);

  total_size := 1+ size(wtest_string1) + size(wtest_char) + size(wtest_real) +
                ((i+2) * size(i)) + j + size(wtest_string2);

  if cursor(test_file) <> total_size     (* cursor equal number storage units *) 
     then error ( 5 );
  if not eof(test_file)           (* not end of files *)
     then error ( 6 );
  if extent(test_file) <> (total_size -1)  
     then error ( 7 );

close(test_file);
  if iostatus <> io_ok 
     then error ( 8 );
reset(test_file,fname,[retry]);
  if iostatus(test_file) <> io_ok 
     then error ( 9 );

(* extent with even number of btyes *)

  if extent(test_file) <> (total_size -1)
     then error ( 10 );

close(test_file);
  if iostatus <> io_ok 
     then error ( 11 );
rewrite(test_file,fname,[retry,preserve]);
  if iostatus(test_file) <> io_ok 
     then error ( 12 );

  if extent(test_file) <> total_size -1
     then error ( 13 );
  if cursor(test_file) <> total_size
     then error ( 14 );

(* extent with an odd number of bytes *)

  write(test_file,wtest_bool);
close(test_file);
  if iostatus <> io_ok 
     then error ( 15 );
reset(test_file,fname,[retry]);
  if iostatus(test_file) <> io_ok 
     then error ( 16 );
  if extent(test_file) <> total_size
     then error ( 17 );

  if cursor(test_file) <> 1
     then error ( 18 );

  read(test_file,rtest_string1,rtest_char);
  if (iostatus(test_file) <> io_ok) or (rtest_string1 <> wtest_string1) or
     (rtest_char <> wtest_char)     (* first sequential read *)
     then error ( 19 );

  read(test_file,rtest_real,rtest_string2);
  if (iostatus(test_file) <> io_ok) or (rtest_real <> wtest_real) or 
     (rtest_string2 <> wtest_string2)     (* second sequential read *)
     then error ( 20 );

  read(test_file,rtest_integer1,rtest_integer2);
  if (iostatus(test_file) <> io_ok) or (rtest_integer1 <> wtest_array[1]) or
     (rtest_integer2 <> wtest_array[5])    (* third sequential read *) 
     then error ( 21 );

(* check formated io *)

  read(test_file,rtest_array:(i *size(i)), w:j);
  if (iostatus(test_file) <> io_ok) or (w <> somedays) or (check_array)
     then error ( 22 );

close(test_file);
  if iostatus <> io_ok 
     then error ( 23 );

$PAGE "RANDOM I/O"

update(test_file,fname,[retry]);
  if iostatus(test_file) <> io_ok 
     then error ( 24 );

  if extent(test_file) <> total_size
     then error ( 25 );
  if cursor(test_file) <> 1 
     then error ( 26 );
  if eof ( test_file )
     then error ( 260 );

  writern(test_file,3,wtest_string1);
  writern(test_file,90,wtest_char,wtest_string2);

  if iostatus(test_file) <> io_ok    (* random write after eof *)
     then error ( 27 );
 seek(test_file,31);

  if iostatus(test_file) <> io_ok 
     then error ( 28 );
  if cursor(test_file) <> 31 
     then error ( 29 );

  write(test_file,wtest_real);

  rtest_char := ' ';
  rtest_string1 := ' ';
  rtest_string2 := ' ';
  rtest_real := 0;

  readrn(test_file,3,rtest_string1);
  if rtest_string1 <> wtest_string1       (* first random read *)
     then error ( 30 );
seek(test_file,90);
  if iostatus(test_file) <> io_ok 
     then error ( 31 );

  read(test_file,rtest_char,rtest_string2);
  (* second random read *)
  if (rtest_char <> wtest_char) or (rtest_string2 <> wtest_string2)
     then error ( 32 );

  readrn(test_file,31,rtest_real);
  if rtest_real <> wtest_real   (* third random read *)
     then error ( 33 );

  if cursor(test_file) <> (31 + size(wtest_real))    (* cursor position after randon read *) 
     then error ( 34 );
  if eof(test_file)       (* not end of file after random read *)
     then error ( 35 );
  (* extent of file *)
  if extent(test_file) <> (89 + size(wtest_char) + size(wtest_string2)) 
     then error ( 36 );
  if extstatus <> 0   (* extstatus equal 0 *)
     then error ( 37 );

empty(test_file);
  if iostatus(test_file) <> io_ok   (* did empty work *)
     then error ( 38 );

close(test_file);
  if iostatus <> io_ok 
     then error ( 39 );
rewrite(test_file,fname,[retry]);
  if iostatus(test_file) <> io_ok 
     then error ( 40 );
  for count := 1 to 512
     do write(test_file,count);

close(test_file);
  if iostatus <> io_ok 
     then error ( 41 );

$PAGE "WRITE BLOCK"

reset(test_file,fname,[retry]);
  if iostatus(test_file) <> io_ok 
     then error ( 42 );

(* check an entire block starting at a block boundary *)

  count := 1;
  while count<=512
     do begin
        read(test_file,rtest_integer1);
        if rtest_integer1 <> count
           then begin
                error ( 43 );
                writeln(tty,'Count= ',count,' Read= ',rtest_integer1);
                break(tty);
                count := 513;
                end;
        count := count +1;
        end;

close(test_file);
  if iostatus <> io_ok 
     then error ( 44 );

$PAGE "RANDOM WRITE IN BLOCK"

(* do a random read in middle of block and written pattern then check *)

update(test_file,fname,[retry]);
  if iostatus(test_file) <> io_ok 
     then error ( 45 );
  seek(test_file,( 32 * size(i) +1) );
  if iostatus(test_file) <> io_ok 
     then error ( 46 );
  for count:=1 to 448   do write(test_file,count);
  
  seek(test_file,1);
  if iostatus(test_file) <> io_ok 
     then error ( 47 );
  count := 1;
  while count <= 512
    do begin
       read(test_file,rtest_integer1);
       if (((count < 33) or (count > 480)) and (rtest_integer1 <> count))
             or
          (((count > 32) and (count < 481)) and (rtest_integer1 <> (count-32)))
          then begin
               error ( 48 );
               writeln(tty,'Count= ',count,' Read= ',rtest_integer1);
               break(tty);
               count := 513;
               end;
       count := count +1;
       end;

close(test_file);
  if iostatus <> io_ok 
     then error ( 49 );

$PAGE "WRITE ACROSS BLOCKS"

rewrite(test_file,fname,[retry,seekok]);
  if iostatus(test_file) <> io_ok 
     then error ( 50 );

(* check an entire block starting at the middle of a block *)

  if cursor(test_file) <> 1
     then error ( 51 );

  seek(test_file,( 64 * size(i) +1) );
  if iostatus(test_file) <> io_ok 
     then error ( 52 );

  for count := 1 to 512
     do write(test_file,count);

close(test_file);
  if iostatus <> io_ok 
     then error ( 53 );
reset(test_file,fname,[retry,seekok]);
  if iostatus(test_file) <> io_ok 
     then error ( 54 );
  seek(test_file,( 64 * size(i) +1) );
  if iostatus(test_file) <> io_ok 
     then error ( 55 );
  count := 1;
  while count<=512
     do begin
        read(test_file,rtest_integer1);
        if rtest_integer1 <> count
           then begin
                error ( 56 );
                writeln(tty,'Count= ',count,' Read= ',rtest_integer1);
                break(tty);
                count := 513;
                end;
        count := count +1;
        end;

close(test_file);
  if iostatus <> io_ok 
     then error ( 57 );

$PAGE "RANDOM WRITE ACROSS BLOCKS"

(* do a random read in middle of block and written pattern then check *)

update(test_file,fname,[retry]);
  if iostatus(test_file) <> io_ok 
     then error ( 58 );
  seek(test_file,( 96 * size(i) +1) );
  if iostatus(test_file) <> io_ok 
     then error ( 59 );

  for count:=1 to 448   do write(test_file,count);
  
  seek(test_file,( 64 * size(i) +1) );
  if iostatus(test_file) <> io_ok 
     then error ( 60 );

  count := 1;
  while count <= 512
    do begin
       read(test_file,rtest_integer1);
       if (((count < 33) or (count > 480)) and (rtest_integer1 <> count))
             or
          (((count > 32) and (count < 481)) and (rtest_integer1 <> (count-32)))
          then begin
               error ( 61 );
               writeln(tty,'Count =',count,' Read= ',rtest_integer1);
               break(tty);
               count := 513;
               end;
       count := count +1;
       end;

close(test_file);
  if iostatus <> io_ok 
     then error ( 62 );

$PAGE "MISCELLANEOUS TESTS"

rewrite(test_file,fname,[retry,preserve,seekok]);
  if iostatus(test_file) <> io_ok 
     then error ( 63 );
  if cursor(test_file) = 1     (* contents should preserved *)
     then error ( 64 );

close(test_file);
  if iostatus <> io_ok 
     then error ( 65 );
rewrite(test_file,fname,[retry]);
  if iostatus(test_file) <> io_ok 
     then error ( 66 );

  write(test_file,wtest_char);
 
close(test_file);
  if iostatus <> io_ok 
     then error ( 67 );
reset(test_file,fname,[retry]);
  if iostatus(test_file) <> io_ok 
     then error ( 68 );
  rtest_char := ' ';
  read(test_file,rtest_char);
  if rtest_char <> wtest_char    (* test using one byte *)
     then error ( 69 );

close(test_file);
  if iostatus <> io_ok 
     then error ( 70 );
rewrite(test_file,fname,[retry]);
  if iostatus(test_file) <> io_ok 
     then error ( 71 );

  write(test_file, const_1, const_2, const_3);

close(test_file);
  if iostatus <> io_ok 
     then error ( 72 );
reset(test_file,fname,[retry]);
  if iostatus(test_file) <> io_ok 
     then error ( 73 );
  rtest_integer1 := 0;
  rtest_integer2 := 0;
  rtest_integer3 := 0;

  read(test_file,rtest_integer1,rtest_integer2,rtest_integer3);

  (* test using odd number of words *)
  if (rtest_integer1 <> 1) or (rtest_integer2 <> 2) or (rtest_integer3 <> 3)
     then error ( 74 ); 

scratch(test_file);
  if iostatus <> io_ok    (* check if scratch was successful *)
     then error ( 75 );

reset(test_file,fname,[retry]);
  if iostatus(test_file) <> io_opnf      (* check if file was opened *)
     then error ( 76 );

update(test_file,fname,[retry]);
  if iostatus(test_file) = io_opnf   (* check if update could open file *)
     then error ( 77 );

seek(test_file,512);

(* test writing the last character of a record *)
write(test_file,wtest_char);       
if cursor(test_file) <> 513 
    then error ( 78 );

readrn(test_file,512,rtest_char);
if (iostatus(test_file) <> io_ok) or (rtest_char <> wtest_char)
    or (cursor(test_file) <> 513)
    then error ( 79 );

seek(test_file,512);

(* test writing the last character of one record and the first character
   of the next *)
write(test_file,wtest_char,wtest_char);
if (iostatus(test_file) <> io_ok) or (cursor(test_file) <> 514)
    then error ( 80 );

seek(test_file,512);

read(test_file,rtest_char,rtest_char);
if (iostatus(test_file) <> io_ok) or (cursor(test_file) <> 514)
    or (rtest_char <> wtest_char)
    then error ( 81 );

(* test writing a string going over two records *)
wtest_string1:='ab';

writern(test_file,512,wtest_string1);   
if iostatus(test_file) <> io_ok
    then error ( 82 );

readrn(test_file,512,rtest_string1);
if (iostatus(test_file) <> io_ok) or (rtest_string1 <> wtest_string1)
    then error ( 83 );

scratch(test_file);

if iostatus <> io_ok then error ( 84 );

$IFANY (m68,vax)
rewrite(test_file,fname,[retry,image]);  (* check that image file was created *)

i:=900;
write(test_file,i);
if iostatus(test_file) <> io_ok 
    then error ( 85 );
close(test_file);
if iostatus <> io_ok
    then error ( 86 );

$IF m68
reset(test_file,fname,[retry]);  (* check that open failed *)
if iostatus(test_file) <> io_opnf
    then error ( 87 );
$ENDIF

reset(test_file,fname,[retry,image]);  (* check that open worked *)
if iostatus(test_file) <> io_ok
    then error ( 88 );

(* test general functions on an image file *)
read(test_file,i);
if (iostatus(test_file) <> io_ok) or (i <> 900)
    then error ( 89 );

scratch(test_file);

rewrite(test_file,fname,[retry,image]);
i:=100;
write(test_file,i);
if iostatus(test_file) <> io_ok 
    then error ( 90 );
if extent(test_file) <> size(i) 
    then error ( 91 );

close(test_file);
if iostatus <> io_ok
    then error ( 910 );

(* write another record so the file can be opened as non-image *)
update(test_file,fname,[retry,image]);
if iostatus(test_file) <> io_ok 
    then error ( 911 );

writern(test_file,257,i);
if iostatus(test_file) <> io_ok 
    then error ( 912 );

$IF m68
close(test_file);
if iostatus <> io_ok then
    error ( 92 );

reset(test_file,fname,[retry]);  (* check that non-image open worked *)
if iostatus(test_file) <> io_ok 
    then error ( 93 );

if extent(test_file) <> 100 
    then error ( 94 );

if eof(test_file)
    then error ( 95 );
$ENDIF

scratch(test_file);
$ENDIF

(* test file eof functions *)
update(test_file,fname,[retry]);
if iostatus(test_file) <> io_ok
    then error ( 96 );
if not eof(test_file) 
    then error ( 97 );

read(test_file,rtest_string1);
if iostatus(test_file) = io_ok
    then error ( 98 ) ;
if not eof(test_file)
    then error ( 99 );

write(test_file,wtest_char);
if (iostatus(test_file) <> io_ok) or (not eof(test_file))
    then error ( 100 );

close(test_file);
if iostatus <> io_ok
    then error ( 101 );

(* test that you can't write to an input only file *)
reset(test_file,fname,[retry]);
if iostatus(test_file) <> io_ok 
    then error ( 102 );

write(test_file,wtest_char);
if iostatus(test_file) <> io_rewr 
   then error ( 103 );

close(test_file);
if iostatus <> io_ok  
    then error ( 104 );

(* test that you can't read from an output only file *)
rewrite(test_file,fname,[retry]);
if iostatus(test_file) <> io_ok
    then error ( 105 );

read(test_file,rtest_char);
if iostatus(test_file) = io_ok
    then error ( 106 );

scratch(test_file);
writeln(tty, 'End EXE011');
end.
   