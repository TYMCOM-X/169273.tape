program EXE012;

$TITLE Test typed I/O.

(* This program is intended to be a test of typed I/O.  For each of several
   types, this program tests whether values placed in a file of that type
   can be retrieved from the file.  For this part of the test,
   the following general method is used:  one value is placed into a
   file by "put", and two by "write".  The first value is retrieved from the
   file by "reset", the second by "get", the third by "read".
 
   It is important to note that this program will not find any pair of errors
   which are each other's inverses, and that this program does not leave a
   file on disk and test whether it is correctly read back by another 
   program. 

   In the next part of the test, random file access is tested by writing, to
   each item of a file of type integer, for 100 items, its item number,
   and reading back these items in random order to see whether the value of
   each item is its item number.

   This part of the test also tests the functions 'extent' and 'cursor', and
   checks whether 'eof' behaves properly on a file which has been opened
   for random access.
   The program then tests other aspects of random access, and the 'empty'
   and 'scratch' functions. *)

type
   recordtype = record
      realfield : real;
      integerfield : integer;
      boolfield : boolean;
      charfield : char;
      end; (* record *)

public var
   realfile : file of real;
   realvariable : real;
   integerfile : file of integer;
   integervariable : integer;
   booleanfile : file of boolean;
   booleanvariable : boolean;
   stringfile : file of string [10];
   stringvariable : string [10];
   charfile : file of char;
   charvariable : char;
   arrayfile : file of array [1..10] of integer;
   arrayvariable : array [1..10] of integer;
   index : 1..100;
   recordfile : file of recordtype;
   recordvariable : recordtype;
   seekset : set of 1..100;
   real_filename : file_name;
   filestatus : io_status;
$PAGE Error.
   procedure error (error_number : integer);

   begin
   writeln (tty, 'Error ', error_number);
   break (tty);
   end;
$PAGE Test file of reals.
procedure testrealfile;
 
   begin
   rewrite (realfile, 'test.tmp');
   real_filename := filename (realfile);
   realfile^ := 123.456e12;
   put (realfile);
   write (realfile, 987.654e3);
   write (realfile, 56.789e2);
   close (realfile);
   reset (realfile, real_filename);
   if realfile^ <> 123.456e12 then
      begin
      error (101);
      writeln (tty, 'Wrong value in realfile: expected: ', 123.456e12,
              ' got: ', realfile^);
      writeln (tty, 'Value placed in file by "put", retrieved by "reset"');
      end;
   get (realfile);
   if realfile^ <> 987.654e3 then
      begin
      error (102);
      writeln (tty, 'Wrong value in realfile: expected: ', 987.654e3,
             ' got: ', realfile^);
      writeln (tty, 'Value placed in file by "write", retrieved by "get"');
      end;
   read (realfile, realvariable);
   read (realfile, realvariable);
   if realvariable <> 56.789e2 then
      begin
      error (103);
      writeln (tty, 'Wrong value in realfile: expected: ', 56.789e2,
             ' got: ', realvariable);
      writeln (tty, 'Value placed in file by "write", retrieved by "read"');
      end;
   if not eof (realfile) then
      begin
      error (104);
      writeln (tty, 'Didn''t get end of file on realfile.');
      end;
   close (realfile);
   end;
$PAGE Test file of integers.
procedure testintegerfile;

   begin
   rewrite (integerfile, real_filename);
   integerfile^ := 123456;
   put (integerfile);
   write (integerfile, 987654);
   write (integerfile, 56789);
   close (integerfile);
   reset (integerfile, real_filename);
   if integerfile^ <> 123456 then
      begin
      error (201);
      writeln (tty, 'Wrong value in integerfile: expected: ', 123456,
                    ' got: ', integerfile^);
      writeln (tty, 'Value placed in file by "put", retrieved by "reset"');
      end;
   get (integerfile);
   if integerfile^ <> 987654 then
      begin
      error (202);
      writeln (tty, 'Wrong value in integerfile: expected: ', 987654,
                 ' got: ', integerfile^);
      writeln (tty, 'Value placed in file by "write", retrieved by "get"');
      end;
   read (integerfile, integervariable);
   read (integerfile, integervariable);
   if integervariable <> 56789 then
      begin
      error (203);
      writeln (tty, 'Wrong value in integerfile: expected: ', 56789,
                ' got: ', integervariable);
      writeln (tty, 'Value placed in file by "write", retrieved by "read"');
      end;
   if not eof (integerfile) then
      begin
      error (204);
      writeln (tty, 'Didn''t get end of file on integerfile.');
      end;
   close (integerfile);
   end;
$PAGE Test file of booleans.
procedure testbooleanfile;

   begin
   rewrite (booleanfile, real_filename);
   booleanfile^ := true;
   put (booleanfile);
   write (booleanfile, false);
   write (booleanfile, true);
   close (booleanfile);
   reset (booleanfile, real_filename);
   if booleanfile^ <> true then
      begin
      error (301);
      writeln (tty, 'Wrong value in booleanfile: expected: ', true,
                ' got: ', booleanfile^);
      writeln (tty, 'Value placed in file by "put", retrieved by "reset"');
      end;
   get (booleanfile);
   if booleanfile^ <> false then
      begin
      error (302);
      writeln (tty, 'Wrong value in booleanfile: expected: ', false,
           ' got: ', booleanfile^);
      writeln (tty, 'Value placed in file by "write", retrieved by "get"');
      end;
   read (booleanfile, booleanvariable);
   read (booleanfile, booleanvariable);
   if booleanvariable <> true then
      begin
      error (303);
      writeln (tty, 'Wrong value in booleanfile: expected: ', true,
                  ' got: ', booleanvariable);
      writeln (tty, 'Value placed in file by "write", retrieved by "read"');
      end;
   if not eof (booleanfile) then
      begin
      error (304);
      writeln (tty, 'Didn''t get end of file on booleanfile.');
      end;
   close (booleanfile);
   end;
$PAGE Test file of strings.
procedure teststringfile;

   begin
   rewrite (stringfile, real_filename);
   stringfile^ := 'the quick ';
   put (stringfile);
   write (stringfile, 'brown fox ');
   write (stringfile, 'jumped ove');
   close (stringfile);
   reset (stringfile, real_filename);
   if stringfile^ <> 'the quick ' then
      begin
      error (401);
      writeln (tty, 'Wrong value in stringfile: expected: ', 'the quick ',
                    ' got: ', stringfile^);
      writeln (tty, 'Value placed in file by "put", retrieved by "reset"');
      end;
   get (stringfile);
   if stringfile^ <> 'brown fox ' then
      begin
      error (402);
      writeln (tty, 'Wrong value in stringfile: expected: ', 'brown fox ',
                 ' got: ', stringfile^);
      writeln (tty, 'Value placed in file by "write", retrieved by "get"');
      end;
   read (stringfile, stringvariable);
   read (stringfile, stringvariable);
   if stringvariable <> 'jumped ove' then
      begin
      error (403);
      writeln (tty, 'Wrong value in stringfile: expected: ', 'jumped ove',
                 ' got: ', stringvariable);
      writeln (tty, 'Value placed in file by "write", retrieved by "read"');
      end;
   if not eof (stringfile) then
      begin
      error (404);
      writeln (tty, 'Didn''t get end of file on stringfile.');
      end;
   close (stringfile);
   end;
$PAGE Test file of characters.
procedure testcharfile;

   begin
   rewrite (charfile, real_filename);
   charfile^ := 'a';
   put (charfile);
   write (charfile, 'b');
   write (charfile, 'c');
   close (charfile);
   reset (charfile, real_filename);
   if charfile^ <> 'a' then
      begin
      error (501);
      writeln (tty, 'Wrong value in charfile: expected: ', 'a',
              ' got: ', charfile^);
      writeln (tty, 'Value placed in file by "put", retrieved by "reset"');
      end;
   get (charfile);
   if charfile^ <> 'b' then
      begin
      error (502);
      writeln (tty, 'Wrong value in charfile: expected: ', 'b',
           ' got: ', charfile^);
      writeln (tty, 'Value placed in file by "write", retrieved by "get"');
      end;
   read (charfile, charvariable);
   read (charfile, charvariable);
   if charvariable <> 'c' then
      begin
      error (503);
      writeln (tty, 'Wrong value in charfile: expected: ', 'c',
                 ' got: ', charvariable);
      writeln (tty, 'Value placed in file by "write", retrieved by "read"');
      end;
   if not eof (charfile) then
      begin
      error (504);
      writeln (tty, 'Didn''t get end of file on charfile.');
      end;
   close (charfile);
   end;
$PAGE Test file of arrays.
procedure testarrayfile;

   begin
   rewrite (arrayfile, real_filename);
   for index := 1 to 10 do
      arrayfile^ [index] := index;
   put (arrayfile);
   for index := 1 to 10 do
      arrayvariable [index] := 10 - index;
   write (arrayfile, arrayvariable);
   for index := 10 downto 1 do
      arrayvariable [index] := index + 26;
   write (arrayfile, arrayvariable);
   close (arrayfile);
   reset (arrayfile, real_filename);
   for index := 1 to 10 do
      begin
      if arrayfile^ [index] <> index    then
      begin
         error (601);
  writeln (tty, 'Wrong value in arrayfile: expected: ',
                     index,
                ' got: ', arrayfile^ [index]);
    writeln (tty, 'Value placed in file by "put" ',
                     'retrieved by "reset"');
        end;
      end; (* for *)
$PAGE Test second and third arrays in file.
   get (arrayfile);
   for index := 1 to 10 do
      begin
      if arrayfile^ [index] <> 10 - index    then
      begin
         error (602);
  writeln (tty, 'Wrong value in arrayfile: expected: ',
                     10 - index,
                   ' got: ', arrayfile^ [index]);
    writeln (tty, 'Value placed in file by "write" ',
                   'retrieved by "get"');
  end;
      end; (* for *)
   read (arrayfile, arrayvariable);
   read (arrayfile, arrayvariable);
   for index := 1 to 10 do
      begin
      if arrayvariable [index] <> index + 26    then
   begin
         error (603);
  writeln (tty, 'Wrong value in arrayfile: expected: ',
                     index + 26,
                   ' got: ', arrayvariable [index]);
         writeln (tty, 'Value placed in file by "write" ',
                   'retrieved by "read"');
         end;
      end; (* for *)
   if not eof (arrayfile) then
      begin
      error (604);
      writeln (tty, 'Didn''t get end of file on arrayfile.');
      end;
   close (arrayfile);
   end;
$PAGE Test file of records.
procedure testrecordfile;

   begin;
   rewrite (recordfile, real_filename);
   recordfile^.realfield := 123.456e12;
   recordfile^.integerfield := 123456;
   recordfile^.boolfield := true;
   recordfile^.charfield := 'a';
   put (recordfile);
   recordvariable.realfield := 987.654e3;
   recordvariable.integerfield := 987654;
   recordvariable.boolfield := false;
   recordvariable.charfield := 'b';
   write (recordfile, recordvariable);
   recordvariable.realfield := 56.789e2;
   recordvariable.integerfield := 56789;
   recordvariable.boolfield := true;
   recordvariable.charfield := 'c';
   write (recordfile, recordvariable);
$PAGE Test first record in recordfile.
   close (recordfile);
   reset (recordfile, real_filename);
   if recordfile^.realfield <> 123.456e12 then
      begin
      error (701);
      writeln (tty, 'Wrong value in recordfile: expected: ', 123.456e12,
                    ' got: ', recordfile^.realfield);
      writeln (tty, 'Value placed in file by "put", retrieved by "reset"');
      end;
   if recordfile^.integerfield <> 123456 then
      begin
      error (702);
      writeln (tty, 'Wrong value in recordfile: expected: ', 123456,
              ' got: ', recordfile^.integerfield);
      writeln (tty, 'Value placed in file by "put", retrieved by "reset"');
      end;
   if recordfile^.boolfield <> true then
      begin
      error (703);
      writeln (tty, 'Wrong value in recordfile: expected: ', true,
                  ' got: ', recordfile^.boolfield);
      writeln (tty, 'Value placed in file by "put", retrieved by "reset"');
      end;
   if recordfile^.charfield <> 'a' then
      begin
      error (704);
      writeln (tty, 'Wrong value in recordfile: expected: ', 'a',
               ' got: ', recordfile^.charfield);
      writeln (tty, 'Value placed in file by "put", retrieved by "reset"');
      end;
$PAGE Test second record in recordfile.
   get (recordfile);
   if recordfile^.realfield <> 987.654e3 then
      begin
      error (705);
      writeln (tty, 'Wrong value in recordfile: expected: ', 987.654e3,
                  ' got: ', recordfile^.realfield);
      writeln (tty, 'Value placed in file by "write", retrieved by "get"');
      end;
   if recordfile^.integerfield <> 987654 then
      begin
      error (706);
      writeln (tty, 'Wrong value in recordfile: expected: ', 987654,
              ' got: ', recordfile^.integerfield);
      writeln (tty, 'Value placed in file by "write", retrieved by "get"');
      end;
   if recordfile^.boolfield <> false then
      begin
      error (707);
      writeln (tty, 'Wrong value in recordfile: expected: ', false,
                ' got: ', recordfile^.boolfield);
      writeln (tty, 'Value placed in file by "write", retrieved by "get"');
      end;
   if recordfile^.charfield <> 'b' then
      begin
      error (708);
      writeln (tty, 'Wrong value in recordfile: expected: ', 'b',
               ' got: ', recordfile^.charfield);
      writeln (tty, 'Value placed in file by "write", retrieved by "get"');
      end;
$PAGE Test final record in recordfile.
   read (recordfile, recordvariable);
   read (recordfile, recordvariable);
   if recordvariable.realfield <> 56.789e2 then
      begin
      error (709);
      writeln (tty, 'Wrong value in recordfile: expected: ', 56.789e2,
                  ' got: ', recordvariable.realfield);
      writeln (tty, 'Value placed in file by "write", retrieved by "read"');
      end;
   if recordvariable.integerfield <> 56789 then
      begin
      error (710);
      writeln (tty, 'Wrong value in recordfile: expected: ', 56789,
                 ' got: ', recordvariable.integerfield);
      writeln (tty, 'Value placed in file by "write", retrieved by "read"');
      end;
   if recordvariable.boolfield <> true then
      begin
      error (711);
      writeln (tty, 'Wrong value in recordfile: expected: ', true,
                   ' got: ', recordvariable.boolfield);
      writeln (tty, 'Value placed in file by "write", retrieved by "read"');
      end;
   if recordvariable.charfield <> 'c' then
      begin
      error (712);
      writeln (tty, 'Wrong value in recordfile: expected: ', 'c',
                ' got: ', recordvariable.charfield);
      writeln (tty, 'Value placed in file by "write", retrieved by "read"');
      end;
   if not eof (recordfile) then
      begin
      error (713);
      writeln (tty, 'Didn''t get end of file on recordfile.');
      end;
   close (recordfile);
   end;
$PAGE Test random file access
procedure testrandomaccess;

   begin
   rewrite (integerfile, real_filename);
   if extent (integerfile) <> 0 then
      begin
      error (801);
      writeln (tty, 'File after ''rewrite''  has extent ',
             extent (integerfile));
      end;
   if not eof (integerfile) then
      begin
      error (802);
      writeln (tty, 'File after ''rewrite'' does not have ''eof''.');
      end;
   if cursor (integerfile) <> 1 then
      begin
      error (803);
      writeln (tty, 'File after ''rewrite'' has cursor ', cursor (integerfile));
      end;

   for index := 1 to 100 do
      write (integerfile, index);
   if not eof (integerfile) then
      begin
      error (804);
      writeln (tty, 'File at end of file does not have ''eof''.');
      end;
   if extent (integerfile) <> 100 then 
      begin
      error (805);
      writeln (tty, 'File on which there have been 100 ''write''s has extent ', 
                    extent (integerfile));
      end;
   close (integerfile);

   update (integerfile, real_filename, [retry]);
   if extent (integerfile) <> 100 then
      begin
      error (806);
      writeln (tty, 'File with 100 elements has extent ', 
               extent (integerfile));
      end;
   if eof (integerfile) then
      begin
      error (807);
      writeln (tty, 'File with 100 elements has ''eof'' after ''update''.');
      end;
   if cursor (integerfile) <> 1 then
      begin
      error (808);
      writeln (tty, 'File after ''update'' has cursor ', cursor (integerfile));

      end;
   seekset := [];
   repeat
      repeat
	 index := trunc (random () * 100) + 1;
      until not (index in seekset);
      seekset := seekset + [index];
      seek (integerfile, index);
      if integerfile^ <> index then
	 begin
	 error (809);
	 writeln (tty, 'Seek on element', index,
		  'cursor', cursor(integerfile),
		  'value', integerfile^);
	 end;
   until seekset = [1..100];


   seek (integerfile, 101);
   if not eof (integerfile) then
      begin
      error (810);
      writeln (tty, 'File of 100 items does not have eof after seek item 101');
      end;


   get (integerfile);
   filestatus := iostatus (integerfile);
   if filestatus <> io_eof then
      begin
      error (811);
      writeln (tty, 'Attempt to read after end of file does not produce io_eof.');
      writeln (tty, 'Cursor =', cursor (integerfile),
                    ', extent =', extent (integerfile));
      writeln (tty, 'IO status was', ord (filestatus));
      end;
 

   seek (integerfile, 110);
   put (integerfile);
   if extent (integerfile) <> 110 then
      beg     error (812);
      writeln (tty, 'File after ''seek (integerfile, 110); put (integerfile)'' ',
                'has extent:', extent (integerfile));
      end;


   for index := 55 downto 50 do
      writern (integerfile, index, index - 50);
   readrn (integerfile, 49, integervariable);
   if integervariable <> 49 then
      begin
      error (813);
      writeln (tty, 'Random writes in middle of file destroyed value of ',
                   'preceding data entry.');
      writeln (tty, 'Item 49 now has value', integerfile^);
      end;
   readrn (integerfile, 56, integervariable);
   if integervariable <> 56 then
      begin
      error (814);
      writeln (tty, 'Random writes in middle of file destroyed value of ',
                  'following data entry.');
      writeln (tty, 'Item 56 now has value', integerfile^);
      end;


   close (integerfile);
   rewrite (integerfile, real_filename, [preserve]);
   if (extent (integerfile) <> 110) or (cursor (integerfile) <> 111) then
      begin
      error (815);
      writeln (tty, 'File of 110 elements on which "rewrite" with "preserve"',
                 ' has been done has extent ', extent (integerfile),
                 ', cursor', cursor (integerfile));
      end;


   close (integerfile);
   rewrite (integerfile, real_filename, [retry]);
   for index := 1 to 100 do
   write (integerfile, index);
   empty (integerfile);
   filestatus := iostatus (integerfile);
   if (extent (integerfile) <> 0) or (filestatus <> IO_OK) then
      begin
      error (816);
      writeln (tty, 'File which has been ''empty''ed has extent:',
               extent (integerfile));
      writeln (tty, 'IO status was', ord (filestatus));
      end;


   scratch (integerfile);
   filestatus := iostatus ();
   if filestatus <> IO_OK then
      begin
      error (817);
      writeln (tty, 'After ''scratch'', IO status was', ord (filestatus));
     end;
   end;
$PAGE Main line.
   begin
   rewrite (tty);
   writeln (tty, 'Begin EXE012');
   break (tty);
   testrealfile;
   testintegerfile;
   testbooleanfile;
   teststringfile;
   testcharfile;
   testarrayfile;
   testrecordfile;
   testrandomaccess;
   writeln (tty, 'End EXE012');
   break (tty);
   end.
  
 )@M