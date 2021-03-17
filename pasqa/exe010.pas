(* EXE010 : Tests text file I/O operations *)

program exe010;

  type  error_range     = 1..500 ;
        flex_string     = string[*];
        flex_array_char = packed array [1..*] of char;

  var
     test_file : file_name;


     (* Declare permissable I/O types and initialize them *)

     integer_type_z : integer := 0;
     integer_type_p : integer := 123;
     integer_type_n : integer := -456;
     real_type_0 : real := 0.0;
     real_type_1 : real := 123.;
     real_type_2 : real := 0.123;
     real_type_3 : real := 123.456;
     real_type_4 : real := 123.456e12;
     real_type_5 : real := 123.456e-12;
     char_type_1 : char := '$';
     string_type_1 : string[ 11 ] := 'Programming';
     string_type_2 : string[ 9 ] := 'Languages';
     string_type_3 : string[ 21 ] := 'Programming Languages';
     string_type_4 : ^flex_string;
     pac_type_1 : packed array [1..11] of char := 'Programming';
     pac_type_2 : packed array [1..9] of char := 'Languages';
     pac_type_3 : ^flex_array_char;
     boolean_type_T : boolean := TRUE;
     boolean_type_F : boolean := FALSE;
     test_integer : integer ;
     test_real : real ;
     test_char : char ;
     test_string : string[ 40 ] ;
     test_pac : packed array [1..15] of char ;
$PAGE   Ascii Option Test Variable Declarations
     ascii_set : array [0..127] of char :=(
	       chr(0), chr(1), chr(2), chr(3),
	       chr(4), chr(5), chr(6), chr(7),
	       chr(8), chr(9), chr(10), chr(11),
	       chr(12), chr(13), chr(14), chr(15),
	       chr(16), chr(17), chr(18), chr(19),
	       chr(20), chr(21), chr(22), chr(23),
	       chr(24), chr(25), chr(26), chr(27),
	       chr(28), chr(29), chr(30), chr(31),
	       chr(32), chr(33), chr(34), chr(35),
	       chr(36), chr(37), chr(38), chr(39),
	       chr(40), chr(41), chr(42), chr(43),
	       chr(44), chr(45), chr(46), chr(47),
	       chr(48), chr(49), chr(50), chr(51),
	       chr(52), chr(53), chr(54), chr(55),
	       chr(56), chr(57), chr(58), chr(59),
	       chr(60), chr(61), chr(62), chr(63),
	       chr(64), chr(65), chr(66), chr(67),
	       chr(68), chr(69), chr(70), chr(71),
	       chr(72), chr(73), chr(74), chr(75),
	       chr(76), chr(77), chr(78), chr(79),
	       chr(80), chr(81), chr(82), chr(83),
	       chr(84), chr(85), chr(86), chr(87),
	       chr(88), chr(89), chr(90), chr(91),
	       chr(92), chr(93), chr(94), chr(95),
	       chr(96), chr(97), chr(98), chr(99),
	       chr(100), chr(101), chr(102), chr(103),
	       chr(104), chr(105), chr(106), chr(107),
	       chr(108), chr(109), chr(110), chr(111),
	       chr(112), chr(113), chr(114), chr(115),
	       chr(116), chr(117), chr(118), chr(119),
	       chr(120), chr(121), chr(122), chr(123),
	       chr(124), chr(125), chr(126), chr(127) );
$PAGE   Direct Buffer I/O Declarations
     characters : array [ 0..7 ] of char :=( 'P', 'D', '+', 'T',
					     ' ', 'R', '+', 'D');
     string_type_5 : string[ 8 ] := 'PD+T R+D';
$PAGE   init
  procedure init;
    begin
      rewrite( tty );
      writeln( tty,'Begin EXE010');
      break( tty );
    end;  (* init *)
$PAGE   error
  procedure error( error_num : error_range );
    begin
      writeln(tty,'Error ', error_num);
      break(tty)
    end; (* error *)
$PAGE   io_status_check
  procedure io_status_check( var check_file : text;
			     err_num : error_range );
    var  stat : io_status ;
    begin
      stat := iostatus( check_file );
      if stat <> io_ok  then begin
	error( err_num );
	writeln( tty,'   IOSTATUS ERROR ', ord(stat))
      end; (* then *)
    end; (* io_status_check *)
$PAGE   compare_io_status
  procedure compare_io_status( var check_file : text;
			       check_status : io_status;
			       err_num      : error_range);
    var  status : io_status;

    begin
      status := iostatus( check_file );
      if  status <> check_status  then begin
	error( err_num );
	writeln( tty,'   IOSTATUS ERROR: is ',ord( status ),
		      ', should be ', ord( check_status ));
      end;  (* then *)
    end;  (* compare_io_status *)

$PAGE   integer_io
  procedure integer_io;    (* Starts with Error Number 1 *)

    const  eight : integer := 8;
           seven : integer := 7;

    begin

      (* Write out all types of integers *)

      rewrite(output);
      test_file := filename( output );  (* GET system indep file name *)

      write( integer_type_z );  writeln;
      write( integer_type_p );  writeln;
      write( integer_type_n );  writeln;

      write( 8 );  writeln;

      write( ((( 5 * 9 ) / 3 ) - eight ) ); writeln;

      close( output );


$PAGE   integer_io
      (* Now verify the contents of the file *)

      open( input, test_file, [retry]);

      readln;
      read( test_integer );
      io_status_check( input, 1 );
      if test_integer <> integer_type_z   then error ( 2 );

      readln;
      read( test_integer );
      io_status_check( input, 3 );
      if test_integer <> integer_type_p   then error ( 4 );

      readln;
      read( test_integer );
      io_status_check( input, 5 );
      if test_integer <> integer_type_n   then error ( 6 );

      readln;
      read( test_integer );
      io_status_check( input, 7 );
      if  test_integer <> eight   then error( 8 );

      readln;
      read( test_integer );
      io_status_check( input, 9 );
      if  test_integer <> seven   then error( 10 );

      close( input );

    end;   (* integer_io *)    (* Ends with Error Number 10 *)
$PAGE   real_io
  procedure real_io;    (* Starts with Error Number 11 *)

    const  one_pt_two : real := 1.2;
           two_pt_one : real := 2.1;
           epsilon : real := 0.0001;

    begin

      (* Write out all types of reals *)

      rewrite( output, test_file );

      write( real_type_0 );  writeln;
      write( real_type_1 );  writeln;
      write( real_type_2 );  writeln;
      write( real_type_3 );  writeln;
      write( real_type_4 );  writeln;
      write( real_type_5 );  writeln;

      write( 2.1 );  writeln;

      write( ((( two_pt_one * 3 ) - 1.5 ) / 4.0 ) );  writeln;

      close( output );


      (* Now verify the contents of the file *)

      open( input, test_file, [retry] );

      readln;
      read( test_real );
      io_status_check( input, 11 );
      if test_real <> real_type_0   then error ( 12 );

      readln;
      read( test_real );
      io_status_check( input, 13 );
      if test_real <> real_type_1   then error ( 14 );
$PAGE   real_io

      readln;
      read( test_real );
      io_status_check( input, 15 );
      if test_real <> real_type_2   then error ( 16 );

      readln;
      read( test_real );
      io_status_check( input, 17 );
      if test_real <> real_type_3   then error ( 18 );

      readln;
      read( test_real );
      io_status_check( input, 19 );
      if test_real <> real_type_4   then error ( 20 );

      readln;
      read( test_real );
      io_status_check( input, 21 );
      if test_real <> real_type_5   then error ( 22 );

      readln;
      read( test_real );
      io_status_check( input, 23 );
      if  abs (test_real - two_pt_one) > epsilon then error( 24 );

      readln;
      read( test_real );
      io_status_check( input, 25 );
      if  abs (test_real - one_pt_two) > epsilon then error( 26 );

      close( input );

    end;  (* real_io *)    (* Ends with Error Number 26 *)
$PAGE   string_io
  procedure string_io;    (* Starts with Error Number 27 *)

    const  company   : string[ 31 ] := 'MDSI=Manufacturing Data Systems';
           initials  : string[ 4 ]  := 'MDSI';

    var    flex_size : integer;

    begin

      (* Write out all the different types of strings *)

      rewrite( output, test_file );

      write( char_type_1 );  writeln;

      write( string_type_1 );  writeln;
      write( string_type_2 );  writeln;
      write( string_type_1 );
      write( ' ' );
      write( string_type_2 );  writeln;

      new( string_type_4, 4);
      string_type_4^ := initials;
      write(  output, string_type_4^ );  writeln;
      write(  output,' 4', string_type_4^ );  writeln;
      dispose( string_type_4 );

      write( initials );
      write( '=' );
      write( 'Manufacturing Data Systems' );  writeln;

      close( output );


      (* Verify everything that was written to the file *)

      open( input, test_file, [retry] );

      readln;
      read( test_char );
      io_status_check( input, 27 );
      if test_char <> char_type_1   then error ( 28 );
$PAGE   string_io

      readln;
      read( test_string );
      io_status_check( input, 29 );
      if test_string <> string_type_1   then error ( 30 );

      readln;
      read( test_string );
      io_status_check( input, 31 );
      if test_string <> string_type_2   then error ( 32 );

      readln;
      read( test_string );
      io_status_check( input, 33 );
      if test_string <> string_type_3   then error ( 34 );

      readln;
      read( test_string );
      io_status_check( input, 35 );
      if  test_string <> initials   then error( 36 );

      readln;
      read( flex_size );
      io_status_check( input, 37 );
      if  flex_size <> 4   then error( 38 );
      new( string_type_4, flex_size );
      read( string_type_4^ );
      io_status_check( input, 39 );
      if  string_type_4^ <> initials   then error( 40 );
      dispose( string_type_4 );

      readln;
      read( test_string );
      io_status_check( input, 41 );
      if  test_string <> company   then error( 42 );

      close( input );

    end;  (* string_io *)    (* Ends with Error Number 42 *)
$PAGE   packed_array_of_char_io
  procedure packed_array_of_char_io;    (* Starts with Error Number 43 *)

    var  flex_size : integer;

    begin

      (* Write out all types *)

      rewrite( output, test_file );

      write( pac_type_1 );  writeln;
      write( pac_type_2 );  writeln;

      new( pac_type_3, 13);
      pac_type_3^ := 'Manufacturing';
      write(  output, pac_type_3^ );  writeln;
$IF fix
      write(  output,' 13', pac_type_3^ );  writeln;
$ENDIF
      dispose( pac_type_3 );

      close( output );


$PAGE   packed_array_of_char_io
      (* Try to read all the types that were just written *)

      open( input, test_file, [retry] );

      readln;
      read( test_pac );
      io_status_check( input, 43 );
      if test_pac <> pac_type_1   then error ( 44 );

      readln;
      read( test_pac );
      io_status_check( input, 45 );
      if test_pac <> pac_type_2   then error ( 46 );

      readln;
      read( test_pac );
      io_status_check( input, 47 );
      if  test_pac <> 'Manufacturing'   then error( 48 );

$IF fix
      readln;
      read( flex_size );
      io_status_check( input, 49 );
      if  flex_size <> 13   then error( 50 );
      new( pac_type_3, flex_size );
      read( pac_type_3^ );
      io_status_check( input, 51 );
      if  pac_type_3^ <> 'Manufacturing'   then error( 52 );
      dispose( pac_type_3 );
$ENDIF

      close( input );

    end; (* packed_array_of_char_io *)    (* Ends with Error Number 52 *)
$PAGE   boolean_io
  procedure boolean_io;    (* Starts with Error Number 53 *)

    begin

      (* Write out TRUE and FALSE values *)

      rewrite( output, test_file );

      write( boolean_type_T );  writeln;
      write( boolean_type_F );  writeln;

      close( output );


      (* Try to read in TRUE and FALSE now *)

      open( input, test_file, [retry] );

      readln;
      read( test_string );
      io_status_check( input, 53 );
      if uppercase( test_string ) <> 'TRUE'   then error( 54 );

      readln;
      read( test_string );
      io_status_check( input, 55 );
      if uppercase( test_string ) <> 'FALSE'  then error( 56 );

      close( input );

    end; (* boolean_io *)    (* Ends with Error Number 56 *)
$PAGE   variations_on_permissables
  procedure variations_on_permissables;    (* Starts with Error Number 57 *)

    var    str_11_chars : string[11];

    begin

      rewrite( output, test_file);


      (* Write out a string to be read into a shorter string *)

      write( string_type_3 );  writeln;


      (* Do a multiple write *)

      write( string_type_1, ' ', string_type_2 );  writeln;


      (* Set up for multiple reads *)

      write( integer_type_p );
      write( real_type_1 );
      writeln;

      write( string_type_1 );
      write( string_type_2 );
      writeln;


      (* Write out all variations to compare write to writeln *)
      (*                  and read to readln                  *)

      write( real_type_1 );  writeln;
      writeln( real_type_2 );
      writeln( real_type_3 );
      write( real_type_4 );  writeln;

      close ( output );
$PAGE   variations_on_permissables
      (* Verify the contents of the file *)

      open( input, test_file, [retry]);


      (* Read in a string shorter than the string in the file *)

      readln;
      read( str_11_chars );
      io_status_check( input, 57 );
      if  str_11_chars <> string_type_1   then error( 58 );


      (* Check multiple writes *)
      
      readln;
      read( test_string );
      io_status_check( input, 59 );
      if test_string <> string_type_3   then error ( 60 );


      (* Check multiple reads *)

      readln;
      test_integer := 0;
      test_real := 0.0;
      read( test_integer, test_real );
      io_status_check( input, 61 );
      if  test_integer <> integer_type_p   then error( 62 );
      if  test_real <> real_type_1   then error( 63 );

      readln;
      str_11_chars := '';
      read( str_11_chars, test_string );
      io_status_check( input, 64 );
      if  str_11_chars <> string_type_1   then error( 65 );
      if  test_string  <> string_type_2   then error( 66 );
$PAGE   variations_on_permissables
      (* From write-writeln check readln *)

      readln;  (* get on proper line to do test *)
      readln( test_real );
      io_status_check( input, 67 );
      if  test_real <> real_type_1   then error( 68 );


      (* From writeln check readln *)

      readln( test_real );
      io_status_check( input, 69 );
      if  test_real <> real_type_2   then error( 70 );


      (* From writeln check read-readln *)

      read( test_real );
      io_status_check( input, 71 );
      readln;
      if  test_real <> real_type_3   then error( 72 );


      (* From write-writeln check read-readln *)

      read( test_real );
      io_status_check( input, 73 );
      readln;
      if  test_real <> real_type_4   then error( 74 );

      if not eof   then error( 75 );
      close( input );

  end;  (* variations_on_permissibles *)    (* Ends with Error Number 75 *)
$PAGE   ascii_option
  procedure ascii_option;    (* Starts with Error Number 76 *)

    var    start_error_num : error_range;
           char_num_to_write : 0..127;
           char_num_to_read  : 0..127;
           ascii_char : char;
           blank      : char;

    begin

      (* Write out ascii(1) thru ascii(127) to a file *)
      (*   ascii(0) seems to be taken in by 10's OS   *)

      rewrite( output, test_file);

      blank := ' ';
      for char_num_to_write:=1 to 127
        do writeln( blank, ascii_set[ char_num_to_write], blank );

      close( output );  (* finished with output *)



      (* Try reading the Ascii Set in using the Ascii Option *)

      open( input, test_file, [ascii,retry]);
      start_error_num := 76;  (* Starting Error Number *)

      for char_num_to_read:=1 to 127  do begin
        readln;
        read( blank, ascii_char, blank );
        io_status_check( input, (char_num_to_read * 2) + start_error_num );
        if ascii_char <> ascii_set[ char_num_to_read]
          then begin
	    error( ((char_num_to_read * 2) + start_error_num + 1) );
            writeln( tty,'  error on ascii char ',char_num_to_read,', ',
                             ord(ascii_char), ' was read in.');
          end;  (* then *)
      end; (* for loop *)

      close( input );

  end;  (* ascii_option *)    (* Ends with Error Number 331 *)
$PAGE   scratch_and_verify
  procedure scratch_and_verify;      (* Start with Error Number 332 *)

    var   io_stat : io_status;

    begin

      (* Write something to a file *)

      rewrite( output, test_file);
      write( string_type_1 );
      write( ' ' );
      write( string_type_2 );  writeln;
      close (output );


      (* Open the file and verify the contents *)

      open( input, test_file, [retry]);
      
      readln;
      read( test_string );
      io_status_check( input, 332 );
      if test_string <> string_type_3   then error ( 333 );


      (* Scratch the file and try to RESET it *)

      scratch( input );
      reset( input, test_file, [retry]);
      compare_io_status( input, io_opnf, 334);


      (* Test end of file flag - should be TRUE *)

      if  not eof  then error( 335 );

    end;  (* scratch_and_verify *)      (* Ends with Error Number 335 *)
$PAGE   direct_buffer_io
  procedure direct_buffer_io;      (* Start with Error Number 336 *)

    var  count : 0..7;

    begin

      (* Write the chars of "characters" to a file *)

      rewrite( output, test_file);

      for  count := 0  to  7   do begin
        output^ := characters[ count ];
        put( output )
      end;
      writeln;

      close (output );


      (* Now try to read in the string just output *)

      open( input, test_file, [retry]);

      get( input );
      test_string := '';
      for  count := 0  to  7   do begin
        test_string := test_string || input^ ;
        get( input );
        io_status_check( input, count + 336 )
      end;

      close( input );


      (* Is the string the same ? *)
      
      if test_string <> string_type_5
        then error( 344 );

  end;  (* direct_buffer_io *)      (* Ends with Error Number 34
$PAGE   end_of_functions
  procedure end_of_functions;      (* Start with Error Number 345 *)

    begin

      (* First write out some lines to a file with page marks *)

      rewrite( output, test_file);
      write( integer_type_z );
      write( real_type_1 );  writeln;
      write( string_type_1 );  writeln;
      page;
      write( integer_type_p );
      write( real_type_2 );  writeln;
      write( string_type_2 );
      page;                    (* test implicit writeln *)
      write( integer_type_n );
      write( real_type_3 );  writeln;
      write( string_type_3 );  writeln;
      close( output );


      (* Now read in file and check the "end_of"s *)

      open( input, test_file, [retry]);
      
      readln;
      read( test_integer );
      io_status_check( input, 345 );
      if test_integer <> integer_type_z   then error ( 346 );

      read( test_real );
      io_status_check( input, 347 );
      if  test_real <> real_type_1   then error( 348 );

      if  not ( eoln )  then error ( 349 );
      
      readln;
      read( test_string );
      io_status_check( input, 350 );
      if test_string <> string_type_1   then error ( 351 );
      if  not eoln   then error( 352 );
      if  not eopage   then error( 353 );
$PAGE   end_of_functions

      
      readln;
      read( test_integer );
      io_status_check( input, 354 );
      if test_integer <> integer_type_p   then error ( 355 );

      read( test_real );
      io_status_check( input, 356 );
      if  test_real <> real_type_2   then error( 357 );

      if  not ( eoln )  then error ( 358 );
      
      readln;
      read( test_string );
      io_status_check( input, 359 );
      if test_string <> string_type_2   then error ( 360 );
      if  not eoln   then error( 361 );
      if  not eopage   then error( 362 );
      
      readln;
      read( test_integer );
      io_status_check( input, 363 );
      if test_integer <> integer_type_n   then error ( 364 );

      read( test_real );
      io_status_check( input, 365 );
      if  test_real <> real_type_3   then error( 366 );

      if  not ( eoln )  then error ( 367 );
      
      readln;
      read( test_string );
      io_status_check( input, 368 );
      if test_string <> string_type_3   then error ( 369 );
      if  not ( eoln )  then error( 370 );
      if  eof  then error( 371 );
      readln;
      if  eoln  then error( 372 );
      if  not eof  then error( 373 );
      close( input );
$PAGE   end_of_functions
      (* Test EOLN, EOPAGE, and EOF after an OPEN *)
      (*   (file  already has something in it)    *)

      open( input, test_file);
      if  not eoln  then error( 374 );
      if  not eopage    then error( 375 );
      if  eof       then error( 376 );
      close ( input );


      (* Test EOLN, EOPAGE, and EOF after a RESET *)
      (*   (file  already has something in it)    *)

      reset( input, test_file);
      if  eoln    then error( 377 );
      if  eopage  then error( 378 );
      if  eof     then error( 379 );
      close ( input );

    end;  (* end_of_functions *)      (* Ends with Error Number 379 *)
$PAGE   iostatus_function
  procedure iostatus_function  options check;      (* Start with Error Number 380 *)

    var  test_range : error_range;
         f          : text;

    begin

      (* Write to a file to set up input errors *)
      (*   while doing this test output errors  *)

      rewrite( f, test_file, [retry]);

      writeln( f, '2147483648');    (* used to test IO_NOVF *)
      writeln( f, '1000');

      writeln( f, 'INTEGER');       (* used to test IO_DGIT *)
      writeln( f, 'REAL');
      writeln( f, '97531');


      (* try reading from the output file *)

$IFNOT read_output
      read( f, test_integer);
      compare_io_status( f, io_eof, 380);
$ENDIF

      close( f );
$PAGE   iostatus_function
      (* Now try reading from the file to check *)
      (* I/O error detection and identification *)
 
      open( f, test_file, [retry]);


      (* Next two reads test arithmetic overflow on conversion *)

      test_integer := 37;      (* test_integer should remain the same *)
      readln( f );
      read ( f, test_integer);
      if  test_integer <> 37   then error ( 381 );
      compare_io_status( f, io_novf, 382);

      test_range := 69;     (* test_range should remain the same *)
      readln( f );
      read( f, test_range);
      if  test_range <> 69   then error( 383 );
      compare_io_status( f, io_novf, 384);


      (* Next three reads test for invalid numeric input *)

      test_integer := 71;      (* should be set to zero on I/O error *)
      readln( f );
      read( f, test_integer);
      if  test_integer <> 0   then error( 385 );
      compare_io_status( f, io_dgit, 386);

      test_real := 12.3;      (* should be set to zero on I/O error *)
      readln( f );
      read( f, test_real);
      if  test_real <> 0.0   then error( 387 );
      compare_io_status( f, io_dgit, 388);

      test_integer := 27;      (* should be set to zero on I/O error *)
      readln( f );
      read( f, test_integer:10:o);
      if  test_integer <> 0   then error( 389 );
      compare_io_status( f, io_dgit, 390);
$PAGE   iostatus_function
$IFNOT write_input
      write( f, 'Try writing to an input file');
      compare_io_status( f, io_rewr, 391);
$ENDIF


      (* try to read beyond the end of the file *)

      readln( f );
      get( f );
      compare_io_status( f, io_eof, 392);
      if not eof( f )
        then error( 393 )
        else begin
          read( f, test_integer);
          compare_io_status( f, io_eof, 394);
        end;  (* else *)

      close ( f );

      (* IO_OPNF tested in scratch_and_verify *)

    end;  (* iostatus_function *)      (* Ends with Error Number 394 *)
$PAGE   preserve_option
  procedure preserve_option;      (* Start with Error Number 395 *)
    
    begin

      (* Write some things to a file *)

      rewrite( output, test_file, [retry]);

      write( integer_type_p );  writeln;
      write( integer_type_n );  writeln;

      close( output );


      (* Now verify the contents of the file *)

      open( input, test_file, [retry]);
      
      readln;
      read( test_integer );
      io_status_check( input, 395 );
      if test_integer <> integer_type_p   then error ( 396 );
      
      readln;
      read( test_integer );
      io_status_check( input, 397 );
      if test_integer <> integer_type_n   then error ( 398 );

      close( input );
$PAGE   preserve_option
      (* Add some more to the file using the PRESERVE option *)

      rewrite( output, test_file, [retry,preserve]);

      write( real_type_1 );  writeln;
      write( real_type_2 );  writeln;

      close( output );


      (* Again verify the contents of the file *)

      open( input, test_file, [retry]);
      
      readln;
      read( test_integer );
      io_status_check( input, 399 );
      if test_integer <> integer_type_p   then error ( 400 );
      
      readln;
      read( test_integer );
      io_status_check( input, 401 );
      if test_integer <> integer_type_n   then error ( 402 );
      
      readln;
      read( test_real );
      io_status_check( input, 403 );
      if test_real <> real_type_1   then error ( 404 );
      
      readln;
      read( test_real );
      io_status_check( input, 405 );
      if test_real <> real_type_2   then error ( 406 );

      close( input );


    end;  (* preserve_option *)      (* Ends with Error Number 406 *)
$PAGE   option_set_as_a_parameter
  procedure option_set_as_a_parameter;      (* Start with Error Number 407 *)

    type  option_set = set of io_options;

    var  io_options_to_use : option_set;



    procedure add_more ( options_on_io : option_set );

      var  f : text;

      begin   (* add more to the test file *)

        rewrite( f, test_file, options_on_io );
        writeln( f, integer_type_p );
        writeln( f, integer_type_n );
        close( f );

      end;  (* add_more *)



$PAGE   option_set_as_a_parameter
    begin

      (* Write two lines to the test file *)

      rewrite( output, test_file );

      write( real_type_1 );  writeln;
      write( real_type_2 );  writeln;

      close( output );


      (* Put PRESERVE option in io_options_to_use and add_more *)

      io_options_to_use := [preserve];
      add_more( io_options_to_use );

      (* Verify the contents of the file *)
      open( input, test_file, [retry] );
      
      readln;
      read( test_real );
      io_status_check( input, 407 );
      if test_real <> real_type_1   then error ( 408 );
      
      readln;
      read( test_real );
      io_status_check( input, 409 );
      if test_real <> real_type_2   then error ( 410 );
      
      readln;
      read( test_integer );
      io_status_check( input, 411 );
      if test_integer <> integer_type_p   then error ( 412 );
      
      readln;
      read( test_integer );
      io_status_check( input, 413 );
      if test_integer <> integer_type_n   then error ( 414 );
      close( input );

  end;  (* option_set_as_a_parameter *)      (* Ends with Error Number 414 *)
$PAGE   cursor_function
  procedure cursor_function;      (* Start with Error Number 415 *)

    const  null_string : string[1] := '';
	   one_long    : string[1] := '0';
	   five_long   : string[5] := ' 123 ';
	   seven_long  : string[7] := '   -456';

      var  f           : text;

    begin

      (* Test the CURSOR function on output *)
      rewrite( f, test_file);
      if  cursor( f ) <> 0   then error( 415 );
      write( f, five_long );
      if  cursor( f ) <> 5   then error( 416 );
      write( f, one_long );
      if  cursor( f ) <> 6   then error( 417 );
      write( f, null_string );
      if  cursor( f ) <> 6   then error( 418 );
      write( f, seven_long );
      if  cursor( f ) <> 13   then error( 419 );
      writeln ( f );
      if cursor( f ) <> 0 then error( 500 );
      close( f );

      (* Test the CURSOR function on input *)
      open( f, test_file );
      if  cursor( f ) <> 0   then error( 420 );
      readln( f );
      if  cursor( f ) <> 1   then error( 421 );
      close( f );

      reset( f, test_file );
      if  cursor( f ) <> 0   then error( 422 );
      read( f, integer_type_p );
      if  cursor( f ) <> 4   then error( 423 );
      read( f, integer_type_z );
      if  cursor( f ) <> 6   then error( 424 );
      read( f, integer_type_n );
      if  cursor( f ) <> 13   then error( 425 );
      close ( f );

  end;  (* cursor_function *)      (* Ends with Error Number 425 *)
$PAGE   clear_and_empty
  procedure clear_and_empty;      (* Start with Error Number 426 *)

    begin

      (* Test the CLEAR function to make sure it does not blow up *)

      rewrite( output, test_file );
      writeln;  break;
      write( 'This is to be CLEARed');
      clear( output );
      write( 'Buffer was CLEARed' );
      close( output );


      (* Test the EMPTY function *)

      rewrite( output, test_file );
      write( integer_type_z );  writeln;
      write( integer_type_p );  writeln;
      write( integer_type_n );  writeln;
      empty( output );
      write( string_type_3 );  writeln;
      close( output );

      reset( input, test_file );
      read( test_string );
      io_status_check( input, 426 );
      if  test_string <> string_type_3   then error( 427 );
      readln;

      if  not eof   then error( 428 );
      close( input );

    end;  (* clear_and_empty *)      (* Ends with Error Number 428 *)
$PAGE   finish
  procedure finish;
    begin
      rewrite( output, test_file );
      scratch( output );
      writeln( tty,'End EXE010');
      break( tty );
      close( ttyoutput );
    end;  (* finish *)
$PAGE   Main Program
  begin    (* BEGIN MAIN - CALLS ALL TEST PROCEDURES *)

    init;
    integer_io;
    real_io;
    string_io;
    packed_array_of_char_io;
    boolean_io;
    variations_on_permissables;
    ascii_option;
    scratch_and_verify;
    direct_buffer_io;
    end_of_functions;
    iostatus_function;
    preserve_option;
    option_set_as_a_parameter;
    cursor_function;
    clear_and_empty;
    finish;

  end.
 3_H	s