




(* EXE014 - string parameter passing test program. *)

program exe014;

type
  fstr_10 = packed array [1..10] of char;
  fstr_20 = packed array [1..20] of char;
  fstr_32 = packed array [1..32] of char;
  fstr_40 = packed array [1..40] of char;
  fstr_300 = packed array [1..300] of char;
  fstr_flex = packed array [1..*] of char;
  vstr_12 = string[ 12 ];
  vstr_24 = string[ 24 ];
  vstr_37 = string[ 37 ];
  vstr_38 = string[ 38 ];
  vstr_300 = string[ 300 ];
  vstr_flex = string[ * ];
  error_range = 0..10000;
  test_range = 1..100;

$PAGE const and var declarations

const
  null = '';
  cf_10a: fstr_10 = 'fixed tena';
  cf_20a: fstr_20 = 'string of length  20';
  cf_40a: fstr_40 = 'contents of a packed array 1..40';
  cf_300a: fstr_300 = 'this is a very, very big string. ' ||
		 '300 characters in all';
  cf_flexa: fstr_32 = 'this is a nonvarying flex string';
  cv_12a: vstr_12 = 'varying 12';
  cv_24a: vstr_24 = 'two dozen string';
  cv_38a: vstr_38 = 'varying string, upperbound of 38';
  cv_300a: vstr_300 = 'this would be really enormous if it was ' ||
		 'the full 300 chars';
  cv_flexa: vstr_37 = 'of what use is a varying flex string?';

  i = 4;

var
  fstr_10a: fstr_10 := cf_10a;
  fstr_20a: fstr_20 := cf_20a;
  fstr_40a: fstr_40 := cf_40a;
  fstr_300a: fstr_300 := cf_300a;
  fstr_flexa: ^fstr_flex;
  vstr_12a: vstr_12 := cv_12a;
  vstr_24a: vstr_24 := cv_24a;
  vstr_38a: vstr_38 := cv_38a;
  vstr_300a: vstr_300 := cv_300a;
  vstr_flexa: ^vstr_flex;
  ch: char := 'c';

$PAGE error

(* ERROR writes an error number to the terminal.  *)

procedure error ( error_number: error_range );

begin
  writeln ( tty, 'Error ', error_number );
  break ( ttyoutput );
end  (* proc error *) ;
$PAGE string functions

(* FSTR_FUNC_10 returns a fixed length string of length 10 *)

function fstr_func_10 : packed array [1..10] of char;

begin
  fstr_func_10 := cf_10a;
end;

(* FSTR_FUNC_40 returns a fixed length string of length 40 *)

function fstr_func_40 : packed array [1..40] of char;

begin
  fstr_func_40 := cf_40a;
end;

(* VSTR_FUNC_12 returns a variable length string of length 12 *)

function vstr_func_12 : string[12] ;

begin
  vstr_func_12 := cv_12a;
end;

(* VSTR_FUNC_38 returns a variable length string of length 38 *)

function vstr_func_38 : string[38] ;

begin
  vstr_func_38 := cv_38a;
end;
$PAGE value_test

(* VALUE_TEST is passed a test case number and four
   string parameters.  The test case number is used as 
   a case statement index to check the parameters
   for the correct values.  *)

public procedure value_test ( test_num: test_range;
			      f_20: fstr_20;
			      f_flex: fstr_flex;
			      v_24: vstr_24;
			      v_flex: vstr_flex ) options check(cases);

begin
  case test_num of

    1:
      begin
	if (length(f_20) <> 20) or (f_20 <> cf_10a)
	  then error( 100 );
	if (length(f_flex) <> 10) or (f_flex <> cf_10a)
	  then error ( 101 );
	if (length(v_24) <> 10) or (v_24 <> cf_10a)
	  then error ( 102 );
	if (length(v_flex) <> 10) or (v_flex <> cf_10a)
	  then error ( 103 );
      end;

    2:
      begin
	if (length(f_20) <> 20) or (f_20 <> cf_20a)
	  then error( 200 );
	if (length(f_flex) <> 20) or (f_flex <> cf_20a)
	  then error ( 201 );
	if (length(v_24) <> 20) or (v_24 <> cf_20a)
	  then error ( 202 );
	if (length(v_flex) <> 20) or (v_flex <> cf_20a)
	  then error ( 203 );
      end;

    3:
      begin
	if (length(f_20) <> 20) or (f_20 <> substr(cf_40a, 1, 20))
	  then error(300);
	if (length(f_flex) <> 40) or (f_flex <> cf_40a)
	  then error ( 301 );
	if (length(v_24) <> 24) or (v_24 <> substr(cf_40a, 1, 24))
	  then error ( 302 );
	if (length(v_flex) <> 40) or (v_flex <> cf_40a)
	  then error ( 303 );
      end;

    4:
      begin
	if (length(f_20) <> 20) or (f_20 <> substr(cf_300a, 1, 20))
	  then error(400);
	if (length(f_flex) <> 300) or (f_flex <> cf_300a)
	  then error ( 401 );
	if (length(v_24) <> 24) or (v_24 <> substr(cf_300a, 1, 24))
	  then error ( 402 );
	if (length(v_flex) <> 300) or (v_flex <> cf_300a)
	  then error ( 403 );
      end;

    5:
      begin
	if (length(f_20) <> 20) or (f_20 <> substr(cf_flexa, 1, 20))
	  then error(500);
	if (length(f_flex) <> 32) or (f_flex <> cf_flexa)
	  then error ( 501 );
	if (length(v_24) <> 24) or (v_24 <> substr(cf_flexa, 1, 24))
	  then error ( 502 );
	if (length(v_flex) <> 32) or (v_flex <> cf_flexa)
	  then error ( 503 );
      end;

    6:
      begin
	if (length(f_20) <> 20) or (f_20 <> cv_12a)
	  then error( 600 );
	if (length(f_flex) <> 10) or (f_flex <> cv_12a)
	  then error ( 601 );
	if (length(v_24) <> 10) or (v_24 <> cv_12a)
	  then error ( 602 );
	if (length(v_flex) <> 10) or (v_flex <> cv_12a)
	  then error ( 603 );
      end;

    7:
      begin
	if (length(f_20) <> 20) or (f_20 <> cv_24a)
	  then error( 700 );
	if (length(f_flex) <> 16) or (f_flex <> cv_24a)
	  then error ( 701 );
	if (length(v_24) <> 16) or (v_24 <> cv_24a)
	  then error ( 702 );
	if (length(v_flex) <> 16) or (v_flex <> cv_24a)
	  then error ( 703 );
      end;

    8:
      begin
	if (length(f_20) <> 20) or (f_20 <> substr(cv_38a, 1, 20))
	  then error(800);
	if (length(f_flex) <> 32) or (f_flex <> cv_38a)
	  then error ( 801 );
	if (length(v_24) <> 24) or (v_24 <> substr(cv_38a, 1, 24))
	  then error ( 802 );
	if (length(v_flex) <> 32) or (v_flex <> cv_38a)
	  then error ( 803 );
      end;

    9:
      begin
	if (length(f_20) <> 20) or (f_20 <> substr(cv_300a, 1, 20))
	  then error(900);
	if (length(f_flex) <> 58) or (f_flex <> cv_300a)
	  then error ( 901 );
	if (length(v_24) <> 24) or (v_24 <> substr(cv_300a, 1, 24))
	  then error ( 902 );
	if (length(v_flex) <> 58) or (v_flex <> cv_300a)
	  then error ( 903 );
      end;

    10:
      begin
	if (length(f_20) <> 20) or (f_20 <> substr(cv_flexa, 1, 20))
	  then error(1000);
	if (length(f_flex) <> 37) or (f_flex <> cv_flexa)
	  then error ( 1001 );
	if (length(v_24) <> 24) or (v_24 <> substr(cv_flexa, 1, 24))
	  then error ( 1002 );
	if (length(v_flex) <> 37) or (v_flex <> cv_flexa)
	  then error ( 1003 );
      end;

    11:
      begin
	if (length(f_20) <> 20) or (f_20 <> ch)
	  then error( 1100 );
	if (length(f_flex) <> 1) or (f_flex <> ch)
	  then error ( 1101 );
	if (length(v_24) <> 1) or (v_24 <> ch)
	  then error ( 1102 );
	if (length(v_flex) <> 1) or (v_flex <> ch)
	  then error ( 1103 );
      end;

    12:
      begin
	if (length(f_20) <> 20) or (f_20 <> 'a')
	  then error( 1200 );
	if (length(f_flex) <> 1) or (f_flex <> 'a')
	  then error ( 1201 );
	if (length(v_24) <> 1) or (v_24 <> 'a')
	  then error ( 1202 );
	if (length(v_flex) <> 1) or (v_flex <> 'a')
	  then error ( 1203 );
      end;

    13:
      begin
	if (length(f_20) <> 20) or (f_20 <> null)
	  then error( 1300 );
	if (length(f_flex) <> 0) or (f_flex <> null)
	  then error ( 1301 );
	if (length(v_24) <> 0) or (v_24 <> null)
	  then error ( 1302 );
	if (length(v_flex) <> 0) or (v_flex <> null)
	  then error ( 1303 );
      end;

    14:
      begin
	if (length(f_20) <> 20) or (f_20 <> substr(cf_20a,i))
	  then error( 1400 ); 
	if (length(f_flex) <> 17) or (f_flex <> substr(cf_20a,i))
	  then error ( 1401 );
	if (length(v_24) <> 17) or (v_24 <> substr(cf_20a,i))
	  then error ( 1402 );
	if (length(v_flex) <> 17) or (v_flex <> substr(cf_20a,i))
	  then error ( 1403 );
      end;

    15:
      begin
	if (length(f_20) <> 20) or (f_20 <> substr(cf_40a,i,20))
	  then error( 1500 );
	if (length(f_flex) <> 37) or (f_flex <> substr(cf_40a,i))
	  then error ( 1501 );
	if (length(v_24) <> 24) or (v_24 <> substr(cf_40a,i,24))
	  then error ( 1502 );
	if (length(v_flex) <> 37) or (v_flex <> substr(cf_40a,i))
	  then error ( 1503 );
      end;

    16:
      begin
	if (length(f_20) <> 20) or (f_20 <> substr(cf_flexa,i,20))
	  then error( 1600 );
	if (length(f_flex) <> 29) or (f_flex <> substr(cf_flexa,i))
	  then error ( 1601 );
	if (length(v_24) <> 24) or (v_24 <> substr(cf_flexa,i,24))
	  then error ( 1602 );
	if (length(v_flex) <> 29) or (v_flex <> substr(cf_flexa,i))
	  then error ( 1603 );
      end;

    17:
      begin
	if (length(f_20) <> 20) or (f_20 <> substr(cv_flexa,i,20))
	  then error( 1700 );
	if (length(f_flex) <> 34) or (f_flex <> substr(cv_flexa,i))
	  then error ( 1701 );
	if (length(v_24) <> 24) or (v_24 <> substr(cv_flexa,i,24))
	  then error ( 1702 );
	if (length(v_flex) <> 34) or (v_flex <> substr(cv_flexa,i))
	  then error ( 1703 );
      end;

    18:
      begin
	if (length(f_20) <> 20) or (f_20 <> uppercase(cf_20a))
	  then error( 1800 );
	if (length(f_flex) <> 20) or (f_flex <> uppercase(cf_20a))
	  then error ( 1801 );
	if (length(v_24) <> 20) or (v_24 <> uppercase(cf_20a))
	  then error ( 1802 );
	if (length(v_flex) <> 20) or (v_flex <> uppercase(cf_20a))
	  then error ( 1803 );
      end;

    19:
      begin
	if (length(f_20) <> 20) or (f_20 <> 'one string then anot')
	  then error( 1900 ); 
	if (length(f_flex) <> 23) or (f_flex <> 'one string then another')
	  then error ( 1901 );
	if (length(v_24) <> 23) or (v_24 <> 'one string then another')
	  then error ( 1902 );
	if (length(v_flex) <> 23) or (v_flex <> 'one string then another')
	  then error ( 1903 );
      end;

    20:
      begin
	if (length(f_20) <> 20) or (f_20 <> cf_10a)
	  then error( 2000 );
	if (length(f_flex) <> 10) or (f_flex <> cf_10a)
	  then error ( 2001 );
	if (length(v_24) <> 10) or (v_24 <> cf_10a)
	  then error ( 2002 );
	if (length(v_flex) <> 10) or (v_flex <> cf_10a)
	  then error ( 2003 );
      end;

    21:
      begin
	if (length(f_20) <> 20) or (f_20 <> substr(cf_40a, 1, 20))
	  then error(2100);
	if (length(f_flex) <> 40) or (f_flex <> cf_40a)
	  then error ( 2101 );
	if (length(v_24) <> 24) or (v_24 <> substr(cf_40a, 1, 24))
	  then error ( 2102 );
	if (length(v_flex) <> 40) or (v_flex <> cf_40a)
	  then error ( 2103 );
      end;

    22:
      begin
	if (length(f_20) <> 20) or (f_20 <> cv_12a)
	  then error( 2200 );
	if (length(f_flex) <> 10) or (f_flex <> cv_12a)
	  then error ( 2201 );
	if (length(v_24) <> 10) or (v_24 <> cv_12a)
	  then error ( 2202 );
	if (length(v_flex) <> 10) or (v_flex <> cv_12a)
	  then error ( 2203 );
      end;

    23:
      begin
	if (length(f_20) <> 20) or (f_20 <> substr(cv_38a, 1, 20))
	  then error(2300);
	if (length(f_flex) <> 32) or (f_flex <> cv_38a)
	  then error ( 2301 );
	if (length(v_24) <> 24) or (v_24 <> substr(cv_38a, 1, 24))
	  then error ( 2302 );
	if (length(v_flex) <> 32) or (v_flex <> cv_38a)
	  then error ( 2303 );
      end;

  end  (* case *) ;
end  (* proc value_test *) ;
$PAGE var_test

(* VAR_TEST is passed a test case number and four
   string parameters.  The test case number is used as 
   a case statement index to check the parameters
   for the correct values.  *)

public procedure var_test ( test_num: test_range;
			    var f_20: fstr_20;
			    var f_flex: fstr_flex;
			    var v_24: vstr_24;
			    var v_flex: vstr_flex ) options check(cases);

begin
  case test_num of

    1:
      begin
	if (length(f_20) <> 20) or (f_20 <> cf_20a)
	  then error (10100 );
	if (length(f_flex) <> 10) or (f_flex <> cf_10a)
	  then error ( 10101 );
	if (length(v_24) <> 16) or (v_24 <> cv_24a)
	  then error ( 10102 );
	if (length(v_flex) <> 10) or (v_flex <> cv_12a)
	  then error ( 10103 );
      end;

    2:
      begin
	if (length(f_20) <> 20) or (f_20 <> cf_20a)
	  then error(10200 );
	if (length(f_flex) <> 20) or (f_flex <> cf_20a)
	  then error ( 10201 );
	if (length(v_24) <> 16) or (v_24 <> cv_24a)
	  then error ( 10202 );
	if (length(v_flex) <> 16) or (v_flex <> cv_24a)
	  then error ( 10203 );
      end;

    3:
      begin
	if (length(f_20) <> 20) or (f_20 <> cf_20a)
	  then error(10300);
	if (length(f_flex) <> 40) or (f_flex <> cf_40a)
	  then error ( 10301 );
	if (length(v_24) <> 16) or (v_24 <> cv_24a)
	  then error ( 10302 );
	if (length(v_flex) <> 32) or (v_flex <> cv_38a)
	  then error ( 10303 );
      end;

    4:
      begin
	if (length(f_20) <> 20) or (f_20 <> cf_20a)
	  then error(10400);
	if (length(f_flex) <> 300) or (f_flex <> cf_300a)
	  then error ( 10401 );
	if (length(v_24) <> 16) or (v_24 <> cv_24a)
	  then error ( 10402 );
	if (length(v_flex) <> 58) or (v_flex <> cv_300a)
	  then error ( 10403 );
      end;

    5:
      begin
	if (length(f_20) <> 20) or (f_20 <> cf_20a)
	  then error(10500);
	if (length(f_flex) <> 32) or (f_flex <> cf_flexa)
	  then error ( 10501 );
	if (length(v_24) <> 16) or (v_24 <> cv_24a)
	  then error ( 10502 );
	if (length(v_flex) <> 37) or (v_flex <> cv_flexa)
	  then error ( 10503 );
      end;

  end  (* case *) ;
end  (* proc value_test *) ;
$PAGE exe014 - body

begin
  rewrite ( tty );
  writeln ( tty, 'Begin EXE014' );
  break ( ttyoutput );

  new ( fstr_flexa, 32 );
  fstr_flexa^ := cf_flexa;

  new ( vstr_flexa, 37 );
  vstr_flexa^ := cv_flexa;

  value_test ( 1, fstr_10a, fstr_10a, fstr_10a, fstr_10a );
  value_test ( 2, fstr_20a, fstr_20a, fstr_20a, fstr_20a );
  value_test ( 3, fstr_40a, fstr_40a, fstr_40a, fstr_40a );
  value_test ( 4, fstr_300a, fstr_300a, fstr_300a, fstr_300a );
  value_test ( 5, fstr_flexa^, fstr_flexa^, fstr_flexa^, fstr_flexa^ );
  value_test ( 6, vstr_12a, vstr_12a, vstr_12a, vstr_12a );
  value_test ( 7, vstr_24a, vstr_24a, vstr_24a, vstr_24a );
  value_test ( 8, vstr_38a, vstr_38a, vstr_38a, vstr_38a );
  value_test ( 9, vstr_300a, vstr_300a, vstr_300a, vstr_300a );
  value_test ( 10, vstr_flexa^, vstr_flexa^, vstr_flexa^, vstr_flexa^ );
  value_test ( 11, ch, ch, ch, ch );
  value_test ( 12, 'a', 'a', 'a', 'a' );
  value_test ( 13, '', '', '', '' );
  value_test ( 14, substr(fstr_20a,i), substr(fstr_20a,i), substr(fstr_20a,i),
    substr(fstr_20a,i) );
  value_test ( 15, substr(fstr_40a,i), substr(fstr_40a,i), substr(fstr_40a,i),
    substr(fstr_40a,i) );
  value_test ( 16, substr(fstr_flexa^,i), substr(fstr_flexa^,i), substr(fstr_flexa^,i),
    substr(fstr_flexa^,i) );
  value_test ( 17, substr(vstr_flexa^,i), substr(vstr_flexa^,i), substr(vstr_flexa^,i),
    substr(vstr_flexa^,i) );
  value_test ( 18, uppercase(fstr_20a), uppercase(fstr_20a), uppercase(fstr_20a),
    uppercase(fstr_20a) );
  value_test ( 19, 'one string'||' then another', 'one string'||' then another',
    'one string'||' then another', 'one string'||' then another' );
  value_test ( 20, fstr_func_10, fstr_func_10, fstr_func_10, fstr_func_10 );
  value_test ( 21, fstr_func_40, fstr_func_40, fstr_func_40, fstr_func_40 );
  value_test ( 22, vstr_func_12, vstr_func_12, vstr_func_12, vstr_func_12 );
  value_test ( 23, vstr_func_38, vstr_func_38, vstr_func_38, vstr_func_38 );

  var_test ( 1, fstr_20a, fstr_10a, vstr_24a, vstr_12a );
  var_test ( 2, fstr_20a, fstr_20a, vstr_24a, vstr_24a );
  var_test ( 3, fstr_20a, fstr_40a, vstr_24a, vstr_38a );
  var_test ( 4, fstr_20a, fstr_300a, vstr_24a, vstr_300a );
  var_test ( 5, fstr_20a, fstr_flexa^, vstr_24a, vstr_flexa^ );

  writeln( tty, 'End EXE014' );
end.
