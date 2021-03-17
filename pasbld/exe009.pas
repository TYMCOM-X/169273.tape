program exe009 options check;
const
   max_integer = maximum(integer);
   max_real    = maximum(real);
var
   excmsg : string;
$PAGE initialize
public procedure initialize;
begin
   rewrite(tty);
   writeln(tty,'Begin EXE009');
   break(tty);
end;
$PAGE error
public procedure error (error_number : integer);
begin
   writeln(tty,'Error ',error_number);
   break(tty);
end;
$PAGE test_integer_overflow
public procedure test_integer_overflow;
var
   integer_1 : integer;
   integer_2 : integer;
begin
   integer_1:=max_integer;
   integer_2:=max_integer;
   integer_1:=integer_2*integer_2;
   error(100);
   exception
      math_error    : if mathstatus<>math_int_ovf then
			 error(101);
      allconditions : error(102);
end;
$PAGE test_real_overflow
public procedure test_real_overflow;
var
   real_1 : real;
   real_2 : real;
begin
   real_1:=max_real;
   real_2:=max_real;
   real_1:=real_2*real_2;
   error(110);
   exception
      math_error    : if mathstatus<>math_flt_ovf then
			 error(111);
      allconditions : error(112);
end;
$IFANY (P10,M68)
$PAGE test_real_underflow
public procedure test_real_underflow;
var
   real_1 : real;
   real_2 : real;
begin
   real_1:=1/max_real;
   real_2:=max_real;
   real_1:=real_1/real_2;
   error(120);
   exception
      math_error    : if mathstatus<>math_flt_und then
			 error(121);
      allconditions : error(122);
end;
$ENDIF
$PAGE test_integer_divide_by_zero
public procedure test_integer_divide_by_zero;
var
   integer_1 : integer;
   integer_2 : integer;
begin
   integer_1:=max_integer;
   integer_2:=0;
   integer_1:=integer_1 div integer_2;
   error(130);
   exception
      math_error    : if mathstatus<>math_zero_divide then
			 error(131);
      allconditions : error(132);
end;
$PAGE test_real_divide_by_zero
public procedure test_real_divide_by_zero;
var
   real_1 : real;
   real_2 : real;
begin
   real_1:=max_real;
   real_2:=0;
   real_1:=real_1/real_2;
   error(140);
   exception
      math_error    : if mathstatus<>math_zero_divide then
			 error(141);
      allconditions : error(142);
end;
$PAGE test_arcsin
public procedure test_arcsin;
var
   local_argument : minimum(real)..maximum(real) prec 4;
begin
   local_argument:=max_real;
   local_argument:=arcsin(local_argument);
   error(150);
   exception
      math_error:
$IF P10			if mathstatus <> math_arg_arcsin then
$IFANY (VAX,M68)	if mathstatus <> math_bad_arg then
			 error(151);
      allconditions : error(152);
end;
$PAGE test_arccos
public procedure test_arccos;
var
   local_argument : minimum(real)..maximum(real) prec 4;
begin
   local_argument:=max_real;
   local_argument:=arccos(local_argument);
   error(160);
   exception
       math_error :
$IF P10			if mathstatus <> math_arg_arccos then
$IFANY (VAX,M68)	if mathstatus <> math_bad_arg then
			 error(161);
      allconditions : error(162);
end;
$PAGE test_stack_overflow
$IFNOT VAX
public procedure test_stack_overflow;

   procedure local_procedure;
   var
      local_array : array [1..10000] of integer;
   begin
      local_procedure;
   end;

begin
   local_procedure;
   error(170);
   exception
	stack_overflow	: ;
	allconditions    : error(171);
end;
$ENDIF
$PAGE test_heap_overflow
public procedure test_heap_overflow;
type
   local_record = record
      next_record : ^local_record;
      data_array  : array [1..1000] of integer;
   end;
var
   list_head : ^local_record;
   curr_node : ^local_record;
   next_node : ^local_record;
begin
   new(list_head);
   new(curr_node);
   list_head^.next_record:=curr_node;
   loop
      new(next_node);
      curr_node^.next_record:=next_node;
      next_node^.next_record:=nil;
      curr_node:=next_node;
   end;
   exception
      storage_overflow : begin
			    curr_node:=list_head;
			    while curr_node<>nil do
			    begin
			       next_node:=curr_node^.next_record;
			       dispose(curr_node);
			       curr_node:=next_node;
			    end;
			 end;
      allconditions    : error(180);
end;
$PAGE test_explicit_signal
public procedure test_explicit_signal;
exception
   explicit_signal;
begin
   signal(explicit_signal);
   error(190);
   exception
      explicit_signal : ;
      allconditions   : error(191);
end;
$PAGE test_aggregate
public procedure test_aggregate;
type
   local_record = record
      field_1 : integer;
      field_2 : array [1..*] of integer;
   end;
var
   pointer_1 : ^local_record;
   pointer_2 : ^local_record;
begin
   new(pointer_1,10);
   new(pointer_2,20);
   pointer_1^:=pointer_2^;
   error(200);
   dispose(pointer_1);
   dispose(pointer_2);
   exception
      program_error : begin
			 dispose(pointer_1);
			 dispose(pointer_2);
			 if programstatus<>program_compatibility then
			    error(201);
		      end;
      allconditions : begin
			 dispose(pointer_1);
			 dispose(pointer_2);
			 error(202);
		      end;
end;
$PAGE test_aggregate_nocheck
public procedure test_aggregate_nocheck options nocheck;
type
   local_record = record
      field_1 : integer;
      field_2 : array [1..*] of integer;
   end;
var
   pointer_1 : ^local_record;
   pointer_2 : ^local_record;
   excmsg : string;
begin
   new(pointer_1,10);
   new(pointer_2,20);
   pointer_1^:=pointer_2^;
   dispose(pointer_1);
   dispose(pointer_2);
   exception
      program_error : error(205);
$IF vax
      special_error : if specialstatus <> special_disp_twice then
                        error ( 206 );
$ENDIF
      allconditions : begin
                        error (207);
			excmsg := exception_message;
			writeln (ttyoutput);
			writeln (ttyoutput, '%', excmsg);
                      end;
end;
$PAGE test_assertion
public procedure test_assertion;
var
   local_flag : boolean;
begin
   local_flag:=false;
   assert(local_flag);
   error(210);
   exception
      program_error : if programstatus<>program_assertion then
			 error(211);
      allconditions : error(212);
end;
$PAGE test_assertion_nocheck
public procedure test_assertion_nocheck options nocheck;
var
   local_flag : boolean;
begin
   local_flag:=false;
   assert(local_flag);
   exception
      program_error : error(215);
      allconditions : error(216);
end;
$PAGE test_case
public procedure test_case;
var
   case_index : integer;
begin
   case_index:=max_integer;
   case case_index of
      1 : case_index:=0;
      2 : case_index:=0;
   end;
   error(220);
   exception
      program_error : if programstatus<>program_case then
			 error(221);
      allconditions : error(222);
end;
$PAGE test_case_nocheck
public procedure test_case_nocheck options nocheck;
var
   case_index : integer;
begin
   case_index:=max_integer;
   case case_index of
      1 : case_index:=0;
      2 : case_index:=0;
   end;
   exception
      program_error : error(225);
      allconditions : error(226);
end;
$PAGE test_file_errors
public procedure test_file_errors;
var
   nil_file : text;
begin
   nil_file:=nilf;
   writeln(nil_file);
   error(230);
   exception
      program_error : if programstatus<>program_file then
			 error(231);
      allconditions : error(232);
end;
$PAGE test_file_errors_nocheck
public procedure test_file_errors_nocheck options nocheck;
var
   nil_file : text;
   excmsg : string;
begin
   nil_file:=nilf;
   writeln(nil_file);
   exception
      program_error : begin
                        error (235);
			excmsg := exception_message;
			writeln (ttyoutput);
			writeln (ttyoutput, '%', excmsg);
                      end;
      io_error :  if exiostatus <> io_povf
                   then error(236);
$IF vax
      special_error : if specialstatus <> special_ill_mem_ref then
                        error (237);
$ENDIF
      allconditions : begin
                        error (238);
			excmsg := exception_message;
			writeln (ttyoutput);
			writeln (ttyoutput, '%', excmsg);
                      end;
end;
$PAGE test_pointer
public procedure test_pointer;
var
   local_pointer : ^integer;
begin
   local_pointer:=nil;
   local_pointer^:=0;
   error(240);
   exception
      program_error : if programstatus<>program_pointer then
			 error(241);
      allconditions : error(242);
end;
$PAGE test_pointer_nocheck
public procedure test_pointer_nocheck options nocheck;
var
   local_pointer : ^integer;
   excmsg : string;
begin
   local_pointer:=nil;
   local_pointer^:=0;
   exception
      program_error : error(245);
      special_error : if specialstatus <> special_ill_mem_ref then 
                        error (246);
      allconditions : begin
                        error (247);
			excmsg := exception_message;
			writeln (ttyoutput);
			writeln (ttyoutput, '%', excmsg);
                      end;
end;
$PAGE test_range
public procedure test_range;
var
   local_integer : 1..2;
begin
   local_integer:=2;
   local_integer:=local_integer+local_integer;
   error(250);
   exception
      program_error : if programstatus<>program_value then
			 error(251);
      allconditions : error(252);
end;
$PAGE test_range_nocheck
public procedure test_range_nocheck options nocheck;
var
   local_integer : 1..2;
begin
   local_integer:=2;
   local_integer:=local_integer+local_integer;
   exception
      program_error : error(255);
      allconditions : error(256);
end;
$PAGE test_subscript
public procedure test_subscript;
var
   local_array : array [1..2] of integer;
   subscript   : integer;
begin
   subscript:=max_integer;
   local_array[subscript]:=0;
   error(260);
   exception
      program_error : if programstatus<>program_subscript then
			 error(261);
$IFNOT P10
      special_error : if specialstatus<>special_vax_index then
			 error(262);
$ENDIF
      allconditions : error(263);
end;
$PAGE test_subscript_nocheck
public procedure test_subscript_nocheck options nocheck;
var
   local_array : array [1..2] of integer;
   subscript   : integer;
begin
   subscript:=max_integer;
   local_array[subscript]:=0;
   exception
      stack_overflow : ;
      program_error : error (256);
$IFNOT P10
      special_error : error(266);
$ENDIF
      allconditions : error(267);
end;
$PAGE test_substring
public procedure test_substring;
type
   string_two = string[2];
var
   local_string  : string_two;
   string_length : integer;
begin
   string_length:=max_integer;
   writeln(tty,substr(local_string,1,string_length));
   error(270);
   exception
      program_error : if programstatus<>program_substring then
			 error(271);
      allconditions : error(272);
end;
$PAGE test_substring_nocheck
public procedure test_substring_nocheck options nocheck;
type
   string_two = string[2];
var
   local_string  : string_two;
   string_length : integer;
   f             : text;
   excmsg : string;
begin
   rewrite (f, 'EXE009.tmp');
   string_length:=max_integer;
   writeln(f,substr(local_string,1,string_length));
   exception
      program_error : error(275);
$IF vax
      math_error : if mathstatus <> math_int_ovf then
                     error (276);
$ENDIF
$IF M68
      special_error : if specialstatus <> special_ill_mem_ref then
                        error (277);
$ENDIF
      allconditions : begin
                        error (278);
			excmsg := exception_message;
			writeln (ttyoutput);
			writeln (ttyoutput, '%', excmsg);
                      end;
end;
$PAGE test_illegal_memory_reference
public procedure test_illegal_memory_reference options special(coercions);
const
$IF VAX illegal_address = 1;
$IF P10 illegal_address = #o400000;
$IF M68 illegal_address = #h7fffffff;
var
   local_pointer : ^integer;
begin
   local_pointer:=ptr(illegal_address);
   local_pointer^:=0;
   error(280);
   exception
      special_error : if specialstatus<>special_ill_mem_ref then
			 error(281);
      allconditions : error(282);
end;
$PAGE test_dispose_bad_ptr
public procedure test_dispose_bad_ptr options special(coercions);
var
   local_pointer : ^integer;
begin
   local_pointer:=ptr(15);
   dispose(local_pointer);
   error(290);
   exception
      special_error : if specialstatus<>special_disp_ptr then
			 error(291);
      allconditions : error(292);
end;
$PAGE test_dispose_twice
public procedure test_dispose_twice;
type
   local_record = record
      data : integer;
   end;
var
   local_ptr : ^local_record;
begin
   new(local_ptr);
   dispose(local_ptr);
   dispose(local_ptr);
   error(300);
   exception
      special_error : if specialstatus<>special_disp_twice then
			 error(301);
      allconditions : error(302);
end;
$PAGE perform_tests
public procedure perform_tests;
begin

   (* math_error tests *)

   test_integer_overflow;
   test_real_overflow;
$IFNOT VAX
   test_real_underflow;
$ENDIF
   test_integer_divide_by_zero;
   test_real_divide_by_zero;
   test_arcsin;
   test_arccos;

   (* stack_overflow tests *)

$IFNOT VAX
   test_stack_overflow;
$ENDIF
   test_heap_overflow;

   (* explicit signal test *)

   test_explicit_signal;

   (* program_error tests with and without check option *)

   test_aggregate;
   test_aggregate_nocheck;
   test_assertion;
   test_assertion_nocheck;
   test_case;
   test_case_nocheck;
   test_file_errors;
   test_file_errors_nocheck;
   test_pointer;
   test_pointer_nocheck;
   test_range;
   test_range_nocheck;
   test_subscript;
   test_substring;
   test_substring_nocheck;

   (* special_error tests *)

   test_illegal_memory_reference;
   test_dispose_bad_ptr;
   test_dispose_twice;

   (* handler to catch any undetected exceptions *)

   exception
      allconditions : begin
			 writeln(tty,'Undetected exception occurred');
			excmsg := exception_message;
			writeln (ttyoutput);
			writeln (ttyoutput, '%', excmsg);
		      end;
end;
$PAGE wrapup
public procedure wrapup;
begin
   writeln(tty,'End EXE009');
end;
$PAGE main
begin
   initialize;
   perform_tests;
   wrapup;
end.

    