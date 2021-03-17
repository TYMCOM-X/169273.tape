program exe015 options check;
const
   max_integer = maximum(integer);
var
   excmsg : string;
$PAGE initialize
public procedure initialize;
begin
   rewrite(tty);
   writeln(tty,'Begin EXE015');
   break(tty);
end;
$PAGE error
public procedure error (error_number : integer);
begin
   writeln(tty,'Error ',error_number);
   break(tty);
end;
$PAGE test_mask_unmask_pending_masked
public procedure test_mask_unmask_pending_masked;
begin
   if masked(attention) then
      error(100);
   if pending(attention) then
      error(101);
   mask(attention);
   if not masked(attention) then
      error(102);
   if pending(attention) then
      error(103);
   unmask(attention);
   if masked(attention) then
      error(104);
   if pending(attention) then
      error(105);
end;
$PAGE test_with
(* verify restoration of "with" register *)
public procedure test_with;
type
   local_record = record
      field_1 : integer;
      field_2 : integer;
   end;
var
   local_structure : ^local_record;

   procedure inner;
   type 
      inner_record = record
         inner_1 : integer;
         inner_2 : integer;
      end;
   var
      inner_structure : ^inner_record;
   begin
      new(inner_structure);
      with inner_structure^ do
      begin
         inner_1:=max_integer;
         inner_2:=max_integer;
         inner_1:=inner_1*inner_2;
         error(110);
         exception
            math_error    : begin
                               if mathstatus<>math_int_ovf then
                                  error(111);
                               if inner_2<>max_integer then
                                  error(112);
                            end;
            allconditions : error(113);
      end;
   end;

begin
   new(local_structure);
   with local_structure^ do
   begin
      field_1:=max_integer;
      field_2:=0;
      inner;
      field_1:=field_1 div field_2;
      error(114);
      exception
         math_error    : begin
			    if mathstatus<>math_zero_divide then
			       error(115);
			    if field_2<>0 then
                               error(116);
			 end;
         allconditions : error(117);
   end;
end;
$PAGE test_others
(* verify invocation of "others" handler *)
public procedure test_others;
var
   integer_1 : integer;
   integer_2 : integer;
begin
   integer_1:=max_integer;
   integer_2:=0;
   integer_1:=integer_1 div integer_2;
   error(120);
   exception
      program_error : error(121);
      others        : ;
end;
$PAGE test_resignal_labelled
(* try resignal from a labelled handler *)
public procedure test_resignal_labelled;

   procedure inner;
   var
      integer_1 : integer;
      integer_2 : integer;
   begin
      integer_1:=max_integer;
      integer_2:=0;
      integer_1:=integer_1 div integer_2;
      error(130);
      exception
         math_error    : begin
			    if mathstatus<>math_zero_divide then
			       error(131);
			    signal();
			 end;
         allconditions : error(132);
   end;

begin
   inner;
   error(133);
   exception
      math_error    : if mathstatus<>math_zero_divide then
			 error(134);
      allconditions : error(135);
end;
$PAGE test_resignal_others
(* try resignal from an others handler *)
public procedure test_resignal_others;

   procedure inner;
   var
      integer_1 : integer;
      integer_2 : integer;
   begin
      integer_1:=max_integer;
      integer_2:=0;
      integer_1:=integer_1 div integer_2;
      error(140);
      exception
         program_error : error(141);
         others        : signal();
   end;

begin
   inner;
   error(142);
   exception
      program_error : error(143);
      others        : ;
end;
$PAGE test_exception_with_inner_handler
(* try an exception within a handler, which should be handled by the local handler *)
public procedure test_exception_with_inner_handler;
exception
   explicit_signal;
var
   integer_1 : integer;
   integer_2 : integer;
begin
   integer_1:=max_integer;
   integer_2:=0;
   integer_1:=integer_1 div integer_2;
   error(150);
   exception
      math_error    : begin
			 if mathstatus<>math_zero_divide then
			    error(151);
                         signal(explicit_signal);
			 error(152);
			 exception
                            explicit_signal : ;
			    allconditions   : error(153);
			 end;
      allconditions : error(154);
end;
$PAGE test_exception_with_outer_handler
(* try an exception within a handler, which should be handled by the outer handler *)
public procedure test_exception_with_outer_handler;
exception
   explicit_signal;
var
   integer_1 : integer;
   integer_2 : integer;
begin
   integer_1:=max_integer;
   integer_2:=0;
   begin
      integer_1:=integer_1 div integer_2;
      error(160);
      exception
         math_error    : begin
                            if mathstatus<>math_zero_divide then
                               error(161);
                            signal(explicit_signal);
                            error(162);
                         end;
         allconditions : error(163);
   end;
   error(164);
   exception
      explicit_signal : ;
      allconditions   : error(165);
end;
$PAGE test_local_goto
(* try a simple goto out of a handler *)
public procedure test_local_goto;
label
   1;
var
   integer_1 : integer;
   integer_2 : integer;
begin
   integer_1:=max_integer;
   integer_2:=0;
   integer_1:=integer_1 div integer_2;
   error(170);
1: return;
   error(171);
   exception
      math_error      : begin
			   if mathstatus<>math_zero_divide then
			      error(172);
			   goto 1;
			   error(173);
                        end;
      allconditions   : error(174);
end;
$PAGE test_non_local_goto
(* try a non-local goto out of a handler *)
public procedure test_non_local_goto;
label
   2;

   procedure inner;
   var
      integer_1 : integer;
      integer_2 : integer;
   begin
      integer_1:=max_integer;
      integer_2:=0;
      integer_1:=integer_1 div integer_2;
      error(180);
      exception
         math_error    : begin
                            if mathstatus<>math_zero_divide then
                               error(181);
                            goto 2;
                            error(182);
                         end;
         allconditions : error(183);
   end;

begin
   inner;
   error(184);
2: return;
end;
$PAGE test_handler_to_exception_goto
(* verify restoration of proper handler when "goto-ing" out of multiple levels of handlers *)
public procedure test_handler_to_exception_goto;
exception
   explicit_signal;
label
   3,4;
var
   integer_1 : integer;
   integer_2 : integer;
begin
   goto 4;
3: signal(explicit_signal);
   error(190);
   begin
4:    integer_1:=max_integer;
      integer_2:=0;
      begin
         integer_1:=integer_1 div integer_2;
         error(191);
         exception
            math_error      : begin
				 if mathstatus<>math_zero_divide then
				    error(192);
				 goto 3;
			      end;
            explicit_signal : error(193);
            allconditions   : error(194);
      end;
      error(195);
      exception
         math_error      : error(196);
         explicit_signal : error(197);
         allconditions   : error(198);
   end;
   error(199);
   exception
      math_error      : error(200);
      explicit_signal : ;
      allconditions   : error(201);
end;
$PAGE test_handler_to_signal_goto
(* verify restoration of proper handler again, this time with user declared condition *)
public procedure test_handler_to_signal_goto;
exception
   explicit_signal;
label
   5,6;
begin
   goto 6;
5: signal(explicit_signal);
   error(210);
   begin
6:    ;
      begin
        signal(explicit_signal);
         error(211);
         exception
            explicit_signal : goto 5;
            allconditions   : error(213);
      end;
      error(214);
      exception
         explicit_signal : error(215);
         allconditions   : error(216);
   end;
   error(217);
   exception
      explicit_signal : ;
      allconditions   : error(218);
end;
$PAGE test_return_handler_to_procedure 
(* try a simple explicit "return" from a handler *)
public procedure test_return_handler_to_procedure; 

   procedure inner;
   var
      integer_1 : integer;
      integer_2 : integer;
   begin
      integer_1:=max_integer;
      integer_2:=0;
      integer_1:=integer_1 div integer_2;
      error(220);
      exception
         math_error    : begin
                            if mathstatus<>math_zero_divide then
                               error(221);
                            return;
                            error(222);
                         end;
         allconditions : error(223);
   end;

begin
   inner;
   exception
      allconditions : error(224);
end;
$PAGE test_return_handler_to_handler 
(* try a "return" from one active handler to another *)
public procedure test_return_handler_to_handler; 
var
   integer_1 : integer;
   integer_2 : integer;

   procedure inner;
   exception
      explicit_signal;
   begin
      signal(explicit_signal);
      error(230);
      exception
         explicit_signal : begin
                              return;
                              error(231);
                           end;
         allconditions   : error(232);
   end;

begin
   integer_1:=max_integer;
   integer_2:=0;
   integer_1:=integer_1 div integer_2;
   error(233);
   exception
      math_error    : begin
                         if mathstatus<>math_zero_divide then
                            error(234);
                         inner;
                      end;
      allconditions : error(235);
end;
$PAGE test_signal_with_inner_handler
(* try explicitly signalling a user condition from within an active handler,
   with the second signal handled by a nested inner handler *)
public procedure test_signal_with_inner_handler;
exception
   explicit_signal;
begin
   signal(explicit_signal);
   error(240);
   exception
      explicit_signal : begin
                           signal(explicit_signal);
			   error(241);
                           exception
			      explicit_signal : ;
			      allconditions   : error(242);
			end;
      allconditions   : error(243);
end;
$PAGE test_signal_with_outer_handler
(* try explicitly signalling a user condition from within an active handler,
   this time with the second signal handled by an outer handler *)
public procedure test_signal_with_outer_handler;
exception
   explicit_signal;
begin
   ;
   begin
      signal(explicit_signal);
      error(250);
      exception
         explicit_signal : begin
			      signal(explicit_signal);
                              error(251);
			   end;
         allconditions   : error(252);
   end;
   error(253);
   exception
      explicit_signal : ;
      allconditions   : error(254);
end;
$PAGE perform_tests
public procedure perform_tests;
begin
   test_mask_unmask_pending_masked;
   test_with;
   test_others;
   test_resignal_labelled;
   test_resignal_others;
   test_exception_with_inner_handler;
   test_exception_with_outer_handler;
   test_local_goto;
   test_non_local_goto;
   test_handler_to_exception_goto;
   test_handler_to_signal_goto;
   test_return_handler_to_procedure;
   test_return_handler_to_handler;
   test_signal_with_inner_handler;
   test_signal_with_outer_handler;

   (* Handler to catch any undetected exceptions *)

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
   writeln(tty,'End EXE015');
end;
$PAGE main
begin
   initialize;
   perform_tests;
   wrapup;
end.
  
