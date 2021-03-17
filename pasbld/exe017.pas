$TITLE EXE017 - MATH TEST ROUTINE

program exe017
   options special(ptr,coe);

(* EXE017 - test all math functions *)
$PAGE EXE017 overview

(* This program tests the math functions by using a library math file
   created on the VAX using the high precision library there.  GENLIB is
   run on the VAX and creates the library file read by EXE017.  This
   file contains a function name, one or two arguments in decimal and
   hex representation and the function expected result, also given in
   decimal and hex representation.  This math library gives a straigh-forward
   method of testing any implementation of math functions.  The values
   in the file are not dependant on the implementation being tested.  

   The program flow is as follows:
      1.  Read the function name
      2.  Using cmd_lookup_record, find the function_type value
          to be used later in a case statement
      3.  If the function to be performed is an integer function,
             a)  Read the first integer argument
             b)  If two arguments are required, read the second argument
             c)  Read the result
          Otherwise,
             a)  Read in the rest of the record, use getstring to
                 get the decimal double precision value, and parse off
                 the hex representation of the value 
             b)  Get a hex string cooresponding to the decimal value read
                 in and a double precision value corresponding to the hex
                 string read in
             c)  Check that the error between the two values is within a
                 tolerable range.  If not print a message - this could indicate
                 a problem with the i/o routines.  The value actually used in
                 the computations is the one read as a decimal number
             d)  If there is a second argument, read it in as outlined above
             e)  Read in the result as outlined above
      4.  Do a case statement on the function_type which executes the proper
          statement to test the function
      5.  Compare the result against the expected result and print out
          an error message if the difference is too large

   Major procedures used are:
      1.  hexreal - translate a hex string to a real number
      2.  realhex - translate a real number to a hex string
      3.  read_next_record - read the next function name and the arguments
          and result for it.  Set up for the case statement.
      4.  check_precision - check that two values are close enough to
          each other.  If not, print an error message.
      5.  subtract_string - subtracts one string format number from another
          string format number giving a string format number as a result.
      6.  cmd_lookup_record - search a list for a specific string and
          and return the match value

   Of the above procedures, hexreal, realhex, subtract_string,
   and cmd_lookup_record are external to the program.

                                                               *)

$PAGE machine specific constants
const
   str_size:=
$IF m68       22;
$IF p10       24;
$IF vax       22;
   mant_size:=
$IF m68       14;
$IF p10       16;
$IF vax       14;
   d_max:minimum(real)..maximum(real) prec 16 := 1.699999976072182e+38;
   d_max_str:=
$IF m68        '[+7F  +FFC99E00000000]';
$IF p10        '[+7F  +FFC99E0000000000]';
$IF vax        '[+7F  +FFC99E00000000]';
   d_min:minimum(real)..maximum(real) prec 16 := 1.000010042080912e-27;
   d_min_str:=
$IF m68        '[-59  +9E753A00000000]';
$IF p10        '[-59  +9E753A0000000000]';
$IF vax        '[-59  +9E753A00000000]';
   s_max:=9.223300018843156e+18;
   s_max_str:=
$IF m68        '[+3F  +FFFF7D00000000]';
$IF p10        '[+3F  +FFFF7D0000000000]';
$IF vax        '[+3F  +FFFF7D00000000]';
   s_min:=1.000000045813705e-18;
   s_min_str:=
$IF m68        '[-3B  +9392EF00000000]';
$IF p10        '[-3B  +9392EF0000000000]';
$IF vax        '[-3B  +9392EF00000000]';
$PAGE external declarations
$INCLUDE (pasdev2)cmdutl.inc

$INCLUDE hexr.inc

external function subtract_string (
                          str1:string[*];
                          str2:string[*]
                          ):string[str_size];

$PAGE global declarations
type
   function_type = (iadd,isub,imult,idiv,imod,ipower,fadd,fsub,fmult,
      fdiv,fpower,fipower,natlog,log10,expon,sine,cosine,tangent,arcsine,
      arccosine,arctangent,arctan2,hsine,hcosine,htangent,sqroot,dummy,
      truncate,round1,round2);
   sets = set of function_type;


var
   msg_str:string[100];    (* exception message text *)
   fctn:function_type;     (* value used in the case statement - computed from funct_name *)
   funct_name:string[254]; (* function to be tested - read in from MATLIB *)
   input_check:boolean;    (* flag - true if comparing the decimal and hex values read in,
                                     false if comparing computed and expected function results *)
   arg1:real;              (* values read in from MATLIB *)
   arg2:real;
   lib_rslt:real;
   d_arg1:d_real;
   d_arg2:d_real;
   d_lib_rslt:d_real;
   int_arg1:integer;
   int_arg2:integer;
   int_lib_rslt:integer;
   strg_arg1:string[70];
   strg_arg2:string[70];
   strg_lib_rslt:string[70];
   rslt:real;              (* computed results *)
   d_rslt:d_real;
   int_rslt:integer;
   strg_rslt:string[70];
   d_d:d_real;             (* dummy variable *)
   matlib:text;            (* math library - contains function name with args and expected result *)
   errfil:text;            (* text file listing of errors encountered during execution *)
   intfunction:sets:=[iadd..ipower];  (* sets to determine what type of arguments to read *)
   twoargs:sets:=[iadd..fipower,arctan2];

$PAGE result_error
procedure result_error(format:real_format);

(* This procedure is called whenever the computed result is not close
   enough to the expected result.  Integer arguments and results
   are output in decimal only.  Double and single precision variables
   are output both in decimal and hex representation.  The hex
   representation is:
       [sEE  sMMMMMMMMMMMMMMMM]
   where 's' is the sign of the exponent or the mantissa, 'E' is the binary
   exponent and 'M' is the normalized mantissa.   

     parameters:   format - tells whether the error was for double
                            or single precision if its not an integer function
     assumptions:  the appropriate arguments, results, and hex representations
                   of arguments and results are properly set up
     output:       a printed error message
     side affects: none
                                                        *)

begin

   if fctn in intfunction then write(errfil,'integer ')
      else if format = d_format then write(errfil,'double precision ')
      else write(errfil,'single precision ');
   writeln(errfil,'error: function ',funct_name);
   if fctn in intfunction then 
   begin
      writeln(errfil,'   arg1            = ',int_arg1);
      if fctn in twoargs then
         writeln(errfil,'   arg2            = ',int_arg2);
      writeln(errfil,'   expected result = ',int_lib_rslt);
      writeln(errfil,'   result          = ',int_rslt);
   end
   else if format = d_format then
   begin
      writeln(errfil,'   arg1            = ',d_arg1,' ',strg_arg1);
      if fctn in twoargs then
         if (fctn = fipower) or (fctn = round2) then
            writeln(errfil,'   arg2            = ',int_arg2)
         else
            writeln(errfil,'   arg2            = ',d_arg2,' ',strg_arg2);
      writeln(errfil,'   expected result = ',d_lib_rslt,' ',strg_lib_rslt);
      writeln(errfil,'   result          = ',d_rslt,' ',strg_rslt);
   end
   else
   begin
      writeln(errfil,'   arg1            = ',arg1,' ',strg_arg1);
      if fctn in twoargs then
         if (fctn = fipower) or (fctn = round2) then
            writeln(errfil,'   arg2            = ',int_arg2)
         else
            writeln(errfil,'   arg2            = ',arg2,' ',strg_arg2);
      writeln(errfil,'   expected result = ',lib_rslt,' ',strg_lib_rslt);
      writeln(errfil,'   result          = ',rslt,' ',strg_rslt);
   end;
   writeln(errfil);
end;

$PAGE check_precision
procedure check_precision (
   d_1:d_real;
   strg1:string[*];
   d_2:d_real;
   strg2:string[*];
   format:real_format
   );

(* This procedure checks that the error between two hex strings passed
   in is in an acceptable range.  The routine subtract_string is used to
   give the difference between the two as a normalized value represented
   as another hex string.  If the result is not within bounds, an error
   message is printed out.  This routine is used to check the result
   of a computation against the expected result and to check that the
   hex and decimal values for a particular argument or result input match.  

     parameters:    d_1 - double precision value cooresponding to the first
                          hex string (valid only if input_check set)
                    strg1 - first hex string to be compared
                    d_2 - double precision value corresponding to the second
                          hex string (valid only if input_check set)
                    strg2 - second hex string to be compared
                    format - indicated precision that the string must be
                             correct to
     assumptions:   input_check is set if the routine is called by 
                       read_next_record to compare decimal and hex values
                       input
                    assumptions required by input_error, result_error, and
                       subtract_string
      output:       none - indirectly a possible error message from one of
                    the error routines
      side effects: none

                                                           *)


$PAGE input_error
procedure input_error;   (* local to check_precision *)

(* This procedure prints the error message when the decimal value and
   hex string read from the file do not match.  


     parameters:   none
     assumptions:  arguments, result, and hex representations of the
                   arguments and result should be set up for the function
     output:       none
     side effects: none
                                         *)

begin
   writeln(errfil,'conversion error - decimal input = ',d_1,' ',strg1);
   writeln(errfil,'                   hex input     = ',d_2,' ',strg2);
   writeln(errfil);
end;

$PAGE check_precision
var
   result:string[str_size];
   exp:integer;
   exp1:integer;
   exp2:integer;

const
   s_error:=
$IF m68      -20;
$IF vax      -18;
$IF p10      -22;
   d_error:=
$IF m68      -51;
$IF vax      -50;
$IF p10      -58;
   zero_str:=
$IF m68       '[+00  +00000000000000]';
$IF p10       '[+00  +0000000000000000]';
$IF vax       '[+00  +00000000000000]';

begin  (* check_precision mainline *)

   result:=subtract_string(strg1,strg2);  (* result := strg1 - strg2 *)
   getstring(substr(result,3,2),exp:2:h);
   if substr(result,2,1) = '-' then exp:=-exp;
   getstring(substr(strg1,3,2),exp1:2:h);
   if substr(strg1,2,1) = '-' then exp1:=-exp1;
   getstring(substr(strg2,3,2),exp2:2:h);
   if substr(strg2,2,1) = '-' then exp2:=-exp2;
   (* check the binary exponent of the error between the expected and
      computed values.  Print an error message if it's too great *)
   exp:=exp - max(exp1,exp2);
   if (format = s_format) and (exp > s_error) and
      (result <> zero_str) then
      if input_check then input_error else result_error(format)
   else if (format = d_format) and (exp > d_error) and 
      (result <> zero_str) then
      if input_check then input_error else result_error(format);
end;

$PAGE get_funct_name
procedure get_funct_name;

(* This procedure reads the math library one character at a time to
   pull off the function name.  It is necessary since the library is
   not fixed format  


     parameters:   none
     assumptions:  none
     output:       sets up funct_name to contain the next string in the file
     side effects: eof or eoln is set if no function name is found
                   file is set up to read the first argument if a name was
                      found
                                                   *)

var
   done:boolean;
   fctn_char:char;

begin

   done:=false;
   funct_name:='';

   while (not(eoln(matlib)) and not(eof(matlib)) and not(done)) do
   begin
      fctn_char:=matlib^;
      get(matlib);
      if fctn_char <> ' ' then funct_name:=funct_name || fctn_char;
      if (fctn_char = ' ') and (funct_name <> '') then done:=true;
   end;
end;

$PAGE hexonly
function hexonly (
   adr:ptr;
   format:real_format
   ):string[70];

(* This routine uses realhex to translate a real number to a string
   representing the number.  It then parses off the hex representation
   portion of the string.  


     parameters:   adr - address of real number to be translated
                   format - format of the real number
     assumptions:  none
     output:       hexonly - a string containing the hex representation
     side effects: none
                                                              *)

var
   start:integer;

begin

   hexonly:=realhex(adr,format);
   start:=search(hexonly,['[']);
   hexonly:=substr(hexonly,start);

end;

$PAGE read_next_record
function read_next_record:boolean;

(* This procedure reads in the function name and arguments and 
   expected result for that function.  It compares the double precision
   and hex representation values for each argument or result read in.  If
   there is a discrepency between the double precision and hex values,
   it could indicate a problem with the input/output conversions.  


     parameters:   none
     assumptions:  file is set up to read the function name
     output:       sets up the appropriate arguments, lib_results, and
                      hex representations
                   fctn is set up
                   an error message is printed if there was a discrepency
                      between the decimal and hex values read in
     side effects: the file is set up to read the next function
                                                                   *)

type
   function_type_list = array[1..ord(maximum(function_type))-3] of cmd_lookup_record;

var 
   start:integer;
   check_val:d_real;
   rec:string[255];
   match:integer;
   index:0..255;

const
   fctn_list:function_type_list := (
      ('IADD',4,ord(iadd)),('ISUB',4,ord(isub)),('IMULT',5,ord(imult)),
      ('IDIV',4,ord(idiv)),('IMOD',4,ord(imod)),('IPOWER',6,ord(ipower)),
      ('FADD',4,ord(fadd)),('FSUB',4,ord(fsub)),('FMULT',5,ord(fmult)),
      ('FDIV',4,ord(fdiv)),('FPOWER',6,ord(fpower)),('FIPOWER',7,ord(fipower)),
      ('NATLOG',6,ord(natlog)),('LOG10',5,ord(log10)),('EXPON',5,ord(expon)),
      ('SINE',4,ord(sine)),('COSINE',6,ord(cosine)),('TANGENT',6,
      ord(tangent)),('ARCSINE',7,ord(arcsine)),('ARCCOSINE',9,
      ord(arccosine)),('ARCTANGENT',10,ord(arctangent)),('ARCTAN2',7,ord(arctan2)),
      ('HSINE',5,ord(hsine)),('HCOSINE',7,ord(hcosine)),
      ('HTANGENT',8,ord(htangent)),('SQROOT',6,ord(sqroot))
      );


begin

   read_next_record:=true;
   get_funct_name;
   if (funct_name <> '') then
   begin
      index:=1;
      if cmd_lookup(funct_name,index,[minimum(char)..maximum(char)],fctn_list,match) then
      begin
         fctn:=function_type(match);
         if fctn in intfunction then
         begin
            if fctn in twoargs then
               readln(matlib,int_arg1,int_arg2,int_lib_rslt)
            else
               readln(matlib,int_arg1,int_lib_rslt);
         end
         else
         begin
            input_check:=true;
            readln(matlib,rec);
            getstring(rec,d_arg1);
            strg_arg1:=hexonly(address(d_arg1),d_format);
            start:=search(rec,['[']);
            strg_rslt:=substr(rec,start,2) || substr(rec,start+4,str_size-3) || ']';
            hexreal(address(check_val),d_format,strg_rslt);
            check_precision(d_arg1,strg_arg1,check_val,strg_rslt,d_format);
            d_arg1:=check_val;
            strg_arg1:=strg_rslt;
            if fctn in twoargs then
            if fctn = fipower then
               readln(matlib,int_arg2)
            else
            begin
               readln(matlib,rec);
               getstring(rec,d_arg2);
               strg_arg2:=hexonly(address(d_arg2),d_format);
               start:=search(rec,['[']);
               strg_rslt:=substr(rec,start,2) || substr(rec,start+4,str_size-3) || ']';
               hexreal(address(check_val),d_format,strg_rslt);
               check_precision(d_arg2,strg_arg2,check_val,strg_rslt,d_format);
               d_arg2:=check_val;
               strg_arg2:=strg_rslt;
            end;
            readln(matlib,rec);
            getstring(rec,d_lib_rslt);
            strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
            start:=search(rec,['[']);
            strg_rslt:=substr(rec,start,2) || substr(rec,start+4,str_size-3) || ']';
            hexreal(address(check_val),d_format,strg_rslt);
            check_precision(d_lib_rslt,strg_lib_rslt,check_val,strg_rslt,
               d_format);
            d_lib_rslt:=check_val;
            strg_lib_rslt:=strg_rslt;
            arg1:=d_arg1;
            arg2:=d_arg2;
            lib_rslt:=d_lib_rslt;
            input_check:=false;
         end;
      end
      else fctn:=dummy;
   end
   else read_next_record:=false;
   if eof(matlib) then read_next_record:=false;

end;

$PAGE rantst
procedure rantst;
var
   times:array[0..99] of integer;
   i,j:integer;
   sum,x:real;

begin
   
   for i:=0 to 99 do
      times[i]:=0;
 
   x:=random(.2);   (* initialize random *)

   for i:=0 to 10000 do
   begin
      x:=random();
      j:=trunc(x*100);
      times[j]:=times;
   end;

   sum:=0;
   for i:=0 to 99 do
      sum:=sum+((times[i]-100)**2)/100;

   writeln(errfil,'random number chi square value = ',sum);

end;

$PAGE special log tests
procedure special_log;
begin

(* test ln(maximum double precision), ln(minimum double precision),
   ln(maximum real), ln(minimum real), 
   log(maximum double precision), log(minimum double precision),
   log(maximum real), and log(minimum real) *)
   fctn:=natlog;
   funct_name:='NATLOG';
   d_arg1:=d_max;
   strg_arg1:=d_max_str;
   d_rslt:=ln(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   d_lib_rslt:=8.802886177076072e+01;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);
   arg1:=s_max;
   strg_arg1:=s_max_str;
   rslt:=ln(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   d_lib_rslt:=4.366826456703760e+01;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   lib_rslt:=d_lib_rslt;
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_arg1:=d_min;
   strg_arg1:=d_min_str;
   d_rslt:=ln(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   d_lib_rslt:=-6.216978746880874e+01;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);
   arg1:=s_min;
   strg_arg1:=s_min_str;
   rslt:=ln(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   d_lib_rslt:=-4.144653162807912e+01;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   lib_rslt:=d_lib_rslt;
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   fctn:=log10;
   funct_name:='LOG10';
   d_arg1:=d_max;
   strg_arg1:=d_max_str;
   d_rslt:=log(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   d_lib_rslt:=3.823044891526550e+01;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);
   arg1:=s_max;
   strg_arg1:=s_max_str;
   rslt:=log(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   d_lib_rslt:=1.896488633575573e+01;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   lib_rslt:=d_lib_rslt;
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_arg1:=d_min;
   strg_arg1:=d_min_str;
   d_rslt:=log(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   d_lib_rslt:=-2.699999563880157e+01;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);
   arg1:=s_min;
   strg_arg1:=s_min_str;
   rslt:=log(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   d_lib_rslt:=-1.799999998010336e+01;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   lib_rslt:=d_lib_rslt;
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered in special log tests');
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered in special log tests');
         writeln(errfil);
      end;

end;

$PAGE special square root tests
procedure special_sqroot;
begin

(* test sqrt(maximum double precision), sqrt(minimum double precision),
   sqrt(maximum real), and sqrt(minimum real) *)
   fctn:=sqroot;
   funct_name:='SQROOT';
   d_arg1:=d_max;
   strg_arg1:=d_max_str;
   d_rslt:=sqrt(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   d_lib_rslt:=1.303840471864630e+19;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);
   arg1:=s_max;
   strg_arg1:=s_max_str;
   rslt:=sqrt(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   d_lib_rslt:=3.036988643186398e+09;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   lib_rslt:=d_lib_rslt;
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_arg1:=d_min;
   strg_arg1:=d_min_str;
   d_rslt:=sqrt(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   d_lib_rslt:=3.162293538052582e-14;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);
   arg1:=s_min;
   strg_arg1:=s_min_str;
   rslt:=sqrt(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   d_lib_rslt:=1.000000022906852e-09;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   lib_rslt:=d_lib_rslt;
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered in special square root tests');
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered in special square root tests');
         writeln(errfil);
      end;

end;

$PAGE special arctangent tests
procedure special_arctan;

begin

(* test arctan(max double precision) and arctan(max single precision) *)
   fctn:=arctangent;
   funct_name:='ARCTANGENT';
   d_arg1:=d_max;
   strg_arg1:=d_max_str;
   d_rslt:=arctan(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   d_lib_rslt:=1.570796326794897e+00;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);
   arg1:=s_max;
   strg_arg1:=s_max_str;
   rslt:=arctan(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   d_lib_rslt:=1.570796326794897e+00;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   lib_rslt:=d_lib_rslt;
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);

(* test arctan2(x,0) *)
   fctn:=arctan2;
   funct_name:='ARCTAN2';
   d_arg1:=7;
   d_arg2:=0;
   d_rslt:=arctan(d_arg1,d_arg2);
   strg_arg1:=hexonly(address(d_arg1),d_format);
   strg_arg2:=hexonly(address(d_arg2),d_format);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   d_lib_rslt:=1.570796326794897e+00;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);
   arg1:=d_arg1;
   arg2:=d_arg2;
   rslt:=arctan(arg1,arg2);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered in special arctangent tests');
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered in special arctangent tests');
         writeln(errfil);
      end;

end;

$PAGE special power tests
procedure special_power;

begin

(* test maximum(double precision) ** minimum(double precision) and
   maximum(real) ** minimum(real) *)
   fctn:=fpower;
   funct_name:='FPOWER';
   d_arg1:=d_max;
   d_arg2:=d_min;
   d_rslt:=d_arg1 ** d_arg2;
   d_lib_rslt:=1.0e+00;
   strg_arg1:=d_max_str;
   strg_arg2:=d_min_str;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);
   arg1:=s_max;
   arg2:=s_min;
   rslt:=arg1 ** arg2;
   d_lib_rslt:=1.000000000000000e+00;
   lib_rslt:=d_lib_rslt;
   d_rslt:=rslt;
   strg_arg1:=s_max_str;
   strg_arg2:=s_min_str;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered in special power tests');
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered in special power tests');
         writeln(errfil);
      end;

end;

$PAGE special sine tests
procedure special_sine;

begin

(* test sine(2**14) *)
   fctn:=sine;
   funct_name:='SINE';
   d_arg1:=1.048576000000000e+06;
   strg_arg1:=hexonly(address(d_arg1),d_format);
   d_rslt:=sin(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   d_lib_rslt:=3.304931400217347e-01;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);
   arg1:=d_arg1;
   rslt:=sin(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   lib_rslt:=d_lib_rslt;
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered in special sine tests');
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered in special sine tests');
         writeln(errfil);
      end;

end;

$PAGE special multiplication test
procedure special_mult;
begin

(* test 5.0 * 0.1 *)
   fctn:=fmult;
   funct_name:='FMULT';
   d_arg1:=5.0;
   strg_arg1:=hexonly(address(d_arg1),d_format);
   d_arg2:=1.000000014901161e-01;
   strg_arg2:=hexonly(address(d_arg2),d_format);
   d_rslt:=d_arg1 * d_arg2;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   d_lib_rslt:=5.000000074505806e-01;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);
   arg1:=d_arg1;
   arg2:=d_arg2;
   rslt:=arg1 * arg2;
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   lib_rslt:=d_lib_rslt;
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered in special multiplication tests');
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered in special multiplication tests');
         writeln(errfil);
      end;

end;
$PAGE special truncation and rounding tests
procedure special_trunc_and_round;

procedure local_error;
begin
   writeln(errfil,'error: function ',funct_name);
   writeln(errfil,'   arg1            = ',arg1);
   writeln(errfil,'   expected result = ',int_lib_rslt);
   writeln(errfil,'   result          = ',int_rslt);
   writeln(errfil);
end;

begin

   fctn:=truncate;
   funct_name:='TRUNC';
   arg1:=133333.33;
   int_rslt:=trunc(arg1);
   int_lib_rslt:=133333;
   if (int_rslt <> int_lib_rslt) then local_error;
   arg1:=-199.99;
   int_rslt:=trunc(arg1);
   int_lib_rslt:=-199;
   if (int_rslt <> int_lib_rslt) then local_error;
   arg1:=199.99;
   int_rslt:=trunc(arg1);
   int_lib_rslt:=199;
   if (int_rslt <> int_lib_rslt) then local_error;
   arg1:=-0.173;
   int_rslt:=trunc(arg1);
   int_lib_rslt:=0;
   if (int_rslt <> int_lib_rslt) then local_error;

   fctn:=round1;
   funct_name:='ROUND';
   arg1:=143.924;
   int_rslt:=round(arg1);
   int_lib_rslt:=144;
   if (int_rslt <> int_lib_rslt) then local_error;
   arg1:=173294.1;
   int_rslt:=round(arg1);
   int_lib_rslt:=173294;
   if (int_rslt <> int_lib_rslt) then local_error;
   arg1:=0.0001;
   int_rslt:=round(arg1);
   int_lib_rslt:=0;
   if (int_rslt <> int_lib_rslt) then local_error;
   arg1:=-173294.1;
   int_rslt:=round(arg1);
   int_lib_rslt:=-173294;
   if (int_rslt <> int_lib_rslt) then local_error;
   arg1:=-143.924;
   int_rslt:=round(arg1);
   int_lib_rslt:=-144;
   if (int_rslt <> int_lib_rslt) then local_error;

   fctn:=round2;
   funct_name:='ROUND2';
   arg1:=143.924;
   int_arg2:=2;
   rslt:=round(arg1,int_arg2);
   lib_rslt:=100.0;
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   d_lib_rslt:=lib_rslt;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   arg1:=173294.1;
   int_arg2:=5;
   rslt:=round(arg1,int_arg2);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   lib_rslt:=200000.0;
   d_lib_rslt:=lib_rslt;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   arg1:=-173294.1;
   int_arg2:=4;
   rslt:=round(arg1,int_arg2);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   lib_rslt:=-170000.0;
   d_lib_rslt:=lib_rslt;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   arg1:=13333.33;
   int_arg2:=3;
   rslt:=round(arg1,int_arg2);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   lib_rslt:=13000.0;
   d_lib_rslt:=lib_rslt;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   arg1:=0.234567;
   int_arg2:=-3;
   rslt:=round(arg1,int_arg2);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   lib_rslt:=0.235;
   d_lib_rslt:=lib_rslt;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   arg1:=-235.666;
   int_arg2:=0;
   rslt:=round(arg1,int_arg2);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   lib_rslt:=-236.0;
   d_lib_rslt:=lib_rslt;
   strg_lib_rslt:=hexonly(address(d_lib_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on special trunc and round tests');
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on special trunc and round tests');
         writeln(errfil);
      end;

end;
$PAGE special tests
procedure special_tests;
begin

   special_log;

   special_sqroot;

   special_arctan;

   special_power;

   special_sine;

   special_mult;

   special_trunc_and_round;

end;

$PAGE test_int_add
procedure test_int_add;
begin
   int_rslt:=int_arg1 + int_arg2;
   if int_rslt <> int_lib_rslt then result_error(s_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on integer add:');
         writeln(tty,'   arg 1 = ',int_arg1,'   arg 2 = ',int_arg2);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on integer add:');
         writeln(errfil,'   arg 1 = ',int_arg1,'   arg 2 = ',int_arg2);
         writeln(errfil);
      end;

end;
$PAGE test_int_sub
procedure test_int_sub;
begin
   int_rslt:=int_arg1 - int_arg2;
   if int_rslt <> int_lib_rslt then result_error(s_format);
   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on integer sub:');
         writeln(tty,'   arg 1 = ',int_arg1,'   arg 2 = ',int_arg2);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on integer sub:');
         writeln(errfil,'   arg 1 = ',int_arg1,'   arg 2 = ',int_arg2);
         writeln(errfil);
      end;
end;
$PAGE test_int_mult
procedure test_int_mult;
begin
   int_rslt:=int_arg1 * int_arg2;
   if int_rslt <> int_lib_rslt then result_error(s_format);
   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on integer multiply:');
         writeln(tty,'   arg 1 = ',int_arg1,'   arg 2 = ',int_arg2);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on integer multiply:');
         writeln(errfil,'   arg 1 = ',int_arg1,'   arg 2 = ',int_arg2);
         writeln(errfil);
      end;
end;
$PAGE test_int_div
procedure test_int_div;
begin
   int_rslt:=int_arg1 div int_arg2;
   if int_rslt <> int_lib_rslt then result_error(s_format);
   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on integer divide:');
         writeln(tty,'   arg 1 = ',int_arg1,'   arg 2 = ',int_arg2);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on integer divide:');
         writeln(errfil,'   arg 1 = ',int_arg1,'   arg 2 = ',int_arg2);
         writeln(errfil);
      end;
end;
$PAGE test_int_mod
procedure test_int_mod;
begin
   int_rslt:=int_arg1 mod int_arg2;
   if int_rslt <> int_lib_rslt then result_error(s_format);
   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on integer mod:');
         writeln(tty,'   arg 1 = ',int_arg1,'   arg 2 = ',int_arg2);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on integer mod:');
         writeln(errfil,'   arg 1 = ',int_arg1,'   arg 2 = ',int_arg2);
         writeln(errfil);
      end;
end;
$PAGE test_int_power
procedure test_int_power;
begin
   int_rslt:=int_arg1 ** int_arg2;
   if int_rslt <> int_lib_rslt then result_error(s_format);
   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on integer power:');
         writeln(tty,'   arg 1 = ',int_arg1,'   arg 2 = ',int_arg2);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on integer power:');
         writeln(errfil,'   arg 1 = ',int_arg1,'   arg 2 = ',int_arg2);
         writeln(errfil);
      end;
end;
$PAGE test_real_add
procedure test_real_add;
begin
   rslt:=arg1 + arg2;
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=d_arg1 + d_arg2;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on real add:');
         writeln(tty,'   arg 1 = ',d_arg1,'   arg 2 = ',d_arg2);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on real add:');
         writeln(errfil,'   arg 1 = ',d_arg1,'   arg 2 = ',d_arg2);
         writeln(errfil);
      end;

end;
$PAGE test_real_sub
procedure test_real_sub;
begin
   rslt:=arg1 - arg2;
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=d_arg1 - d_arg2;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on real subtract:');
         writeln(tty,'   arg 1 = ',d_arg1,'   arg 2 = ',d_arg2);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on real subtract:');
         writeln(errfil,'   arg 1 = ',d_arg1,'   arg 2 = ',d_arg2);
         writeln(errfil);
      end;

end;
$PAGE test_real_mult
procedure test_real_mult;
begin
   rslt:=arg1 * arg2;
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=d_arg1 * d_arg2;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on real multiplication:');
         writeln(tty,'   arg 1 = ',d_arg1,'   arg 2 = ',d_arg2);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on real multiplication:');
         writeln(errfil,'   arg 1 = ',d_arg1,'   arg 2 = ',d_arg2);
         writeln(errfil);
      end;

end;
$PAGE test_real_div
procedure test_real_div;
begin
   rslt:=arg1 / arg2;
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=d_arg1 / d_arg2;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on real division:');
         writeln(tty,'   arg 1 = ',d_arg1,'   arg 2 = ',d_arg2);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on real division:');
         writeln(errfil,'   arg 1 = ',d_arg1,'   arg 2 = ',d_arg2);
         writeln(errfil);
      end;

end;
$PAGE test_real_power
procedure test_real_power;
begin
   rslt:=arg1 ** arg2;
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=d_arg1 ** d_arg2;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on real power:');
         writeln(tty,'   arg 1 = ',d_arg1,'   arg 2 = ',d_arg2);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on real power:');
         writeln(errfil,'   arg 1 = ',d_arg1,'   arg 2 = ',d_arg2);
         writeln(errfil);
      end;

end;
$PAGE test_real_to_integer_power
procedure test_real_to_integer_power;
begin
   rslt:=arg1 ** int_arg2;
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=d_arg1 ** int_arg2;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on real to integer power:');
         writeln(tty,'   arg 1 = ',d_arg1,'   arg 2 = ',int_arg2);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on real to integer power:');
         writeln(errfil,'   arg 1 = ',d_arg1,'   arg 2 = ',int_arg2);
         writeln(errfil);
      end;

end;
$PAGE test_natlog
procedure test_natlog;
begin
   rslt:=ln(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=ln(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on natural log:');
         writeln(tty,'   arg = ',d_arg1);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on natural log:');
         writeln(errfil,'   arg = ',d_arg1);
         writeln(errfil);
      end;

end;
$PAGE test_log10
procedure test_log10;
begin
   rslt:=log(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=log(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on log10:');
         writeln(tty,'   arg = ',d_arg1);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on log10:');
         writeln(errfil,'   arg = ',d_arg1);
         writeln(errfil);
      end;

end;
$PAGE test_expon
procedure test_expon;
begin
   rslt:=exp(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=exp(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on exp:');
         writeln(tty,'   arg = ',d_arg1);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on exp:');
         writeln(errfil,'   arg = ',d_arg1);
         writeln(errfil);
      end;

end;
$PAGE test_sine
procedure test_sine;
begin
   rslt:=sin(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=sin(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on sine:');
         writeln(tty,'   arg = ',d_arg1);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on sine:');
         writeln(errfil,'   arg = ',d_arg1);
         writeln(errfil);
      end;

end;
$PAGE test_cosine
procedure test_cosine;
begin
   rslt:=cos(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=cos(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on cosine:');
         writeln(tty,'   arg = ',d_arg1);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on cosine:');
         writeln(errfil,'   arg = ',d_arg1);
         writeln(errfil);
      end;

end;
$PAGE test_tangent
procedure test_tangent;
begin
   rslt:=tan(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=tan(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on tangent:');
         writeln(tty,'   arg = ',d_arg1);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on tangent:');
         writeln(errfil,'   arg = ',d_arg1);
         writeln(errfil);
      end;

end;
$PAGE test_arcsine
procedure test_arcsine;
begin
   rslt:=arcsin(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=arcsin(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on arcsine:');
         writeln(tty,'   arg = ',d_arg1);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on arcsine:');
         writeln(errfil,'   arg = ',d_arg1);
         writeln(errfil);
      end;

end;
$PAGE test_arccosine
procedure test_arccosine;
begin
   rslt:=arccos(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=arccos(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on arccosine:');
         writeln(tty,'   arg = ',d_arg1);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on arccosine:');
         writeln(errfil,'   arg = ',d_arg1);
         writeln(errfil);
      end;

end;
$PAGE test_arctangent
procedure test_arctangent;
begin
   rslt:=arctan(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=arctan(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on arctangent:');
         writeln(tty,'   arg = ',d_arg1);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on arctangent:');
         writeln(errfil,'   arg = ',d_arg1);
         writeln(errfil);
      end;

end;
$PAGE test_arctan2
procedure test_arctan2;
begin
   rslt:=arctan(arg1,arg2);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=arctan(d_arg1,d_arg2);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on arctangent (two arg):');
         writeln(tty,'   arg 1 = ',d_arg1,'   arg 2 = ',d_arg2);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on arctangent (two arg):');
         writeln(errfil,'   arg 1 = ',d_arg1,'   arg 2 = ',d_arg2);
         writeln(errfil);
      end;

end;
$PAGE test_hsine
procedure test_hsine;
begin
   rslt:=sinh(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=sinh(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on sineh:');
         writeln(tty,'   arg = ',d_arg1);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on sineh:');
         writeln(errfil,'   arg = ',d_arg1);
         writeln(errfil);
      end;

end;
$PAGE test_hcosine
procedure test_hcosine;
begin
   rslt:=cosh(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=cosh(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on cosineh:');
         writeln(tty,'   arg = ',d_arg1);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on cosineh:');
         writeln(errfil,'   arg = ',d_arg1);
         writeln(errfil);
      end;

end;
$PAGE test_htangent
procedure test_htangent;
begin
   rslt:=tanh(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=tanh(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on tangenth:');
         writeln(tty,'   arg = ',d_arg1);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on tangenth:');
         writeln(errfil,'   arg = ',d_arg1);
         writeln(errfil);
      end;

end;
$PAGE test_sqroot
procedure test_sqroot;
begin
   rslt:=sqrt(arg1);
   d_rslt:=rslt;
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,s_format);
   d_rslt:=sqrt(d_arg1);
   strg_rslt:=hexonly(address(d_rslt),d_format);
   check_precision(d_d,strg_rslt,d_d,strg_lib_rslt,d_format);

   exception
      allconditions:
      begin
         msg_str:=exception_message;
         writeln(tty,msg_str);
         writeln(tty,'fatal error encountered on square root:');
         writeln(tty,'   arg = ',d_arg1);
         writeln(tty);
         writeln(errfil,msg_str);
         writeln(errfil,'fatal error encountered on square root:');
         writeln(errfil,'   arg = ',d_arg1);
         writeln(errfil);
      end;

end;

$PAGE exe017
begin

   rewrite(tty);
   rewrite(errfil);
   writeln(tty,'begin math test');
   break(tty);
   reset(matlib,[retry]);


(* The first section test specific function cases - i.e. sqrt(max value) *)

   special_tests;

(* This section is simply a case statement on the function to
   be used.  The result is translated to the standard hex string 
   format and then compared to the expected result using
   check_precision.  If the function is an integer function,
   the result is directly compared to the expected result since
   integers should be precise.  *)

   while read_next_record do
   begin

      if fctn = dummy then
      begin
         writeln(errfil,'unrecognized function skipped; function = ',
            funct_name);
         readln(matlib);   (* throw away rest of line *)
      end
      else case fctn of
         iadd:  test_int_add;
         isub:  test_int_sub;
         imult:  test_int_mult;
         idiv:  test_int_div;
         imod:  test_int_mod;
         ipower:  test_int_power;
         fadd:  test_real_add;
         fsub:  test_real_sub;
         fmult:  test_real_mult;
         fdiv:  test_real_div;
         fpower:  test_real_power;
         fipower:  test_real_to_integer_power;
         natlog:  test_natlog;
         log10:  test_log10;
         expon:  test_expon;
         sine:  test_sine;
         cosine:  test_cosine;
         tangent:  test_tangent;
         arcsine:  test_arcsine;
         arccosine:  test_arccosine;
         arctangent:  test_arctangent;
         arctan2:  test_arctan2;
         hsine:  test_hsine;
         hcosine:  test_hcosine;
         htangent:  test_htangent;
         sqroot:  test_sqroot;
      end;

   end;

   rantst;

   writeln(tty,'end math test');

end.
 
   @<¤