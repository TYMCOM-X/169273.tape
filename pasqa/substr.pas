const
   mant_size:=
$IF m68       14;
$IF p10       16;
$IF vax       14;
   str_size:=
$IF m68       22;
$IF p10       24;
$IF vax       22;
   zero_str:=
$IF m68       '[+00  +00000000000000]';
$IF p10       '[+00  +0000000000000000]';
$IF vax       '[+00  +00000000000000]';

public function subtract_string(
                   strg1:string[*];
                   strg2:string[*]
                   ):string[str_size];

(* This function subtracts strg2 from strg1 and puts the result
   in subtract_string.

   Strings are of the form:
      [see  smmmmmmmmmmmmmmmm]
      (a total of str_size characters including mant_size matissa characters)

   The value returned is normalized.
                                                            *)

type
   hex_digit = 0..15;
   table_entry = record
      result:hex_digit;
      carry:boolean;
   end;

const
   r_shift_table:array[hex_digit,boolean] of table_entry := (
      ((0,false),(8,false)),((0,true),(8,true)),
      ((1,false),(9,false)),((1,true),(9,true)),
      ((2,false),(10,false)),((2,true),(10,true)),
      ((3,false),(11,false)),((3,true),(11,true)),
      ((4,false),(12,false)),((4,true),(12,true)),
      ((5,false),(13,false)),((5,true),(13,true)),
      ((6,false),(14,false)),((6,true),(14,true)),
      ((7,false),(15,false)),((7,true),(15,true))
      );
   l_shift_table:array[hex_digit,boolean] of table_entry := (
      ((0,false),(1,false)),((2,false),(3,false)),
      ((4,false),(5,false)),((6,false),(7,false)),
      ((8,false),(9,false)),((10,false),(11,false)),
      ((12,false),(13,false)),((14,false),(15,false)),
      ((0,true),(1,true)),((2,true),(3,true)),
      ((4,true),(5,true)),((6,true),(7,true)),
      ((8,true),(9,true)),((10,true),(11,true)),
      ((12,true),(13,true)),((14,true),(15,true))
      );

procedure equalize_magnitude(var str1:string[*];
                             var str2:string[*]);

function shift(instring:string[*];
               shift_count:integer
               ):string[mant_size+1];
var
   outer_count:integer;
   inner_count:integer;
   temprec:table_entry;
   last_carry:boolean;
   char_result:string[1];
   dgt:hex_digit;
   tempstr:string[mant_size+1];

begin
   shift:=instring;
   for outer_count:=1 to shift_count do
   begin
      last_carry:=false;
      tempstr:='';
      for inner_count:=1 to mant_size+1 do
      begin
         getstring(substr(shift,inner_count,1),dgt:1:h);
         temprec:=r_shift_table[dgt,last_carry];
         putstring(char_result,temprec.result:1:h);
         tempstr:=tempstr || char_result;
         last_carry:=temprec.carry;
      end;
      shift:=tempstr;
   end;
end;  (* of shift *)

var
   tempi:integer;
   mag1:integer;
   mag2:integer;
   tempstr:string[str_size];

begin
   getstring(substr(str1,3,2),mag1:2:h);
   if (substr(str1,2,1) = '-') then mag1:=-mag1;
   getstring(substr(str2,3,2),mag2:2:h);
   if (substr(str2,2,1) = '-') then mag2:=-mag2;
   tempi:=mag1 - mag2;
   if tempi <> 0 then
      if (tempi > 0) then   (* str1 is greater *)
         str2:=substr(str1,1,6) || substr(str2,7,1) ||
            shift(substr(str2,8,mant_size+1),tempi) || ']'
      else
         str1:=substr(str2,1,6) || substr(str1,7,1) ||
            shift(substr(str1,8,mant_size+1),abs(tempi)) || ']';
end;  (* of equalize_magnitude *)


procedure normalize_string(var strg:string[*]);
var
   ind:integer;
   exp:integer;
   exp_char:string[2];
   sign:char;
   i:integer;
   j:integer;
   dgt:hex_digit;
   tempstr:string[mant_size];
   temprec:table_entry;
   last_carry:boolean;
   char_result:string[1];

begin
   ind:=verify(substr(strg,8),['0']);  (* find first non-zero digit *)
   if (ind = str_size) then strg:=zero_str
   else
   begin
      getstring(substr(strg,7+ind,1),dgt:1:h);
      ind:=(ind-1) * 4;
      if (dgt >= 8) then ind:=ind + 0
      else if (dgt >= 4) then ind:=ind + 1
      else if (dgt >= 2) then ind:=ind + 2
      else ind:=ind+3;
      if (ind > 0) then
      begin
         for i:=1 to ind do
         begin
            tempstr:='';
            last_carry:=false;
            for j:=1 to mant_size do
            begin
               getstring(substr(strg,str_size-j,1),dgt:1:h);
               temprec:=l_shift_table[dgt,last_carry];
               putstring(char_result,temprec.result:1:h);
               tempstr:=char_result || tempstr;
               last_carry:=temprec.carry;
            end;
            strg:=substr(strg,1,7) || tempstr || ']';
         end;
         getstring(substr(strg,3,2),exp:2:h);
         if (substr(strg,2,1) = '-') then exp:=-exp;
         exp:=exp - ind;
         if (exp >= 0) then sign:='+' else sign:='-';
         exp:=abs(exp);
         putstring(exp_char,exp:2:h);
         strg:='[' || sign || exp_char || substr(strg,5,str_size-4);
      end;
   end;
end;  (* of normalize_string *)



function subtract_mant(mant1:string[*];
                       mant2:string[*]
                       ):string[mant_size+1];
var
   subtract_entry:table_entry;
   i:integer;
   last_carry:boolean;
   dgt1:hex_digit;
   dgt2:hex_digit;
   char_result:string[1];
   temp:integer;

begin
   last_carry:=false;
   subtract_mant:='';
   for i:=1 to mant_size+1 do
   begin
      getstring(substr(mant1,mant_size+2-i,1),dgt1:1:h);
      getstring(substr(mant2,mant_size+2-i,1),dgt2:1:h);
      (* subtract_entry:=subtract_table[dgt1,dgt2,last_carry]; *)
      temp:=dgt1 - dgt2;
      if last_carry then temp:=temp - 1;
      if temp < 0 then 
      begin
         temp:=temp + 16;
         subtract_entry.carry:=true;
      end
      else subtract_entry.carry:=false;
      subtract_entry.result:=temp;
      putstring(char_result,subtract_entry.result:1:h);
      subtract_mant:=char_result || subtract_mant;
      last_carry:=subtract_entry.carry;
   end;
end;  (* of subtract mantissa *)



function add_mant(mant1:string[*];
                  mant2:string[*];
                  var ovrflow:boolean
                  ):string[mant_size+1];
var
   add_entry:table_entry;
   i:integer;
   last_carry:boolean;
   dgt1:hex_digit;
   dgt2:hex_digit;
   tempstr:string[mant_size+1];
   char_result:string[1];
   temp:integer;

begin
   ovrflow:=false;
   add_mant:='';
   last_carry:=false;
   for i:=1 to mant_size+1 do
   begin
      getstring(substr(mant1,mant_size+2-i,1),dgt1:1:h);
      getstring(substr(mant2,mant_size+2-i,1),dgt2:1:h);
      (* add_entry:=add_table[dgt1,dgt2,last_carry]; *)
      temp:=dgt1 + dgt2;
      if last_carry then temp:=temp + 1;
      if temp > 15 then
      begin
         add_entry.carry:=true;
         temp:=temp - 16;
      end
      else add_entry.carry:=false;
      add_entry.result:=temp;
      putstring(char_result,add_entry.result:1:h);
      add_mant:=char_result || add_mant;
      last_carry:=add_entry.carry;
   end;
   if last_carry then
   begin
      tempstr:='';
      for i:=1 to mant_size+1 do
      begin
         getstring(substr(add_mant,i,1),dgt1:1:h);
         add_entry:=r_shift_table[dgt1,last_carry];
         putstring(char_result,add_entry.result:1:h);
         tempstr:=tempstr || char_result;
         last_carry:=add_entry.carry;
      end;
      add_mant:=tempstr;
      ovrflow:=true;
   end;
end;  (* of add_mant *)





var
   min_pos:boolean;
   ops_equal:boolean;
   i:integer;
   exp:integer;
   expstr:string[2];
   str1_gtr:boolean;
   my_str1:string[str_size+1];
   my_str2:string[str_size+1];
   sign1:string[1];
   sign2:string[1];
   mant1:string[mant_size+1];
   mant2:string[mant_size+1];
   mant_rslt:string[mant_size+1];
   dgt1:hex_digit;
   dgt2:hex_digit;
   overflow:boolean;

begin  (* mainline *)
(* Move the user strings into temporary strings, adding a guard
   digit to each.  The guard digit is used throughout subtract_string
   until the result is moved to the string returned.  Normalize does not
   expect the guard digit.   *)
   my_str1:=substr(strg1,1,str_size-1) || '0]';
   my_str2:=substr(strg2,1,str_size-1) || '0]';
   equalize_magnitude(my_str1,my_str2);
   sign1:=substr(my_str1,7,1);
   sign2:=substr(my_str2,7,1);
   ops_equal:=false;
   if (sign1 = '+') and (sign2 = '-') then str1_gtr:=true
   else if (sign1 = '-') and (sign2 = '+') then str1_gtr:=false
   else
   begin
      i:=8;
      ops_equal:=true;
      repeat
         if substr(my_str1,i,1) <> substr(my_str2,i,1) then
            ops_equal:=false;
         i:=i+1;
      until not(ops_equal) or (i = str_size+1);
      if not(ops_equal) then
      begin
         getstring(substr(my_str1,i-1,1),dgt1:1:h);
         getstring(substr(my_str2,i-1,1),dgt2:1:h);
         if dgt1 < dgt2 then str1_gtr:=false
            else str1_gtr:=true;
      end;
   end;
   if ops_equal and (sign1 = sign2) then subtract_string:=zero_str
   else
   begin
      mant1:=substr(my_str1,8,mant_size+1);
      mant2:=substr(my_str2,8,mant_size+1);
      if (sign1 = sign2) then
      begin
         if str1_gtr then
            mant_rslt:=subtract_mant(mant1,mant2)
         else mant_rslt:=subtract_mant(mant2,mant1);
         if ((str1_gtr and (sign1 = '+')) or
             (not(str1_gtr) and (sign2 = '-'))) then
            sign1:='+'
         else sign1:='-';
         subtract_string:=substr(my_str1,1,6) || sign1 || substr(mant_rslt,1,mant_size) || ']';
      end
      else
      begin
         mant_rslt:=add_mant(mant1,mant2,overflow);
         if overflow then
         begin
            getstring(substr(my_str1,3,2),exp:2:h);
            if substr(my_str1,2,1) = '-' then exp:=-exp;
            exp:=exp + 1;
            if exp < 0 then sign2:='-' else sign2:='+';
            putstring(expstr,abs(exp):2:h);
            subtract_string:='[' || sign2 || expstr || substr(my_str1,5,3)
                  || substr(mant_rslt,1,mant_size) || ']';
         end
         else
            subtract_string:=substr(my_str1,1,7) || substr(mant_rslt,1,mant_size) || ']';
      end;
      normalize_string(subtract_string);
   end;
end.  (* of subtract_string *)
   