(*$M-,R-,T-,C+,L+*)
(********** command line module **********)






const
   commandlinelen=100; (* commmand line length *)
   listlen=100; (* length of lists *)
   tab=9;
   null=0;
   cr=13;
   lf=10;





type
   integer8=0..255;
   integer16=0..32767;
   charlist=packed array [1..listlen] of char; 




var
   cmdline : packed array [1..commandlinelen] of char; (* command line *)
   cmdlineptr : integer8; (* pointer to command line buffer *)
   cmdchar : char; (* character at pointer position *)
   fullcmdname : boolean; (* true - total match  false - paritial match *)
   cmdnamelen : integer8; (* length of command name found *)
   tokenlist : charlist; (* list of tokens *)










(************************************************************)










function uppercase (c : char (* character to change *)) : char;

(* this procedure returns the argument changed to uppercase *)



begin

if (c>='a') and (c<='z') then
   uppercase:=chr(ord(c)-32) (* it's lower case, change *)

else
   uppercase:=c (* leave alone *)

end (* uppercase *);










(************************************************************)










function charfrom (var listptr : integer8; (* location to get character from *)
		     entrylist : charlist; (* list to get character from *)
			 var c : char) (* character at that location *)
			       : char; (* character returned *)

(* this procedure returns a character from a list at the posiition
pointed to by one of the paramaters.  it also increments that paramater
and returns the character found in another parameter *)



begin
c:=entrylist[listptr]; (* get character *)
listptr:=listptr+1; (* move to next one *)
charfrom:=c (* return character *)
end (* charfrom *);










(************************************************************)










function ptrtopart (n : integer8; (* entry in list *)
              cmdlist : charlist) (* list *)
                      : integer8; (* pointer to entry *)

(* this procedure returns a pointer to the nth entry
of a list *)



var
   delimiter : char; (* delimiter between commands *)
   listptr : integer8; (* position in list *)
   step : integer8; (* count down *)



begin
listptr:=1; (* first character in list *)
delimiter:=cmdlist[1]; (* is the delimiter between entries *)
step:=n; (* start *)

(* go through till the nth one *)
while step>0 do
   begin
   step:=step-1; (* next one *)
   listptr:=listptr+1; (* skip past delimiter *)

   (* go past whole entry *)
   while cmdlist[listptr]<>delimiter do
      listptr:=listptr+1 (* move *)

   end;

ptrtopart:=listptr (* return pointer to entry *)
end (* ptrtopart *);










(************************************************************)










procedure setcmd (lineptr : integer8);

(* procedure to set current position in line *)



begin
cmdlineptr:=lineptr; (* set to position indicated *)
cmdchar:=cmdline[cmdlineptr] (* character at that position *)
end (* setcmd *);










(************************************************************)










procedure inccmd;

(* procedure to advance one position in line *)



begin
cmdlineptr:=cmdlineptr+1; (* move to next position *)
cmdchar:=cmdline[cmdlineptr] (* character at that position *)
end (* inccmd *);










(************************************************************)










function alpha (c : char (* character to be tested *)) : boolean;

(* test for alpha chars
returns true if character is an alphabetic character *)


var
  tempc: char;

begin
tempc := uppercase(c);
alpha := (tempc >= 'A') and (tempc <= 'Z')
end (* alpha *);










(************************************************************)










procedure skipblanks;

(* procedure to scan past white space - blanks, tabs, and nulls *)



begin

while (cmdchar=' ') or (cmdchar=chr(tab)) or (cmdchar=chr(null)) do
   inccmd (* skip *)

end (* skipblanks *);










(************************************************************)










function checknumber : boolean;

(* procedure to check if current character in command line is a digit
returns true if it is a digit *)



begin
checknumber := (cmdchar >= '0') and (cmdchar <= '9')
end (* checknumber *);










(************************************************************)










function cmdget (cmdlist : charlist) (* list of commands *) 
			        : integer8; (* returned command number *)

label
  100;
(* get a command from a line and look it up in a command list.
if a match is found the line pointer is positioned after
the last character in the command and the number of the command
in the list is returned.  otherwise the line pointer is 
not moved and a zero is returned *)



var
   lineptrtemp : integer8; (* beginning of command in line *)
   delimiter : char; (* delimiter between list entries *)
   endofcmdlist : char; (* character to mark end of command list *)
   n : integer8; (* number of command in list *)
   tempcmdlistptr : integer8; (* pointer within command list *)
   savelineptr : integer8; (* start of command in line *)
   templineptr : integer8; (* pointer within command line *)
   i,j : integer8;
   cmdnamechar : char; (* character at current position in command list *)



   function alphanumeric : boolean;
   (* this procedure tells if the character in the line is an alphabetic
   or numeric character *)

   begin
   alphanumeric:=checknumber or alpha(cmdchar)
   end (* alphanumeric *);



begin
lineptrtemp:=cmdlineptr; (* start of command *)
delimiter:=charfrom(tempcmdlistptr,cmdlist,delimiter); (* delimiter between *)
endofcmdlist:=charfrom(tempcmdlistptr,cmdlist,delimiter); (* end of list *)
n:=1; (* first command *)
tempcmdlist := ptrtopart(n,cmdlist);
tempcmdlist := tempcmdlist + 1;
skipblanks; (* start of command in line *)
savelineptr:=cmdlineptr; (* save it *)

(* do for each command in command list *)
while charfrom(tempcmdlistptr,cmdlist,cmdnamechar)<>endofcmdlist do
   begin
   cmdchar:=uppercase(charfrom(templineptr,cmdline,cmdchar)); (* from line *)

   if cmdchar=cmdnamechar then
      begin (* match *)

      if not alphanumeric then
         begin (* one character command *)
         fullcmdname:=true; (* found all of it *)
         cmdlineptr:=templineptr; (* move line pointer *)
         cmdnamelen:=1; (* length one *)
         cmdget:=n; (* return command number *)
         goto 100
         end

      else
         begin (* more than one character *)
         cmdnamelen:=0; (* ready to loop through command *)

         (* loop through command line and list, looking for matches *)
         while (cmdchar=cmdnamechar) and alphanumeric do
            begin
            cmdnamelen:=cmdnamelen+1; (* one more match *)
            (* get next characters *)
            cmdchar:=uppercase(charfrom(templineptr,cmdline,cmdchar)); 
            cmdnamechar:=charfrom(tempcmdlistptr,cmdlist,cmdnamechar); 
            end;

         if not alphanumeric then
            begin (* matched the entire command in command line *)
            fullcmdname:=(cmdnamechar=delimiter); (* found it all? *)
            cmdlineptr:=templineptr-1; (* move line pointer *)
            cmdget:=n; (* return command number *)
            goto 100
            end

         end

      end;

   n:=n+1; (* next command *)
   tempcmdlist := ptrtopart(n,cmdlist);
   tempcmdlist := tempcmdlist + 1;
   templineptr:=savelineptr (* back to start of command in command line *)
   end;

setcmd(savelineptr); (* backup, not found *)
cmdget:=0; (* return zero *)
100:end (* cmdget *);










(************************************************************)










function checktoken (tokennum : integer8) (* token to be matched *)
				     : boolean;

(* procedure to check if a literal token appears in a line.
if a match is found the function returns true and the line pointer
is positioned at the character after the token.  otherwise
it returns false and the line pointer remains unchanged *)



var
   lineptrtemp : integer8; (* beginning of token *)
   idx : integer8; (* number of token actually matched *)



begin
   lineptrtemp:=cmdlineptr; (* start of token *)
   idx:=cmdget(tokenlist); (* find token *)

   if fullcmdname and (idx=tokennum) then (* is it the one we wanted? *)
      checktoken:=true (* yes *)

   else
      begin (* no *)
      setcmd(lineptrtemp); (* backup *)
      checktoken:=false
      end

end (* checktoken *);










(************************************************************)










function checkeoln : boolean;

(* procedure to check for the end of line
returns true on end of line.  line pointer is not moved if not end of line *)



var
   lineptrtemp : integer8; (* we are here now *)



begin
lineptrtemp:=cmdlineptr; (* where we are at *)
skipblanks;
if (cmdchar<>chr(cr)) and (cmdchar<>chr(lf)) then

   begin (* not end of line *)
   setcmd(lineptrtemp); (* backup *)
   checkeoln:=false
   end

else
   checkeoln:=true (* end of line *)

end (* checkeoln *);










(************************************************************)










function checkpunctuation (punctchar : char) (* character looked for *)
					    : boolean;

(* procedure to check for a specific puncation mark.  if it
is found the function returns true and line pointer is positioned past 
the puncation mark unless it is a carraige return.  otherwise the line  
pointer is not changed and the function returns false *)


label
  100;

var
   lineptrtemp : integer8; (* we are here now *)



begin
   lineptrtemp:=cmdlineptr; (* where we are at *)
   if punctchar=' ' then
      begin (* looking for a blank *)
      if (cmdchar<>' ') and (cmdchar<>chr(tab)) then
         begin (* no blanks *)
         checkpunctuation:=false;
         goto 100
         end;
      skipblanks (* move past the blanks *)
      end
   else
      begin (* not looking for blanks *)
      skipblanks;
      if cmdchar<>punctchar then
         begin (* didn't find it *)
         setcmd(lineptrtemp); (* backup *) 
         checkpunctuation:=false;
         goto 100
         end;
      if cmdchar<>chr(cr) then
         inccmd (* move one more if not carraige return *) 
      end;
   checkpunctuation:=true; (* found *)
100:end (* checkpunctation *);










(************************************************************)










function readnumber : integer16;

(* procedure to read a number from a line, returns
zero in case no number is found *)


var
  readtemp: integer16;

begin
skipblanks;
readtemp:=0; (* zero to start *)
while checknumber do
   begin (* it is a digit *)
   readtemp:=(readtemp*10)+(ord(cmdchar)-ord('0')); (* add to number *)
   inccmd (* next digit *)
   end;
readnumber := readtemp
end (* readnumber *).
    