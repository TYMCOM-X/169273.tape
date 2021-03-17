(*$E+*)
(*******************************************************************
: **                  PROPRIETARY INFORMATION                     **
: **                                                              **
: **  This  source code listing constitutes the proprietary pro-  **
: **  perty of TYMNET.  The recipient, by receiving this program  **
: **  listing, agrees that neither this listing nor the informa-  **
: **  tion disclosed herein nor any part thereof shall be repro-  **
: **  duced or transferred to other documents or used or dis-     **
: **  closed to others for manufacturing or for any other purpose **
: **  except as specifically authorized in writing by TYMNET.     **
: ******************************************************************
: **                   PROGRAM IDENTIFICATION                     **
: **                                                              **
: **  Version Number     : 01.02         Release Date : 12/15/86  **
: **                                                              **
: **  File Name          : cpb101.pas                             **
: **                                                              **
: **  File Description   :                                        **
: **                                                              **
: **     This file contains common test output routines for       **
: **     concurrent and sequential pascal compilers.              **
: **     These routines generate token representations of the     **
: **     compiler intermediate pass files and append them to      **
: **     the listing file.  These routines are activated when     **
: **     option 't' is used in the source file.                   **
: **                                                              **
: **  File Abstract      :                                        **
: **                                                              **
: ******************************************************************
: **                      MAINTENANCE HISTORY                     **
: **                                                              **
: **  Ver   Date    By   PIR/NSR         Reason for Change        **
: ** ----- -------- ---  -------- ------------------------------  **
: ** 01.01 3/14/79  DEG           Original                        **
: ** 01.01 3/26/79  DEG   1162    Corrected len and made          **
: **                                 printeol accessible.         **
: ** 01.01 04/5/79  DEG   1162    Corrected use of chr in printff.**
: ** 01.01 03/17/81 BH    1162    Made output more readable.      **
: ** 01.02 12/15/86 PJH   1162    ADDED PROPRIETARY BANNER        **
: **                                                              **
: ******************************************************************
: **                 SUBROUTINE IDENTIFICATION                    **
: **                                                              **
: **  Routine Abstract   :  Newline                               **
: **                                                              **
: **     Newline sets the count of items printed (printed) to     **
: **     printlimit, so that the next time something gets printed **
: **     it will begin on the next line of the listing.           **
: **                                                              **
: ******************************************************************
: **  Routine Abstract   :  Printarg                              **
: **                                                              **
: **     Printarg prints the arg token to the listing file        **
: **                                                              **
: **  Parameters         :                                        **
: **                                                              **
: **     f   - packed file of ascii                               **
: **                                                              **
: **     arg - integer token that is a descriptor of an op token. **
: **                                                              **
: ******************************************************************
: **  Routine Abstract   :  Printeol                              **
: **                                                              **
: **     Printeol writes a newline character to the listing file  **
: **     and sets a count of items printed (printed) to 0.        **
: **                                                              **
: **  Parameters         :                                        **
: **                                                              **
: **     f - packed file of ascii                                 **
: **                                                              **
: ******************************************************************
: **  Routine Abstract   :  Printff                               **
: **                                                              **
: **     Printff prints out a form feed character if the compiler **
: **     is in the first pass (param p == 1) otherwise it         **
: **     prints out 132 alphanumeric representations of p.        **
: **                                                              **
: **  Parameters         :                                        **
: **                                                              **
: **     f - packed file of ascii                                 **
: **                                                              **
: **     p - pass_range (range of integer 0..7)                   **
: **                                                              **
: ******************************************************************
: **  Routine Abstract   :  Printop                               **
: **                                                              **
: **     Printop prints a character 'C' followed by the integer   **
: **     token (param op).                                        **
: **                                                              **
: **  Parameters         :                                        **
: **                                                              **
: **     f  - packed file of ascii                                **
: **                                                              **
: **     op - integer token representing an operator              **
: **                                                              **
: ******************************************************************
: **  Routine Abstract   :  Tmpnam                                **
: **                                                              **
: **     Tmpnam creates a unique name for a temporary file.       **
: **     using the parameter e and the process job number.        **
: **     The resultant filename is returned in the var parameter  **
: **     filename.                                                **
: **                                                              **
: **  Parameters         :                                        **
: **                                                              **
: **     e    - character                                         **
: **                                                              **
: **     name - filename (packed array[1..9] of char)             **
: **                                                              **
: *****************************************************************)

program common, newline, printarg, printeol, printff, printop, tmpnam;

const
  maxdigit = 6;
  printlimit = 18;

type
  atext = packed file of ascii;
  pass_range = 0 .. 7;
  filename = packed array[1..9] of char;

var
  printed: 0 .. printlimit;

function pasjob: integer; extern;

function len(arg: integer): integer;
var
  i, l: integer;
begin
  i := 1;  l := 0;
  repeat
    i := 10 * i;  l := succ(l)
  until
    i > arg;
  len := l
end   (* len *) ;

procedure printeol(var f: text);
begin
  writeln(f);  printed := 0
end;

procedure newline;
begin
  printed := printlimit
end;

procedure printarg(var f: text;  arg: integer);
var
  l: integer;
begin
  if printed = printlimit then printeol(f);
  if arg >= 0
  then write(f, ' ')
  else
    begin
      write(f, '-');  arg := - arg
    end;
  l := len(arg);
  write(f, arg:l, ' ':maxdigit-l);
  printed := succ(printed)
end   (* printarg *) ;

procedure printff(var f: text;  p: pass_range);
var
  c: char;
  i: 1 .. 132;
begin
  if p = 1
  then
    begin
      page(f);  printed := 0
    end
  else
    begin
      c := chr(p+ord('0'));
      printeol(f);
      for i := 1 to 132 do write(f, c)
    end
end   (* printff *) ;

procedure printop(var f: text;  op: integer);
var
  l: integer;
begin
  printeol(f);
  l := len(op);
  write(f, 'C', op:l, ' ':maxdigit-l);
  printed := succ(printed)
end   (* printop *) ;

procedure tmpnam(e: char; var name: filename);
var
  jobnum, i: integer;

begin
  jobnum := pasjob;
  name[1] := e;
  name[2] := 'p';
  name[3] := 's';
  for i := 6 downto 4 do begin
    name[i] := chr(ord('0')+(jobnum mod 10));
    jobnum := jobnum div 10
  end;
  name[7] := 't';
  name[8] := 'm';
  name[9] := 'p'
end;

begin end.
    