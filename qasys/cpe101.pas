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
: **  File Name          : cpe101.pas                             **
: **                                                              **
: **  File Description   :                                        **
: **                                                              **
: **     This file contains the Concurrent Pascal error messages  **
: **     and the routines which generate them.                    **
: **                                                              **
: **  File Abstract      :                                        **
: **                                                              **
: ******************************************************************
: **                      MAINTENANCE HISTORY                     **
: **                                                              **
: **  Ver   Date    By   PIR/NSR         Reason for Change        **
: ** ----- -------- ---  -------- ------------------------------  **
: ** 01.02 12/15/86 PJH  1162     ADDITION OF PROPRIETARY BANNER  **
: **                                                              **
: ******************************************************************
: **                 SUBROUTINE IDENTIFICATION                    **
: **                                                              **
: **  Routine Abstract   :  Errmess                               **
: **                                                              **
: **     Errmess returns a pointer to a string in the var         **
: **     parameter outmess.  This string describes the error      **
: **     condition indicated by parameters pass and number.       **
: **                                                              **
: **  Parameters         :                                        **
: **                                                              **
: **     pass    - an integer indicating which pass of the        **
: **               compiler the error message originated from.    **
: **                                                              **
: **     number  - a number indicating the type of error          **
: **               encountered.                                   **
: **                                                              **
: **     outmess - returns a string describing the error which    **
: **               was encountered.                               **
: **                                                              **
: ******************************************************************
: **                                                              **
: **  Routine Abstract   :  Giverr                                **
: **                                                              **
: **     This routine generates error messages.                   **                                                              **
: **                                                              **
: **  Parameters         :                                        **
: **                                                              **
: **     line   -  an integer indicating the line number in the   **
: **               concurrent pascal source where the error was   **
: **               encountered.                                   **
: **                                                              **
: **     pass   -  an integer indicating the pass in which the    **
: **               error was found.                               **
: **                                                              **
: **     number -  a number indicating the error encountered.     **
: **                                                              **
: *****************************************************************)

(*$e+*)
program emessage, errmess, giverr;

type 
  messtype = packed array[1..25] of char;

var
  firsterr: boolean;
  curpass: integer;
  prevline: integer;

procedure errinit;
begin
  firsterr := true;
  curpass := 0;
  prevline := 0
end;

procedure errmess(pass, number: integer; var outmess: messtype);
begin
case pass of 
  1: case number of (* pass 1 *)
    1: outmess := 'endless comment          ';
    2: outmess := 'invalid number           ';
    3: outmess := 'table overflow           ';
    4: outmess := 'invalid string           ';
    5: outmess := 'bad character            '
  end (* case *) ;

  2: case number of (* pass 2 *)
    1: outmess := 'sequential program       ';
    2: outmess := 'declaration              ';
    3: outmess := 'constant definition      ';
    4: outmess := 'type definition          ';
    5: outmess := 'type                     ';
    6: outmess := 'enumeration type         ';
    7: outmess := 'subrange type            ';
    8: outmess := 'set type                 ';
    9: outmess := 'array type               ';
   10: outmess := 'record type              ';
   11: outmess := 'stack length             ';
   12: outmess := 'variable declaration     ';
   13: outmess := 'routine                  ';
   14: outmess := 'procedure                ';
   15: outmess := 'function                 ';
   16: outmess := 'with statement           ';
   17: outmess := 'parameter                ';
   18: outmess := 'body                     ';
   19: outmess := 'statement list           ';
   20: outmess := 'statement                ';
   21: outmess := 'id statement             ';
   22: outmess := 'argument                 ';
   23: outmess := 'compound statement       ';
   24: outmess := 'if statement             ';
   25: outmess := 'case statement           ';
   26: outmess := 'label list               ';
   27: outmess := 'while statement          ';
   28: outmess := 'repeat statement         ';
   29: outmess := 'for statement            ';
   30: outmess := 'cycle statement          ';
   31: outmess := 'expression               ';
   32: outmess := 'variable                 ';
   33: outmess := 'constant                 ';
   34: outmess := 'init statement           ';
   35: outmess := 'termination              ';
   36: outmess := 'pointer type             ';
   37: outmess := 'prefix                   ';
   38: outmess := 'interface                '
  end (* case *) ;

  3: case number of (* pass 3 *)
    1: outmess := 'unresolved routine       ';
    2: outmess := 'ambiguous identifier     ';
    3: outmess := 'compiler abort           ';
    4: outmess := 'invalid constant         ';
    5: outmess := 'invalid subrange         ';
    6: outmess := 'missing argument         ';
    7: outmess := 'not a routine            ';
    8: outmess := 'too many arguments       ';
    9: outmess := 'label value too large    ';
   10: outmess := 'invalid label            ';
   11: outmess := 'ambiguous label          ';
   12: outmess := 'invalid with variable    ';
   13: outmess := 'invalid initialization   ';
   14: outmess := 'not a function           ';
   15: outmess := 'invalid name usage       ';
   16: outmess := 'invalid selection        ';
   17: outmess := 'invalid subscripting     ';
   18: outmess := 'invalid interface        ';
   19: outmess := 'invalid call             ';
   20: outmess := 'division by zero         ';
   21: outmess := 'invalid dereference      ';
   22: outmess := 'invalid resolution       ';
  end (* case *) ;

  4: case number of (* pass 4 *)
    1: outmess := 'invalid nesting          ';
    2: outmess := 'address overflow         ';
    3: outmess := 'active variable          ';
    4: outmess := 'queue variable           ';
    5: outmess := 'nested process           ';
    6: outmess := 'invalid entry variable   ';
    7: outmess := 'invalid function type    ';
    8: ;
    9: outmess := 'record enumeration       ';
   10: outmess := 'long enumeration         ';
   11: outmess := 'invalid index type       ';
   12: outmess := 'invalid member type      ';
   13: outmess := 'process stack usage      ';
   14, 15, 16, 17, 18, 19, 20:
       outmess := 'invalid parameter        ';
   21: outmess := 'compiler abort           ';
   22: outmess := 'odd length string type   ';
   23: outmess := 'invalid resolution       ';
   24: outmess := 'invalid tag type         ';
   25: outmess := 'record pointer type      '
  end (* case *) ;

  5: case number of (* pass 5 *)
    1: outmess := 'compiler abort           ';
    2: outmess := 'operand type             ';
    3: outmess := 'not a variable           ';
    4: outmess := 'not assignable           ';
    5: outmess := 'invalid initialization   ';
    6: outmess := 'string argument too long '
  end (* case *) ;

  6: case number of (* pass 6 *)
    1: outmess := 'too much stack           ';
    2: outmess := 'too much code            ';
    3: outmess := 'out of registers         ';
    4: outmess := 'not implemented          ';
    5: ; (* this is the bugbug message *)
    6: outmess := 'range error              ';
    7: outmess := 'arithmetic overflow      ';
    8: outmess := 'division by zero         '
  end (* case *) 
end; (* case *)
end; (* errmess *)

procedure giverr(line, pass, number: integer);
var
  mess: messtype;
begin
  if line < prevline
  then curpass := succ(curpass);
  if pass > curpass
  then begin
    curpass := pass
  end;
  if (pass=6) and (number=5) (* the bug message *)
  then if firsterr
       then begin
	 writeln(tty);
	 writeln(tty, '*** PASCAL Compiler Bug at line ', line:7, ' ***');
	 writeln(tty, 'Please contact your representative.  Sorry.');
         halt;
       end
       else
  else if (curpass=pass) and (line<>prevline)
  then begin
    errmess(pass, number, mess);
    writeln(tty);
    writeln(tty, '****** line', line:7, ' ', mess);
    break(tty)
  end;
  firsterr := false;
  prevline := line
end;

begin end.
 