(*$r122*************************************************************
: **                                                              **
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
: **  File Name          : CP0101.PAS                             **
: **                                                              **
: **  File Description   :                                        **
: **                                                              **
: **     Driver for Concurrent Pascal compiler.                   **
: **                                                              **
: **  File Abstract      :                                        **
: **                                                              **
: ******************************************************************
: **                CONFIGURATION SPECIFICATION                   **
: **                                                              **
: **  Core Memory Requirement: 150                                **
: **                                                              **
: **  Basic Code Size: 90                                         **
: **                                                              **
: **  Estimated Code Growth for Next 12 Months: 0%                **
: **                                                              **
: **  Other Software Requirements:                                **
: **                                                              **
: **     Files :                                                  **
: **                                                              **
: **        cp1101.pas   cp2101.pas   cp3101.pas   cp4101.pas     **
: **        cp5101.pas   cp6101.pas   cp7101.pas   cpx101.pas     **
: **        cpa101.pas   cpb101.pas   cpc101.pas   cpd101.pas     **
: **        cpe101.pas   cpf101.mac   cpg101.mac                  **
: **                                                              **
: ******************************************************************
: **                   DOCUMENT SPECIFICATION                     **
: **                                                              **
: **  Software Requirements Specification       :                 **
: **                                                              **
: **  Functional Specification (GED)            :                 **
: **                                                              **
: **  Other         :                                             **
: **                                                              **
: **     (peggyh)epas.doc                                         **
: **                                                              **
: ******************************************************************
: **                      MAINTENANCE HISTORY                     **
: **                                                              **
: **  Ver   Date    By   PIR/NSR         Reason for Change        **
: ** ----- -------- ---  -------- ------------------------------  **
: ** 01.01 03/14/79 DEG  1162     Original                        **
: ** 01.01 08/30/79 DEG  1162     Let Pascal open Object, List,   **
: **                              and Source                      **
: ** 01.01 07/24/81 BH   1162     Changed TMP files to include    **
: **                              job number.                     **
: ** 01.02 12/15/86 PJH  1162     ADDED PROPRIETARY BANNER        **
: **                                                              **
: ******************************************************************
: **                 SUBROUTINE IDENTIFICATION                    **
: **                                                              **
: **  Routine Abstract   :  initialize                            **
: **                                                              **
: **     Initialize prints out the compiler identity message and  **
: **     opens the temporary files for writing.                   **
: **                                                              **
: ******************************************************************
: **                                                              **
: **  Routine Abstract   :  newini                                **
: **                                                              **
: **     Newini initializes the heap pointers for routines new    **
: **     and dispose.                                             **
: **                                                              **
: ******************************************************************
: **                                                              **
: **  Routine Abstract   :  cpas1e                                **
: **                                                              **
: **     The first pass of the concurrent pascal compiler.        **
: **     Lexical analysis.                                        **
: **                                                              **
: ******************************************************************
: **                                                              **
: **  Routine Abstract   :  cpass2                                **
: **                                                              **
: **     The second pass of the concurrent pascal compiler.       **
: **     Syntax analysis.                                         **
: **                                                              **
: ******************************************************************
: **                                                              **
: **  Routine Abstract   :  cpas3e                                **
: **                                                              **
: **     The third pass of the concurrent pascal compiler.        **
: **     Scope analysis.                                          **
: **                                                              **
: ******************************************************************
: **                                                              **
: **  Routine Abstract   :  cpas4e                                **
: **                                                              **
: **     The fourth pass of the concurrent pascal compiler.       **
: **     Declaration analysis.                                    **
: **                                                              **
: ******************************************************************
: **                                                              **
: **  Routine Abstract   :  cpas5e                                **
: **                                                              **
: **     The fifth pass of the concurrent pascal compiler.        **
: **     Concurrent body semantic analysis.                       **
: **                                                              **
: ******************************************************************
: **                                                              **
: **  Routine Abstract   :  pass61                                **
: **                                                              **
: **     The sixth pass of the concurrent pascal compiler.        **
: **     Selection of engine instructions to be generated.        **
: **                                                              **
: ******************************************************************
: **                                                              **
: **  Routine Abstract   :  passx                                 **
: **                                                              **
: **     The cross reference pass falls between pass 6 and pass   **
: **     7 in the concurrent pascal compiler.  The code for       **
: **     producing a cross reference listing is activated by      **
: **     using option 'x' or option 'r' in the concurrent pascal  **
: **     source file.                                             **
: **                                                              **
: ******************************************************************
: **                                                              **
: **  Routine Abstract   :  pass7e                                **
: **                                                              **
: **     The seventh pass of the concurrent pascal compiler.      **
: **     Generation of engine assembly code.                      **
: **                                                              **
: *****************************************************************)

program cpas1(object, list, source*);

const
  identity = 'CPASCAL/ENGINE-1 of 11 JUNE MCMLXXXVI';

type
  int_file = file of integer;
  goodfile = packed file of ascii;
  pointer = ^ integer;
  passptr = ^ passlink;
  passlink = record
               options: set of 0 .. 8;
               labels, blocks, constants: integer;
               resetpoint: pointer;
               tables: pointer
             end;
  filename = packed array[1..9] of char;

var
  source, temp, list: goodfile;
  temp1, temp2, temp3, object: int_file;
  ptr: passptr;
  t1name, t2name, t3name, t4name: filename;
  

(**********************************************************************)
(*                                                                    *)
(*                            externals                               *)
(*                                                                    *)
(**********************************************************************)

procedure newini; extern;

procedure cpas1e(var p: passptr;
                 var source, list: goodfile;
	         var ilout: int_file;
		 var xref: int_file);  extern;

procedure cpass2(p: passptr;
                 var ilin, ilout: int_file;
                 var list: goodfile);  extern;

procedure cpas3e(p: passptr;
                 var ilin, ilout: int_file;
                 var list: goodfile;
		 var xref: int_file);  extern;

procedure cpas4e(p: passptr;
                 var ilin, ilout: int_file;
                 var list: goodfile;
		 var xref: int_file);  extern;

procedure cpas5e(p: passptr;
                 var ilin, ilout: int_file;
                 var list: goodfile);  extern;

procedure pass61(p: passptr;
                 var ilin, ilout: int_file;
                 var list: goodfile;
                 concurrent: boolean);  extern;

procedure  passx(p: passptr;
                 var list: goodfile;
                 var temp: goodfile;
		 var ilin: int_file);  extern;

procedure pass7e(p: passptr;
                 var ilin, object: int_file;
                 var list: goodfile);  extern;

procedure tmpnam(e: char; var n: filename);  extern;

(**********************************************************************)
(*                                                                    *)
(*                             initialize                             *)
(*                                                                    *)
(**********************************************************************)

procedure initialize;

begin
    message(identity);
    rewrite(list);
    tmpnam('a', t1name);
    rewrite(temp1, t1name);
    tmpnam('b', t2name);
    rewrite(temp2, t2name);
    tmpnam('c', t3name);
    rewrite(temp3, t3name);
    tmpnam('d', t4name);
    rewrite(temp, t4name);
end   (* initialize *);


(**********************************************************************)
(*                                                                    *)
(*                            main program                            *)
(*                                                                    *)
(**********************************************************************)

begin (* cpas1 *)
  newini; (* initialize NEW and DISPOSE before doing anything else *)
  initialize;
  cpas1e(ptr, source, list, temp1, temp3);
  cpass2(ptr, temp1, temp2, list);
  cpas3e(ptr, temp2, temp1, list, temp3);
  cpas4e(ptr, temp1, temp2, list, temp3);
  cpas5e(ptr, temp2, temp1, list);
  pass61(ptr, temp1, temp2, list, true);
  passx (ptr, list, temp, temp3);
  pass7e(ptr, temp2, object, list);
  rewrite(temp1);
  rewrite(temp2);
  rewrite(temp3);
end   (* cpas1 *) .

   