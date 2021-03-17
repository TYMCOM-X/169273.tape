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
: **  File Name          : cpa101.pas                             **
: **                                                              **
: **  File Description   :                                        **
: **                                                              **
: **     This file contains routines which handle set operations. **
: **                                                              **
: **  File Abstract      :                                        **
: **                                                              **
: ******************************************************************
: **                      MAINTENANCE HISTORY                     **
: **                                                              **
: **  Ver   Date    By   PIR/NSR         Reason for Change        **
: ** ----- -------- ---  -------- ------------------------------  **
: ** 01.02 12/15/86 PJH  1162     ADDITION OF PRORIETARY BANNER   **
: **                                                              **
: ******************************************************************
: **                 SUBROUTINE IDENTIFICATION                    **
: **                                                              **
: **  Routine Abstract   :  Setinitialize                         **
: **                                                              **
: **     Setinitialize initializes the set (parameter) s to empty.**
: **                                                              **
: **  Parameters         :                                        **
: **                                                              **
: **     s - big_set (array[0..3] of set of 0..63)                **
: **                                                              **
: ******************************************************************
: **                                                              **
: **  Routine Abstract   :  Setinsert                             **
: **                                                              **
: **     Setinsert adds the (parameter) element e to the          **
: **     (var parameter) set s.                                   **
: **                                                              **
: **  Parameters         :                                        **
: **                                                              **
: **     s - big_set (array[0..3] of set of 0..63)                **
: **                                                              **
: **     e - big_set_element (range of integer 0..63)             **
: **                                                              **
: ******************************************************************
: **                                                              **
: **  Routine Abstract   :  Setunion                              **
: **                                                              **
: **     Setunion takes the union of the two (parameter) sets.    **
: **     The result is returned in (var parameter) s.             **
: **                                                              **
: **  Parameters         :                                        **
: **                                                              **
: **     s - (var) big_set (array[0..3] of set of 0..63)          **
: **                                                              **
: **     t - big_set (array[0..3] of set of 0..63)                **
: **                                                              **
: **                                                              **
: ******************************************************************
: **                                                              **
: **  Routine Abstract   :  Setdifference                         **
: **                                                              **
: **     Setdifference takes the difference of the two            **
: **     (parameter) sets.  The result is returned in             **
: **     (var parameter) s.                                       **
: **                                                              **
: **  Parameters         :                                        **
: **                                                              **
: **     s - (var) big_set (array[0..3] of set of 0..63)          **
: **                                                              **
: **     t - big_set (array[0..3] of set of 0..63)                **
: **                                                              **
: ******************************************************************
: **                                                              **
: **  Routine Abstract   :  Setmember                             **
: **                                                              **
: **     Setmember tests whether (parameter) element e is a       **
: **     member of (parameter) set s.  If so, Setmember returns   **
: **     a value of TRUE, otherwise it returns a value of FALSE.  **
: **                                                              **
: **  Parameters         :                                        **
: **                                                              **
: **                                                              **
: **     s - big_set (array[0..3] of set of 0..63)                **
: **                                                              **
: **     e - big_set_element (integer range of 0..63)             **
: **                                                              **
: **  Returned Values    :                                        **
: **                                                              **
: **     Returns a Boolean .. True or False                       **
: **                                                              **
: *****************************************************************)

(*$e+*)
program setops, setinitialize, setinsert, setunion, setdifference,
        setmember;
const
  limit = 3;
type
  index = 0 .. limit;
  big_set = array[index] of set of 0..63;
  big_set_element = 0 .. 255;

procedure setinitialize(var s: big_set);
var
  i:  index;
begin
  for i := 0 to limit do s[i] := []
end;

procedure setinsert(var s: big_set;  e: big_set_element);
var
  i:  index;
begin
  i := e div 64;
  s[i] := s[i] + [e mod 64]
end;

procedure setunion(var s: big_set;  t: big_set);
var
  i:  index;
begin
  for i := 0 to limit do s[i] := s[i] + t[i]
end;

procedure setdifference(var s: big_set;  t: big_set);
var
  i:  index;
begin
  for i := 0 to limit do s[i] := s[i] - t[i]
end;

function setmember(s: big_set;  e: big_set_element): boolean;
begin
  setmember := e mod 64 in s[e div 64]
end;

begin end.
  