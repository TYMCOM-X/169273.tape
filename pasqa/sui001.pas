
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number   1*)
(*TEST 5.2.2-1, CLASS=QUALITY*)
(*
----------------------------------------------------------------------
(C) Copyright
A.H.J. Sale
R.A. Freak
July 1979
All rights reserved
This material may not be reproduced or copied in
whole or part without written permission from the authors.
Department of Information Science
University of Tasmania
Box 252C, G.P.O.,
Hobart 7000.
Tasmania
Australia.
----------------------------------------------------------------------
*)
(* This program does not conform to the standard because its

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

  meaning is altered by the truncation of its identifiers to 8
  characters. Does the processor provide any indication that the
  program does not conform?
  Such surreptitious changes of meaning are dangerous.
  Obviously processors with 8-character significance will have
  difficulty in detecting such problems, but it can be done.
  For processors with full significance it is easier. *)
program t5p2p2d1;
const
   valueofaverylongidentifier1 = 10;
procedure p;
var
   valueofaverylongidentifier2:integer;
begin
   valueofaverylongidentifier2:=11;
   if valueofaverylongidentifier1 <>
      valueofaverylongidentifier2 then
      writeln(' IDENTIFIERS DISTINGUISHED...5.2.2-1')
   else
      writeln(' IDENTIFIERS NOT DISTINGUISHED...5.2.2-1')

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #001');
   p
end.
