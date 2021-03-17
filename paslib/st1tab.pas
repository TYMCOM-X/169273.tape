(*$M-,R-,T-,C+,L+*)
(********** tabulation module **********)





const
   datalinelength=253; (* avoid boundry problems *)





type
   integer8=0..255;





var
   tabpoint : array [1..datalinelength] of boolean; (* the tabstops *)










(************************************************************)










procedure resettabs;

(* procedure to reset tabs *)



var i : integer8;



begin

for i:=1 to datalinelength do
   tabpoint[i]:=false (* set all the tabstops false *)
end (* resettabs *);










(************************************************************)










procedure setdefaulttabs;

(* procedure to set tabs at every 8 column positions *)



var i : integer8;



begin
i:=1;

repeat
   tabpoint[i]:=true; (* every eighth one *)
   i:=i+8
until i>datalinelength

end (* setdefaulttabs *);

(**************************************************)

function TABSTOP (col: integer8): boolean;

(* a function indicating whether or not a tab stop has
   been set to a particular column *)

  begin
  if col > datalinelength then
    TABSTOP := true (* always set at end of line *)
  else
    TABSTOP := tabpoint[col]
  end
. 
