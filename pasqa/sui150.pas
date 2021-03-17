
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 150*)
(*TEST 6.6.3.1-2, CLASS=CONFORMANCE*)
(* This program is similar to 6.6.3.1-1, except that set,
  record and array parameter lists are tested.
  The compiler fails if the program does not compile. *)
program t6p6p3p1d2;
type
   sett     = set of 0..20;
   rekord   = record
               a : integer
              end;
   urray    = array[boolean] of boolean;
var
   setone, settwo,setthree,setfour,setfive,setsix : sett;
   recone,rectwo,recthree,recfour,recfive : rekord;
   urrayone,urraytwo,urraythree,urrayfour : urray;
procedure testone(set1,set2,set3,set4,set5,set6 : sett;
                  rec1,rec2,rec3,rec4,rec5 : rekord;
                  urray1,urray2,urray3,urray4 : urray);
begin

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   write(' PASS')
end;
procedure testtwo(var set1,set2,set3,set4,set5,set6 : sett;
                  var rec1,rec2,rec3,rec4,rec5 : rekord;
                  var urray1,urray2,urray3,urray4 : urray);
begin
   writeln('...6.6.3.1-2')
end;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #150');
   setone:=[1];   settwo:=[1];   setthree:=[1];
   setfour:=[1];  setfive:=[1];  setsix:=[1];
   recone.a:=1;   rectwo.a:=1;   recthree.a:=1;
   recfour.a:=1;  recfive.a:=1;
   urrayone[true]:=false;  urraytwo[true]:=false;
   urraythree[true]:=false;   urrayfour[true]:=false;
   testone(setone,settwo,setthree,setfour,setfive,setsix,
            recone,rectwo,recthree,recfour,recfive,
            urrayone,urraytwo,urraythree,urrayfour);
   testtwo(setone,settwo,setthree,setfour,setfive,setsix,

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

            recone,rectwo,recthree,recfour,recfive,
            urrayone,urraytwo,urraythree,urrayfour);
end.
