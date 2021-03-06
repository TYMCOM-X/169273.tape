
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 286*)
(*TEST 6.8.3.10-7, CLASS=QUALITY*)
(* This test checks that with statements may be nested to 15
  levels. The test may break a compiler limit in some compilers,
  particularly if a register is allocated for every selected
  variable. *)
program t6p8p3p10d7;
type
   rec1 = record
             i:integer
           end;
   rec2 = record
             i:integer
           end;
   rec3 = record
             i:integer
           end;
   rec4 = record
             i:integer
           end;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   rec5 = record
             i:integer
           end;
   rec6 = record
             i:integer
           end;
   rec7 = record
             i:integer
           end;
   rec8 = record
             i:integer
           end;
   rec9 = record
             i:integer
           end;
   rec10 = record
             i:integer
           end;
   rec11 = record
             i:integer

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

           end;
   rec12 = record
             i:integer
           end;
   rec13 = record
             i:integer
           end;
   rec14 = record
             i:integer
           end;
   rec15 = record
             i:integer
           end;
   p1 = ^rec1;
   p2 = ^rec2;
   p3 = ^rec3;
   p4 = ^rec4;
   p5 = ^rec5;
   p6 = ^rec6;
   p7 = ^rec7;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   p8 = ^rec8;
   p9 = ^rec9;
   p10 = ^rec10;
   p11 = ^rec11;
   p12 = ^rec12;
   p13 = ^rec13;
   p14 = ^rec14;
   p15 = ^rec15;
var
   ptr1 : p1;
   ptr2 : p2;
   ptr3 : p3;
   ptr4 : p4;
   ptr5 : p5;
   ptr6 : p6;
   ptr7 : p7;
   ptr8 : p8;
   ptr9 : p9;
   ptr10 : p10;
   ptr11 : p11;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   ptr12 : p12;
   ptr13 : p13;
   ptr14 : p14;
   ptr15 : p15;
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #286');
   new(ptr1); ptr1^.i:=0;
   new(ptr2); ptr2^.i:=0;
   new(ptr3); ptr3^.i:=0;
   new(ptr4); ptr4^.i:=0;
   new(ptr5); ptr5^.i:=0;
   new(ptr6); ptr6^.i:=0;
   new(ptr7); ptr7^.i:=0;
   new(ptr8); ptr8^.i:=0;
   new(ptr9); ptr9^.i:=0;
   new(ptr10); ptr10^.i:=0;
   new(ptr11); ptr11^.i:=0;
   new(ptr12); ptr12^.i:=0;
   new(ptr13); ptr13^.i:=0;
   new(ptr14); ptr14^.i:=0;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   new(ptr15); ptr15^.i:=0;
   with ptr1^ do
      with ptr2^ do
         with ptr3^ do
            with ptr4^ do
               with ptr5^ do
                  with ptr6^ do
                     with ptr7^ do
                        with ptr8^ do
                           with ptr9^ do
                              with ptr10^ do
                                 with ptr11^ do
                                    with ptr12^ do
                                       with ptr13^ do
                                          with ptr14^ do
                                             with ptr15^ do
                                                  i:=5;
   writeln(' >15 LEVELS OF WITH STATEMENTS ALLOWED...6.8.3.10-7');
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

 