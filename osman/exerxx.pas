
(*							Guven Osman
							Date due: 9/16
							Chapter 1
							CSE 110, MW, 19:00
*)

(*	This is exercises 1-14.
	This program computes an electric bill given the number of
	kilowatt hours. Bill also contains 10% surcharge and 3% utility
	tax added to it.
*)

PROGRAM	ElectricBill;

VAR
Kwh, Sum:	REAL;

BEGIN
(*	OPEN Statement below makes terminal the INPUT and OUTPUT
	device. This PASCAL runs on DEC's PDP-10 mainframe.
*)
OPEN (TTY); REWRITE (TTYOUTPUT); INPUT:=TTY; OUTPUT:=TTYOUTPUT;

WRITELN;
WRITELN ('************************************************************');
WRITELN;
WRITELN ('Start of program "ElectricBill"');
WRITELN;
WRITE ('Enter number of kilowatt hours: ');
BREAK;
READ (Kwh);
Sum:=((Kwh*4.75)+(Kwh*0.1)+(Kwh*0.03))/100;
WRITELN;
WRITELN ('If you use',Kwh:7:2,' kwhs of electricity');
WRITELN ('Electric bill will be',Sum:6:2,' dollers.');
WRITELN;
WRITELN ('End of program "ElectricBill"');
WRITELN;
WRITELN ('************************************************************');
END.

(*							Guven Osman
							Date due: 9/16
							Chapter 1
							CSE 110, MW, 19:00
*)

(*	This is exercises 1-17.
	This program takes the date in American form as in mm/dd/yy and
	converts it to European date form as in dd,mm,yy and prints it.
*)

PROGRAM	ConvertDate;

VAR
Month, Day, Year:	INTEGER;
GarbChar:		CHAR;

BEGIN
(*	OPEN Statement below makes terminal the INPUT and OUTPUT
	device. This PASCAL runs on DEC's PDP-10 mainframe.
*)
OPEN (TTY); REWRITE (TTYOUTPUT); INPUT:=TTY; OUTPUT:=TTYOUTPUT;

WRITELN;
WRITELN ('************************************************************');
WRITELN;
WRITELN ('Start of program "ConvertDate"');
WRITELN;
WRITE ('Enter any date(mm/dd/yy):');
BREAK;
READ (Month,GarbChar,Day,GarbChar,Year);
WRITELN ('You have entered: ',Day:1,',',Month:1,',',Year:1);
WRITELN;
WRITELN ('End of program "ConvertDate"');
WRITELN;
WRITELN ('************************************************************');
END.

(*							Guven Osman
							Date due: 9/16
							Chapter 2
							CSE 110, MW, 19:00
*)

(*	This is exercises 2-15
	This program computes the whole sale price of an item.
	Basically program takes off 5.5% sales tax and then
	subtracts 40% more for the markup.
*)

PROGRAM	Wholesale;

VAR
WholeSalePrice, PPrice:	REAL;

BEGIN
(*	OPEN Statement below makes terminal the INPUT and OUTPUT
	device. This PASCAL runs on DEC's PDP-10 mainframe.
*)
OPEN (TTY); REWRITE (TTYOUTPUT); INPUT:=TTY; OUTPUT:=TTYOUTPUT;

WRITELN;
WRITELN ('************************************************************');
WRITELN;
WRITELN ('Start of program "WholeSale"');
WRITELN;
WRITE ('Enter purchase price of an item: ');
BREAK;
READ (PPrice);
WholeSalePrice:= PPrice-((PPrice-(PPrice*.055))*.4);
WRITELN ('The whole sale price of the item is: ',WholeSalePrice:10:2);
WRITELN;
WRITELN ('End of program "WholeSale"');
WRITELN;
WRITELN ('************************************************************');
END.

(*							Guven Osman
							Date due: 9/16
							Chapter 1
							CSE 110, MW 19:00
*)

(*	This is exercises 2-23
	This program takes an integer value in seconds and
	converts it to days:hrs:mins:secs.
*)

PROGRAM	Seconds;

CONST
Day= 86400;	(*Number of seconds in one day*)
Hour= 3600;	(*Number of seconds in one hour*)
Min= 60;	(*Number of seconds in one minutes*)

VAR
NumOfDays, NumOfHours, NumOfMins, NumOfSecs:	INTEGER;
TotalSeconds, Remainder:	INTEGER;

BEGIN
(*	OPEN Statement below makes terminal the INPUT and OUTPUT
	device. This PASCAL runs on DEC's PDP-10 mainframe.
*)
OPEN (TTY); REWRITE (TTYOUTPUT); INPUT:=TTY; OUTPUT:=TTYOUTPUT;

WRITELN;
WRITELN ('************************************************************');
WRITELN;
WRITELN ('Start of program "Seconds"');
WRITELN;
WRITE ('Enter number of seconds: ');
BREAK;
READ (TotalSeconds);
NumOfDays:= TotalSeconds DIV Day;
Remainder:= TotalSeconds MOD Day;
NumOfHours:= Remainder DIV Hour;
Remainder:= Remainder MOD Hour;
NumOfMins:= Remainder DIV Min;
Remainder:= Remainder MOD Min;
NumOfSecs:= Remainder;
WRITE ('It is...',NumOfDays,'days:',NumOfHours,'hours:');
WRITELN (NumOfMins,'mins:',NumOfSecs,'secs');
WRITELN;
WRITELN ('End of program "Seconds"');
WRITELN;
WRITELN ('************************************************************');
END.

(*							Guven Osman
							Date due: 10/2
							Chapter 3
							CSE 110, MW 19:00


	This is exercises 3-19
	This program figures out weekly salary under three different
	salary payment structure. (a)Staright weekly salary of $325.
	(b)Hourly pay of $3.50 plus 15% commission. (c)Only 20%
	commission plus $1 for each item sold.
	
*)

PROGRAM Salary;

CONST
SaleCommission= .15;
Commission= .20;
Weeklysalary= 325;
Hourly= 3.50;

VAR
NumberOfTubs, HourlyAndCom1, JustCom1:		INTEGER;
PriceOfTub, TotalSale, HourlyAndCom, JustCom:	REAL;

BEGIN
(*	OPEN Statement below makes terminal the INPUT and OUTPUT
	device. This PASCAL runs on DEC's PDP-10 mainframe.
*)
OPEN (TTY); REWRITE (TTYOUTPUT); INPUT:=TTY; OUTPUT:=TTYOUTPUT;

WRITELN;
WRITELN ('************************************************************');
WRITELN;
WRITELN ('Start of program "Salary"');
WRITELN;
WRITE ('How many hut tubs have you sold? ');
BREAK;
READ (NumberOfTubs);
WRITE ('How much is each hot tub? ');
BREAK;
READ (PriceOfTub);
TotalSale:= NumberOfTubs * PriceOfTub;
HourlyAndCom:= (40 * Hourly) + (TotalSale * SaleCommission);
HourlyAndCom1:= TRUNC(HourlyAndCom);
JustCom:= (Commission * TotalSale) + NumberOfTubs;
JustCom1:= TRUNC (JustCom);
WRITELN ('Weekly you will be making:');
WRITELN;
WRITELN ('Straight      Hourly        Just');
WRITELN ('Weekl          and           20%');
WRITELN ('Salary    15% Commision   Commission');
WRITELN ('------------------------------------');
WRITELN (WeeklySalary,'       ',HourlyAndCom1:7,'      ',JustCom1:7);
WRITELN;
WRITELN ('End of program "Salary"');
WRITELN;
WRITELN ('************************************************************');
END.

(*							Guven Osman
							Date due: 10/9
							Chapter 4
							CSE 110, MW 19:00


	This is exercises 4-10
	
*)

PROGRAM PrintRandomPosition;

CONST
NumberOfTimes= 100;
CharPerLine= 15;

VAR
EvenNumber, FifthNumber, SixthNumber:	INTEGER;
Dummy, Temp1:	INTEGER;
Every3rdChar, Every4thChar:	CHAR;
A, Z, Letter:	CHAR;

BEGIN
(*	OPEN Statement below makes terminal the INPUT and OUTPUT
	device. This PASCAL runs on DEC's PDP-10 mainframe.
*)
OPEN (TTY); REWRITE (TTYOUTPUT); INPUT:=TTY; OUTPUT:=TTYOUTPUT;

WRITELN;
WRITELN ('************************************************************');
WRITELN;
WRITELN ('Start of program "PrintRandomPosition"');
WRITELN;

WRITELN ('Even numbers from 0 to ',NumberOfTimes,' is: ');
Dummy:= 0;
FOR EvenNumber:= 0 TO NumberOfTimes DO
    CASE (EvenNumber MOD 2) OF
    0: BEGIN
       Dummy:= Dummy+1;
       IF Dummy < CharPerLine THEN
       WRITE (EvenNumber:4) ELSE
           BEGIN
           WRITELN (EvenNumber:4);
           Dummy:= 0;
           END;
       END; (*End for the COMPOUND statement*)
    1: ;
    END; (*End for the CASE statement*)	
WRITELN;
WRITELN;

WRITELN ('Every other letter from A to Z is: ');
Dummy:= 0;
FOR Letter:= 'A' TO 'Z' DO
    CASE (ORD (Letter) MOD 2) OF
    1: BEGIN
       Dummy:= Dummy + 1;
       IF Dummy < CharPerLine THEN
       WRITE (Letter:4) ELSE
         BEGIN
         WRITELN (Letter:4);
         Dummy:= 0
         END;
       END; (*End for the COMPOUND statement*)
    0: ;
    END; (*End for the CASE statement*)

WRITELN;
WRITELN;
WRITELN ('Every fifth number from 0 to 100 is: ');
FOR FifthNumber:= 0 TO NumberOfTimes DO
  CASE (FifthNumber MOD 5) OF
  1,2,3,4: ;
  0: BEGIN
     IF (FifthNumber DIV 5) > 0 THEN
     WRITE (FifthNUmber);
     END;
  END;(*For CASE*)
WRITELN;
WRITELN;

WRITELN ('Every sixth number from 2 to 110 is: ');
FOR SixthNumber:= 2 TO 110 DO
  CASE (SixthNumber MOD 6) OF
  1,2,3,4,5: ;
  0: BEGIN
     IF (SixthNumber DIV 6) > 0 THEN
     WRITE (SixthNumber);
     END;
  END;(*For CASE*)
WRITELN;
WRITELN;

WRITELN ('Every third character starting with C is:');
FOR Every3rdChar:= 'A' TO 'Z' DO
  CASE ( ORD (Every3rdChar) MOD 3) OF
  0,2: ;
  1: WRITE (Every3rdChar:4);
  END;(*For CASE*)
WRITELN;
WRITELN;

WRITELN ('Every forth character in this computers character set is:');
Dummy:=0;
FOR Every4thChar:= ' ' TO '~' DO
  CASE (ORD (Every4thChar) MOD 4) OF
  1,2,3: ;
  0: BEGIN
     Dummy:= Dummy +1;
     IF Dummy < 15 THEN
     WRITE (Every4thChar:4) ELSE
       BEGIN
       WRITELN (Every4thChar:4);
       Dummy:= 0;
       END;
     END;
  END;(*For CASE*)

WRITELN;
WRITELN ('End of program "PrintRandomPosition"');
WRITELN;
WRITELN ('************************************************************');
END.

(*							Guven Osman
							Date due: 10/9
							Chapter 4
							CSE 110, MW 19:00


	This is exercises 4-13
	
*)

PROGRAM Roots;

VAR
Number:	INTEGER;
Dummy:	INTEGER;
Square, SquareRoot, CubeRoot, FourthRoot:	REAL;

BEGIN
(*	OPEN Statement below makes terminal the INPUT and OUTPUT
	device. This PASCAL runs on DEC's PDP-10 mainframe.
*)
OPEN (TTY); REWRITE (TTYOUTPUT); INPUT:=TTY; OUTPUT:=TTYOUTPUT;

WRITELN;
WRITELN ('************************************************************');
WRITELN;
WRITELN ('Start of program "Roots"');
WRITELN;
WRITELN ('      ','           ','  Square ','   Cube  ','  Fourth ');
WRITELN ('Number','   Square  ','   Root  ','   Root  ','   Root  ');
WRITELN ('------','  -------  ',' ------- ',' ------- ',' ------- ');
FOR Number:= 1 TO 20 DO
  BEGIN
  Square:= SQR(Number);
  SquareRoot:= SQRT(Number);
  CubeRoot:= EXP((1/3) * LN(Number));
  FourthRoot:= EXP ((1/4) * LN(Number));
  WRITELN (Number:3,'    ',Square:1,SquareRoot:1,CubeRoot:1,FourthRoot:1);
  END;
WRITELN;

WRITELN ('End of program "Roots"');
WRITELN;
WRITELN ('************************************************************');
END.

(*							Guven Osman
							Date due: 10/9
							Chapter 5
							CSE 110, MW, 19:00


	This is exercises 5-13

*)
PROGRAM	Markup;

VAR
RealPrice, WholeSalePrice, SalePrice, Percent:		REAL;
IRealPrice, IWholeSalePrice, ISalePrice, IPercent:	INTEGER;
SignChar:	CHAR;

BEGIN
(*	OPEN Statement below makes terminal the INPUT and OUTPUT
	device. This PASCAL runs on DEC's PDP-10 mainframe.
*)
OPEN (TTY); REWRITE (TTYOUTPUT); INPUT:=TTY; OUTPUT:=TTYOUTPUT;

WRITELN;
WRITELN ('************************************************************');
WRITELN;
WRITELN ('Start of program "Markup"');
WRITELN;
WRITE ('How much is the real price of the item: ');
BREAK;
READ (RealPrice);
WRITELN ('What % do you want to markup or down.');
WRITELN ('Enter as  +10 (cr) for 10% markup and');
WRITELN ('          -15 (cr) for 10% markdown.');
WRITE ('What %..?: ');
BREAK;
READ (SignChar,Percent);
WholeSaleprice:=0; SalePrice:=0;
IF SignChar= '+' THEN
WholeSalePrice:= (RealPrice + (RealPrice * (Percent/100))) ELSE
WholeSalePrice:= (RealPrice - (RealPrice * (Percent/100)));
SalePrice:= WholeSalePrice * .066;
IRealPrice:= TRUNC (RealPrice);
IPercent:= TRUNC (Percent);
IWholeSalePrice:= TRUNC (IWholeSalePrice);
ISalePrice:= TRUNC (SalePrice);
WRITELN (' Real    Percent     Whole   Sale');
WRITELN (' Price   Up/Down     Sale    Price');
WRITELN ('------- ---------   ------- -------');
WRITE (IRealPrice:5, SignChar, IPercent:8);
WRITELN (IWholeSalePrice:9, ISalePrice:5);
WRITELN;
WRITELN ('End of program "Markup"');
WRITELN;
WRITELN ('************************************************************');
END.

(*							Guven Osman
							Date due: 10/9
							Chapter 5
							CSE 110, MW 19:00


	This is exercises 5-18
	
*)

PROGRAM TVChannel;

VAR
ChannelNumber:	INTEGER;

BEGIN
(*	OPEN Statement below makes terminal the INPUT and OUTPUT
	device. This PASCAL runs on DEC's PDP-10 mainframe.
*)
OPEN (TTY); REWRITE (TTYOUTPUT); INPUT:=TTY; OUTPUT:=TTYOUTPUT;

WRITELN;
WRITELN ('************************************************************');
WRITELN;
WRITELN ('Start of program "TVChannel"');
WRITELN;
WRITE ('Enter a TV channel between 2 and 13: ');
BREAK;
READ (ChannelNumber);
CASE (ChannelNumber) OF
3,6,8,10,12,13: WRITELN ('Nothing on this channel.');
2: WRITELN ('Oakland');
4: WRITELN ('Oakland-NBC-KRON');
5: WRITELN ('CBS');
7: WRITELN ('San Francisco-ABC-KGO');
9: WRITELN ('San Francisco-PBS');
END;
WRITELN;
WRITELN ('End of program "TVChannel"');
WRITELN;
WRITELN ('************************************************************');
END.

(*							Guven Osman
							Date due: 10/14
							Chapter 6
							CSE 110, MW 19:00


	This is exercises 6-23
        This program sums the squares of the first 333
        odd integers.
	
*)

PROGRAM OddSquare;

VAR
OddCounter, SumSquare:	INTEGER;

BEGIN
(*	OPEN Statement below makes terminal the INPUT and OUTPUT
	device. This PASCAL runs on DEC's PDP-10 mainframe.
*)
OPEN (TTY); REWRITE (TTYOUTPUT); INPUT:=TTY; OUTPUT:=TTYOUTPUT;

WRITELN;
WRITELN ('************************************************************');
WRITELN;
WRITELN ('Start of program "OddSquare"');
WRITELN;
FOR OddCounter:= 1 TO 333 DO
  CASE (OddCounter MOD 2) OF
  0: ;
  1: SumSquare:= SumSquare + SQR(OddCounter);
  END;
WRITELN ('Sum of square of odd integers from 1 to 333 is: ',SumSquare);
WRITELN;
WRITELN ('End of program "OddSquare"');
WRITELN;
WRITELN ('************************************************************');
END.

(*							Guven Osman
							Date due: 10/14
							Chapter 7
							CSE 110, MW 19:00


	This is exercises 7-13
        Peculiar multiplication. Algorithm used is this. To multiply
        any two integer, find the large one. If the large one is odd
        add the small number. Divide the large number and double the
        small number. Every time division of large number result in
        odd number add the doubled small number untill the large num
	ber reduces to 1. Accumulative sum of the doubled small number
        is the answer to the multiplying the two number.
*)

PROGRAM Multiply;

VAR
InputNumber1, InputNumber2:	INTEGER;
BigNumber, SmallNumber, MultiplyIs:	INTEGER;

BEGIN
(*	OPEN Statement below makes terminal the INPUT and OUTPUT
	device. This PASCAL runs on DEC's PDP-10 mainframe.
*)
OPEN (TTY); REWRITE (TTYOUTPUT); INPUT:=TTY; OUTPUT:=TTYOUTPUT;

WRITELN;
WRITELN ('************************************************************');
WRITELN;
WRITELN ('Start of program "Multiply"');
WRITELN;
WRITE ('Enter two integer to be multilied as nn mm: ');
BREAK;
InputNUmber1:= 0; InputNumber2:= 0; MultiplyIs:= 0;
BigNumber:= 0; SmallNumber:= 0;
READ (InputNumber1, InputNumber2);
IF InputNumber1 > InputNumber2 THEN
  BEGIN
  BigNumber:= InputNumber1;
  SmallNumber:= InputNumber2;
  END
  ELSE
  BEGIN
  BigNumber:= InputNUmber2;
  SmallNumber:= InputNumber1;
  END;	
REPEAT
CASE (BigNumber MOD 2) OF
0: BEGIN
   SmallNumber:= SmallNumber * 2;
   BigNumber:= BigNumber DIV 2;
   END;
1: BEGIN
   MultiplyIs:= MultiplyIs + SmallNumber;
   BigNumber:= BigNumber DIV 2;
   SmallNumber:= SmallNumber * 2;
   END;
END;
UNTIL BigNumber < 1;
WRITE ('Multipling',InputNumber1,' with',InputNUmber2,' is: ');
WRITELN (MultiplyIs:6);
WRITELN;
WRITELN ('End of program "Multiply"');
WRITELN;
WRITELN ('************************************************************');
END.

(*							Guven Osman
							Date due: 10/16
							Chapter 7
							CSE 110, MW 19:00


	This is exercises 7-14
        This program finds the number which when divided
        by 2,3,4,5,6 remainder is 1 and when divided by 7
        remainder is 0. Range of numbers is between 7-300.
	
*)

PROGRAM ChooseNumber;

VAR
NumberFound, MainCount:	INTEGER;
NumberFoundCount:	INTEGER;

PROCEDURE FindIT(Number: INTEGER; VAR NumberFound: INTEGER);
VAR
MaybeTheNumber, Divider:	INTEGER;

BEGIN
Divider:= 2;
REPEAT
MaybeTheNumber:= Number MOD Divider;
IF MaybeTheNumber = 1
THEN Divider:= 7
ELSE Divider:= Divider + 1;
UNTIL (Divider = 7) OR (Divider <= 6);
IF Divider = 7 THEN
  BEGIN
  MaybeTheNumber:= Number MOD Divider;
  IF MaybeTheNumber = 0 THEN
    BEGIN
    NumberFound:= Number;
    WRITELN('NumberFound= ',NumberFound);
    END;
  END;
END;

BEGIN
(*OPEN Statement below makes terminal the INPUT and OUTPUT
device. This PASCAL runs on DEC's PDP-10 mainframe.*)
OPEN (TTY); REWRITE (TTYOUTPUT); INPUT:=TTY; OUTPUT:=TTYOUTPUT;

WRITELN;
WRITELN ('************************************************************');
WRITELN;
WRITELN ('Start of program "ChooseNumber"');
WRITELN;
BREAK;
MainCount:= 0;
NumberFound:= 0;
NumberFoundCount:= 0;
FOR MainCount:= 7 TO 300 DO
FindIT (MainCount,NumberFound);
WRITELN ('End of program "ChooseNumber"');
WRITELN;
WRITELN ('************************************************************');
END.
  