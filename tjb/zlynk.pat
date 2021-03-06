From: TJB
Date: JULY 12,1983 21:36
Tim:
    Revised patch after partial testing is at 47A8. Data was 0A, changed
to 00. Other patch must be returned to 47A3:0D. (We find that this CR is
required!)  Testing continues as we attempt to make these amazing
computers do what we want, instead of what they are told. 21:45 Tuesday
evening.

From: TJB
Date: JULY 12,1983 21:29
Tim:
     
     This is a test file to check out the double-spacing problem  which
occurs when  transmitting  a  text  file  using  the "S" (Send a file
blind) option of ZLYNK.  As you know, ZLYNK assumes that the receiving
computer requires both  a carriage  return  and  a line feed at the end
of each line.  When the receiving computer (taking care of itself, thank
you) generates its own  line  feed  upon receipt  of  a carriage return,
as does System 930, then normally single-spaced text  becomes
involuntarily  double-spaced.   If  you  are  reading  this   as
single-spaced, it means I've probably cured the problem by patching ZLYNK.

From: TJB
Date: JULY 19,1983 18:59
Tim: (7/19/83)
     
     This  is  a  REVISED test file to check out the operation of the patch we
have installed in ZLYNK  to  correct  the  double-spacing  which  occurs  when
transmitting a text file to the DEC 10 (System 930) using the "S" (Send a file
blind) option of ZLYNK.  This paragraph  is  filled  and  justified  to  a  78
character  line.   As  you  know,  ZLYNK  assumes  that the receiving computer
requires both a carriage return and a line feed at the end of each line.  When
the  receiving  computer  (taking care of itself, thank you) generates its own
line feed upon receipt of a carriage return, as does System 930, then normally
single-spaced  text  becomes  involuntarily double-spaced.  If you are reading
this as single-spaced, it means I've probably cured the  problem  by  patching
ZLYNK.

     WOW!!!!  I think we've finally got it!!  (We're doing this as an exercise
in our Hack and Patch Seminar.)  I haven't tested all the functions yet to see
if  the  patch affects anything else.  That will take some time.  The patch is
at address 47A3.  Original data was 0DH (carriage return).  New  data  is  00.
This  appears  to  be  in a subroutine that tests for 0A (Heath's newline) and
adds a 0D whenever it finds one.  Time  to  sign  off  and  get  back  to  the
seminar.  -ASB

Tim:
     Revised  patch after partial testing is at 47A8.  Data was 0A, changed to
00.  Other patch must be returned to  47A3:0D.   (We  find  that  this  CR  is
required!)  Testing continues as we attempt to make these amazing computers do
what we want, instead of what they are told.  21:45 Tuesday evening.

Partial disassembly of ZLYNK using RDT yields the following code:

        .
        .
        .
479D    FE 0A           CPI     0AH     ;IS THIS A LINE FEED?
479F    C2 B3 47        JNZ     47B3H   ;IF NOT, GO SOMEWHERE ELSE 
47A2    0E 0D           MVI     C,0DH   ;IF IT IS, PUT A CARRIAGE RETURN
                                        ;IN THE C REGISTER
47A4    CD 67 48        CALL    4867H   ;PROBABLY A SUBROUTINE TO SEND IT
47A7    0E 0A           MVI     C,0AH   ;PUT THE LINEFEED BACK IN C
47A9    CD 67 48        CALL    4867H   ;THE SAME SUBROUTINE TO SEND IT
47AC    AF              XRA     A       ;ZERO THE A REGISTER
        .
        .
        .
The subroutine that begins at 4867H is very convoluted, calling other
routines and writing and reading to memory locations within the program
bounds.  It does appear to send the characters sent to it as well as provide
for the four character times at the end of each line as described in the
program documentation by consecutive calls to a subroutine at 48E4H.

When we substituted 00 for the 0A at 47A8, the program simply substitutes a
carriage return (0D) for the HDOS newline character (0A).

We tried patching a null in place of the carriage return at 47A3 but that
failed to give carriage returns on the DEC 10.
   