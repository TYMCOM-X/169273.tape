0010�	#TEXT
  0020�	
  0030�	var11=line 1 scan " FAIL " characters 31
  0040�	var12=line 1 scan " PAGE " skip blank characters 10 stop on blank
  0050�	var13=line 1 scan " PAGE " skip blank number
   0060�	var14=line 1 scan " " skip blank characters 10 stop after item
0070�	var15=line 1 start 1 for 40 stop on blank
 0080�	
  0090�	var24=line 2 scan " " skip blank characters 10 stop after item
0100�	var25=line 2 start 1 for 40 stop on blank
 0110�	
  0120�	var9= var15 + IFNO var15 [var14 + IFNO var14 [var9]]
0130�	var8= var25 + IFNO var25 [var24 + IFNO var24 [var8]]
0140�	var7= var11 start 21 for 10
0150�	
            0160�	VAR1=LINE 1 SCAN "PAGE" SKIP BLANK characters 8 stop on blank
 0170�	VAR2=LINE 2 SCAN "SYMBOL TABLE"="S";
 0180�		!"SYMBOL CROSS REFERENCE" ="C";
0190�		!"MACRO/OPDEF CROSS REFERENCE"="M";
 0200�		!"OPCODE CROSS REFERENCE"="O"
  0210�	VAR3=LINE FIRST AFTER 2 START 1 FOR 6 STOP AFTER ITEM
    0220�	VAR4=LINE LAST AFTER 2 START 1 FOR 6 STOP AFTER ITEM
0230�	VAR5= VAR4 + IFNO VAR4 [VAR5]
   0240�	VAR6=IF VAR4 [VAR2]+IFNO VAR4 [VAR6]
 0250�	outvar= title + "-" + fichen + " " + loc + ": ";
    0260�		+ var9 + " " + var8 + " " + var7 + " P " + var12;
  0270�		+ " C." + var3
  0280�	
  0290�	#PAGE
  0300�	
            0310�	KEEP VAR6
   0320�	KEEP VAR5
   0330�	
  0340�	#OVERFLOW PAGE
   0350�	
  0360�	KEEP VAR1
   0370�	KEEP VAR2
   0380�	KEEP VAR4
   0390�	KEEP VAR5
   0400�	KEEP VAR6
   0410�	BLANK 3
0420�	
  0430�	#INDEX
 0440�	
  0450�	HEADING "NAME    INDEX     PAGE  LOC"
0460�	
  0470�	ITEM 1 SIZE 6 TITLE SUPRESS UNCHANGED
0480�	LOCATE 9
    0490�	ITEM 2 SIZE 6 VAR3
    0500�	LOCATE 17
   0510�	ITEM 3 SIZE 1 VAR2
    0520�	LOCATE 20
   0530�	ITEM 4 SIZE 3 VAR1 RIGHT
   0540�	LOCATE 25
   0550�	ITEM 5 SIZE 4 LOC
0560�	
  0570�	#FICHE
 0580�	
  0590�	REVERSE
0600�	
                                               0610�	GROUP FRAMES 2 CHARACTERS 3 LINES 1 IF GLOBAL START 7 FOR 6 [;
0620�		CHARACTERS 4 IF GLOBAL START 9 FOR 4 [FRAMES 3 CHARACTERS 3;
 0630�		IF GLOBAL START 10 FOR 3 [CHARACTERS 4]]]
0640�		TLINE 1 SIZE 12 GLOBAL
    0650�	
  0660�	GROUP FRAMES 1 LINES 0 IF GLOBAL START 9 FOR 4 [FRAMES 0]
0670�	
  0680�	GROUP FRAMES 9 LINES 1 IF TITLE4 [LINES 2] CHARACTERS 3
  0690�		TLINE 1 SIZE 24 TITLE1+IF TITLE2 [","+TITLE2+IF TITLE3 [;
    0700�			","+TITLE3]] CENTER
 0710�		TLINE 2 SIZE 24 IF TITLE4 [TITLE4+IF TITLE5 [","+TITLE5+;
    0720�			IF TITLE6 [","+TITLE6]]] CENTER
    0730�	
  0740�	GROUP FRAMES 2 LINES 2
          0750�		TLINE 1 SIZE 6 DAY+"-"+MON3 CENTER
  0760�		TLINE 2 SIZE 6 YEAR CENTER
0770�	
  0780�	GROUP FRAMES 2 IFNO GLOBAL [LINES 0]
 0790�		TLINE 1 SIZE 6 FICHEN RIGHT
    0800�	
  0810�	#OVERFLOW FICHE
  0820�	
  0830�	REVERSE
0840�	
  0850�	GROUP FRAMES 2 CHARACTERS 3 LINES 1 IF GLOBAL START 7 FOR 6 [;
0860�		CHARACTERS 4 IF GLOBAL START 9 FOR 4 [FRAMES 3 CHARACTERS 3;
 0870�		IF GLOBAL START 10 FOR 3 [CHARACTERS 4]]]
0880�		TLINE 1 SIZE 12 GLOBAL
    0890�	
  0900�	GROUP FRAMES 1 LINES 0 IF GLOBAL START 9 FOR 4 [FRAMES 0]
0910�	
  0920�	GROUP FRAMES 4 LINES 1 CHARACTERS 3
  0930�		TLINE 1 SIZE 9 TITLE RIGHT
     0940�			SIZE 3 "-"+BREAKN
   0950�	
  0960�	GROUP FRAMES 3 LINES 2
0970�		TLINE 1 SIZE 3 " "+INDEX ITEM 3 PAGE 1
   0980�			SIZE 6 INDEX ITEM 2 PAGE 1
    0990�		TLINE 2 SIZE 3 " "+VAR6
   1000�			SIZE 6 VAR5
    1010�	
  1020�	GROUP FRAMES 2 LINES 0
1030�		TLINE 1
    1040�	
  1050�	GROUP FRAMES 2 LINES 2
1060�		TLINE 1 SIZE 6 DAY+"-"+MON3 CENTER
  1070�		TLINE 2 SIZE 6 YEAR CENTER
1080�	
  1090�	GROUP FRAMES 2 IFNO GLOBAL [LINES 0]
 1100�		TLINE 1 SIZE 6 FICHEN RIGHT
    1110�	
  1120�	#NFILE
 1130�	
  1140�	REVERSE
1150�	
  1160�	GROUP FRAMES 1 IFNO TITLE2 [FRAMES 0] CHARACTERS 3 LINES 2
                   1170�		TLINE 1 SIZE 3 TITLE START 1 FOR 3
  1180�		TLINE 2 SIZE 3 TITLE START 4 FOR 3
  1190�	
  1200�	#END DEBUG
  