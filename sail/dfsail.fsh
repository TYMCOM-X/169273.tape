0010�	#TEXT
  0020�	
      �  0010�	var41= line 1 scan ;
  0020�		 " FAIL "			characters 26;
0030�		!"SYMBOL TABLE"		      ="____________________T|Symb";
   0040�		!"SYMBOL CROSS REFERENCE"     ="____________________X|Symb";
 0050�		!"MACRO/OPDEF CROSS REFERENCE"="____________________X|MaOp";
 0060�		!"OPCODE CROSS REFERENCE"     ="____________________X|Opcd";
 0070�	
  0080�	var8= var41 start 21 for 6 ;
    0090�		+ IFNO var41 ["T|Blok"] + IF var41 ;
0100�		[ IFNO var41 start 21 for 6 [var8] ]
0110�	
  0120�	var2= line 1 scan " PAGE  " characters 6 stop on blank
   0130�	
                                          0140�	var46= line 2 scan " " skip blank characters 10 stop after item
    0150�	var45= line 2 start 1 for 40 stop on blank
0160�	var4= var45 + IFNO var45 [var46 + IFNO var46 [var4]]
0170�	
  0180�	var3= line first after 2 start 1 for 8 stop after item
   0190�	var5= line first after 2 start 1 for 8 stop after item
   0200�	
  0210�	var6= line last  after 2 start 1 for 8 stop after item
   0220�	
  0230�	
  0240�	var9= if var4;
   0250�	      [ ifdif	"," + var4;	(if new block)
  0260�			ifdif "," var9 start 1 for 1 [","] + var9;
   0270�		[ ifdif var4 "SAIL" [","] ];	make SAIL look like continue
                        0280�		+ var4]
    0290�	
  0300�	var11= var11 +  ifeq "," var9 start 1 for 1;
   0310�			[ifno var11 start 42 for 8 [var9]]
 0320�	var12= var12 +  ifeq "," var9 start 1 for 1;
   0330�			[ifno var12 start 42 for 8;
   0340�			 [if  var11 start 42 for 8 [var9]]]
0350�	var13= var13 +  ifeq "," var9 start 1 for 1;
   0360�			[ifno var13 start 47 for 8;
   0370�			 [if  var12 start 42 for 8;
   0380�			  [if  var11 start 42 for 8 [var9]]]]
   0390�	
  0400�	outvar=	title + "/"+breakn + loc start 1 for 1 + loc start 3 for 2 +;
   0410�		"/"+var3 + "/"+var6 + "/"+var8 + "/"+var4 + "/"+var2 + "/"+var9
       �            0010�	#page
  0020�	KEEP var41
  0030�	KEEP var8
   0040�	KEEP var2
   0050�	KEEP var46
  0060�	KEEP var45
  0070�	KEEP var4
   0080�	KEEP var5
   0090�	KEEP var6
   0100�	KEEP var9
   0110�	KEEP var11
  0120�	KEEP var12
  0130�	KEEP var13
  0140�	
  0150�	#overflow page
   0160�	KEEP var3
   0170�	KEEP var41
  0180�	KEEP var8
   0190�	KEEP var2
   0200�	KEEP var46
  0210�	KEEP var45
  0220�	KEEP var4
   0230�	KEEP var5
   0240�	KEEP var6
   0250�	KEEP var9
   0260�	KEEP var11
  0270�	KEEP var12
  0280�	KEEP var13
  0290�	
  0300�	blank 3
0310�	
      �  0010�	#index
                               0020�	heading "Source Page Loc  Coord Block"
    0030�	item 1 size 6 var8 change clear item 5 supress unchanged
 0040�	locate 8
    0050�	item 2 size 4 var2 + "....."
    0060�	locate 13
   0070�	item 3 size 3 loc start 1 for 1 + loc start 3 for 2
 0080�	locate 17
   0090�	item 4 size 6	ifdif "|" var4 start 2 for 1 [var6] +;
0100�			ifeq  "|" var4 start 2 for 1 [var3]
0110�	locate 24
   0120�	item 5 size 7	ifdif "|" var4 start 2 for 1 [" " + var4] +;
    0130�			ifeq  "|" var4 start 2 for 1 [":" + var6];
   0140�			supress unchanged
   0150�	
  0160�	#fiche
 0170�	
  0180�	group frames 2 lines 1 characters 3;
      0190�		if breakn [if file start 5 for 2 [characters 4]];
  0200�		if file start 7 for 2 [characters 4]
0210�		tline 1 size 8 file start 1 for 6 +;
0220�				if breakn ["-" + breakn] +;
  0230�				ifno breakn [file start 7 for 2]
  0240�	
  0250�	group frames 12 characters 4 lines 1;
0260�			if var12 [lines 2];
 0270�			if var13 [lines 3]
  0280�		tline 1 size 48 var11 start 2 for 48 center
   0290�		tline 2 size 48 var12 start 2 for 48 center
   0300�		tline 3 size 48 var13 start 2 for 48 center
   0310�	
  0320�	group frames 2 characters 4 lines 2
  0330�		tline 1 size 8 " " + var5
                                    0340�		tline 2 size 8 " " + var6
 0350�	
  0360�	#nfile
 0370�	group frames 1 characters 3 lines 1;
 0380�		ifno file2 [frames 0];
    0390�		if file start 4 for 1 [characters 4];
    0400�		if file start 5 for 2 [characters 3 lines 2]
  0410�	
  0420�		tline 1 size 4 file start 1 for 3 +;
0430�				ifno file start 5 for 2 [file start 4 for 1]
0440�		tline 2 size 3 file start 4 for 3
   0450�	#END
   