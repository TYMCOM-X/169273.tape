0010�	#TEXT
  0020�	
      �  0010�	var41= line 1 scan ;
  0020�		 " FAIL "			characters 26;
0030�		!"SYMBOL TABLE"		      ="____________________T!Symb";
   0040�		!"SYMBOL CROSS REFERENCE"     ="____________________X!Symb";
 0050�		!"MACRO/OPDEF CROSS REFERENCE"="____________________X!MaOp";
 0060�		!"OPCODE CROSS REFERENCE"     ="____________________X!Opcd";
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
  0180�	var5= line first after 2 start 1 for 8 stop after item
   0190�	
  0200�	var6= line last  after 2 start 1 for 8 stop after item
   0210�	
  0220�	
  0230�	var9= if var4;
   0240�	      [ ifdif	"," + var4;	(if new block)
  0250�			ifdif "," var9 start 1 for 1 [","] + var9;
   0260�		[ ifdif var4 "SAIL" [","] ];	make SAIL look like continue
    0270�		+ var4]
    0280�	
                                                         0290�	var11= var11 +  ifeq "," var9 start 1 for 1;
   0300�			[ifno var11 start 32 for 8 [var9]]
 0310�	var12= var12 +  ifeq "," var9 start 1 for 1;
   0320�			[ifno var12 start 32 for 8;
   0330�			 [if  var11 start 32 for 8 [var9]]]
0340�	var13= var13 +  ifeq "," var9 start 1 for 1;
   0350�			[ifno var13 start 32 for 8;
   0360�			 [if  var12 start 32 for 8;
   0370�			  [if  var11 start 32 for 8 [var9]]]]
   0375�	
  0380�	outvar= fichen+if var13 [var12+"/3\"+var13]+;
  0385�		ifno var13 [var11+"/2\"+var12]
     �  0010�	#page
  0020�	KEEP var41
  0030�	KEEP var8
   0040�	KEEP var2
   0050�	KEEP var46
       0060�	KEEP var45
  0070�	KEEP var4
   0080�	KEEP var5
   0090�	KEEP var6
   0100�	KEEP var9
   0110�	KEEP var11
  0120�	KEEP var12
  0130�	KEEP var13
  0140�	
  0150�	#overflow page
   0160�	KEEP var41
  0170�	KEEP var8
   0180�	KEEP var2
   0190�	KEEP var46
  0200�	KEEP var45
  0210�	KEEP var4
   0220�	KEEP var5
   0230�	KEEP var6
   0240�	KEEP var9
   0250�	KEEP var11
  0260�	KEEP var12
  0270�	KEEP var13
  0280�	
  0290�	blank 3
0300�	
      �  0010�	#index
 0020�	heading "<--------><--------><-------->"
  0030�	item 1 size 6 var8 change clear item 5 supress unchanged
 0040�	locate 8
              0050�	item 2 size 4 var2 + "....."
    0060�	locate 13
   0070�	item 3 size 4 loc start 1 for 1 + loc start 3 for 2
 0080�	locate 18
   0090�	item 4 size 6 var6 right
   0100�	locate 25
   0110�	item 5 size 6 var4 supress unchanged
 0120�	
  0130�	#fiche
 0140�	
  0150�	group frames 2 lines 1 characters 3;
 0160�			if global start 6 for 1 [characters 4]
  0170�		tline 1 size 8 if "*" [global + "       "]start 1 for 7 +;
   0175�				fichen;
  0180�	
  0190�	group frames 10 characters 4 lines 1;
0200�			if var12 [lines 2];
 0210�			if var13 [lines 3]
  0220�		tline 1 size 40 var11 start 2 for 40 center
             0230�		tline 2 size 40 var12 start 2 for 40 center
   0240�		tline 3 size 40 var13 start 2 for 40 center
   0250�	
  0260�	group frames 4 characters 4 lines 2
  0270�		tline 1 size 16 " SAIL Compiler " + fichen
    0280�		tline 2 size 16 " " + if "*" [var5+"      "] start 1 for 6 +;
0290�				":" + if "*" [var6+"      "] start 1 for 6
  0300�	
  0310�	#nfile
 0320�	group frames 1 characters 3 lines 1;
 0330�		ifno title2 [frames 0]
    0340�		tline 1 size 3 "???"
 0350�	#END DEBUG
  