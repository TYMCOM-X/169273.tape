0010�	#TEXT
  0020�	;	head!part	 var41
    0030�	;	page!file	 var42
    0040�	;	l1!start!token	 var43
    0050�	;	l1!scan!token	 var44
0060�	;	l2!start!token	 var45
    0070�	;	l2!scan!token	 var46
0080�	;	new!block	 var47
    0090�	
  0100�	;	page!code	 var2
0110�	;	page!outer!block var3
    0120�	;	page!inner!block var4
    0130�	;	page!first!index var5
    0140�	;	page!end!index	 var6
0150�	;	fiche!end!index	 var7
    0160�	;	source!file	 var8
   0170�	;	block!on!fiche!N var(9+N)
    �  0010�	
  0020�	var41=line 1 scan ;
   0030�	     " FAIL "	characters 26; 20 chars date/time, 6 for source-file
           0040�	;WARNING -- this field spec is not generally correct for Fail,
0050�	;		it only happens to work for the SAIL compiler
    0060�	    !"SYMBOL TABLE"		    ="____________________T|Symb";
  0070�	    !"SYMBOL CROSS REFERENCE"	    ="____________________X|Symb";
   0080�	    !"MACRO/OPDEF CROSS REFERENCE"  ="____________________X|MaOp";
 0090�	    !"OPCODE CROSS REFERENCE"	    ="____________________X|Opcd"
    0100�	;var42= var41 + IFNO var41 [	     "____________________T|Blok"]
    0110�	
  0120�	var8= var41 start 21 for 6 ;
    0130�		+ IFNO var41 ["T|Blok"] + IF var41 ;
0140�		[ IFNO var41 start 21 for 6 [var8] ]
     0150�	
  0160�	;var8 has either the source file FAIL used to compile this page
    0170�	;     or a fake file name (uses lower case) to indicate CROSS-generated
 0180�	
  0190�	var2=line 1 scan " PAGE  " characters 6 stop on blank
    0200�	
  0210�	;var2 has the page number (or number-number) on the source file
    0220�	
  0230�	;var44=line 1 scan " " skip blank characters 10 stop after item
    0240�	;var43=line 1 start 1 for 40 stop on blank
0250�	;var3= var43 + IFNO var43;
 0260�	;	[var44 + IFNO var44 [var3]]
   0270�	
  0280�	;var3 is the title or outer block name (line 1 of listing page data)
    0290�	
       0300�	var46=line 2 scan " " skip blank characters 10 stop after item
0310�	var45=line 2 start 1 for 40 stop on blank
 0320�	var4= var45 + IFNO var45;
  0330�		[var46 + IFNO var46 [var4]]
    0340�	
  0350�	;var4 is the subtitle or inner block name (line 2 of listing page data)
 0360�	
      �  0010�	
  0020�	var5=LINE FIRST AFTER 2 START 1 FOR 8 STOP AFTER ITEM
    0030�	
  0040�	;var5 is the first element on this page (cref # or symbol)
    0050�	
  0060�	var6=LINE  LAST AFTER 2 START 1 FOR 8 STOP AFTER ITEM
    0070�	
  0080�	;var6 is the last element on this page (cref # or symbol)
0090�	
                      0100�	var7= var6 + IFNO var6 [var7]
   0110�	
  0120�	;var7 is the last element on this fiche-set (cref # or symbol)
0130�	
      �  0010�	var10= var10 + IFNO var10 [var8]	;the first block encountered
 0020�	
  0030�	var11= var11 + IF var10 [IFNO var11 [IFDIF var10 var8 [;
 0040�	;; *** was:	IFDIF var8 "SAIL" [var8]]]]
   0050�		var8]]]
    0060�	var12= var12 + IF var11 [IFNO var12 [IFDIF var11 var8 [;
 0070�		var8]]]
    0080�	var13= var13 + IF var12 [IFNO var13 [IFDIF var12 var8 [;
 0090�		var8]]]
    0100�	var14= var14 + IF var13 [IFNO var14 [IFDIF var13 var8 [;
 0110�		var8]]]
                                  0120�	;var15= var15 + IF var14 [IFNO var15 [IFDIF var14 var8 [;
0130�	;	var8]]]
   0140�	;var16= var16 + IF var15 [IFNO var16 [IFDIF var15 var8 [;
0150�	;	var8]]]
   0160�	;var17= var17 + IF var16 [IFNO var17 [IFDIF var16 var8 [;
0170�	;	var8]]]
   0180�	;var18= var18 + IF var17 [IFNO var18 [IFDIF var17 var8 [;
0190�	;	var8]]]
   0200�	;var19= var19 + IF var18 [IFNO var19 [IFDIF var18 var8 [;
0210�	;	var8]]]
   0220�	;var20= var20 + IF var19 [IFNO var20 [IFDIF var19 var8 [;
0230�	;	var8]]]
   0240�	;var21= var21 + IF var20 [IFNO var21 [IFDIF var20 var8 [;
0250�	;	var8]]]
                                                0260�	;var22= var22 + IF var21 [IFNO var22 [IFDIF var21 var8 [;
0270�	;	var8]]]
   0280�	;var23= var23 + IF var22 [IFNO var23 [IFDIF var22 var8 [;
0290�	;	var8]]]
   0300�	;var24= var24 + IF var23 [IFNO var24 [IFDIF var23 var8 [;
0310�	;	var8]]]
   0320�	;var25= var25 + IF var24 [IFNO var25 [IFDIF var24 var8 [;
0330�	;	var8]]]
   0340�	;var26= var26 + IF var25 [IFNO var26 [IFDIF var25 var8 [;
0350�	;	var8]]]
   0360�	;var27= var27 + IF var26 [IFNO var27 [IFDIF var26 var8 [;
0370�	;	var8]]]
   0380�	;var28= var28 + IF var27 [IFNO var28 [IFDIF var27 var8 [;
0390�	;	var8]]]
                                                0400�	;var29= IF var28 [IFNO var8 [var9] + IF var8
   0410�	;		 [IFEQ var8 "SAIL" [var9] + var8]]]
    0420�	;;;; *** note this follows the LAST block ***
      �  0010�	outvar	=  LOC start 1 for 1 + loc start 3 for 2 + " "	;shortened LOC
    0020�		+ "/" + breakn + IFNO breakn ["0"]	;fiche number
   0030�		+ " [" + var5 + ":" + var7 + "] ";
  0040�		+ " " + var8 + "[" + var4 + "]";
    0050�		+ var2
0060�	
  0070�	#PAGE
  0080�	
  0090�	KEEP var7
   0100�	KEEP var8
   0110�	KEEP var10
  0120�	KEEP var11
  0130�	KEEP var12
  0140�	KEEP var13
  0150�	KEEP var14
  0160�	KEEP var15
  0170�	KEEP var16
            0180�	KEEP var17
  0190�	KEEP var18
  0200�	KEEP var19
  0210�	KEEP var20
  0220�	KEEP var21
  0230�	KEEP var22
  0240�	KEEP var23
  0250�	KEEP var24
  0260�	KEEP var25
  0270�	KEEP var26
  0280�	KEEP var27
  0290�	KEEP var28
  0300�	KEEP var29
  0310�	
  0320�	#OVERFLOW PAGE
   0330�	
  0340�	KEEP var8
   0350�	KEEP var2
   0360�	KEEP var6
   0370�	KEEP var7
   0380�	;KEEP var3
  0390�	KEEP var4
   0400�	KEEP var10
  0410�	KEEP var11
  0420�	KEEP var12
  0430�	KEEP var13
  0440�	KEEP var14
  0450�	KEEP var15
  0460�	KEEP var16
  0470�	KEEP var17
  0480�	KEEP var18
  0490�	KEEP var19
                 0500�	KEEP var20
  0510�	KEEP var21
  0520�	KEEP var22
  0530�	KEEP var23
  0540�	KEEP var24
  0550�	KEEP var25
  0560�	KEEP var26
  0570�	KEEP var27
  0580�	KEEP var28
  0590�	KEEP var29
  0600�	BLANK 3
0610�	
      �  0010�	#INDEX
 0020�	
  0030�	;;	"OPFILE 22-10~~NAMEIO~crefn~B 1"
  0040�	;;	"123456789 123456789 123456789 "
  0050�	HEADING "Source Page    Block Index Loc"
  0060�	
  0070�	item 1 size 6 var8 change clear item 3 supress unchanged
 0080�	;file name
  0090�	locate 8
    0100�	item 2 size 6 var2 + IFNO var2 [" *** "]
  0110�	;page-in-file
    0120�	locate 15
                                 0130�	item 3 size 6 var4 right supress unchanged
0140�	;inner block name
0150�	locate 21
   0160�	item 4 size 6	;; XXX --var5 right
    0170�			;; XXX -- first index on page
 0180�			var7 right
0190�	;last index on page (in fact, so far)
0200�	locate 28
   0210�	item 5 size 3   loc start 1 for 1 + loc start 3 for 2
    0220�	;this is simply a compressed (3-character) LOC
 0230�	
      �  0010�	#FICHE
 0020�	
  0030�	GROUP FRAMES 2 CHARACTERS 3 LINES 1;
 0040�		IF GLOBAL START 7 FOR 1 [CHARACTERS 4]
   0050�		TLINE 1 SIZE 8 GLOBAL start 1 for 8
 0060�	
  0070�	group frames 12 lines 1 characters 3
                0080�		tline 1 size 36;
0090�		var10+ IF var11 [","+var11]+ IF var12 [","+var12]+;
0100�		IF var13 [","+var13]+ IF var14 [","+var14]	center
      �  0010�	
  0020�	GROUP FRAMES 2 LINES 2 characters 3
  0030�		TLINE 1 SIZE 6 DAY+"-"+MON3 CENTER
  0040�		TLINE 2 SIZE 6 YEAR CENTER
0050�	
  0060�	#NFILE
 0070�	
  0080�	GROUP FRAMES 1 CHARACTERS 3 LINES 1;
 0090�		if title start 4 for 1 [ characters 4 ;
  0100�		if title start 5 for 1 [ characters 3 lines 2 ] ]
  0110�		TLINE 1 SIZE 4 TITLE START 1 FOR 3 +;
    0120�			if title start 4 for 1;
  0130�			[ifno title start 5 for 1 [title start 4 for 1]]
                 0140�		TLINE 2 SIZE 3 TITLE START 4 FOR 3
  0150�	
  0160�	#END DEBUG
  