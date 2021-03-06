
EXTERNAL PROCEDURE FRQ1P( INTEGER AOBJN!PTR );
Comment	FRQ1P - count the characters in XWD -size,,first [no repeats]
	increments elements of arrays A.6, A.7, A.9 to reflect totals
;

EXTERNAL PROCEDURE FRQ1PX( INTEGER AOBJN!PTR );
Comment	FRQ1PX - count the characters in XWD -size,,first [use repeats]
	increments elements of arrays B.6, B.7, B.9 to reflect totals
	*** warning: repeaters DONT cross "blocks", 
	*** use the same AOBJN!PTR sequence when storing
;

EXTERNAL INTEGER PROCEDURE PASS1B( INTEGER ARRAY FREQDIST );
Comment
  return: -1=error, no results (eg: frequencies are 0)
  return non-0 is variable number of words to represent file
				[size(tree) + size(bitstream)]
	Note: this is NOT file size (need: treeSiz, inBytes, CodeType)
		TREE[0] is tree size (and number of top node)
		TREE[1 : TREE[0]] is the tree itself
		TREE[1+TREE[0]] is the "frequency total"
		FREQ[n] is the "bit string" for character N
			[DWS JFFO format]
;
Comment: the "straightforward" frequency tables;
EXTERNAL INTEGER ARRAY A.6[0:'77], A.7[0:'200], A.9[0:'777];

REDEFINE #=-100;	Comment: subject to change, use arrinfo;
Comment: the frequency tables with repeat counts in -n;
EXTERNAL INTEGER ARRAY B.6[#:'77], B.7[#:'200], B.9[#:'777];

EXTERNAL INTEGER ARRAY FREQ [#:'777];

EXTERNAL INTEGER ARRAY TREE [0:'1000];	Comment upper bound varies;
Comment typical use:
	INTEGER bestCode, words:
	INTEGER ARRAY bestTree[0:'1000]:

	PROCEDURE try( INTEGER code: INTEGER ARRAY ar ):
	 BEGIN INTEGER t:
	 IF 0 < (t_ pass1b(a)) < words  AND  tree[0] < '1000
	  THEN BEGIN 
		bestCode_ code: 
		words_ t:
		ARRBLT( bestTree[0], tree[0], tree[0]+2 ): 
		ARRBLT( ar[t_arrinfo(ar,1)], freq[t], arrinfo(ar,0) )
	       END
	 END:
	INTEGER abjp, chan,eof,blocksize:
	... set up chan mode 8(='10) with eof vbl, choose blocksize ...
	ARRCLR(a.6): ARRCLR(a.7): ARRCLR(a.9): 
	ARRCLR(b.6): ARRCLR(b.7): ARRCLR(b.9): 
	abjp_ -blockSize LSH 18 + LOCATION(inbuf[0]):
	bestCode_ 0: words_ 4: << since less leader needed >>
	   DO BEGIN
		ARRYIN(inbuf[0],blocksize):
		IF eof
		 THEN BEGIN
			abjp_ -eof LSH 18 + LOCATION(inbuf[0]):
			words_ words + (eof LAND (-1 LSH -18))
		      END
		 ELSE words_ words + blocksize:
		frq1p( abjp ):
		frq1px( abjp )
	      END
	 UNTIL EOF:
	try(1, a.6): try(2, a.7): try(3, a.9): 
	try(4, b.6): try(5, b.7): try(6, b.9): 
	CASE bestCode 
	   OF BEGIN
		noSquish: 
		squish(6,a.6): squish(7,a.7): squish(9,a.9): 
		squish(6,b.6): squish(7,b.7): squish(9,b.9)
	      END
;
    