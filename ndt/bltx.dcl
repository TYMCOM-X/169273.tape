
external integer procedure BLOAT( integer code; 
		integer  input!AOBJN; integer procedure more!input;
		integer output!AOBJN; integer procedure more!output );

COMMENT code= 6, 7, 9, -6, -7, or -9
	BLOAT used a table in TREE and an input stream of bits to
		produce an output byte stream [7-bitters only SET the
		line number bit, they don't clear it]
		eof on input or output are done by returning 0, else
		wants XWD -len,firstloc returned
		(can either start with those ptrs, or 0 to get by pcall)
;

