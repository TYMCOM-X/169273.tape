10	extend
20	input "Input file "; ifile$
22	input "Output file [KB:] "; ofile$
24	if len(ofile$) = 0% then ofile$ = "kb:"
26	open ofile$ for output as file #2
28	print #2, "Hex dump of file ";cvt$$(ifile$,32%)
30	open ifile$ for input as file #1
32	input #1, header$
34	print #2, header$
40	for i% = 1% to 2048%
50	  input #1, char$
60	  print #2, " "+cvt$$(char$,32%);
66	  if i%/16%*16% = i% then print #2
70	  next i%
72	print #2, chr$(12%)
80	close #1,#2
90	goto 20
32767	end
              