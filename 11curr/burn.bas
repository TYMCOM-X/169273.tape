1	extend

5	dim #10%, d$(4096) &
\	open "burn.tmp" as file 10% &
\	print "Prom burner  V1.2" &
\	input "Reset prom burner with power switch and type <cr>";t$ &

100	! Get unit number for prom burner keyboard, &
	! assign the unit (to keep it connected), and &
	! initialize miscellaneous variables &
	dim s%[30] &
\	change sys(chr$(6%)+chr$(-23%)+"PB:") to s% &
\	pb% = s%(25%) &
\	s%(0%) = 30% &
\	s%(1%) =  6% &
\	s%(2%) = 10% &
\	s%(i%) =  0% for i%=3% to 22% &
\	change s% to s$ &
\	s$ = sys(s$) &
\	pb.chan% = 2%		! I/O channel for PB &
\	false% = 0%		! Failure indicator &
\	true% = -1%		! Success indicator &
\	ttclr$ = chr$(11%)	! Clear type ahead &
\	esc$ = chr$(27%)	! Escape character &
	! Define network terminal parameters &
\	tpcic% = 1%		! Command interrupt chr (^\) &
\	tpech% = 2%		! Echo control &
\	tprat% = 9%		! Baud rate (input/output) &
\	tphic% = 13%		! Hard interrupt chr (^C) &
\	tpsic% = 14%		! Soft interrupt chr (^Z) &
\	tposc% = 15%		! Output suppress chr (^O) &
\	tpdel% = 16%		! Delete chr (RUBOUT) &
\	tpldc% = 17%		! Line delete chr (^U) &
\	tplrc% = 18%		! Line retype chr (^R) &
\	tplfi% = 31%		! Line feed insertion &
\	tpcri% = 32%		! Carriage return insertion &

120	! Set error trap, init prom burner, and &
	! set terminal characteristics &
	on error goto 19000 &
\	open "PB:/MO:1" as file pb.chan% &
\	field #pb.chan%,1% as p$ &
\	s$ = sys(chr$(6%)+chr$(16%)+chr$(-3%)+chr$(pb%)+ &
		chr$(tpcic%)+chr$(0%)+ &
		chr$(tpech%)+chr$(0%)+ &
		chr$(tprat%)+chr$(14%)+ &
		chr$(tphic%)+chr$(0%)+ &
		chr$(tpsic%)+chr$(0%)+ &
		chr$(tposc%)+chr$(0%)+ &
		chr$(tpdel%)+chr$(0%)+ &
		chr$(tpldc%)+chr$(0%)+ &
		chr$(tplrc%)+chr$(0%)+ &
		chr$(tplfi%)+chr$(0%)+ &
		chr$(tpcri%)+chr$(0%)+ &
			chr$(0%)) &

200	! Try to get the prom burner in sync &
	print "PB: synchronizing..." &
\	if fnsync%<>true% then print "?Can't synchronize" &
\	goto 32760 &

220	on error goto 19000
230	print "# "; \ get \ field recount as cmd$ &
\	cmd$ = left(cvt$$(cmd$,32% or 4%),1%)
234	goto 1000 if cmd$="L"
236	goto 2000 if cmd$="P"
240	goto 200 if cmd$="R"
242	goto 32760 if cmd$="E"
244	print "L => list" \ print "P => program"
246	print "R => reset" \ print "E => exit" &
\	goto 230
1000	! &
	!	L(ist) command &

1010	input "Output file name [KB:] ";t$ &
\	o% = 0% &
\	if len(t$) then o%=1% &
\	t$ = t$+".LST" if instr(1%,t$,".")=0% &
\	open t$ for output as file o% &

1020	input "Rom type";h$ &
\	rom.size%=3% &

1030	input "Rom start, stop addresses to list (octal)";a%,b% &
\	a$ = fno2h$(a%,rom.size%)+fno2h$(b%,rom.size%)+"L" &
\	print "Listing ..." &
\	print #o%,h$+","; &
\	print "?Out of sync" if fnlst%(a$)<>true% &
\	close o% &
\	goto 230 &

2000! Program routine
2002	print "ROM file"; &
\	input line romfil$ &
\	romfil$ = cvt$$(romfil$,-1%) &
\	romfil$ = romfil$+".ROM" if instr(1%,romfil$,".")=0% &
\	open romfil$ for input as file 1% &
\	input #1,romtyp$,romsiz0%,romsiz1%,firstad$,lastad$,version$,comments$ &
\	firstad$ = cvt$$(firstad$,32%) &
\	lastad$ = cvt$$(lastad$,32%)
2010	print "Is the promburner configured for:" &
\	print "  ";romtyp$;" => "; romsiz0%; " X "; romsiz1%; &
\	input t$ &
\	if "Y" <> left(cvt$$(t$,32% or 4%),1%)  goto 2010 &
		else  print "Reading ROM file ... ";
2012	for i%=1% to romsiz0% &
\	  input #1%, w$ &
\	  d$(i%) = cvt$$(w$,32% or 4%) &
\	  next i% &
\	close #1% &
\	on error goto 6700 &

2020	print "done" + cvt%$(3338%)+"Prechecking ";romtyp$; " ... "; &
\	open "nl:" as file #1% &
\	a$ = firstad$ + lastad$ + "P" &
\	sleep 2%	!Wait for PB: to initialize and precheck &
\	gosub 6000 &
\	gosub 6600
2026	p% = 0%
2030	if b$=chr$(21%) then &
	print "" &
\	p% = 1% &
\	input "?Burnt chip - continue";T$ &
\	goto 230 if ascii(cvt$$(t$,32%))<>89% &
\	b$=chr$(6%)
2040	if b$ <> chr$(6%) goto 2300
2050	gosub 6600 &
\	if b$ <> chr$(2%) goto 2300
2060	gosub 6600 &
\	if b$ <> chr$(13%) goto 2300
2070	gosub 6600 &
\	if b$ <> chr$(10%) goto 2300
2080	sleep 2%
2090	if p% = 0% then print "appears good." &

2100	if t$="2708" then print "Downloading PB: ... "; else &
		print "Programming PB: ... ";
2102	n% = 0%
2104	for j% = 1% to romsiz0%
2105	  a$ = d$(j%)
2106	  for k% = 1% to romsiz1%/4%
2108	    print #2, mid(a$,k%,1%);
2112	    gosub 6600
2114	    next k%
2116	  gosub 6600 if j% <> romsiz0%
2118	  if b$="?" then print if pos(0%) &
\	  print "? Failure at address ";a$ \ goto 2810 &
\	  n% = n% + 1% &
\	  if n% < 16% goto 2190
2186	  print #2%, chr$(13%); \ gosub 6600
2190	  next j% &
\	if romtyp$<>"2708" then 2800 else print "done." &
\	print "Programming ... "; &
\	badflg% = 0% &
\	bad$ = "" &

2200! send data and process responses
2201	for kk% = 1% to 5% &
\	  gosub 6600  while  b$=chr$(10%) or b$=chr$(13%) or b$=chr$(32%) &
\	  if b$ = chr$(42%) goto 2800 &
\	  if b$=chr$(42%) and badflg%=0% goto 2810 &
\	  if b$=chr$(42%) and badflg%<>1% goto 2800 &
\	  badflg% = 1% &
\	  bad$ = bad$ + b$ &
\	  next kk%
2262	print chr$(7%);"Failed !"
2270	print "Bad location ";left(bad$,3%);" returned value ";right(bad$,4%)
2280	goto 2199
2300	print "Out of sequence - help! I just got a",ascii(b$)
2302	stop
2800	print "successful !",chr$(7%)
2802	goto 200
2810	print chr$(7%);"Programming failed - try a new chip!";chr$(7%)
2902	goto 200
3000	input "Type file name to compare",a$
3001	on error goto 6700
3010	open a$ for input as file #1%
3020	input #1%, promtyp$, x$, y$
3030	a$ = "0003FFL"
3040	gosub 6000
3050	for j% = 1% to 1024%
3060	  input #1%, z$
3070	  gosub 6600
3080	  y$ = b$
3090	  gosub 6600
3100	  y$ = y$ + b$
3110	  if y$ <> z$ goto 3200
3120	  next j%
3200	print "Mismatch at location ";j%,"File =>";ascii(z$);" Prom => ";ascii(y$)
3210	goto 230
6000	for i% = 1% to len(a$)
6010	print #2%, mid(a$,i%,1%);
6030	  gosub 6600
6040	  print #1%, b$;
6050	  next i%
6060	return
6600	wait 5% &
\	get #2%, count 1% &
\	field #2%, recount as b$ \ wait 0% \ b$ = cvt$$(b$,1%) &
\	return
6700	resume 32760 if erl=230 &
\	open 'PB:/MO:1' as file pb.chan% &
\	resume 6600 if err=11% or err=15% &
\	print "error ",err,"at line ",erl &
\	resume 230
7000	! &
	!	FNH$ - Hexidecimal conversion function &

7010	def fnh$(t%,t1%) &
\	t$ = string$(t1%,ascii("0")) &
\	rset t$ = t$+mid("0123456789ABCDEF",((t%/16%^t2%) and 15%)+1%,1%) &
		for t2% = t1%-1% to 0% step -1% &
\	fnh$ = t$ &

7020	fnend &

7050	! &
	!	FNO2H - Octal to hexidecimal function &

7060	def fno2h$(n%,t1%) &
\	t$ = num1$(n%) &
\	t2% = 0% &
\	t2% = t2%*8%+ascii(mid(t$,t%,1%))-48% for t%=1% to len(t$) &
\	fno2h$ = fnh$(t2%,t1%) &

7080	fnend &

7100	! &
	!	FNSYNC - Synchronization function &

7110	def fnsync% &
\	t$ = sys(ttclr$+chr$(pb.chan%)) &
\	print #pb.chan%,record 4096%,"X"; &
\	on error goto 7170 &
\	wait 5% &
	! Clear input buffer, print an "X", and allow 5 second timeout &

7120	while t$<>"*" &
\	get #pb.chan%,count 1% &
\	t$ = cvt$$(p$,1%) &
\	if t$<>"*" then s$=sys(ttclr$+chr$(pb.chan%)) &
\	print #pb.chan%,record 4096%,esc$; &
	! Function loop - read characters (stripping off parity bit) &
	! until we see a splat.  Print escapes along the way &

7130	next &
\	fnsync% = true% &
\	goto 7180 &
	! Got the splat, so say we found it and exit &

7170	fnsync%=false% &
\	resume 7180 &
	! Any error means it didn't work, so return FALSE &

7180	wait 0% &
\	on error goto 19000 &
	! Reset terminal input wait time and error trap &

7190	fnend &

7200	! &
	!	FNLST - output a string to prom burner &

7210	def fnlst%(t$) &
\	on error goto 7270 &
\	wait 5% &
\	fnlst% = false% &
	! Set error trap and timeout, assume failure &

7220	for t% = 1% to len(t$) &
\	print #pb.chan%,record 4096%,mid(t$,t%,1%); &
\	get #pb.chan% &
\	field #pb.chan%,recount as s$ &
\	print #o%,s$; &
	! Output the string until done. &

7230	next t% &
\	fnlst% = fnin% &
\	goto 7280 &
	! Loop until done, and then get any pending input &

7270	resume 7280 &
	! Catch errors &

7280	wait 0% &
\	on error goto 19000 &
	! Restore wait timer and error trap &

7290	fnend &

7300	! &
	!	FNIN - Input text from prom burner &

7310	def fnin% &
\	on error goto 7340 &
\	wait 5% &
\	fnin% = false% &
	! Set timeout and error trap &

7320	while true% and cvt$$(p$,1%)<>"*" &
\	get #pb.chan%,count 1% &
\	print #o%,cvt$$(p$,1%); &
	! Main function loop - print until splat or timeout &

7330	next &
\	fnin% = true% &
\	goto 7380 &
	! Return success when splat found &

7340	resume 7380 &
	! Timeout, so return failure &

7380	wait 0% &
\	on error goto 19000 &
\	print #o% if pos(0%)<>0% &
	! Reset timeout to infinity, reset error trap &

7390	fnend &

19000	resume 32760 if erl=230
19999	on error goto 0
32760	close pb.chan%
32762	a$=sys(chr$(6%)+chr$(11%)+string$(20%,0%)+"KB"+chr$(15%)+chr$(255%))
32764	close 10%
32766	kill "burn.tmp"
32767	end
                                                                                                                                                                                                                               