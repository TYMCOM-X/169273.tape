login: wrs
Password:
Login incorrect
login: ferns

Vi is working again.  So is nroff -me.  Send mail
to root if anything else is missing.

Account name: wrs
Unauthorized user go bye-bye.

login: guest

Vi is working again.  So is nroff -me.  Send mail
to root if anything else is missing.

No messages.
1% ps
   PID TTY TIME CMD
  3721 1   0:02 ps 
  3714 1   0:04 -csh 
3% ls
core
4% mail
No mail.
8% cd /usr/ferns
10% ls -l
total 5
drwx--x--x 2 fmanag       48 Mar 19 01:19 Bin
drwxrwxrwx 3 fmanag      192 Mar 19 02:06 Lib
drwx------ 2 fmanag      768 Mar 19 01:19 Source
drwx------ 2 fmanag      160 Mar 19 02:06 Util
21% cd Lib
22% ls -l
total 21
-rwxr-xr-x 1 fmanag      747 Mar 18 19:11 HELPMSG
drwxrwxrwx 2 fmanag      160 Mar 19 07:10 Messages
-rwxr-xr-x 1 fmanag        0 Mar 18 19:11 arpamail
-rwxr-xr-x 1 fmanag       75 Mar 18 19:18 fmand
-rwxr-xr-x 1 fmanag      245 Mar 18 19:11 fmotd
-rwxrwxrwx 1 fmanag     3402 Mar 19 07:51 fusers
-rwxrwxrwx 1 fmanag       88 Mar 19 07:54 futmp
-rwxrwxrwx 1 fmanag     1728 Mar 19 07:12 headers
-rwxr-xr-x 1 fmanag      975 Mar 18 19:17 menu
-rwxr-xr-x 1 fmanag      751 Mar 18 19:17 news
24% cat menu

Command:                Description:
-------------------------------------
menu                    Get this list
news                    Get system news
list                    Get list of messages
read                    Read a message
write                   Write a message
delete                  Delete a message
passwd                  Change your password
search                  Search headers for specific information
system                  Get into main system (^D to get back to ferns)
who                     What ferns users are currently logged on
users                   Get a list of all ferns users
operator                Get Sysop's attention (if logged in)
chat                    Talk with another user
help                    Get help with system/functions
doc                     Read documentation on various topics
rmail                   Read mail sent here from the ARPAnet
amail                   Send mail to someone on the ARPAnet
quit                    Get out of ferns

25% who
rabin    console Mar 19 09:00
guest    tty1    Mar 19 12:33
rabin    tty7    Mar 19 07:22
27% users
guest rabin rabin
1% cd /usr/ferns/Lib
2% cat arpamail
3% cd Messages
4% ls -l
total 7
-rwxrwxrwx 1 fmanag      547 Mar 18 19:17 1
-rwxrwxrwx 1 fmanag      376 Mar 18 19:18 3
-rwxrwxrwx 1 fmanag      294 Mar 18 19:18 4
-rwxrwxrwx 1 fmanag      236 Mar 19 01:18 6
-rwxr-xr-x 1 ferns        27 Mar 19 05:55 7
-rwxr-xr-x 1 ferns       136 Mar 19 07:11 8
5% cat 1
Well, I found the bugs in the C compiler, and, after 1
week, Onyx Ferns Message System is back!  I have added a couple
new features.  They are:
	1) Message deleting
	2) Read-protected messages
I intend to add new features soon, such as:
	1) Security levels (not too important)
	2) Printer/Video-type editing
	3) More documentation (yeah!)
If you have any comments/suggestions, please leave me a message
here, or on 8bbs, or send mail to me at TIM@AI on arpanet.
This system was created for your enjoyment.  Have fun!


					-Tim Bessie (sysop)

.
7% cat 3
Hi Tim,
   This is much nicer than blindly flailing around in the %&%@! shell.
I accidently got into the shell by typing the 'system' command 'cause
it says 'get inFo system'  should be 'inTo' ... had to hang up and
re-dial 'cause I didn't know how to get back to 'ferns'.  Maybe the 
'list' starting message prompt could also show the msg range?  'Nuf
for now.  Regards,..
.
8% cat 4
Does this system have the game EMPIRE which was distributed for
the unix rev 7 users group meeting (I think in 1979)?
Not so much that I want to play it here, but I've been going
nuts trying to find a copy of the game to transport to some systems
that I have access to.		thanks...   ...phil

.
10% cat 6
I found that I opened the file that has your login statictics
to READ, and not both READ and WRITE.  Therefore, it was not
updating the files as it should.  I have put the correct command
in, and it now works correctly.

							-Tim

.
11% cat 7
No 1200 baud?
			...phil
.
12% cat 8
Tim,
Thanks for putting me on this system.  I like what i see so far
and i like the potential .   Again , thanks.  See ya soon,
jude

.
16% cd /usr/ferns/Lib
17% ls
HELPMSG		arpamail	fmotd		futmp		menu
Messages	fmand		fusers		headers		news
30% ls -l /usr/ferns
total 5
drwx--x--x 2 fmanag       48 Mar 19 01:19 Bin
drwxrwxrwx 3 fmanag      192 Mar 19 02:06 Lib
drwx------ 2 fmanag      768 Mar 19 01:19 Source
drwx------ 2 fmanag      160 Mar 19 02:06 Util
31% cd /usr
32% ls
adm	dict	games	include	man	pub	spool	sys	ucb
bin	ferns	guest	lib	msgs	public	src	tmp
34% ls src
cmd		openup.c
35% ls lib
atrun		eign		libmp.a		lpd		term
calendar	ex2.14strings	libretro.a	makekey		tmac
cign		font		libtermcap.a	me		units
crontab		learn		llib-lc		refer		uucp
diff3		lex		llib-lm		spell		yaccpar
diffh		libln.a		llib-port	struct
36% ls games
backgammon	fish		quiz		quiz.k
39% ls -l dict
total 678
-rw-rw-r-- 1 bin       50000 Oct 29 05:46 hlista
-rw-rw-r-- 1 bin       50000 Oct 29 05:46 hlistb
-rw-rw-r-- 1 bin       50000 Oct 29 05:46 hstop
-rw-rw-r-- 1 bin           0 Oct 29 05:46 spellhist
-rw-r--r-- 1 bin      196513 Oct 29 05:55 words
40% ls bin
arcv		deroff		mail		restor		touch
at		df		man		rev		tr
basename	diff3		mesg		rmail		troff
bc		du		mkdir		sanders		true
cal		dump		ncheck		sleep		tsort
calendar	dumpdir		neqn		sort		tty
cb		echo		newgrp		sp		uniq
checkeq		egrep		nice		spell		uucp
chgrp		enroll		nm		split		uulog
chmod		eqn		nroff		strip		uuname
chown		false		od		su		uustat
clri		fgrep		osh		sum		uux
cmp		file		passwd		tabs		wc
col		find		prep		tail		write
comm		icheck		print		tar		xget
crypt		join		pstat		tc		xsend
cu		kill		ptx		tee		yacc
dc		ln		punch		tension
dcheck		look		quot		test
dd		lorder		ranlib		tk
43% cd /etc
44% ls
cron	getty	init	motd	openup	termcap	update
ddate	glob	mkfs	mount	passwd	ttys	utmp
dmesg	group	mknod	mtab	rc	umount	wall
45% cat passwd
root:xrYpgrHn3CD0.:0:1		   :The Super User:/:/bin/csh
daemon:x:1:1			   :Cron and Co.:/:
bin:OfRFvyjKdNDBI:3:3		   ::/bin:/bin/csh
evan:HBwCptKzypV/M:101:1	   :& Schaffer:/a/evan:/bin/csh
bob:W7.2I26GwPm1Q:102:100	   :& Adams:/a/bob:/bin/csh
bill::103:100			   :& Robinson:/a/bill:/bin/csh
ellen::104:100			   :& Borger:/a/ellen:/bin/csh
tim:zLr79.yWx1uPY:105:110	   :& Bessie:/a/tim:/bin/csh
rabin:OeZk.GRPZ1Jec:106:1	   :Isaac K. &ovitch:/a/rabin:/bin/csh
uucp:YEFhue2T3N0CU:107:100	   :network:/usr/spool/uucppublic:/usr/lib/uucp/uucico
dave:088hkXNveLG/w:108:100	   :& Adams:/a/dave:/bin/csh
david:gnk4rfRclhvzc:109:100	   :& Robinson:/a/david:/bin/csh
bobby:088hkXNveLG/w:110:100	   :& Adams:/a/bobby:/bin/csh
wright:1CcX8FbwVhg9.:111:1	   :barbara &:/a/wright:/bin/csh
gayla:cWGhYtQDIfpqI:112:100	   :& Nethercott:/a/gayla:/bin/csh
michael::113:1			   :& Toy:/a/michael:/bin/csh
gripe::114:100			   ::/a/gripe:/bin/csh
ron:ZK/3V.XgGI5h.:115:100	   :& Henderson:/a/ron:/bin/csh
melanie:LS2ZmPKU0brUI:116:100	   :& Mayer:/a/melanie:/bin/csh
perkins::117:100		   :dan &:/a/perkins:/bin/csh
dan::118:100			   :& perkins:/a/perkins:/bin/csh
pohl:qN8mbns07vuGM:119:100	   :ira &:/a/pohl:/bin/csh
ira:qN8mbns07vuGM:120:100	   :& pohl:/a/pohl:/bin/csh
tenspeed::121:100		   :ira pohl:/bin/csh
blufox::122:100			   :al kelley:/a/blufox:/bin/csh
glendon:MXCGfUJctIcqg:123:100	   :& Wichman:/a/glendon:/bin/csh
figgy:StGvgminomHOE:124:100	   :peter broadwell:/a/figgy:/bin/csh
asgardd::125:100		   :J. R. Stoner:/a/asgardd:/bin/csh
who::126:100			   :The WHO program:/:/bin/who
tty::126:100			   :The TTY program:/:/usr/bin/tty
anton::127:100			   :& Dovydaitis:/a/anton:/bin/csh
diane::128:100			   :& Lewis:/a/diane:/bin/csh
traugott::129:100		   :mark &:/a/traugott:/bin/csh
friedla:Nbgu8ljOvYGo.:130:100	   :bill &nd:/a/friedla:/bin/csh
amy:bQHdgb2yT0LpI:131:100	   :& Robinson:/a/amy:/bin/csh
aeb:hqj0GRJQcULDs:132:100	   :Amy Barton:/a/aeb:/bin/csh
burlini:efmXvy978Ryjc:133:100	   :Dave Burlini:/a/burlini:/bin/csh
dennis:W8xkIR/4egG72:134:100	   :& McElrath:/a/dennis:/bin/csh
wittman::137:100		   :Donald &:/a/wittman:/bin/csh
guest::138:100			   :The ONYX & Account:/usr/guest:/bin/csh
dectek:xLzvBfKrzjB7c:139:110	   :Bernard Klatt:/a/dectek:/bin/csh
games:wn0QVyGnPCorA:140:100	   :The ONYX Games Account:/a/games:/bin/csh
steiny:vgVNRCJObHl8Q:141:100	   :Don &:/a/steiny:/bin/csh
paul::142:100			   :& Urbick III:/a/paul:/bin/csh
pomer:wkXRZ7/NiK3/E:143:100	   ::/a/pomer:/bin/csh
tessier:9zl0SLrko0gn.:144:100	   :Jim &:/a/tessier:/bin/csh
inp::145:100			   ::/a/inp:/bin/csh
huskey:5RD6A8odnTWOM:146:100	   ::/a/huskey:/bin/csh
pam:ysbLgf5uNnUVQ:147:100	   ::/a/pam:/bin/csh
archer:tZe7h2DPr6bao:148:100	   :Dane &:/a/archer:/bin/csh
eddie:rTy1mxcqxSrtU:149:100	   :& depew etsu:/a/eddie:/bin/csh
frank:wGiPJYgKM3Zh2:150:100	   :& Hausman:/a/frank:/bin/csh
ferns::152:110			   :Ferns Message System:/a/ferns:/usr/ferns/Bin/fernsh
film:E1m5FGp.u82hQ:153:100	   :David Meyers:/a/film:/bin/csh
bartlett:NbinYBnfSDceo:154:100	   :John &:/a/bartlett:/bin/csh
cuba:sVCgbUC5dOZXk:155:100	   :Larry &:/a/cuba:/bin/csh
leader:hACgZpFm.WzyE:156:110	   :Carl Hewitt:/a/leader:/bin/csh
fmanag:zLr79.yWx1uPY:157:110	   :Ferns Manager:/usr/ferns:/bin/csh
fortune::158:100		   :The Funny Fortunes Freaks:/:/usr/games/fortune
levin:dYYQYjpnhAnAk:159:100	   :tim &:/a/levin:/bin/csh
sherrin:m6KptDib0j6Lk:160:100	   :Shelley Errington:/a/sherrin:/bin/csh
sail::161:100			   :J. R. Stoner:/a/sail:/bin/csh
palca:bTIw3H2yIHP5Y:162:100	   :Joe Palca:/a/palca:/bin/csh
bourne:bSQlcQjrwQ3Yg:163:100	   :Patricia Bourne:/a/bourne/:/bin/csh
[circuit zapped]
1% cd /usr/ferns/Lib
2% ls -l
total 21
-rwxr-xr-x 1 fmanag      747 Mar 18 19:11 HELPMSG
drwxrwxrwx 2 fmanag      160 Mar 19 07:10 Messages
-rwxr-xr-x 1 fmanag        0 Mar 18 19:11 arpamail
-rwxr-xr-x 1 fmanag       75 Mar 18 19:18 fmand
-rwxr-xr-x 1 fmanag      245 Mar 18 19:11 fmotd
-rwxrwxrwx 1 fmanag     3402 Mar 19 07:51 fusers
-rwxrwxrwx 1 fmanag       88 Mar 19 07:54 futmp
-rwxrwxrwx 1 fmanag     1728 Mar 19 07:12 headers
-rwxr-xr-x 1 fmanag      975 Mar 18 19:17 menu
-rwxr-xr-x 1 fmanag      751 Mar 18 19:17 news
3% cat fmand
New commands:
        delete:         Delete old messages
        passwd:         Change your password
4% cat fmotd

Type 'help' to get help, and 'menu' to get a list of
commands.  Not all commands work yet, but will soon.

Please tell trustworthy people you know about this system!  Get
them interested!  We need more people to make this feasable!

                                        -Tim

5% cat fusers
Tim Bessie		sysop k.hmMruWho.7o Thu Mar 19 02:39:36 1981
Carl Hewitt		leader k.kt7cpbvSTwI ue Mar  3 16:36:42 1981
Kevin Burnet		tkjb k.OrCIFHrPYE6 Sat Feb 28 12:31:33 1981
Stan Getz		stan k.5.tSlXc/Lbo Mon Mar  2 16:56:51 1981
Joe McGuckin		mcguc kink.2929SWqZacM
Evan Schaffer		evan k.fmGPtwtqPbw
Lewis Henderson		lewis k.9N8fAQhF/66 Sun Mar  1 00:31:36 1981
Jordan Hubbard		hubbard k.fbUIM2/RxwY Sat Feb 28 15:16:07 1981
Richard Diaz		rdbh k.0wGnY3rAGeU Wed Feb 25 13:49:54 1981
Phillip Reed		pcr k.uW4KubwXmf.Thu Mar 19 05:51:38 1981
Rob Griffithsr		griffiths k.cgOXHBiclgM
Stephen Robbins		stever k.JPK9/jntGTg
Paul Traina		pst k.682pmV65Mo6 Sat Feb 28 23:25:49 1981
Al Walker		hobbit k.guQeeS5/jjE Sun Mar  1 00:59:05 1981
Bernard Klatt		dectek k.NtvUWBp4JwU
Ed Kochanowski		egk k.e9cfL82WMWc Tue Mar  3 22:16:31 1981
Sean N. Levy		attila k.CH6ZKZUfxfs
Josh Mereminsky		ess k.QrAM2KXH1vo
Bob Alexander		sorc k.QLt13KPtMz6
Jude Saucier		saucierjb k.Adb0zXLFZ4A Thu Mar 19 07:51:22 1981
Allan Hessenflow	poly k.SmG91OIjzC2!qvr
Meg Kazmar		megk k.0cnbHyuS0fs!qvr
Michael Burleson	burleson k.vH7BnaZ1dIU!qvr
Richard Beal		teacup k.KTZR8cb193Y!qvr	
Bill Doger		doger k.3u7qj1tyUTE!qvr
Mark Thorson		maroth k.JQXQtOPbwK2!qvr
William Davis		aarrgh k.B/W16oxzL22!qvr
6% cat futmp
saucierjb Thu Mar 19 07:51:22 1981
7% cat arpamail
15% Message from evan tty7...

hi there. howya doin?
16% Okay.you can use
rity evan
to reply
>
16% rity evan
good. now type something
>hello
>hello
>go ahead
>What's up?
>not a whole lot. who are you?
>Bill
>and you?
>evan. i work here. where are you?
>Sunnyvale, where is this syetm (other than in Santa Cruz)?
>university of california. kerr hall. bill who?
>Soley ... warning - the link I have times out and is about to so
I may go away - will call back if so...
>i must leave now too. welcome to tim's onyx. see you
EOF

From evan (tty7): use ctrl-d to end rity session
17% ps
   PID TTY TIME CMD
  3822 1   0:02 ps 
  3776 1   0:06 -csh 
18% who
rabin    console Mar 19 09:00
guest    tty1    Mar 19 12:57
20% cd /usr/ferns
21% ls -l
total 5
drwx--x--x 2 fmanag       48 Mar 19 01:19 Bin
drwxrwxrwx 3 fmanag      192 Mar 19 02:06 Lib
drwx------ 2 fmanag      768 Mar 19 01:19 Source
drwx------ 2 fmanag      160 Mar 19 02:06 Util
22% cd /usr/ferns/Lib
23% ls
HELPMSG   arpamail  fmotd     futmp     menu
Messages  fmand     fusers    headers   news
    