From: Joe Smith <JMS@X930.Tymnet> 
Date: Thu, 21 Aug 86 13:37:47 PDT 
To: MRC-MM 
Subject: The "wonderfullness" of TYMCOM-X 

Consider it to be an idiot-savant.  Light-years ahead of TOPS-10 in some
respects, brain-damaged in others.

The SET TTY TYPE command has not yet been updated to DEC functionality.  It
currently accepts only an octal number, where 0 means normal ASCII, and nonzero
means KATAKANA (Japanese terminal using JIS-8 ASCII).  The command SET TTY
CLASS FOO sets the terminal type as far as programs are conserned.  It does
not set width, xon, backspace, etc.  The best way to set terminal
characteristics is via SWITCH.INI.  Next time you call TYMNET, note the four
digit node number.  This will match the last line that LOGON outputs.
Modify SWITCH.INI to select VT102 based on this node number.  Type "INITIA" to
test your changes.

EDIT10 is an old version of SOS, VUE and PEAK are both EMACS clones, and 
SYS:TECO is DEC version 21 (ugh!).  I have defined TECO to run Texas TECO
version 124, and SED is available.  If you say ".EMACS FILE" it will run
VUE which is close enough.  Most of the commands run RPG (alias COMPIL) to
translate usernames to PPNs.  You can modify RPGINI.CTL then "PCOM RPGINI"
to make a new RPG.INI.

TYMCOM-X is very into licenses.  If you don't have any license, it won't
show anybody but you (so your business competitors can't see what you are
doing).  I'll turn on Acct Sup license so you can see everyone in [3,*].

/Joe

  