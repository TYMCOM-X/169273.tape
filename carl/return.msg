Received: from F33.Tymnet by D35.Tymnet; Sun, 7 Feb 88 18:02:56 PST
Return-path: <Postmaster@F33.Tymnet> 
From: Postmaster@F33.Tymnet 
Date: Sun, 7 Feb 88 18:02:46 PST 
To: Carl A Baltrunas <Carl> 
Subject: Returned MAIL 

The following message was not delivered because:
User has not read mail in 30 days: NETALERT
Last logout: 6 Feb 88


----------
RCPT To: <netalert> 
From: Carl A Baltrunas <Carl> 
Date: Mon, 18 Jan 88 2:37:06 PST 
To: tcsup, netalert 
Bcc: CARL@F33.Tymnet 
Subject: (NETALERT)ALERT 

To who if may concern... if you receive this and you are not responsible
for (NETALERT)ALERT, please pass it on to whomever is now responsible...

Why are there 5 jobs running ALERT?  I thought that the proposed idea was
to keep a single ALERT program running on multiple systems so that ONE of
them would always be running even when systems are taken down or crash...

If this is still true, and there is more than copy of ALERT running on each
system, then somethng is wrong with the logic in the program.  Someone needs
to look at it.

I've only brought this up, and sent this message because I recall a comment
in a meeting with Gary Walker, that (NETALERT) was one of the largest CPU
users on system 70 just before it went away...  If multiple copies of ALERT
are running on the same system, we are wasting TYMNET's computing resources.

Thank you for your attention.

/Carl Baltrunas, Tymcom-X Operating System Support Group
Received: from D55.Tymnet by D35.Tymnet; Wed, 24 Feb 88 6:29:24 PST
Return-path: <Postmaster@D55.Tymnet> 
From: Postmaster@D55.Tymnet 
Date: Wed, 24 Feb 88 5:49:46 PST 
To: Carl A Baltrunas <Carl@D35.Tymnet> 
Subject: Returned MAIL 

The following message was not delivered because:

  The remote host returned an error code in response to the command:
    RCPT TO: <dencoff@D35.Tymnet>

  The error code and message was:
    550 <dencoff@D35.Tymnet> invalid, not sending mail

  As a result of the error, TUMS was unable to deliver this message to
  the following recipients (any other recipients are not affected):
    <dencoff@D35.Tymnet>

----------
RCPT To: <dencoff@D35.Tymnet> 
From: Carl A Baltrunas <Carl@D35.Tymnet> 
Date: Wed, 24 Feb 88 5:34:22 PST 
To: Monitor Group <CARL@D35>, Osman Guven <Osman@D35.Tymnet>, Joe Smith
	<JMS@D35.Tymnet> 
Cc: Craig Fletcher <fletcherc@D35.Tymnet>, Dennis Coffey <dencoff@D35.Tymnet> 
Bcc: CARL@D55.Tymnet 
Subject: Hotline call 

Talk about procedural problems....

System D55: Contact Louie, 214-637-7435

Problem:  Their customer RAILTRACK, calles them complaining that their
          SUBMIT jobs are not running.  They've entered them a few times
          and they are not doing anything.

Research: (SUBMIT)*.JQL shows that nothing has run since 21-Feb-88.  The
          isn't any problem with (SYS)XEXEC or (SYS)EXECX.

          CTYLOG shows that the system has been up since Sunday 20:05 in
          "MANUAL" mode, no DSKCLN has been run, system was unshut after
          running ASP.   (An aside: we need to have ASP write to the CTY
          so that we can detect this condition.)

Solution: I masked as PERPOPER and started XEXEC.  I also impressed on the
          operator and shift supervisor the gravity of what "could" be a
          major file-system integrity problem due to the failure to reload
          and run DSKCLN.  I advised them to reload and do a FULL DSKCLN.

As we left it, Marconi was due in at any moment and he would authorize them
to do whatever they feel is necessary as far as scheduling down-time to try
to correct the problem.   I'll follow-up and contact Marconi later today.

/Carl
Received: from D65.Tymnet by D35.Tymnet; Fri, 15 Apr 88 18:20:15 PDT
Return-path: <Postmaster@D65.Tymnet> 
From: Postmaster@D65.Tymnet 
Date: Fri, 15 Apr 88 17:28:37 PDT 
To: Carl A Baltrunas <Carl@D35.Tymnet> 
Subject: Returned MAIL 

The following message was not delivered because:

  The remote host returned an error code in response to the command:
    RCPT TO: <dencoff@D35.Tymnet>

  The error code and message was:
    550 <dencoff@D35.Tymnet> invalid, not sending mail

  As a result of the error, TUMS was unable to deliver this message to
  the following recipients (any other recipients are not affected):
    <dencoff@D35.Tymnet>

----------
RCPT To: <dencoff@D35.Tymnet> 
From: Carl A Baltrunas <Carl@D35.Tymnet> 
Date: Fri, 15 Apr 88 19:23:52 CDT 
To: Monitor Group <CARL@D35>, Osman Guven <Osman@D35.Tymnet>, Joe Smith
	<JMS@D35.Tymnet> 
Cc: Craig Fletcher <fletcherc@D35.Tymnet>, Dennis Coffey <dencoff@D35.Tymnet> 
Bcc: CARL@D65.Tymnet 
Subject: D65 crash CHTFNF 

It appears that CHTFNF is not related to the problem we have seen on both
F33 and F38. The running job was (ANDERSONK) running TUMS.  UP%MUU was a
PERSET T1, at CLRCOW+32.  UP%UUO was a CHANIO T3, for the user.

Since I'm looking for something else, I didn't do any more checking into
the real failure in this crash.

/Carl
Received: from F33.Tymnet by D35.Tymnet; Tue, 10 May 88 20:01:37 PDT
Return-path: <Postmaster@F33.Tymnet> 
From: Postmaster@F33.Tymnet 
Date: Tue, 10 May 88 20:01:26 PDT 
To: CARL@F33.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
Invalid address(s):
<REQUESTOP>


----------
RCPT To: <REQUESTOP> 
From: CARL@F33.Tymnet 
Date: Tue, 10 May 88 20:01:25 PDT 
To: REQUESTOP 
Subject: SETOP User Request at 11-May-88 03:01:24 

Oper:  DENSITY      manager: DENCOFF     
Password hash: 323043606022
Privileges:    LG
Mktg District: 3
Name:  Population Density
ISC Id#     0 center: Other
Phone: x6209; Mailstop: B51
User:  DENCOFF
Mail:  -none-
Supv:  Dennis Coffey, x6209, B51
Dept:  MDCNSC (Tymnet) Software QA
Why:   fo the heck of it


Received: from F33.Tymnet by D35.Tymnet; Tue, 10 May 88 20:03:55 PDT
Return-path: <Postmaster@F33.Tymnet> 
From: Postmaster@F33.Tymnet 
Date: Tue, 10 May 88 20:03:45 PDT 
To: CARL@F33.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
Invalid address(s):
<REQUESTOP>


----------
RCPT To: <REQUESTOP> 
From: CARL@F33.Tymnet 
Date: Tue, 10 May 88 20:03:45 PDT 
To: REQUESTOP 
Subject: SETOP User Request at 11-May-88 03:03:44 

Oper:  DIFFERENCE   manager: DENCOFF     
Password hash: 571636423112
Privileges:    LG
Mktg District: 3
Name:  I donno
ISC Id#     1 center: Other
Phone: x6209; Mailstop: none
User:  none
Mail:  none
Supv:  none
Dept:  none
Why:   none


Received: from F33.Tymnet by D35.Tymnet; Wed, 11 May 88 16:28:09 PDT
Return-path: <Postmaster@F33.Tymnet> 
From: Postmaster@F33.Tymnet 
Date: Wed, 11 May 88 16:27:58 PDT 
To: CARL@F33.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
Invalid address(s):
<REQUESTOP>


----------
RCPT To: <REQUESTOP> 
From: CARL@F33.Tymnet 
Date: Wed, 11 May 88 16:27:58 PDT 
To: REQUESTOP 
Subject: SETOP User Request at 11-May-88 23:27:56 

Oper:  DCOFFEY      manager: DENCOFF     
Password hash: 123653051542
Privileges:    LG
Mktg District: 3
Name:  NoName
ISC Id#     0 center: Other
Phone: x6209; Mailstop: none
User:  dcoffey
Mail:  dcoffey@tymnet
Supv:  none
Dept:  none
Why:   not really needed


Received: from F33.Tymnet by D35.Tymnet; Wed, 11 May 88 16:32:34 PDT
Return-path: <Postmaster@F33.Tymnet> 
From: Postmaster@F33.Tymnet 
Date: Wed, 11 May 88 16:32:24 PDT 
To: CARL@F33.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
Invalid address(s):
<REQUESTOP>


----------
RCPT To: <REQUESTOP> 
From: CARL@F33.Tymnet 
Date: Wed, 11 May 88 16:32:23 PDT 
To: REQUESTOP 
Subject: SETOP User Request at 11-May-88 23:32:21 

Oper:  FOO          manager: CARL        
Password hash: 165554361772
Privileges:    MG LG
Mktg District: 2
Name:  Foo Bar of the great Plain
ISC Id#231432 center: Other
Phone: 202 020 2020; Mailstop: 
User:  none
Mail:  
Supv:  
Dept:  
Why:   


Received: from F33.Tymnet by D35.Tymnet; Wed, 11 May 88 16:33:55 PDT
Return-path: <Postmaster@F33.Tymnet> 
From: Postmaster@F33.Tymnet 
Date: Wed, 11 May 88 16:33:46 PDT 
To: CARL@F33.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
Invalid address(s):
<REQUESTOP>


----------
RCPT To: <REQUESTOP> 
From: CARL@F33.Tymnet 
Date: Wed, 11 May 88 16:33:46 PDT 
To: REQUESTOP 
Subject: SETOP User Request at 11-May-88 23:33:44 

Oper:  FOOBAR       manager: CARL        
Password hash: 264533561214
Privileges:    LG
Mktg District: 2
Name:  foobar
ISC Id# 34234 center: Other
Phone: 234234242; Mailstop: 
User:  whoknows
Mail:  whoknows
Supv:  
Dept:  
Why:   whoknows


Received: from F33.Tymnet by D35.Tymnet; Wed, 11 May 88 16:35:58 PDT
Return-path: <Postmaster@F33.Tymnet> 
From: Postmaster@F33.Tymnet 
Date: Wed, 11 May 88 16:35:49 PDT 
To: CARL@F33.Tymnet 
Subject: Returned MAIL 

The following message was not delivered because:
Invalid address(s):
<REQUESTOP>


----------
RCPT To: <REQUESTOP> 
From: CARL@F33.Tymnet 
Date: Wed, 11 May 88 16:35:49 PDT 
To: REQUESTOP 
Subject: SETOP User Request at 11-May-88 23:35:47 

Oper:  FOO          manager: CARL        
Password hash: 457107174523
Privileges:    LG
Mktg District: 0
Name:  
ISC Id#     0 center: Other
Phone: ; Mailstop: 
User:  
Mail:  
Supv:  
Dept:  
Why:   


Received: from D31.Tymnet by D35.Tymnet; Mon, 8 Aug 88 10:49:02 PDT
Return-path: <Postmaster@D31.Tymnet> 
From: Postmaster@D31.Tymnet 
Date: Mon, 8 Aug 88 10:48:56 PDT 
To: Carl A Baltrunas <Carl@D35.Tymnet> 
Subject: Returned MAIL 

The following message was not delivered because:
No router table entry for "CRALST.Q".



----------
RCPT To: <@(util)cralst> 
From: Carl A Baltrunas <Carl@D35.Tymnet> 
Date: Wed, 13 Jul 88 20:17:08 PDT 
To: @(util)cralst 
Bcc: CARL@D31.Tymnet 
Subject: Re: D31 downtime - UFD rebuild 


The problem on D31 today was that the RIB pointer for the (SYS) directory
was wiped out by a data page for some directory file.  The cause of this
catastrophe is unknown.

What transpired today was an educated re-creation of the initial RIB data
which contained pointers to all the "special" system files, in place of the
destroyed RIB page for the (SYS) directory.

After this step the system was reloaded several times in an effort to verify
that each step was complete and to restore as many critical files as would
fit on D31 with the limited disk space available.  An hour and a half was
lost getting DSKCLN to run to recover lost pages (after we ran out of disk
space).

After DSKCLN completed, I finished restoring the remainder of the missing
files for (SYS) from the most recent all-files tape.  Operations finished
the job by restoring the files from the 12 daily backup tapes and the
system was up for timesharing with everything restored.

I do not have the precise times for any of these events.  The system was
answered immediately after DSKCLN finished, then Operations shut the system
to begin restoring files from the backups and all-files.  When the all-files
and the 12th day backup was completed, the system was un-shut.  Operations
proceeded to restore day 11 through day 1 in reverse order as per my 
instructions.

/Carl Baltrunas
Received: from D31.Tymnet by D35.Tymnet; Mon, 8 Aug 88 10:49:04 PDT
Return-path: <Postmaster@D31.Tymnet> 
From: Postmaster@D31.Tymnet 
Date: Mon, 8 Aug 88 10:48:56 PDT 
To: Carl A Baltrunas <Carl@D35.Tymnet> 
Subject: Returned MAIL 

The following message was not delivered because:
No router table entry for "CRALST.Q".



----------
RCPT To: <@(util)cralst> 
From: Carl A Baltrunas <Carl@D35.Tymnet> 
Date: Wed, 13 Jul 88 20:32:07 PDT 
To: @(util)cralst 
Bcc: CARL@D31.Tymnet 
Subject: re: D31 UFD rebuild 

Note:  I failed to mention this, but... the time spent re-constructing the
       (SYS) directory RIB and restoring the missing (SYS) files saved D31
       from having to do a full rebuild and prevented the loss of any work
       done between the time of the last daily backup and the time of the
       RIB failure.

/Carl
Received: from F74.Tymnet by D35.Tymnet; Thu, 8 Sep 88 9:01:46 PDT
Return-path: <Postmaster@F74.Tymnet> 
From: Postmaster@F74.Tymnet 
Date: Wed, 7 Sep 88 21:09:21 PDT 
To: Carl A Baltrunas <Carl@D35.Tymnet> 
Subject: Returned MAIL 

The following message was not delivered because:

  The remote host returned an error code in response to the command:
    RCPT TO: <dencoff@D35.Tymnet>

  The error code and message was:
    550 <dencoff@D35.Tymnet> invalid, not sending mail

  As a result of the error, TUMS was unable to deliver this message to
  the following recipients (any other recipients are not affected):
    <dencoff@D35.Tymnet>

----------
RCPT To: <dencoff@D35.Tymnet> 
From: Carl A Baltrunas <Carl@D35.Tymnet> 
Date: Wed, 7 Sep 88 20:52:10 PDT 
To: Monitor Group <CARL@D35>, Osman Guven <Osman@D35.Tymnet>, Joe Smith
	<JMS@D35.Tymnet> 
Cc: Craig Fletcher <fletcherc@D35.Tymnet>, Dennis Coffey <dencoff@D35.Tymnet> 
Bcc: CARL@F74.Tymnet 
Subject: re: SPAGES.SYS writing 

Joe,
  There is "sort of" a way to get the page information for SPAGES.SYS
without without too much trouble...

  VDSKPT AC,    ; where ac/ memory page

  First, we lookup SPAGES.SYS to get the size.
  Then we do a DSKCHR on the STR to get the # pages in STR and each UNIT.
  The number of units is deduceable from the DSKCHR info.
  The size of SPAGES div #-UNITS = 2 times pages per cylinder and gives
      the offset into SPAGES per unit.
  This offset div 2 = Max pages we want to write per unit, thereby only
      using 1 cylinder of the 2 on each unit that SPAGES uses.
  Then we do CHANIO .chMFP each of the pages we wish to scribble in
      for safety's sake (and we do want to compare info later).
  Now that it's mapped, we use VDSKPT to get the retrieval pointer on disk
      for each page "before" we overwrite it.
  To overwrite it, we take the retrieval pointer, calculate the appropriate
      structure page that matches the <UNIT>B16+<PAGE> and super-map that
      page read/write and then scribble in it and unmap both pages.

**This is still much less work than deciphering rib data structures in a
  user program, and depending on things that ONLY the monitor and DSKCLN
  ought to know in the first place!

Craig,
  What this means is that writing SPAGES is simple and straight-forward and
the extra steps taken here will provide greater reliability that QIKSAV will
NOT damage anything on the disk.  If SPAGES doesn't exist, it will report it
as such and maybe write a disk file, but won't just go scribbling on various
unknown cylinders, corrupting the file system in an effor to save it.

/Carl
    