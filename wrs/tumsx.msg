Received: from X930.Tymnet by B39.Tymnet; Mon, 14 Jul 86 15:49:04 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 14 Jul 86 15:45:28 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Cc: Joe Smith <jms@X930.Tymnet> 
Subject: TUMS gap 

Re: Previous message...  I thought I could say "TUMS SEND expression"

Apparently, this doesn't work... it assumes we are always sending... I don't
know if I like that.  I'd like to be able to run tums to just pick up my
messages and do a "TUMS LIST NEW" command.  This would have worked in the
original version that required "TUMS SEND exp" or "TUMS READ exp".  I think
this is an invaluable feature... that is lacking.

/
/carl
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 14 Jul 86 15:47:27 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: TUMS/SENDMA gap 
In-reply-to: your message of Mon, 14 Jul 86 15:33:57 PDT

That's an interesting idea, it does that now to some extent in that if it
is run with a command of "SEND??" it will prompt for a To: in the case of
no arguments.  There are some non-obvious things that would need to be
decided, like...
	1. should init commands be suppressed in that case?
	2. should user be left at command level (probably)?
		TUMS command level that is - so he can do NEXT/REPLY/etc.
-Bill
Received: from X930.Tymnet by B39.Tymnet; Mon, 14 Jul 86 20:16:44 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Mon, 14 Jul 86 19:43:50 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Subject: TUMS <command> [argument-expression(s)] 

How does this sound...

Read the user profile so that listing formats and macro's work... in
fact the command given may be a macro which expands to a real command.
Do not process any INIT commands... just definitions and flags.

Read the command line, and translate any "macros" seen.

If the first token is TUMS or something other than a legal TUMS command,
(for example, FOOBAR) then assume this is the command which invoked TUMS
and eat it.  It may be the name that the TUMS program is going by, or a
program designed to invoke TUMS.

If the next token (the first if it was not eaten) is a legal command then
dispatch to that command and take your consequences on the rest of the
line regarding formatting, etc.

What to do next if the token doesn't match a legal command?  Assume a
SEND command and dispatch.

Last but not least, since this command started at EXEC level, it ought
to finish at EXEC level.  Of course, If I say, "TUMS<CR>"  or "R TUMS"
or even "RUN (MAIL)TUMS" it should detect that and put me at command
level in TUMS.



Does this solve "TUMS READ"?  Yes, if "READ<CR>" does anything...
Definately "TUMS READ NEW" works.   "READ" should if it doesn't, default
to "READ ALL UNREAD" if nothing else has been specified or "READ ." if
we have specified a single message, or "READ $" if we have selected a
list of messages.  (IT may already, I haven't tried it for awhile).

"TUMS <SEND>" works fine too.  "TUMS foo"  where "foo" := "COPY file"
should work fine as well.  "TUMS SEND foo" may not translate too well,
but that would happen anyway if you try to send to a macro.

That sounds very reasonable.  Find any holes in it, let me know.
/Carl
From: William R. Soley <WRS@B39.Tymnet> 
Date: Mon, 14 Jul 86 21:40:02 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: TUMS <command> [argument-expression(s)] 
In-reply-to: your message of Mon, 14 Jul 86 19:43:50 PDT

Hmmm, I think that's good, except READ still doesn't seem right.  Remember
you only get the first message, then back to command level (the exec in
this case).  I suppose I could keep a flag that said to return to monitor
whenever a next with no pending messages is done.  That's the only glitch
I can think of.  I'm also not sure what you meant about if its ran by another
name.  It always gets the whole command line, so it will see
	TUMS SEND WRS
or
if run by another program,
	READ FROM JOE
Now, in that case (run by a RUN uuo from program READ), should I throw
away the READ and barf on the FROM, or process the FROM?  I think you meant
to process the FROM, right?  -Bill
Received: from X930.Tymnet by B39.Tymnet; Tue, 15 Jul 86 0:43:42 PDT
Return-path: <Carl@X930.Tymnet> 
From: Carl A Baltrunas <Carl@X930.Tymnet> 
Date: Tue, 15 Jul 86 0:30:55 PDT 
To: William R Soley <wrs@C39.Tymnet> 
Subject: READ FRMO JOE 

If run by another program, say one called "READ", I'd say...
	READ FROM JOE
this runs program READ which is designed to run (MAIL)TUMS
TUMS sees the line "READ FROM JOE".
The first token IS a legal command, so it dispatches to READroutine
which then processes "FROM JOE".

This works the same as if I had said "TUMS<CR>" followed by "READ FROM JOE".
The only exception is that it was really run from EXEC level.
----------------
Hmmmm... Yes, READ only returning one message is a problem.  Actually I
find it annoying that it only returns one message.  I seem to anticipate
a whole slew of messages and only get one, to which I then continue to
read by typing ^J or ^K  (next and next-delete).  It wouldn't be so bad
if READ did read all of them... RDMAIL reads all of them!  The current
functionality of READ is to PERUSE the mail anyway, not to just READ it.
There is no way to currently issue one command (say from a command file)
which will READ ALL UNREAD and actually get all of the mail.

Maybe a new modifier ONCE or ONE could be added and if set, will only
print the first message {yuk} "READ ONCE ALL UNREAD"  or better yet,
"READ ALL UNREAD PAUSE" will pause after each one (back to command mode),
allowing commands like NEXT/PREVIOUS/etc to work on CSET like READ works
now.
-----------------------
PS. Better to make sweeping changes like this now, before it goes to the
whole world.  sigh.   /Carl
From: William R. Soley <WRS@B39.Tymnet> 
Date: Tue, 15 Jul 86 1:38:29 PDT 
To: Carl A Baltrunas <Carl@X930.Tymnet> 
Subject: Re: READ FRMO JOE 
In-reply-to: your message of Tue, 15 Jul 86 0:30:55 PDT

TYPE will read all the messages at once, like RDMAIL does.  Its arguments
are identical to READ.  If you prefer it, you can do a
	SET MACRO READ "TYPE NEW UNREAD"
or whatever your fancy.  I would like to add a message at the prompt that
says how many unread messages from the read-set have yet to be displayed.
So even if you are reading already read messages, like READ FROM CARL, it
would remind you how many you have to go in some fashion like:
	[3 more msgs]
	>next
	...
	[2 more msgs]
	>
etc.

I like your suggestion and will plan on implementing it in the near
future.  Thanks

-Bill
    