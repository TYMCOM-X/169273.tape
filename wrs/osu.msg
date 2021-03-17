#TUMS ARCHIVE
RCPT To: WRS/OSU.MSG@B39
From: William R. Soley <WRS@C39.Tymnet> 
Date: Tue, 3 Jun 86 16:56:32 PDT 
To: wrs/osu@39 
Bcc: WRS@B39.Tymnet 
Subject: testing new feature of TUMS 

Archive files and distribution lists.  If a file exists named
	(username)filename.TUM
and it contains on the first line
	#TUMS ARCHIVE
then a message sent to
	username/filename@host
will be appended to the file.  The #TUMS is for security since there
is no protection across systems.

Likewise, if it contains
	#TUMS DISTRIBUTION LIST comments
	name1, name2, name3,
	name4, name5 ... namen
then a message sent to it will be redistributed to all the names
on the list.  Note that an extension can be specified, the default
is .TUM.  So anyway, this only works on 39 at the moment, but you
can use it by creating distribution lists on some directory on 39
and then sending to it.  I'll send it off to 930 when its fully
debugged.

-Bill
