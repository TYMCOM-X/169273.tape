Reply-to: WRS@Office-2.ARPA
From: William R. Soley <WRS@C39.Tymnet>
Date: Tue, 5 Feb 85 13:26:36 PST
To: JAlrich.es@Xerox.ARPA
Keywords: JWK
Subject: status of hard disk drivers

The problem is clearly in the allocation of extents.  I am porting some
CP/M diagnostic software from SIMTEL-20.ARPA which will help me diagnose
the problems.  All of the tools I'm developing will be included in the
next release to you.  I am told these programs will allow selective
examination and patching of the allocation tables, directory blocks and
file space on either hard disks or floppies.  Some of this is automated.
Also included is a backup utility but I don't have any idea, yet, how
useful it is.

They keep the files compressed on SIMTEL to conserve disk space and load
time - it is taking some extra time to get these unsquoze - I have to
copy them to MIT to unsqueeze them and then back to Tymshare and
downline to my Cromix system and then via disk to Joe's system to
assemble them.  Its not quite as bad as it sounds, but its been taking a
long time to make the arrangements.

At this time, the evidence points to a bug in the portion of CP/M which
is supplied by Digital Research in object form only.  If this is true,
it means I won't be able to fix it, I'll have to work around it.  The
worst effect this may have on you, as a user, is to take more memory
away from the user area (58K in stead of 59K).

The programs I'm porting from SIMTEL have been unsqueezed and are now on
my account at MIT-XX.ARPA waiting to be copied.  I should have them on
my system in te next day or two.  I can then assemble them (but not
test) and get ready for when Joe gets back next week.  It should only
take a couple of days to locate the problem after that (this means
2/17).  If the problem turns out to be in my code, it will probably be
fixed on the spot and we can send you a new disk on Monday.  If the
problem is in CP/M, we will have to evaluate the situation then, when we
have more specific information.

I will try to keep you posted of status as it goes.  As usual, don't
hesitate to send me a message or call if you have a question, etc.
Please acknowledge getting this message as our gateway to ARPAnet is not
always reliable.  Thank you for your patience.  -Bill
Reply-to: WRS@Office-2.ARPA
From: William R. Soley <WRS@C39.Tymnet>
Date: Tue, 12 Feb 85 1:44:20 PST
To: JAlrich.es@Xerox.ARPA
Cc: Kaminski.pa@Xerox.ARPA
Keywords: JWK
Subject: hard disk progress

I believe that as a result of tonight's testing, we will be able to send
you a new release next week.  The problem does appear to be in CP/M as I
feared, however, there does seem to be a simple solution that doesn't
require disassembling CP/M.

Files are allocated in "allocation groups" which is the smallest piece
of disk space that can be allocated.  In your case, all of your
partitions have an "allocation group" of size 4K bytes.  This is why the
smallest file you can have takes up 4K bytes.  Each allocation group on
a partition is numbered, starting with 0, up through the size of the
disk.  The directory entry for a particular file has the number of each
allocation group which makes up the file.  Old versions of CP/M had 16
1-byte group pointers.  This obviously means there can't be more than
256 groups (1 byte).  Newer versions of CP/M support two formats:  the
old one, and a new one which has 8 2-byte group pointers.  The new
format will handle disks with up to 65536 groups (2 bytes).  The problem
arises when you attempt to create a file with a new format directory
entry which has more than 8 groups.  From the behavoir, CP/M seems to
think it can still fit 16 groups in a directory entry even though each
one is two bytes in stead of one.

In your case, with 4K byte allocation groups, any partition larger than
1024K would try to use the new directory format.  This appears to work
fine until a file is created which has more than 8 groups (8 X 4K = 32K).

The solution is to reformat your disk so no partition has more than 256
groups.  This can be done by making the groups bigger (16K) or by making
the partitions smaller, or some combination thereof.  This will result
in only the old 1-byte format being used.

For a 5 MB disk, the partitions might look like this:
		
		size	group
		----	-----
	  E:	2 MB	16 KB
	  F:	1 MB	 4 KB
	  G:	1 MB	 4 KB
	  H:	1 MB	 4 KB
		----
		5 MB

Preliminary tests tonight indicated that this will allow files limited in
size only by the size of the partition.

-Bill
    