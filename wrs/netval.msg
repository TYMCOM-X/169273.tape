From: JMARCIN@C39.Tymnet
Date: Tue, 29 Jan 85 19:34:16 PST
To: wrs@39.tymnet
Cc: ntd.j/neidrauer@ontyme.tymnet
Keywords: NETVAL
Subject: CUD data program



Bill,

Here is a description of how I think it will work...the follwing information
is also in (netvalcode:39)cuddat.mem.  Please let me know any comments or
questions...



	This memo will describe a function in version 3.00 of NETVAL that
	will allow a "master" program to retrive sequential records of CUD
	data.  This function will only be available if the NETVAL used is
	compiled with the NETVAL_IS_A_SLAVE option enabled (as it will be
	in TYMNET).


	The master program builds a circuit to the username NCUD1, and
	performas a "handshake."  The handshake characters 1, 2, 3, 4
	are first transmitted from NETVAL and then input by NETVAL.


	The protocol used is the one currently used to communicate with
	the slave program on the TYMCOM-IX, described as follows (all
	numbers are decimal):

	- MASTER sends a one byte command code (8) to NETVAL

	- MASTER sends a two byte value containing the block number
	  in which to start the user list (in TYMNET, the acceptable
	  range is 0 - 10223.)

	- MASTER sends a one byte sync code (32.) to NETVAL to signify
	  the end of the command input


	- NETVAL sends a one byte code specifying that data follows (1)

	  - NETVAL sends a two byte block number

	  - NETVAL sends two bytes of byte count (number of bytes following);
	    if there is an error in the CUD format, and the size of the data
	    cannot be determined properly, the entire CUD block will be
	    transmitted.

	  - NETVAL sends CUD data:

	    - CUD data is made up of CUD entries, and a terminating word of
	      either zero or -1 (zero marks the end of the data, -1 signifies
	      an overflow block, which is irrelevant here).

	    - CUD entries begin with a one byte entry size which is the
	      size of the entire entry in bytes.

	    - The second word begins the username; the username is an
	      integral number of words, from 1 to 5; bit 0 is ON in the
	      final username word.

	    - The words following the username are:
			password
			second password word or entry change date
			GAN
			UUN
			control bits and district
			irc, password change date
			access profile

	    - CUD entries are terminated with either a half or a full
	      word of bytes of 0FF.

	- the above sequence is repeated until the end of the CUD is
	  reached (including any overflow blocks); NETVAL will then send
	  a one byte code that specifies the end of data (2)

	- other possible NETVAL codes (program is returned to "command
	  level" where a new command may be received):
		4	CUD file busy
		5	out of sync
		6	illegal command
		17	general error (disk error, etc.)
From: JMARCIN@C39.Tymnet
Date: Wed, 30 Jan 85 10:36:57 PST
To: wrs@39.tymnet
Cc: ntd.j/neidrauer@ontyme.tymnet
Keywords: NETVAL
Subject: NETVAL

Bill,

I have added another command to NETVAL to retrieve its CUD "version"
and "revision" level.  The current level of the CUD will be defined
as version 1, revision 0.  I updated the (netvalcode:39)cuddat.mem to
describe that command also.


I also added the sending of a sync character from NETVAL at the
end of the code 8...that change is also reflected in that write-up.


In code 8, NETVAL sends the byte count for the entire block; THAT is 
the count that would include the word of zero or -1 at the end of the
data in the block.  The byte count at the beginning of each ENTRY
includes a half or full word of bytes of 0FF, and that is the indication
about how long the entry itself is.


Let me know if you have any further questions or comments...

Thanks...jill
From: JMARCIN@C39.Tymnet
Date: Wed, 30 Jan 85 14:24:30 PST
To: wrs@39.tymnet
Cc: ntd.j/neidrauer@ontyme.tymnet
Keywords: NETVAL
Subject: checking old hashes

surprize...surprize...surprize...

the old 940 system is not ALL that bad!!

There is already a command (31.) to check a users current password (I
believe this is used by some 370 programs now.)

The command requires a username, one word password cipher and sync.
NETVAL returns a code that says whether or not that is the correct
cipher for the username

I will go update (netvalcode:39)cuddat.mem to describe that code...
and i think, since the file is expanding, i will rename it to
(netvalcode:39)ntvinf.mem...


Also, i am having a name, VALNETNCUD1, validated with transparent
login set and homed to the VALNET gateway so current programs
can be changed to test with the new NETVAL...could you, sometime,
make up a version of password that builds circuits to VALNETNCUD1 for me?


thanks...jill
Received: from B36.Tymnet by B39.Tymnet; Thu, 24 Jul 86 19:21:47 PDT
Return-path: <JMARCIN@B36.Tymnet> 
From: JMARCIN@B36.Tymnet 
Date: Thu, 24 Jul 86 19:18:05 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Re: how to hash a username into a block number 
In-reply-to: your message of Thu, 24 Jul 86 19:08:13 PDT


The following are the Pascal routines from (netvalcode:36)cudent.ntv
that are used to compute the hash on the username.


(************************************************************************)
(*      Return the next name character, incrementing name_pointer.	*)
(************************************************************************)

procedure get_next_name_character(
      name		: username_string;
  var name_pointer	: username_text_index;
  var name_char		: char);

begin
  if name_pointer < name.size
  then
    begin
      name_char := name.text[name_pointer];
      name_pointer := name_pointer + 1
    end
end (* get_next_name_character *);



(************************************************************************)
(*      Return the next three characters from the user name,		*)
(*	right justified in a word; increment name_pointer		*)
(*	as appropriate.							*)
(************************************************************************)

procedure next_name_word(
      name		: username_string;
  var name_pointer	: username_text_index;
  var name_word		: univ word_in_bytes);

begin
  get_next_name_character(name, name_pointer, name_word[1]);
  get_next_name_character(name, name_pointer, name_word[2]);
  get_next_name_character(name, name_pointer, name_word[3])
end (* next_name_word *);



(************************************************************************)
(*      Return the hashed CUD block number for the name.		*)
(************************************************************************)

function hashed_block(
      name	: username_string) : normal_cud_block_index;

var
  name_pointer : username_text_index;
  name_word,
  sum : integer;

begin
  name_pointer := 0; sum := 0;
  repeat
    name_word := 0;
    next_name_word(name, name_pointer, name_word);
    sum := sum + name_word
  until name_pointer >= name.size;
  hashed_block := (sum mod BIT7) mod MAXIMUM_NORMAL_CUD_BLOCK
end (* hashed_block *);



Let me know if you need additional definitions...they are in files like:
(netvalcode:36)globl.ntv,(netvalcode:36)cuddef.ntv.

No, its not written up anywhere.

jill
Received: from B36.Tymnet by B39.Tymnet; Fri, 25 Jul 86 7:44:39 PDT
Return-path: <JMARCIN@B36.Tymnet> 
From: JMARCIN@B36.Tymnet 
Date: Fri, 25 Jul 86 7:33:35 PDT 
To: William R. Soley <WRS@B39.Tymnet> 
Subject: Netval usage 

I don't know if you have used NVALPROG to login to Netval as yet, or
discovered what you can do there, but you can find out lots of the
things you asked me yourself by doing that, i.e.:

GET username			displays the CUD/MUD block number for
				"username"

READ CUD BLOCK b		reads CUD block "b" into a CUD buffer in
				memory

READ MUD s BLOCK b		reads MUD block "b" from Supervisor "s"
				into a MUD buffer in memory

DISPLAY CUD/MUD FORMATTED	displays interpreted CUD or MUD data from
				the buffer (name, GAN, UUN, dates, etc.)

DISPLAY CUD/MUD			displays the CUD or MUD buffer as
				hexadecimal data (to see password cipher,
				new password bit)

DISPLAY USER username		displays a single CUD/MUD entry

etc.
etc.
etc.

If you need any additional info, let me know...

J

Received: from C26.Tymnet by B39.Tymnet; Wed, 30 Jul 86 16:19:51 PDT
Received: from EMSBUB.Ontyme.Tymnet by C26.Tymnet; Wed, 30 Jul 86 23:17:00 UT
Received: from B36.Tymnet by EMSBUB.Ontyme.Tymnet; Wed, 30 Jul 86 16:14:15 PDT
Return-path: <JMARCIN@B36.Tymnet> 
From: JMARCIN@B36.Tymnet 
Date: Wed, 30 Jul 86 16:06:06 PDT 
To: bub.b/soley@emsbub.ontyme.tymnet 
Subject: Code 9 

The latest netval code is loaded into the bubbnet node, and code 9 outputs
the following:

		      01 	acceptance of command code
		      10	count of bytes following (16.)
		    0001	"version" number for CUD
		    0000	"revision number for CUD
		000007F7	number of blocks in the CUD (2039.)
		00000000	number of overflow blocks
		00000004	number of sectors in a CUD block
		      20	sync char
Jill
Received: from B36.Tymnet by B39.Tymnet; Tue, 29 Jul 86 10:00:05 PDT
Return-path: <JMARCIN@B36.Tymnet> 
From: JMARCIN@B36.Tymnet 
Date: Tue, 29 Jul 86 9:58:41 PDT 
To: William R. Soley <WRS@C39.Tymnet> 
Subject: Re: 83 
In-reply-to: your message of Mon, 28 Jul 86 20:53:36 PDT

I am checking out a crash...Netval should crash if its time changes
more than 10 minutes, since it finds it difficult to schedule things
if the time jumps around, as it often does in Bubbnet.

If you need it, let me know...
J
Received: from C26.Tymnet by B39.Tymnet; Tue, 29 Jul 86 18:17:12 PDT
Received: from EMSNTD.Ontyme.Tymnet by C26.Tymnet; Wed, 30 Jul 86 1:06:32 UT
Received: from B36.Tymnet by EMSNTD.Ontyme.Tymnet; Tue, 29 Jul 86 17:32:45 PDT
Return-path: <JMARCIN@B36.Tymnet> 
From: JMARCIN@B36.Tymnet 
Date: Tue, 29 Jul 86 16:30:06 PDT 
To: ntd.b/soley@ontyme.tymnet 
Subject: Netval Code 9 

I have modified Netval to do the following for code 9.  I am compiling it now
and will load it into Bubbnet when it is complete (I'll let you know when its
loaded).  I will go ahead and transmit it to Betatest and get it up in the
Public Network also.  I updated (netvalcode:36)slv300.mem with the following:


			RETRIEVAL OF Netval INFORMATION

	Netval code 9 (get Netval Information), will work as follows:

	- MASTER sends a one byte command code (9) to Netval
	- MASTER sends a one byte sync code (32.) to Netval to signify
	  the end of the command input


	- Netval sends a one byte code (1) specifying that the data follows
	  (or error code, as before)
	- Netval sends a one byte count of the number of bytes of data that
	  follows (all data bytes, not counting final sync)
	- Netval sends a 2 byte CUD "version" number; this will be incremented
	  whenever the general format of the CUD entry or block changes.
	- Netval sends a 2 byte CUD "revision" number; this will be incremented
	  whenever there is a non-format change, such as a new option bit.
	- Netval sends 4 bytes that is the number of "normal" CUD blocks,
	  i.e., the number used in the hashing algorithm.
	- Netval sends 4 bytes that is the number of overflow CUD blocks.
	- Netval sends 4 bytes that is the size of a CUD block in sectors.
	- Netval sends a one byte sync code (32.) to signify the end of the
	  data.

Jill
 