Received: from C36.Tymnet by B39.Tymnet; Mon, 28 Apr 86 18:26:15 PDT
Received: from tymix.Tymnet by C36.Tymnet; Mon, 28 Apr 86 18:23:40 PDT
Received: by tymix.Tymnet (4.12/4.7) id AA16475; Mon, 28 Apr 86 18:04:04 pdt
Return-path: <feldman@tymix.Tymnet> 
From: feldman@tymix.Tymnet (Steve Feldman) 
Date: Mon, 28 Apr 86 18:04:04 pdt 
To: WRS@C39.Tymnet 
Message-id: <8604290104.AA16475@tymix.Tymnet> 
Subject: Re: SysV C Compiler 

To use the compiler you have to add "/usr/68ktools" to your search path.
Here's a copy of the memo Martin Shen sent out (he brought up the compiler).

-----------------------------------------------------------------------------
To compile:

The '68kcc' command is the AT&T C compiler for the 68000. Please
refer to the AT&T User's Manual for the Motorola 68000 Software
Software Generation System for the usage of the command.  Note
that the prefix of the command has been changed to '68k', instead
of 'MC68' described in the manual.


The option -T processing has been added to the linking editor '68kld'. 
The user can use this option for specifying the load-starting address
in the '68kcc' command.

The standard C library is taken from the BDE library with some 
modifications for running on the stand alone UPI/XPI environment.
For detail, please refer to the READ_ME files in the 
/c/68ktools/src/bdelibc directory.

Examples:

	68kcc -Dmc68000 -Uvax -T 1000 omboot.c -lom -o omboot

	68kcc -Dmc68000 -Uvax -c a.c
	68kcc -Dmc68000 -Uvax -c b.c
	68kcc -Dmc68000 -Uvax -T 1000 a.c b.c -o myprog

The object code produced by the '68kcc' is in the AT&T System V 'a.out'
format.  The program '68kconv' can be used to convert the System V
'a.out' format file into a BSD 4.2 ZMAGIC 'a.out'  format file:

	68kconv -t m68k myprog

The output file will be named myprog.av .
 



To load:


To convert the BSD 4.2 ZMAGIC 'a.out' format file into S-records file:

	68ktoS -T 1000 -o myprog.hex myprog.av

-----------------------------------------------------------------------------

I haven't yet tried linking and running a complete program, but others around
here have and claim it works.
	Steve
From: ebs@eunix (Elliot B. Schneider) 
Date: Tue, 29 Jul 86 11:57:50 pdt 
To: baba@eunix, comstock@eunix, coons@eunix, davarj@eunix, drawson@eunix,
	ebs@eunix, joesal@eunix, melcruts@eunix, wrs@eunix 
Message-id: <8607291857.AA07777@eunix.Tymnet> 
Subject: Assembler and Compiler Versions 

To 68000 code developers:

Notes on the Microtec Assembler/Loader:

There are currently three versions of assemblers/loaders available.  The
following list summarizes where they are and what they are:

Version	Location		Notes
3.0	/usr/local/asm68k	This is the "old" assembler.  It works, but
	/usr/local/lnk68k	 it is SLOW!  It doesn't handle 68010 codes.

4.2	/b/microtec/ver4.2/nasm68k This is the "pre-release" version.  The
	/b/microtec/ver4.2/lod68k  assembler won't assembler large files.
				   The loader won't link more than 14 modules.
				   I don't recommend its use.

5.1	/usr/masm68k/nasm68k	This is the latest release.  The assembler has
	/usr/masm68k/lod68k	some bugs which can be gotten around (see
				below).  I've just installed the loader- it
				should work.  Let me know about any problems.

Version 5.1 known bugs and work arounds:
1) RTS and RTE will give errors if there is a comment field which
   doesn't start with a ';'.  Work around- start comments field with ';'

2) Expressions within parenthesis may produce errors.  Work around- use
   equ's to break expression into non-parenthesized form.
     Ex. 	test	equ	(*-5)/4		; will get an error
		test1	equ	*-5		; sub-expression
		test	equ	test1/4		; will assemble

3) The absolute assembly option does not work.  Work around- assemble as
   relocatable and use loader to produce S-record file.

Compiler Notes:

The Green Hills C cross-compiler is in /usr/ghs/VAX/compiler/ccom.  If you 
need a manual or more information see me.

Elliot (eunix:ebs)
Received: from C57.Tymnet by B39.Tymnet; Fri, 1 Aug 86 17:58:05 PDT
Received: from tymix.Tymnet by C57.Tymnet; Fri, 1 Aug 86 17:57:23 PDT
Received: from eunix.Tymnet (eunix.ARPA) by tymix.Tymnet (4.12/4.7) id AA06468;
	Fri, 1 Aug 86 14:29:22 pdt
Received: by eunix.Tymnet (4.12/4.7) id AA03971; Fri, 1 Aug 86 14:28:36 pdt
Return-path: <ebs@eunix.Tymnet> 
From: ebs@eunix (Elliot B. Schneider) 
Date: Fri, 1 Aug 86 14:28:36 pdt 
To: baba@eunix, comstock@eunix, coons@eunix, davarj@eunix, drawson@eunix,
	ebs@eunix, joesal@eunix, melcruts@eunix, wrs@eunix 
Message-id: <8608012128.AA03971@eunix.Tymnet> 
Subject: Microtec Assembler V5.2 "gotcha" 

1) Just for the record- my previous mail referred to assembler version V5.1- it 
is really V5.2.

2) This one seems to have caught just about everyone, so far:  V5.2 has an 
incompatibility with previous versions with respect to its evaluation of
PC relative operands.  The manual describes the incompatibility, however the
workaround is an option called "abspcadd" which evaluates PC relative the
same way that the old assembler did.  This option can be invoked in one of
two ways: an "opt -abspcadd" line in the source code, or an option on the
command line as follows (thanks to Duane for discovering how to use the 
command line option):

  /usr/masm68k/nasm68k {source} {other options}  -f noabspcadd

As far as I can tell, this option should ALWAYS be used, since leaving it out
seems to produce illegal code along with an assembler warning.  

Green Hills C users- you MUST use the above option when assembling the compiled
code.

3) Speaking of command lines- V5.2 does not default for listings and object 
files as did V3.0.  I use a simple shell script for my assemblies:

% cat asm
#  Need this line to invoke C-Shell
/usr/masm68k/nasm68k ${1} -l ${1:r}.lis -o ${1:r}.sre -f noabspcadd

   Example of usage:
% chmod a+x asm		# makes file executable- only do this once
% asm test.68k		# assemble file using script
% ls test*		# listing (.lis) and object (.sre) result
test.68k	test.lis	test.sre

Of course if you use 'make files' you probably don't care.

That's all for now.

Elliot

Received: from C57.Tymnet by B39.Tymnet; Wed, 27 Aug 86 15:59:47 PDT
Received: from tymix.Tymnet by C57.Tymnet; Wed, 27 Aug 86 15:59:32 PDT
Received: from eunix.Tymnet (eunix.ARPA) by tymix.Tymnet (4.12/4.7) id AA15811;
	Wed, 27 Aug 86 15:58:19 pdt
Received: by eunix.Tymnet (4.12/4.7) id AA09820; Wed, 27 Aug 86 11:05:54 pdt
Return-path: <ebs@eunix.Tymnet> 
From: ebs@eunix (Elliot B. Schneider) 
Date: Wed, 27 Aug 86 11:05:54 pdt 
To: baba@eunix, comstock@eunix, coons@eunix, davarj@eunix, drawson@eunix,
	ebs@eunix, joesal@eunix, melcruts@eunix, wrs@eunix 
Message-id: <8608271805.AA09820@eunix.Tymnet> 
Subject: Microtec C compiler -r bug and workaround 

When using the -r option (for producing rommable code) to compile the
Microtec csys68k.c file, a compiler bug causes the struct _iob to be put
into a rom section instead of ram.  I've made a fix to stdio.h which should
correct the problem, however the fix is specific to the csys68k.c module.  If
you are compiling csys68k.c, you should add the following line to csys68k.c
before the 'include' for <stdio.h>:

#define CSYS68K

Other modules need not be modifed.
Received: from C57.Tymnet by B39.Tymnet; Thu, 11 Sep 86 12:50:49 PDT
Received: from tymix.Tymnet by C57.Tymnet; Thu, 11 Sep 86 12:50:27 PDT
Received: from eunix.Tymnet (eunix.ARPA) by tymix.Tymnet (4.12/4.7) id AA03810;
	Thu, 11 Sep 86 12:49:52 pdt
Received: by eunix.Tymnet (4.12/4.7) id AA12724; Thu, 11 Sep 86 12:48:43 pdt
Return-path: <ebs@eunix.Tymnet> 
From: ebs@eunix (Elliot B. Schneider) 
Date: Thu, 11 Sep 86 12:48:43 pdt 
To: baba@eunix, comstock@eunix, coons@eunix, davarj@eunix, drawson@eunix,
	ebs@eunix, feldman@eunix, joesal@eunix, melcruts@eunix, wrs@eunix 
Message-id: <8609111948.AA12724@eunix.Tymnet> 
Subject: Microtec C- Who's on first? 

In order to (hopefully) make it easier to find Microtec C related files, I've
rearranged the /usr/mcc68k directory into sub-directories, and included a README
file.  If, after "cat"ing the README, you need to find a file and still can't, 
let me know.

Elliot Schneider
Received: from C57.Tymnet by B39.Tymnet; Mon, 15 Sep 86 17:23:41 PDT
Received: from tymix.Tymnet by C57.Tymnet; Mon, 15 Sep 86 17:23:03 PDT
Received: from eunix.Tymnet (eunix.ARPA) by tymix.Tymnet (4.12/4.7) id AA00782;
	Mon, 15 Sep 86 17:22:34 pdt
Received: by eunix.Tymnet (4.12/4.7) id AA23969; Mon, 15 Sep 86 17:21:13 pdt
Return-path: <ebs@eunix.Tymnet> 
From: ebs@eunix (Elliot B. Schneider) 
Date: Mon, 15 Sep 86 17:21:13 pdt 
To: baba@eunix, comstock@eunix, coons@eunix, davarj@eunix, drawson@eunix,
	ebs@eunix, feldman@eunix, joesal@eunix, melcruts@eunix, wrs@eunix 
Message-id: <8609160021.AA23969@eunix.Tymnet> 
Subject: C Library Source 

We are now the proud owners of the complete source code to the Microtec C
library.  The code is located in 
  /usr/mcc68k/src.orig

If anybody finds any bugs and/or corrections to bugs in the Microtec source,
please let me know so I can tell Microtec (this is a condition of their giving
us the source for free).  Thanks.

Elliot Schneider
Received: from C57.Tymnet by B39.Tymnet; Mon, 15 Sep 86 17:25:05 PDT
Received: from tymix.Tymnet by C57.Tymnet; Mon, 15 Sep 86 17:24:35 PDT
Received: from eunix.Tymnet (eunix.ARPA) by tymix.Tymnet (4.12/4.7) id AA00804;
	Mon, 15 Sep 86 17:24:05 pdt
Received: by eunix.Tymnet (4.12/4.7) id AA23980; Mon, 15 Sep 86 17:22:55 pdt
Return-path: <ebs@eunix.Tymnet> 
From: ebs@eunix (Elliot B. Schneider) 
Date: Mon, 15 Sep 86 17:22:55 pdt 
To: baba@eunix, comstock@eunix, coons@eunix, davarj@eunix, drawson@eunix,
	ebs@eunix, feldman@eunix, joesal@eunix, melcruts@eunix, wrs@eunix 
Message-id: <8609160022.AA23980@eunix.Tymnet> 
Subject: Microtec Librarian Manual 

We've got a copy of the Microtec Librarian Manual, for those who need it.

Elliot
    