Received: from C57.Tymnet by B39.Tymnet; Tue, 28 Oct 86 9:02:27 PST
Received: from tymix.Tymnet by C57.Tymnet; Tue, 28 Oct 86 9:01:48 PST
Received: from eunix.Tymnet (eunix.ARPA) by tymix.Tymnet (4.12/4.7) id AA06947;
	Tue, 28 Oct 86 09:01:24 pst
Received: by eunix.Tymnet (4.12/4.7) id AA14844; Tue, 28 Oct 86 09:00:22 pst
Return-path: <ebs@eunix.Tymnet> 
From: ebs@eunix (Elliot B. Schneider) 
Date: Tue, 28 Oct 86 09:00:22 pst 
To: baba@eunix, comstock@eunix, coons@eunix, davarj@eunix, drawson@eunix,
	ebs@eunix, feldman@eunix, joesal@eunix, melcruts@eunix, wrs@eunix 
Message-id: <8610281700.AA14844@eunix.Tymnet> 
Subject: Microtec Assembler V5.2a 

V5.2a of the Microtec 680xx cross assembler has been received and installed in

  /usr/bin/mcc68k/*

Documentation describing changes implemented in this release can be found in

  /usr/masm68k/v5.2a/asm68k.doc

Received: from tymix.Tymnet by B39.Tymnet; Wed, 4 Feb 87 9:23:04 PST
Received: by tymix.Tymnet (5.51/4.7) id AA13454; Wed, 4 Feb 87 09:22:21 PST
Received: by eunix.Tymnet (5.51/4.7) id AA09956; Wed, 4 Feb 87 09:22:13 PST
Return-path: <ebs@eunix> 
From: ebs@eunix (Elliot B. Schneider) 
Date: Wed, 4 Feb 87 09:22:13 PST 
To: microtec@eunix 
Message-id: <8702041722.AA09956@eunix.Tymnet> 
Subject: mcc68k V3 

I have released the version 3 Microtec C compiler into /usr/bin/mcc68k/mcc68k.

IMPORTANT! In order to use this compiler with the current Microtec assembler, 
you must add the -v option to your compiler flags.  Please direct any problems
to my mailname.

 -ebs

Received: from B36.Tymnet by B39.Tymnet; Thu, 19 Mar 87 18:14:34 PST
Received: from tymix.Tymnet by B36.Tymnet; Thu, 19 Mar 87 18:14:22 PST
Received: by tymix.Tymnet (5.51/4.7) id AA27704; Thu, 19 Mar 87 17:05:43 PST
Received: by eunix.Tymnet (5.51/4.7) id AA02664; Thu, 19 Mar 87 17:05:22 PST
Return-path: <ebs@eunix> 
From: ebs@eunix (Elliot B. Schneider) 
Date: Thu, 19 Mar 87 17:05:22 PST 
To: microtec@eunix 
Message-id: <8703200105.AA02664@eunix.Tymnet> 
Subject: V6 Assembler/Linker 

Version 6 of the Microtec Assembler and Linker are here!  They are
currently untested (by me, at least), but can be accessed using the
following paths:

  /usr/mcc68k/mcc.v3.0D/a68k      (assembler)
  /usr/mcc68k/mcc.v3.0D/l68k      (linker)

For information on the new assembler, please read 
  /usr/mcc68k/mcc.v3.0D/asm68k.doc

I have been informed that there are copies of the Microtec manuals in
the NTD library in Bldg. D.  These should correspond to the version 3
compiler and the version 6 assembler.

Elliot (no longer presiding sysadmin)
    