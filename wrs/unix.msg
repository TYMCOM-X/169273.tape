Received: from tymix.Tymnet by B39.Tymnet; Wed, 21 Oct 87 12:13:59 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA12684; Wed, 21 Oct 87 12:17:33 PDT
Received: from comet.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA19059;
	Wed, 21 Oct 87 12:17:27 PDT
Received: by comet.Tymnet.com (3.2/SMI-3.2) id AA16581; Wed, 21 Oct 87 12:17:21
	PDT
Return-path: <feldman@comet> 
From: feldman@comet (Steve Feldman) 
Date: Wed, 21 Oct 87 12:17:21 PDT 
To: wrs@comet 
Message-id: <8710211917.AA16581@comet.Tymnet.com> 
Subject: printer queue order 

The printer queue order is determined by the modification times on the
jobs' control files.  The "lpc topq" function works by changing
the mod. time of the requested job to be one second earlier than the job
at the head of the queue, and setting the public execute bit on the queue's
lock file.  The daemon checks this bit after printing each job, and if
set it recomputes the queue order and clears the bit.

Look in tymix:/usr/src/usr.lib/lpr/cmds.c for details.

	steve
   