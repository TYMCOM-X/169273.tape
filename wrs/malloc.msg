Received: from tymix.Tymnet by B39.Tymnet; Tue, 26 May 87 17:14:57 PDT
Received: by tymix.Tymnet (5.51/4.7) id AA02291; Tue, 26 May 87 16:37:26 PDT
Received: from opus.Tymnet.com by hobbes.Tymnet.com (3.2/SMI-3.2) id AA01226;
	Tue, 26 May 87 16:36:56 PDT
Received: by opus.Tymnet.com (3.2/SMI-3.2) id AA00519; Tue, 26 May 87 16:37:25
	PDT
Return-path: <coons@opus> 
From: coons@opus (John Coons) 
Date: Tue, 26 May 87 16:37:25 PDT 
To: wrs@hobbes 
Message-id: <8705262337.AA00519@opus.Tymnet.com> 
Subject: Microtec malloc() source 


Bill,

The source for malloc should be in eunix:/usr/mcc68k/src.orig
This directory contains "original" (unmodified) libraries as originally
distributed to us by Microtec last year (bugs, non-re-entrancy and all).
We modified some routines and fixed others, which may be found in
src.rel. Feel free to poke around and find what you need.

Regards,

John
