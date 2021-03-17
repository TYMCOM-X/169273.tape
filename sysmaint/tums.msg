From: Dennis Coffey <DENCOFF@930.Tymnet> 
Date: Tue, 11 Jun 85 15:34:57 PDT 
To: baigent, sysmaint 
Subject: update of update to packaging doc. 

----------------------------------------------------------------
From: Dennis Coffey <DENCOFF@930.Tymnet> 
Date: Thu, 6 Jun 85 8:48:59 PDT 
To: sysmaint, baigent 
Bcc: DENCOFF@C930.Tymnet 
Subject: Packaging change (simplification) 

	First item:
Craig agrees that we do the following to save unnecessary redundancy
when transmitting software to the domestic, customer 2020's.

We check the current copy of the executable binary file of the program
on the "production location hosts" (as they are called on the
transmittal form) to determine if they are identical with the executable
binary file created by our reconstruction of the file from the archived
source files.  This checking is done in this way:
	1.)  Determine the size of the file built from the sources and
of the files on the production hosts, in memory words (not just in
memory pages).  This is done with the "DIR" command, using the "/WORDS"
switch.
	2.)  Determine the version number of the file built from the
sources and of the files on the production hosts.
	3.)  Determine the checksum of the file built from the sources
and of the files on the production hosts.

If the size (in memory words), the version number and the checksum are
all identical, it can be assumed that the files are identical.  If the
executable binary file from the production host is identical to the
executable binary file built from sources, transmittal of the executable
binary file to the customer host (such as a domestic 2020) should not be
done.

	Second item:
The names of the packages that Craig suggested for the transmittal data
base, TRABAS, (and thus for everyone's access to the packages in
archives) is a 6 character mnemonic name, such as the filename part of
the file-id in the .DOC file in the archive FDM file, with ".PKG" as the
last four of the 10 characters allowed in the TRABAS and ARCHIV data
bases.  E.g.: the COMMAND package will be called "COMMAN.PKG" on the
"Software name" line of the transmittal form.

Since this name will be the name by which the package will be known
henceforth, we should be careful about what we choose for the first six
characters.  Coordination of this naming is recommended if you have any
questions.

	Third item:

Re. retransmittals of previously released packages:  If the package has
been transmitted, the "production" hosts' software has been installed,
and no changes to these installed files are to be made, then the new
transmittal should be marked "Archive only".

Every package transmitted should be re-transmitted with the following
changes, and these changes should be implemented for every new package
transmitted.:
	1.)  Package name should follow the new convention of
"<6-character name>.PKG".
	2.)  Version number(s) for packages should reflect the new
convention, including the manner in which the FDM file's name is
affected by the version numbering scheme.  Package file name format:

	"<4-char pkg name abbrev.>n[A|T].mmm"
(where:  n = digit 0 thru 9, reflecting the host class;
	 "A" or "T" = code reflecting Archive or Transmittal file; and
	 mmm = package version number, starting with 001.)

	3.)  The transmittal should include instructions to Software
Distribution to enter the following information about EACH program in
the package into the TRABAS data base.  The information marked with an
asterisk should be included in the package's documentation file, for
information to Software Distribution.  The instructions should indicate
this location of this information.
	    a.)  System number:  packaging host number;
	    b.)  *  Program name:  name of individual program, as in
last transmittal of individual program;
	    c.)  *  Version number:  version number of the individual
program;
	    d.)  *  Programmer name:  name of programmer who wrote the
current version of the individual program;
	    e.)  File name:  name of the package's current archive FDM
file; 
	    f.)  File extension:  extension of the package's current
archive FDM file.

If you have any questions, please ask,
Dennis
----------------------------------------------------------------
From: Dennis Coffey <DENCOFF@930.Tymnet> 
Date: Tue, 11 Jun 85 15:40:35 PDT 
To: baigent, sysmaint 
Cc: fletcherc 
Subject: Pkging: 2020s 

In the first round, we will not create any packages for any of the 2020
hosts, unless the the software on the 2020s won't require updating.  In
these cases (probably very few) the package for the 2020's will be for
"archive only".

In the first round, in most cases the package to create will be the
package for the PDP-10's.  We will create a zero level (universal)
package only if that will be the final configuration:  only if the
software on the 2020's is exactly the same as the software on the
PDP-10's.  If there is to be a "2" level package (for the 2020's only)
with any software updating, it will not be assembled until after the
packages for all the Tymshare PDP-10s are completed.

Dennis
From: Dennis Coffey <DENCOFF@930.Tymnet> 
Date: Tue, 11 Jun 85 15:34:57 PDT 
To: baigent, sysmaint 
Subject: update of update to packaging doc. 

----------------------------------------------------------------
From: Dennis Coffey <DENCOFF@930.Tymnet> 
Date: Thu, 6 Jun 85 8:48:59 PDT 
To: sysmaint, baigent 
Bcc: DENCOFF@C930.Tymnet 
Subject: Packaging change (simplification) 

	First item:
Craig agrees that we do the following to save unnecessary redundancy
when transmitting software to the domestic, customer 2020's.

We check the current copy of the executable binary file of the program
on the "production location hosts" (as they are called on the
transmittal form) to determine if they are identical with the executable
binary file created by our reconstruction of the file from the archived
source files.  This checking is done in this way:
	1.)  Determine the size of the file built from the sources and
of the files on the production hosts, in memory words (not just in
memory pages).  This is done with the "DIR" command, using the "/WORDS"
switch.
	2.)  Determine the version number of the file built from the
sources and of the files on the production hosts.
	3.)  Determine the checksum of the file built from the sources
and of the files on the production hosts.

If the size (in memory words), the version number and the checksum are
all identical, it can be assumed that the files are identical.  If the
executable binary file from the production host is identical to the
executable binary file built from sources, transmittal of the executable
binary file to the customer host (such as a domestic 2020) should not be
done.

	Second item:
The names of the packages that Craig suggested for the transmittal data
base, TRABAS, (and thus for everyone's access to the packages in
archives) is a 6 character mnemonic name, such as the filename part of
the file-id in the .DOC file in the archive FDM file, with ".PKG" as the
last four of the 10 characters allowed in the TRABAS and ARCHIV data
bases.  E.g.: the COMMAND package will be called "COMMAN.PKG" on the
"Software name" line of the transmittal form.

Since this name will be the name by which the package will be known
henceforth, we should be careful about what we choose for the first six
characters.  Coordination of this naming is recommended if you have any
questions.

	Third item:

Re. retransmittals of previously released packages:  If the package has
been transmitted, the "production" hosts' software has been installed,
and no changes to these installed files are to be made, then the new
transmittal should be marked "Archive only".

Every package transmitted should be re-transmitted with the following
changes, and these changes should be implemented for every new package
transmitted.:
	1.)  Package name should follow the new convention of
"<6-character name>.PKG".
	2.)  Version number(s) for packages should reflect the new
convention, including the manner in which the FDM file's name is
affected by the version numbering scheme.  Package file name format:

	"<4-char pkg name abbrev.>n[A|T].mmm"
(where:  n = digit 0 thru 9, reflecting the host class;
	 "A" or "T" = code reflecting Archive or Transmittal file; and
	 mmm = package version number, starting with 001.)

	3.)  The transmittal should include instructions to Software
Distribution to enter the following information about EACH program in
the package into the TRABAS data base.  The information marked with an
asterisk should be included in the package's documentation file, for
information to Software Distribution.  The instructions should indicate
this location of this information.
	    a.)  System number:  packaging host number;
	    b.)  *  Program name:  name of individual program, as in
last transmittal of individual program;
	    c.)  *  Version number:  version number of the individual
program;
	    d.)  *  Programmer name:  name of programmer who wrote the
current version of the individual program;
	    e.)  File name:  name of the package's current archive FDM
file; 
	    f.)  File extension:  extension of the package's current
archive FDM file.

If you have any questions, please ask,
Dennis
----------------------------------------------------------------
From: Dennis Coffey <DENCOFF@930.Tymnet> 
Date: Tue, 11 Jun 85 15:40:35 PDT 
To: baigent, sysmaint 
Cc: fletcherc 
Subject: Pkging: 2020s 

In the first round, we will not create any packages for any of the 2020
hosts, unless the the software on the 2020s won't require updating.  In
these cases (probably very few) the package for the 2020's will be for
"archive only".

In the first round, in most cases the package to create will be the
package for the PDP-10's.  We will create a zero level (universal)
package only if that will be the final configuration:  only if the
software on the 2020's is exactly the same as the software on the
PDP-10's.  If there is to be a "2" level package (for the 2020's only)
with any software updating, it will not be assembled until after the
packages for all the Tymshare PDP-10s are completed.

Dennis
 