
        FTP group -- file definitions

        The protocol provides a means for one end of a communication
line to describe a file to the other end of that communication line;
the purpose of the communication is to allow the second end to produce
a file which is "similar" to the described file (ideally, one that fits
the original description).  Since this protocol is to be used in the
TYMSHARE environment of many distinct architecture/operating system
pairs, we must either develop a machine-independent definition of a
file or restrict our file transfers to transfers between equivalent
"machines".  The purpose of this section is to come up with a useful
description of a file based on the information it encodes, rather than
the bits which represent that information.

        There are a couple of distinct parts to a file that it seems
useful to deal with separately.  It is common to think of two distinct
files on a single operating system as being "the same", even though
they may not be precisely the same file.  In fact, most installations
have at least one, and possibly more than one, utility which compares
two different files to see if they are the "same".  In this comparison,
some detectable differences are almost invariably ignored (for example:
file "NAME", when the file was last modified, and so on).  Other
differences in files are ignored depending upon the information the
file is viewed as containing, typically in text files.  Typical things
to ignore in text files are: trailing spaces on a line, NULs (ASCII code
0), and sequence numbers.

        That portion of the files which is normally not regarded as
being "in" the file will be called "folder" information, the name being
taken from the file-folder analogy for computer files.  The folder
attributes that we intend to send in a "machine-independent" format
are:
        1) "full file name"
        2) version number
        3) Access rights [READ,WRITE,MODIFY,EXECUTE,DELETE,SEE FILE]
          A) OWNER: <protection>
          B) OTHER USERS (undistinguished): <protection>
          C) PRIMARY WORK GROUP: <protection>
          D) NAMED GROUP
                1) who in list.
                2) <protection> for them.
        4) File History:
          A) Creation [date/time, who]
          B) Write [count, last write date/time, last write author]
          C) Read [count, last write date/time, last write author]
          D) Reference [count, last write date/time, last write author]
          E) Archival [if so, date/time, by whom, archive address]
        5) Expiration: ON READ | AFTER <date/time>
        6) OS block:    (binary block for identical host<->host xfer)
            [*** this is a kluge, implement all previous possible ***]

[* for this section, (unless otherwise negotiated) a "date/time" will
   be represented as: YYYYMMDDHHMMSSmmmmmmnnnppp]
Disposition:
	<temporary>
        <message> <TO: whom> <FROM: whom>
        <process data> <process name>
        <file> <AS: file-name>



File contents:

length format V: 0 => variable, unlimited;
                positive => fixed at val;
                negative => variable, maximum is -val

File type:
   I. print-image (eg line printer)
        A) line width (V)
        B) page height (V)
        C) file length guess (pages)
  II. card-image
        A) card width (V)
        B) file length guess (cards)
 III. text

  IV. byte-stream
        A) byte width (in bits)
        B) file length guess (bytes)
   V. binary record stream
        A) byte width (in bits)
        B) record size (in bytes)
        C) file length guess (in records)
  VI. Formatted records
        A) Record template
          1) number of fields
          2) field spec list:
                A) Fixed numeric        grain,range,offset
                B) Floating numeric     base,range,offset
                C) Date/time            range,offset
                D) Text                 character length (V)
                E) Pointer              record or label number or key field
          3) keyed field list
        B) file length guess (in records)
"RETRIEVAL" names

        retrieve all <retrieval-group-name>
        retrieve the <retrieval-group-name>     [exactly one must exist]
        retrieve a <retrieval-group-name>       [any one]

        count <retrieval-group-name>
        directory <retrieval-group-name> <desired folder-attributes>

 