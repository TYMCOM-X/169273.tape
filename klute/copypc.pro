                PRODUCT DEVELOPMENT PROJECT DEFINITION
                ======================================

Date:               5/25/84

Project:            COPYPC.X

Subproject:

Requested by:       INSG

Needed by:          October 1984


NARRATIVE DESCRIPTION:
=====================

The TYM/COMM product on the IBM PC under MS-DOS does terminal  emulation
and file transfer to and from  the 370s under CMS using COPYPC.   COPYPC
is a CMS  COPYFILE simulator, except  that one of  the files is  a micro
file and the other a mainframe file.  The position and syntax of the two
filenames determines the direction of transfer.

COPYPC.X  will  provide  file  transfer  to  and  from the PDP-10s.  The
program must interface with TYM/COMM and function in a manner similar to
COPYPC.

TYM/COMM implements the Tymshare  File Transfer Protocol, therefore  the
COPYPC.X program must also implement the protocol.

Both text and  binary file transfers  are required, however,  text files
will contain 7-bit ASCII  characters.  Binary files will  be transferred
in image format.  It will be possible to use COPYPC.X to transfer a  DOS
code file to the PDP-10 and for another PC to transfer the file down and
execute the file.  It will not be  possible to use a binary DOS file  as
input  to  MAGNUM  on  the  PDP-10.   It  will be possible to use a file
transferred in text format as input to MAGNUM.


OBJECTIVES:
==========

   1.  The program will function as close to COPYPC as possible, given
       the constraints of the PDP-10.

   2.  The program will transfer files in the most efficient manner
       available.  The objective here is both throughput and least cost
       to Tymshare.


ACTIONS:
=======

                                                  Manpower
                                                  Estimate
                                                  ========

   1.  Analysis phase
          learn MS-DOS                               2 weeks
          learn PDP-10 environment                   2 weeks
          learn SAIL                                 4 weeks
          review TYM/COMM, File Transfer Protocol    4 weeks
          review CEGI SAIL code                      1 week
          write functional specification             2 weeks
          Analysis walkthrough

   2.  Design phase
          investigate block mode transmission        1 week
          determine user interface                   1 week
          text considerations                        1 week
          binary considerations                      1 week
          write design manual                        2 weeks

   4.  Implementation
          tasks to be added after design phase

   5.  Test

   6.  Document

                                                  ==========

                                        TOTAL


REMARKS:
=======

   1.  CEGI Tymshare has a working version of COPYPC.X which effects
       file transfers in ASCII (text) mode only.  We are trying to get
       copies of their code and internals documents.  The problem here
       is that their work is written in French and they are an affiliate.

     * We have asked Fran White to obtain the code for us.
    ** Len Solimene is gathering and clarifying user requirements.

   2.  We have heard that CEGI is working on adding binary file
       capability and we are attempting to work with them.

   3.  Brief discussions with Jon Marcus indicate that TYM/COMM and
       COPYPC work in character by character mode and are very costly
       to the company.  Jon suggests we implement our end using block
       mode if possible.  We intend to investigate whether this is
       possible without modifying the TYM/COMM code itself.

   4.  Manpower estimates include learning time for both Lucille Parnes
       and Mary Jane Grano.



