/* Buffered Binary I/O package
 * This set of procedures provides a C program with a simple buffered binary
 * I/O capability.  It blocks/deblocks data from buffers contained in FIO
 * structures passed to the routines.  A set of system level block I/O
 * functions are required (x-routines).
 */

#include <std.h>
#define RSTSEOF -11			/* RSTS End-of-file return from xread */

/* bopen procedure
 * This is the bufferd binary open procedure.
 * Synopsis:
 * FILE bopen(bfd, name)
 *	FIO *bfd;			FIO containing RSTS channel number
 *	TEXT *name;			RSTS file name string
 * This procedure initializes the FIO block for the file, and opens the
 * file.  The RSTS channel number must have been previously placed in the
 * FIO block.  If the file exists, it is opened for reading and the RSTS
 * channel number is returned.  If the file does not exist, -1 is returned.
 */
FILE bopen(bfd, name)
FIO *bfd;
TEXT *name;
  {int errcode;
   if ((errcode = xopen(bfd->_fd, name)) < 0) return errcode;
   bfd->_nleft = 0;
   bfd->_fmode = READ;
   return bfd->_fd;}


/* bcreate procedure
 * This is the bufferd binary create procedure.
 * Synopsis:
 * FILE bcreate(bfd, name)
 *	FIO *bfd;			FIO containing RSTS channel number
 *	TEXT *name;			RSTS file name string
 * This procedure initializes the FIO block for the file, and opens the
 * file.  The RSTS channel number must have been previously placed in the
 * FIO block.  If possible, the file is opened for writing and the RSTS
 * channel number is returned.  If not possible, -1 is returned.
 */
FILE bcreate(bfd, name)
FIO *bfd;
TEXT *name;
  {int errcode;
   if ((errcode = xcreate(bfd->_fd, name)) < 0) return errcode;
   bfd->_nleft = BUFSIZE;
   bfd->_fmode = WRITE;
   bfd->_pnext = bfd->_buf;
   return bfd->_fd;}


/* bread procedure
 * This is the buffered binary read procedure.
 * Synopsis:
 *	int bread(bfd,buffer,size)
 *	   FIO *bfd;			binary file descriptor pointer
 *	   char *buffer;		pointer to buffer
 *	   int size;			size of buffer in chars
 * This procedure handles deblocking from operating system
 * fixed-length buffers.  Data is transfered in binary mode; no translation
 * of characters is performed at all.  The actual number of characters
 * placed into the buffer is returned, unless an operating system error
 * is encountered.  In this case, the (negative) operating system error code
 * is returned.  The number of characters placed into the buffer can be less
 * than the size of the buffer if end of file is encountered.  Subsequent
 * bread calls will return zero.
 */
bread(bfd, buffer, size)
FIO *bfd;
char *buffer;
int size;
  {int i, n;

   if (bfd->_nleft < 0) return 0;		/* End-of-file */
   n = size;
   while (n > bfd->_nleft)
      if (bfd->_nleft == 0)
         if ((i = xread(bfd->_fd, bfd->_pnext=bfd->_buf, BUFSIZE)) <= 0)
            if (i == RSTSEOF)
              {bfd->_nleft = -1;
               return size-n;}
            else
               return i;			/* return RSTS error code */
         else
            bfd->_nleft = i;
      else
        {for ( i=0 ; i<bfd->_nleft ; i++ )
            *buffer++ = *bfd->_pnext++;
         n -= bfd->_nleft;
         bfd->_nleft = 0;}
   if (n > 0)
     {for ( i=0 ; i<n ; i++ )
         *buffer++ = *bfd->_pnext++;
      bfd->_nleft -= n;};
   return size;}


/* bwrite procedure
 * This is the buffered binary write procedure.
 * Synopsis:
 *	int bwrite(pfd,buffer,size)
 *	   FIO *bfd;			binary file descriptor pointer
 *	   char *buffer;		pointer to buffer
 *	   int size;			size of buffer in chars
 * This procedure handles blocking from operating system fixed-length buffers.
 * Data is transfered in binary mode; no translation of characters is
 * performed at all.  Zero is returned unless an operating system error is
 * encountered.  In this case, the (negative) operating system error code is
 * returned.

 */
bwrite(bfd, buffer, size)
FIO *bfd;
char *buffer;
int size;
  {int i, n;

   n = size;
   while (n > bfd->_nleft)
      if (bfd->_nleft == 0)
         if (
            (i = xwrite(bfd->_fd, bfd->_pnext=bfd->_buf, bfd->_nleft = BUFSIZE))
               < 0)
            return i;			/* return RSTS error code */
      else
        {for ( i=0 ; i<bfd->_nleft ; i++ )
            *bfd->_pnext++ = *buffer++;
         n -= bfd->_nleft;
         bfd->_nleft = 0;}
   if (n > 0)
     {for ( i=0 ; i<n ; i++ )
         *bfd->_pnext++ = *buffer++;
      bfd->_nleft -= n;}
   return 0;}


/* bclose procedure
 * This is the buffered binary close procedure.
 * Synopsis:
 * FILE bclose(bfd)
 *	FIO *bfd;			FIO contains RSTS channel number
 * This procedure closes the file described by the provided FIO block.
 * If file was opened for reading, this fact is erased.  If file was
 * opened for writing, the buffers are flushed, the file closed, the the
 * fact that it was open for writing erased.  The RSTS channel number is
 * returned.
 */
FILE bclose(bfd)
FIO *bfd;
  {int errcode;
   switch (bfd->_fmode)
     {case WRITE:
         if (bfd->_nleft < BUFSIZE)
            if ((errcode = xwrite(bfd->_fd, bfd->_buf, BUFSIZE-bfd->_nleft))
                  < 0) return errcode;
      default:
         bfd->_fmode = -1;}
   if ((errcode = xclose(bfd->_fd)) < 0) return errcode;
   return bfd->_fd;}
                                                                                                                                                                                                                         