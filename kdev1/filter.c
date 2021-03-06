/* title Pgm Filter */
/********************
 *
 *  Filter.C - A filter program to create a double spaced right justified
 *             rendition of the input for text processing purposes.
 *
 ************/

#ifndef NO_SCCS_ID
  static char
    filter_c_id [] = "@(#) Filter.C: Ver 1.0(11)  2-Oct-1986";
#endif

/************/


#include <stdio.h>
#include <io.h>
#include "kd.h"

                                /********/

typedef int
  control_type;                         /* get_next_token()'s control result */

#define CTL_NORMAL      0               /* Normal token returned */
#define CTL_EOF         1               /* End of file - no token returned */
#define CTL_BOP         2               /* Token returned is the beginning */
                                        /*   of a paragraph */
control_type
  control;

                                /********/


#define LINEBUF_LEN     132             /* length of a text line */
#define TOKEN_LEN       80              /* length of a given token */

FILE
  *infile,
  *outfile;

DSTRING
  tokbuf,                               /* the current token */
  linebuf_in,                           /* input line buffer */
  linebuf_out;                          /* output line buffer */

                                /********/

STRING
  TmpName;
/* title Rtn main */
/*  main - Do everything that needs to be done.
 */

void main (argc, argv, envp)
  int
    argc;
  STRING
    argv [];
  STRING
    envp [];
{
  /* <preprocess arguments> */

  init_dstring(tokbuf,      TOKEN_LEN);
  init_dstring(linebuf_in,  LINEBUF_LEN);
  init_dstring(linebuf_out, LINEBUF_LEN);

  if ((infile  = fopen(argv[1], "rt")) == NULL)
  {
    printf("% Problem opening input file \"%s\"\n", argv[1]);
    exit(0);
  }

  TmpName = mktemp("filXXXXX");

  if ((outfile = fopen(TmpName, "wt")) == NULL)
  {
    printf("% Problem opening output file \"%s\"\n", TmpName);
    exit(0);
  }

  for (clear_linebuf(linebuf_in), clear_tokbuf(tokbuf);
      (control = get_next_token(tokbuf)) != CTL_EOF;
      )
  {
    if (control == CTL_BOP)
      flush_linebuf(linebuf_out);

    put_this_token(tokbuf);
  }

  if (strlen(linebuf_out.str))
    flush_linebuf(linebuf_out);

  fclose(infile);
  fclose(outfile);

  if (unlink(BackupName))
  {
    printf("% Problem deleting file \"%s\"\n", BackupName);
    exit(0);
  }

  if (rename(argv[1], BackupName))
  {
    printf("% Problem renaming file \"%s\" to \"%s\"\n", argv[1],
        BackupName);
    exit(0);
  }

  if (rename(TmpName, argv[1]))
  {
    printf("% Problem renaming file \"%s\" to \"%s\"\n", TmpName,
        argv[1]);
    exit(0);
  }
}
/* title Rtn clear_linebuf */
/*  The indicated line buffer should be zeroed to the cosmos.
 */

void clear_linebuf (buffer);
  DSTRING
    buffer;
{
}
/* title Rtn flush_linebuf */
/*  The output line buffer needs to be flushed to the stdio.  Perform no
 *  fill or justify operations.
 */

void flush_linebuf (buffer);
  DSTRING
    buffer;
{
}
/* title Rtn clear_tokbuf */
/*  Clear the indicated token buffer.

void clear_tokbuf (token);
  DSTRING
    token;
{
}
/* title Rtn get_next_token */
/*  Pick up the next token from the input file and place it in the
 *  specified buffer.
 */

control_type get_next_token (buffer);
  DSTRING
    buffer;
{
}
/* title Rtn put_this_token */
/*  Whatever it may mean, throw this provided token to the output device.
 */

void put_this_token (token)
  DSTRING
    token;
{
}


/************
 *  End of Filter.C
 ********************/
   