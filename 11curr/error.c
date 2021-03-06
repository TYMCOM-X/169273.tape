/* error procedure
 * This procedure terminates the program abnormally with an error text.
 * Synopsis:
 *	int error(msg)
 *	   char *msg;
 * where msg is pointer to character string, null terminated.
 * The program will be terminated via exit(NO);.
 */

#include <std.h>

error(msg)
TEXT *msg;
  {errfmt(msg);
   exit(NO);}
                                                                                                                                                                            