/*  title Hdr Kd */
/********************
 *
 *  The kd variations
 *
 ************/

#ifndef NO_SCCS_ID
  static char
    kd_h_id [] = "@(#) Kd.H: Ver 1.0(8)  29-Sep-1986";
#endif

                              /************/


typedef char *
  STRING;

typedef unsigned
  BOOLEAN;

                              /************/

struct d_string
{
  int
    length,
    max_length;
  STRING
    str;
};

typedef struct d_string
  DSTRING;

  /*  DSTRING stands for "dynamic string", a concept ripped off from SAIL.
   *  With respect to the operations designed specifically to support
   *  dynamic strings, we need not care how big the data objects involved
   *  are.  They will expand as needed to fit the operation.
   */

extern int
  init_dstring (DSTRING, int);


/************
 *  End of Kd.H
 ********************/
   