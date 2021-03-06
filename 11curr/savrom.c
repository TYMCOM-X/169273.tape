/* SAVROM program
 * This program translates ROM .SAV files into .ROM files.  The .SAV file
 * provides the ROM parameters in the first 1000 octal bytes.  The image
 * of the ROM itself starts at address 1000.
 *
 * The format of the .SAV file is:
 *	Bytes 0-13	Chip type and manufacturer, left justified in ASCII.
 *	Bytes 14-15	Number of ROM words (2**n).
 *	Bytes 16-17	Number of ROM data bits per word.
 *	Bytes 20-21	Starting address within ROM for ROM burner.
 *	Bytes 22-23	Last address within ROM for ROM burner.
 *	Bytes 24-25	Version number - max 8 bits.
 *	Bytes 26-75	40 character (decimal) comment field.
 *	Bytes 76-77	Default for ROM data word contents.  This value is
 *			not expected to be used by this program.
 *	Bytes 100-101	Size of .RMC file.  This value is not expected to be
 *			used by this program.
 *	Bytes starting at address 1000 octal contain ROM word data values
 *	in sequence from ROM address zero.  Data is right justified.
 *
 * The format of the .ROM file is a sequence of text lines.  The first line
 * contains up to seven fields, separated by commas.  The fields are:
 *	Field 1		Chip type and manufacturer.  Up to 12 characters,
 *			selected from the radix 50 character set (alpha
 *			characters must be upper case).  Trailing blanks
 *			should not be present.
 *	Field 2		Number of words in the ROM (2**n, n is number of
 *			address lines).  Decimal.
 *	Field 3		Number of data bits per ROM word.  Decimal.
 *	Field 4		Lowest address to burn.  Hexadecimal, upper case.
 *	Field 5		Highest ROM address to burn.  Hexadecimal, upper case.
 *	Field 6		Version number.  Unsigned decimal in the range 0-255.
 *	Field 7		Comment field.  40 (decimal) ASCII characters, copied
 *			from .SAV file verbatim.
 * Fields 4, 5, 6, and 7 may be omitted or left null.
 *
 * Lines after the first contain one or two hexadecimal upper case characters
 * (one for ROMs whose data words are 4 bits or less, two for ROMs whose data
 * words are 5 to 8 bits wide).  Each such line specifies data for one word in
 * the ROM, starting at the address specified in Field 4 of the first line.
 * There must be exactly (Field 5) - (Field 4) + 1 such lines.
 */

#include <std.h>
#include "rom.h#"

#define ADDRCHRS 5
#define WORDCHARS 16			/* Max chars in ROM word (hex) + 1 */

main()
  {FIO dot_SAV, dot_ROM;
   char c;
   struct header head;
   char *buffer;
   int i, j, n, nchars, errcode;
   char hexstart[ADDRCHRS], hexstop[ADDRCHRS];
   char data[WORDCHARS];

/*
 * Establish input and output files.
 */
   openfiles(&dot_SAV, &dot_ROM);

/*
 * First, read in the header of the .SAV file and save it.  Then skip to the
 * data section.
 */
   if ((errcode = bread(&dot_SAV, &head, sizeof(head)))
         < sizeof(head)) readerr(errcode);
   buffer = alloc((n = ORG - sizeof(head)), NULL);
   if ((errcode = bread(&dot_SAV, buffer, n)) < n) readerr(errcode);
   free(buffer, NULL);

/*
 * Now write first line.  Convert addresses to hex; map to upper case.
 * Scan label, replacing blanks with underscores, mapping to upper case.
 */
   cvthex(hexstart, head.start);
   cvthex(hexstop, head.stop);
   for ( n=0 ; n<LABELSIZE ; n++ )
      head.label[n] =
         (head.label[n]==' ') ? '_' : toupper(head.label[n]);
   for ( n=0 ; n<COMMENTSIZE ; n++ )
      if (head.comment[n]==' ') head.comment[n] = '_';
/* Note LABELSIZE and COMMENTSIZE reflected in magic numbers next line */
   putf(&dot_ROM, "%.12p,%i,%i,%p,%p,%i,%.40p\n",
      &head.label, head.romsize, head.wordsize,
      hexstart, hexstop, head.version, head.comment);
   putfmt("Header: %.12p [%ix%i] (%p thru %p) Version: %i; %.40p\n",
      &head.label, head.romsize, head.wordsize,
      hexstart, hexstop, head.version, head.comment);

/*
 * Output ROM data in hex.
 */
   for ( n=0 ; n<head.romsize ; n++ )
      if ((errcode = bread(&dot_SAV, &c, 1)) == 1)
        {j = itob(&data, (int) c, 16);
         nchars = (head.wordsize-1)/4+1;
         for ( i=0 ; i<nchars-j ; i++ )
            putc(&dot_ROM, '0');
         for ( i=j-nchars<0?0:j-nchars ; i<j ; i++ )
            putc(&dot_ROM, toupper(data[i]));
         putc(&dot_ROM, '\n');}
      else
         readerr(errcode);

/*
 * Close files, exit
 */
   fclose(&dot_ROM);
   bclose(&dot_SAV);
   putfmt("All done.\n");}


/* openfiles procedure
 * This procedure asks the terminal operator for the names of the input
 * and output files, and opens them.
 */
openfiles(in, out)
FIO *in, *out;
  {TEXT name[81];			/* buffer for RSTS file names */
   int i;
   in->_fd = 14;			/* RSTS channel number for input */

   do
     {putfmt("Enter name of input (.SAV) file:\n");
      if (getfmt("%.81p", &name) == EOF) exit(YES);
      if ((i = bopen(in, &name)) < 0)
         errfmt("? Can't open input file: %p, error code %i.\n", &name, i);}
   while (i < 0);
   do
     {putfmt("Enter name of output (.ROM) file:\n");
      if (getfmt("%.81p", &name) == EOF) exit(YES);
      if ((i = fcreate(out, &name, WRITE)) == NULL)
         errfmt("? Can't open output file: %p.\n", &name);}
   while (i == NULL);
   return;}


/* readerr procedure
 * This is the read error handler procedure.  Entered when you didn't get
 * as much as you expected.  If RSTS error, it will be output.  Otherwise,
 * 'file too short' will be issued.  In either case, program dies.
 */
readerr(n)
int n;
  {if (n < 0)
     {errfmt("%% RSTS error %i\n", -n);
      error("? Error reading .SAV file\n");}
   else
     {errfmt("%% Got %i bytes\n", n);
      error("? .SAV file too short\n");}}


/*
 * cvthex procedure
 * This routine converts an address (16 bits max) into upper-case hexadecimal.
 * Synopsis:
 *	cvthex(buf, addr)
 *	int addr;
 *	char *buf;
 * Buffer is assumed to be at least ADDRCHRS in length.
 */
cvthex(buf, addr)
int addr;
char *buf;
  {buf[decode(buf, ADDRCHRS-1, "%+03h\0", addr)] = '\0';
   while ((*buf = toupper(*buf)) != '\0')
      buf++;}
                                                                                 