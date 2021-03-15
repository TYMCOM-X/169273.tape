
begin

require "(SAILIB)SAIL.DEF"   source!file;
define /* = {comment}, */ = {;};

/* 
 * Compress - data compression program 
 */
/*
 * Set USERMEM to the maximum amount of physical user memory available
 * in bytes.  USERMEM is used to determine the maximum BITS that can be used
 * for compression.
 *
 * SACREDMEM is the amount of physical memory saved for others; compress
 * will hog the rest.
 */

define	SACREDMEM = 0;

define	PBITS = 16;		/*  make this a variable-switch later */
define	BITS  = PBITS;
define	HSIZE = 69001;		/* 95% occupancy */

				/* HSIZE table based on BITS */
				/*  69001   16   95%  */
				/*  35023   15   94%  */
				/*  18013   14   91%  */
				/*   9001   13   91%  */
				/*   5003 <=12  *80%  */

/*
 * a code_int must be able to hold 2**BITS values of type int, and also -1
 */

define	code!int  = {integer};
define	count!int = {integer};

define	char!type = {integer};

char!type magic!header[] = { "\037\235" };	/* 1F 9D */

/* Defines for third byte of header */
define	BIT!MASK	= '037;
define	BLOCK!MASK	= '200;

/*f Masks '100 and '040 are free.  I think '040 should mean that there is
   a fourth header byte (for expansion).
*/

define INIT!BITS = 9;			/* initial number of bits/code */

/*
 * compress.c - File compression ala IEEE Computer, June 1984.
 *
 * Authors:	Spencer W. Thomas	(decvax!harpo!utah-cs!utah-gr!thomas)
 *		Jim McKie		(decvax!mcvax!jim)
 *		Steve Davies		(decvax!vax135!petsd!peora!srd)
 *		Ken Turkowski		(decvax!decwrl!turtlevax!ken)
 *		James A. Woods		(decvax!ihnp4!ames!jaw)
 *		Joe Orost		(decvax!vax135!petsd!joe)
 *
 */

#define ARGVAL() (*++(*argv) || (--argc && *++argv))

preload!with BITS;
integer array maxbitsarray[0:0];	/* user settable max # bits/code */
define maxbits = {maxbitsarray[0]};

integer n!bits;				/* number of bits/code */
code!int maxcode;			/* maximum code, given n!bits */

code!int maxmaxcode = 1L << BITS;	/* should NEVER generate this code */
#ifdef COMPATIBLE		/* But wrong! */
# define MAXCODE(n!bits)	(1L << (n!bits) - 1)
#else
# define MAXCODE(n!bits)	((1L << (n!bits)) - 1)
#endif /* COMPATIBLE */

integer array htab[HSIZE]?
#ifdef XENIX!16
count!int htab0[8192];
count!int htab1[8192];
count!int htab2[8192];
count!int htab3[8192];
count!int htab4[8192];
count!int htab5[8192];
count!int htab6[8192];
count!int htab7[8192];
count!int htab8[HSIZE-65536];
count!int * htab[9] = {
	htab0, htab1, htab2, htab3, htab4, htab5, htab6, htab7, htab8 };

#define htabof(i)	(htab[(i) >> 13][(i) & 0x1fff])
unsigned short code0tab[16384];
unsigned short code1tab[16384];
unsigned short code2tab[16384];
unsigned short code3tab[16384];
unsigned short code4tab[16384];
unsigned short * codetab[5] = {
	code0tab, code1tab, code2tab, code3tab, code4tab };

#define codetabof(i)	(codetab[(i) >> 14][(i) & 0x3fff])

#else	/* Normal machine */
# ifdef sel
/* support gould base register problems */
/*NOBASE*/
count!int htab [HSIZE];
unsigned short codetab [HSIZE];
/*NOBASE*/
# else /* !gould */
count!int htab [HSIZE];
unsigned short codetab [HSIZE];
# endif /* !gould */
#define htabof(i)	htab[i]
#define codetabof(i)	codetab[i]
#endif	/* !XENIX!16 */
code!int hsize = HSIZE;			/* for dynamic table sizing */
count!int fsize;

/*
 * To save much memory, we overlay the table used by compress() with those
 * used by decompress().  The tab!prefix table is the same size and type
 * as the codetab.  The tab!suffix table needs 2**BITS characters.  We
 * get this from the beginning of htab.  The output stack uses the rest
 * of htab, and contains characters.  There is plenty of room for any
 * possible stack (stack used to be 8000 characters).
 */

#define tab!prefixof(i)	codetabof(i)
#ifdef XENIX!16
# define tab!suffixof(i)	((char!type *)htab[(i)>>15])[(i) & 0x7fff]
# define de!stack		((char!type *)(htab2))
#else	/* Normal machine */
# define tab!suffixof(i)	((char!type *)(htab))[i]
# define de!stack		((char!type *)&tab!suffixof(1<<BITS))
#endif	/* XENIX!16 */

code!int free!ent = 0;			/* first unused entry */
int exit!stat = 0;

code!int getcode();

simple procedure Usage;
print("Usage: compress [-dfvcV] [-b maxbits] [file ...]"&crlf);

int nomagic = 0;	/* Use a 3-byte magic number header, unless old file */
int zcat!flg = 0;	/* Write output on stdout, suppress messages */
int quiet = 1;		/* don't tell me about compression */

/*
 * block compression parameters -- after all codes are used up,
 * and compression rate changes, start over.
 */
int block!compress = BLOCK!MASK;
int clear!flg = 0;
long int ratio = 0;
#define CHECK!GAP 10000	/* ratio check interval */
count!int checkpoint = CHECK!GAP;
/*
 * the next two codes should not be changed lightly, as they must not
 * lie within the contiguous general code space.
 */ 
#define FIRST	257	/* first free entry */
#define	CLEAR	256	/* table clear output code */

int force = 0;
char ofname [100];
#ifdef DEBUG
int verbose = 0;
#endif /* DEBUG */
int (*bgnd!flag)();

int do!decomp = 0;



static int offset;
long int in!count = 1;			/* length of input */
long int bytes!out;			/* length of compressed output */
long int out!count = 0;			/* # of codes output (for debugging) */

/*
 * compress stdin to stdout
 *
 * Algorithm:  use open addressing double hashing (no chaining) on the 
 * prefix code / next character combination.  We do a variant of Knuth's
 * algorithm D (vol. 3, sec. 6.4) along with G. Knott's relatively-prime
 * secondary probe.  Here, the modular division first probe is gives way
 * to a faster exclusive-or manipulation.  Also do block compression with
 * an adaptive reset, whereby the code table is cleared when the compression
 * ratio decreases, but after the table fills.  The variable-length output
 * codes are re-sized at this point, and a special CLEAR code is generated
 * for the decompressor.  Late addition:  construct the table according to
 * file size for noticeable speed improvement on small files.  Please direct
 * questions about this implementation to ames!jaw.
 */

compress() {
    register long fcode;
    register code!int i = 0;
    register int c;
    register code!int ent;
#ifdef XENIX!16
    register code!int disp;
#else	/* Normal machine */
    register int disp;
#endif
    register code!int hsize!reg;
    register int hshift;

#ifndef COMPATIBLE
    if (nomagic == 0) {
	putchar(magic!header[0]); putchar(magic!header[1]);
	putchar((char)(maxbits | block!compress));
	if(ferror(stdout))
		writeerr();
    }
#endif /* COMPATIBLE */

    offset = 0;
    bytes!out = 3;		/* includes 3-byte header mojo */
    out!count = 0;
    clear!flg = 0;
    ratio = 0;
    in!count = 1;
    checkpoint = CHECK!GAP;
    maxcode = MAXCODE(n!bits = INIT!BITS);
    free!ent = ((block!compress) ? FIRST : 256 );

    ent = getchar ();

    hshift = 0;
    for ( fcode = (long) hsize;  fcode < 65536L; fcode *= 2L )
    	hshift++;
    hshift = 8 - hshift;		/* set hash code range bound */

    hsize!reg = hsize;
    cl!hash( (count!int) hsize!reg);		/* clear hash table */

#ifdef SIGNED!COMPARE!SLOW
    while ( (c = getchar()) != (unsigned) EOF ) {
#else
    while ( (c = getchar()) != EOF ) {
#endif
	in!count++;
	fcode = (long) (((long) c << maxbits) + ent);
 	i = (((long)c << hshift) ^ ent);	/* xor hashing */

	if ( htabof (i) == fcode ) {
	    ent = codetabof (i);
	    continue;
	} else if ( (long)htabof (i) < 0 )	/* empty slot */
	    goto nomatch;
 	disp = hsize!reg - i;		/* secondary hash (after G. Knott) */
	if ( i == 0 )
	    disp = 1;
probe:
	if ( (i -= disp) < 0 )
	    i += hsize!reg;

	if ( htabof (i) == fcode ) {
	    ent = codetabof (i);
	    continue;
	}
	if ( (long)htabof (i) > 0 ) 
	    goto probe;
nomatch:
	output ( (code!int) ent );
	out!count++;
 	ent = c;
#ifdef SIGNED!COMPARE!SLOW
	if ( (unsigned) free!ent < (unsigned) maxmaxcode) {
#else
	if ( free!ent < maxmaxcode ) {
#endif
 	    codetabof (i) = free!ent++;	/* code -> hashtable */
	    htabof (i) = fcode;
	}
	else if ( (count!int)in!count >= checkpoint && block!compress )
	    cl!block ();
    }
    /*
     * Put out the final code.
     */
    output( (code!int)ent );
    out!count++;
    output( (code!int)-1 );

    /*
     * Print out stats on stderr
     */
    if(zcat!flg == 0 && !quiet) {
#ifdef DEBUG
	fprintf( stderr,
		"%ld chars in, %ld codes (%ld bytes) out, compression factor: ",
		in!count, out!count, bytes!out );
	prratio( stderr, in!count, bytes!out );
	fprintf( stderr, "\n");
	fprintf( stderr, "\tCompression as in compact: " );
	prratio( stderr, in!count-bytes!out, in!count );
	fprintf( stderr, "\n");
	fprintf( stderr, "\tLargest code (of last block) was %d (%d bits)\n",
		free!ent - 1, n!bits );
#else /* !DEBUG */
	fprintf( stderr, "Compression: " );
	prratio( stderr, in!count-bytes!out, in!count );
#endif /* DEBUG */
    }
    if(bytes!out > in!count)	/* exit(2) if no savings */
	exit!stat = 2;
    return;
}

/*****************************************************************
 * TAG( output )
 *
 * Output the given code.
 * Inputs:
 * 	code:	A n!bits-bit integer.  If == -1, then EOF.  This assumes
 *		that n!bits =< (long)wordsize - 1.
 * Outputs:
 * 	Outputs code to the file.
 * Assumptions:
 *	Chars are 8 bits long.
 * Algorithm:
 * 	Maintain a BITS character long buffer (so that 8 codes will
 * fit in it exactly).  Use the VAX insv instruction to insert each
 * code in turn.  When the buffer fills up empty it and start over.
 */

static char buf[BITS];

#ifndef vax
char!type lmask[9] = {0xff, 0xfe, 0xfc, 0xf8, 0xf0, 0xe0, 0xc0, 0x80, 0x00};
char!type rmask[9] = {0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f, 0xff};
#endif /* vax */

output( code )
code!int  code;
{
#ifdef DEBUG
    static int col = 0;
#endif /* DEBUG */

    /*
     * On the VAX, it is important to have the register declarations
     * in exactly the order given, or the asm will break.
     */
    register int r!off = offset, bits= n!bits;
    register char * bp = buf;

#ifdef DEBUG
	if ( verbose )
	    fprintf( stderr, "%5d%c", code,
		    (col+=6) >= 74 ? (col = 0, '\n') : ' ' );
#endif /* DEBUG */
    if ( code >= 0 ) {
#ifdef vax
	/* VAX DEPENDENT!! Implementation on other machines is below.
	 *
	 * Translation: Insert BITS bits from the argument starting at
	 * offset bits from the beginning of buf.
	 */
	0;	/* Work around for pcc -O bug with asm and if stmt */
	asm( "insv	4(ap),r11,r10,(r9)" );
#else /* not a vax */
/* 
 * byte/bit numbering on the VAX is simulated by the following code
 */
	/*
	 * Get to the first byte.
	 */
	bp += (r!off >> 3);
	r!off &= 7;
	/*
	 * Since code is always >= 8 bits, only need to mask the first
	 * hunk on the left.
	 */
	*bp = (*bp & rmask[r!off]) | (code << r!off) & lmask[r!off];
	bp++;
	bits -= (8 - r!off);
	code >>= 8 - r!off;
	/* Get any 8 bit parts in the middle (<=1 for up to 16 bits). */
	if ( bits >= 8 ) {
	    *bp++ = code;
	    code >>= 8;
	    bits -= 8;
	}
	/* Last bits. */
	if(bits)
	    *bp = code;
#endif /* vax */
	offset += n!bits;
	if ( offset == (n!bits << 3) ) {
	    bp = buf;
	    bits = n!bits;
	    bytes!out += bits;
	    do
		putchar(*bp++);
	    while(--bits);
	    offset = 0;
	}

	/*
	 * If the next entry is going to be too big for the code size,
	 * then increase it, if possible.
	 */
	if ( free!ent > maxcode || (clear!flg > 0))
	{
	    /*
	     * Write the whole buffer, because the input side won't
	     * discover the size increase until after it has read it.
	     */
	    if ( offset > 0 ) {
		if( fwrite( buf, 1, n!bits, stdout ) != n!bits)
			writeerr();
		bytes!out += n!bits;
	    }
	    offset = 0;

	    if ( clear!flg ) {
    	        maxcode = MAXCODE (n!bits = INIT!BITS);
	        clear!flg = 0;
	    }
	    else {
	    	n!bits++;
	    	if ( n!bits == maxbits )
		    maxcode = maxmaxcode;
	    	else
		    maxcode = MAXCODE(n!bits);
	    }
#ifdef DEBUG
	    if ( debug ) {
		fprintf( stderr, "\nChange to %d bits\n", n!bits );
		col = 0;
	    }
#endif /* DEBUG */
	}
    } else {
	/*
	 * At EOF, write the rest of the buffer.
	 */
	if ( offset > 0 )
	    fwrite( buf, 1, (offset + 7) / 8, stdout );
	bytes!out += (offset + 7) / 8;
	offset = 0;
	fflush( stdout );
#ifdef DEBUG
	if ( verbose )
	    fprintf( stderr, "\n" );
#endif /* DEBUG */
	if( ferror( stdout ) )
		writeerr();
    }
}

/*
 * Decompress stdin to stdout.  This routine adapts to the codes in the
 * file building the "string" table on-the-fly; requiring no table to
 * be stored in the compressed file.  The tables used herein are shared
 * with those of the compress() routine.  See the definitions above.
 */

decompress() {
    register char!type *stackp;
    register int finchar;
    register code!int code, oldcode, incode;

    /*
     * As above, initialize the first 256 entries in the table.
     */
    maxcode = MAXCODE(n!bits = INIT!BITS);
    for ( code = 255; code >= 0; code-- ) {
	tab!prefixof(code) = 0;
	tab!suffixof(code) = (char!type)code;
    }
    free!ent = ((block!compress) ? FIRST : 256 );

    finchar = oldcode = getcode();
    if(oldcode == -1)	/* EOF already? */
	return;			/* Get out of here */
    putchar( (char)finchar );		/* first code must be 8 bits = char */
    if(ferror(stdout))		/* Crash if can't write */
	writeerr();
    stackp = de!stack;

    while ( (code = getcode()) > -1 ) {

	if ( (code == CLEAR) && block!compress ) {
	    for ( code = 255; code >= 0; code-- )
		tab!prefixof(code) = 0;
	    clear!flg = 1;
	    free!ent = FIRST - 1;
	    if ( (code = getcode ()) == -1 )	/* O, untimely death! */
		break;
	}
	incode = code;
	/*
	 * Special case for KwKwK string.
	 */
	if ( code >= free!ent ) {
            *stackp++ = finchar;
	    code = oldcode;
	}

	/*
	 * Generate output characters in reverse order
	 */
#ifdef SIGNED!COMPARE!SLOW
	while ( ((unsigned long)code) >= ((unsigned long)256) ) {
#else
	while ( code >= 256 ) {
#endif
	    *stackp++ = tab!suffixof(code);
	    code = tab!prefixof(code);
	}
	*stackp++ = finchar = tab!suffixof(code);

	/*
	 * And put them out in forward order
	 */
	do
	    putchar ( *--stackp );
	while ( stackp > de!stack );

	/*
	 * Generate the new entry.
	 */
	if ( (code=free!ent) < maxmaxcode ) {
	    tab!prefixof(code) = (unsigned short)oldcode;
	    tab!suffixof(code) = finchar;
	    free!ent = code+1;
	} 
	/*
	 * Remember previous code.
	 */
	oldcode = incode;
    }
    fflush( stdout );
    if(ferror(stdout))
	writeerr();
}

/*****************************************************************
 * TAG( getcode )
 *
 * Read one code from the standard input.  If EOF, return -1.
 * Inputs:
 * 	stdin
 * Outputs:
 * 	code or -1 is returned.
 */

code!int
getcode() {
    /*
     * On the VAX, it is important to have the register declarations
     * in exactly the order given, or the asm will break.
     */
    register code!int code;
    static int offset = 0, size = 0;
    static char!type buf[BITS];
    register int r!off, bits;
    register char!type *bp = buf;

    if ( clear!flg > 0 || offset >= size || free!ent > maxcode ) {
	/*
	 * If the next entry will be too big for the current code
	 * size, then we must increase the size.  This implies reading
	 * a new buffer full, too.
	 */
	if ( free!ent > maxcode ) {
	    n!bits++;
	    if ( n!bits == maxbits )
		maxcode = maxmaxcode;	/* won't get any bigger now */
	    else
		maxcode = MAXCODE(n!bits);
	}
	if ( clear!flg > 0) {
    	    maxcode = MAXCODE (n!bits = INIT!BITS);
	    clear!flg = 0;
	}
	size = fread( buf, 1, n!bits, stdin );
	if ( size <= 0 )
	    return -1;			/* end of file */
	offset = 0;
	/* Round size down to integral number of codes */
	size = (size << 3) - (n!bits - 1);
    }
    r!off = offset;
    bits = n!bits;
#ifdef vax
    asm( "extzv   r10,r9,(r8),r11" );
#else /* not a vax */
	/*
	 * Get to the first byte.
	 */
	bp += (r!off >> 3);
	r!off &= 7;
	/* Get first part (low order bits) */
#ifdef NO!UCHAR
	code = ((*bp++ >> r!off) & rmask[8 - r!off]) & 0xff;
#else
	code = (*bp++ >> r!off);
#endif /* NO!UCHAR */
	bits -= (8 - r!off);
	r!off = 8 - r!off;		/* now, offset into code word */
	/* Get any 8 bit parts in the middle (<=1 for up to 16 bits). */
	if ( bits >= 8 ) {
#ifdef NO!UCHAR
	    code |= (*bp++ & 0xff) << r!off;
#else
	    code |= *bp++ << r!off;
#endif /* NO!UCHAR */
	    r!off += 8;
	    bits -= 8;
	}
	/* high order bits. */
	code |= (*bp & rmask[bits]) << r!off;
#endif /* vax */
    offset += n!bits;

    return code;
}

char *
rindex(s, c)		/* For those who don't have it in libc.a */
register char *s, c;
{
	char *p;
	for (p = NULL; *s; s++)
	    if (*s == c)
		p = s;
	return(p);
}

#ifdef DEBUG
printcodes()
{
    /*
     * Just print out codes from input file.  For debugging.
     */
    code!int code;
    int col = 0, bits;

    bits = n!bits = INIT!BITS;
    maxcode = MAXCODE(n!bits);
    free!ent = ((block!compress) ? FIRST : 256 );
    while ( ( code = getcode() ) >= 0 ) {
	if ( (code == CLEAR) && block!compress ) {
   	    free!ent = FIRST - 1;
   	    clear!flg = 1;
	}
	else if ( free!ent < maxmaxcode )
	    free!ent++;
	if ( bits != n!bits ) {
	    fprintf(stderr, "\nChange to %d bits\n", n!bits );
	    bits = n!bits;
	    col = 0;
	}
	fprintf(stderr, "%5d%c", code, (col+=6) >= 74 ? (col = 0, '\n') : ' ' );
    }
    putc( '\n', stderr );
    exit( 0 );
}

#ifdef XENIX!16
code!int stab1[8192] ;
code!int stab2[8192] ;
code!int stab3[8192] ;
code!int stab4[8192] ;
code!int stab5[8192] ;
code!int stab6[8192] ;
code!int stab7[8192] ;
code!int stab8[8192] ;
code!int * sorttab[8] = {stab1, stab2, stab3, stab4, stab5, stab6, stab7,
						 stab8 } ;
#define stabof(i) (sorttab[(i) >> 13][(i) & 0x1fff]) 
#else
code!int sorttab[HSIZE];	/* sorted pointers into htab */
#define stabof(i) (sorttab[i])
#endif

dump!tab()	/* dump string table */
{
 gister int i, first;
    register ent;
#define STACK!SIZE	15000
    int stack!top = STACK!SIZE;
    register c;
	unsigned mbshift ;

    if(do!decomp == 0) {	/* compressing */
	register int flag = 1;

	for(i=0; i<hsize; i++) {	/* build sort pointers */
		if((long)htabof(i) >= 0) {
			stabof(codetabof(i)) = i;
		}
	}
	first = block!compress ? FIRST : 256;
	for(i = first; i < free!ent; i++) {
		fprintf(stderr, "%5d: \"", i);
		de!stack[--stack!top] = '\n';
		de!stack[--stack!top] = '"';
		stack!top = in!stack((htabof(stabof(i))>>maxbits)&0xff, 
                                     stack!top);
/*		for(ent=htabof(stabof(i)) & ((1<<maxbits)-1); */
		mbshift = ((1 << maxbits) - 1) ;
		ent = htabof(stabof(i)) & mbshift ;
		for(;
		    ent > 256;
		    /* ent=htabof(stabof(ent)) & ((1<<maxbits)-1)) { */
		    ent=htabof(stabof(ent)) & mbshift) {
			stack!top = in!stack(htabof(stabof(ent)) >> maxbits,
						stack!top);
		}
		stack!top = in!stack(ent, stack!top);
		fwrite( &de!stack[stack!top], 1, STACK!SIZE-stack!top, stderr);
	   	stack!top = STACK!SIZE;
	}
   } else if(!debug) {	/* decompressing */

       for ( i = 0; i < free!ent; i++ ) {
	   ent = i;
	   c = tab!suffixof(ent);
	   if ( isascii(c) && isprint(c) )
	       fprintf( stderr, "%5d: %5d/'%c'  \"",
			   ent, tab!prefixof(ent), c );
	   else
	       fprintf( stderr, "%5d: %5d/\\%03o \"",
			   ent, tab!prefixof(ent), c );
	   de!stack[--stack!top] = '\n';
	   de!stack[--stack!top] = '"';
	   for ( ; ent != 0;
		   ent = (ent >= FIRST ? tab!prefixof(ent) : 0) ) {
	       stack!top = in!stack(tab!suffixof(ent), stack!top);
	   }
	   fwrite( &de!stack[stack!top], 1, STACK!SIZE - stack!top, stderr );
	   stack!top = STACK!SIZE;
       }
    }
}

int
in!stack(c, stack!top)
	register c, stack!top;
{
	if ( (isascii(c) && isprint(c) && c != '\\') || c == ' ' ) {
	    de!stack[--stack!top] = c;
	} else {
	    switch( c ) {
	    case '\n': de!stack[--stack!top] = 'n'; break;
	    case '\t': de!stack[--stack!top] = 't'; break;
	    case '\b': de!stack[--stack!top] = 'b'; break;
	    case '\f': de!stack[--stack!top] = 'f'; break;
	    case '\r': de!stack[--stack!top] = 'r'; break;
	    case '\\': de!stack[--stack!top] = '\\'; break;
	    default:
	 	de!stack[--stack!top] = '0' + c % 8;
	 	de!stack[--stack!top] = '0' + (c / 8) % 8;
	 	de!stack[--stack!top] = '0' + c / 64;
	 	break;
	    }
	    de!stack[--stack!top] = '\\';
	}
	return stack!top;
}
#endif /* DEBUG */

writeerr()
{
    perror ( ofname );
    unlink ( ofname );
    exit ( 1 );
}

copystat(ifname, ofname)
char *ifname, *ofname;
{
    struct stat statbuf;
    int mode;
    time!t timep[2];

    fclose(stdout);
    if (stat(ifname, &statbuf)) {		/* Get stat on input file */
	perror(ifname);
	return;
    }
    if ((statbuf.st!mode & S!IFMT/*0170000*/) != S!IFREG/*0100000*/) {
	if(quiet)
	    	fprintf(stderr, "%s: ", ifname);
	fprintf(stderr, " -- not a regular file: unchanged");
	exit!stat = 1;
    } else if (statbuf.st!nlink > 1) {
	if(quiet)
	    	fprintf(stderr, "%s: ", ifname);
	fprintf(stderr, " -- has %d other links: unchanged",
		statbuf.st!nlink - 1);
	exit!stat = 1;
    } else if (exit!stat == 2 && (!force)) { /* No compression: remove file.Z */
	if(!quiet)
		fprintf(stderr, " -- file unchanged");
    } else {			/* ***** Successful Compression ***** */
	exit!stat = 0;
	mode = statbuf.st!mode & 07777;
	if (chmod(ofname, mode))		/* Copy modes */
	    perror(ofname);
	chown(ofname, statbuf.st!uid, statbuf.st!gid);	/* Copy ownership */
	timep[0] = statbuf.st!atime;
	timep[1] = statbuf.st!mtime;
	utime(ofname, timep);	/* Update last accessed and modified times */
	if (unlink(ifname))	/* Remove input file */
	    perror(ifname);
	if(!quiet)
		fprintf(stderr, " -- replaced with %s", ofname);
	return;		/* Successful return */
    }

    /* Unsuccessful return -- one of the tests failed */
    if (unlink(ofname))
	perror(ofname);
}
/*
 * This routine returns 1 if we are running in the foreground and stderr
 * is a tty.
 */
foreground()
{
	if(bgnd!flag) {	/* background? */
		return(0);
	} else {			/* foreground */
		if(isatty(2)) {		/* and stderr is a tty */
			return(1);
		} else {
			return(0);
		}
	}
}

onintr ( )
{
    unlink ( ofname );
    exit ( 1 );
}

oops ( )	/* wild pointer -- assume bad input */
{
    if ( do!decomp == 1 ) 
    	fprintf ( stderr, "uncompress: corrupt input\n" );
    unlink ( ofname );
    exit ( 1 );
}

cl!block ()		/* table clear for block compress */
{
    register long int rat;

    checkpoint = in!count + CHECK!GAP;
#ifdef DEBUG
	if ( debug ) {
    		fprintf ( stderr, "count: %ld, ratio: ", in!count );
     		prratio ( stderr, in!count, bytes!out );
		fprintf ( stderr, "\n");
	}
#endif /* DEBUG */

    if(in!count > 0x007fffff) {	/* shift will overflow */
	rat = bytes!out >> 8;
	if(rat == 0) {		/* Don't divide by zero */
	    rat = 0x7fffffff;
	} else {
	    rat = in!count / rat;
	}
    } else {
	rat = (in!count << 8) / bytes!out;	/* 8 fractional bits */
    }
    if ( rat > ratio ) {
	ratio = rat;
    } else {
	ratio = 0;
#ifdef DEBUG
	if(verbose)
		dump!tab();	/* dump string table */
#endif
 	cl!hash ( (count!int) hsize );
	free!ent = FIRST;
	clear!flg = 1;
	output ( (code!int) CLEAR );
#ifdef DEBUG
	if(debug)
    		fprintf ( stderr, "clear\n" );
#endif /* DEBUG */
    }
}

cl!hash(hsize)		/* reset code table */
	register count!int hsize;
{
#ifndef XENIX!16	/* Normal machine */
	register count!int *htab!p = htab+hsize;
#else
	register j;
	register long k = hsize;
	register count!int *htab!p;
#endif
	register long i;
	register long m1 = -1;

#ifdef XENIX!16
    for(j=0; j<=8 && k>=0; j++,k-=8192) {
	i = 8192;
	if(k < 8192) {
		i = k;
	}
	htab!p = &(htab[j][i]);
	i -= 16;
	if(i > 0) {
#else
	i = hsize - 16;
#endif
 	do {				/* might use Sys V memset(3) here */
		*(htab!p-16) = m1;
		*(htab!p-15) = m1;
		*(htab!p-14) = m1;
		*(htab!p-13) = m1;
		*(htab!p-12) = m1;
		*(htab!p-11) = m1;
		*(htab!p-10) = m1;
		*(htab!p-9) = m1;
		*(htab!p-8) = m1;
		*(htab!p-7) = m1;
		*(htab!p-6) = m1;
		*(htab!p-5) = m1;
		*(htab!p-4) = m1;
		*(htab!p-3) = m1;
		*(htab!p-2) = m1;
		*(htab!p-1) = m1;
		htab!p -= 16;
	} while ((i -= 16) >= 0);
#ifdef XENIX!16
	}
    }
#endif
    	for ( i += 16; i > 0; i-- )
		*--htab!p = m1;
}

prratio(stream, num, den)
FILE *stream;
long int num, den;
{
	register int q;			/* Doesn't need to be long */

	if(num > 214748L) {		/* 2147483647/10000 */
		q = num / (den / 10000L);
	} else {
		q = 10000L * num / den;		/* Long calculations, though */
	}
	if (q < 0) {
		putc('-', stream);
		q = -q;
	}
	fprintf(stream, "%d.%02d%%", q / 100, q % 100);
}

version()
{
	fprintf(stderr, "%s\n", rcs!ident);
	fprintf(stderr, "Options: ");
#ifdef vax
	fprintf(stderr, "vax, ");
#endif
#ifdef NO!UCHAR
	fprintf(stderr, "NO!UCHAR, ");
#endif
#ifdef SIGNED!COMPARE!SLOW
	fprintf(stderr, "SIGNED!COMPARE!SLOW, ");
#endif
#ifdef XENIX!16
	fprintf(stderr, "XENIX!16, ");
#endif
#ifdef COMPATIBLE
	fprintf(stderr, "COMPATIBLE, ");
#endif
#ifdef DEBUG
	fprintf(stderr, "DEBUG, ");
#endif
#ifdef BSD4!2
	fprintf(stderr, "BSD4.2, ");
#endif
	fprintf(stderr, "BITS = %d\n", BITS);
}
/*****************************************************************
 * TAG( main )
 *
 * Algorithm from "A Technique for High Performance Data Compression",
 * Terry A. Welch, IEEE Computer Vol 17, No 6 (June 1984), pp 8-19.
 *
 * Usage: compress [-dfvc] [-b bits] [file ...]
 * Inputs:
 *	-d:	    If given, decompression is done instead.
 *
 *      -c:         Write output on stdout, don't remove original.
 *
 *      -b:         Parameter limits the max number of bits/code.
 *
 *	-f:	    Forces output file to be generated, even if one already
 *		    exists, and even if no space is saved by compressing.
 *		    If -f is not used, the user will be prompted if stdin is
 *		    a tty, otherwise, the output file will not be overwritten.
 *
 *      -v:	    Write compression statistics
 *
 * 	file ...:   Files to be compressed.  If none specified, stdin
 *		    is used.
 * Outputs:
 *	file.Z:	    Compressed form of file with same mode, owner, and utimes
 * 	or stdout   (if stdin used as input)
 *
 * Assumptions:
 *	When filenames are given, replaces with the compressed version
 *	(.Z suffix) only if the file decreases in size.
 * Algorithm:
 * 	Modified Lempel-Ziv method (LZW).  Basically finds common
 * substrings and replaces them with a variable size code.  This is
 * deterministic, and can be done on the fly.  Thus, the decompression
 * procedure needs no input table, but tracks the way the table was built.
 */

    int overwrite = 0;	/* Do not overwrite unless given -f flag */
    char tempname[100];
    char **filelist, **fileptr;
    char *cp, *rindex(), *malloc();
    struct stat statbuf;
    extern onintr(), oops();


#ifdef COMPATIBLE
    nomagic = 1;	/* Original didn't have a magic number */
#endif /* COMPATIBLE */

    filelist = fileptr = (char **)(malloc(argc * sizeof(*argv)));
    *filelist = NULL;

    if((cp = rindex(argv[0], '/')) != 0) {
	cp++;
    } else {
	cp = argv[0];
    }
    if(strcmp(cp, "uncompress") == 0) {
	do!decomp = 1;
    } else if(strcmp(cp, "zcat") == 0) {
	do!decomp = 1;
	zcat!flg = 1;
    }

#ifdef BSD4!2
    /* 4.2BSD dependent - take it out if not */
    setlinebuf( stderr );
#endif /* BSD4!2 */

    /* Argument Processing
     * All flags are optional.
     * -D => debug
     * -V => print Version; debug verbose
     * -d => do!decomp
     * -v => unquiet
     * -f => force overwrite of output file
     * -n => no header: useful to uncompress old files
     * -b maxbits => maxbits.  If -b is specified, then maxbits MUST be
     *	    given also.
     * -c => cat all output to stdout
     * -C => generate output compatible with compress 2.0.
     * if a string is left, must be an input filename.
     */
    for (argc--, argv++; argc > 0; argc--, argv++) {
	if (**argv == '-') {	/* A flag argument */
	    while (*++(*argv)) {	/* Process all flags in this arg */
		switch (**argv) {
#ifdef DEBUG
		    case 'D':
			debug = 1;
			break;
		    case 'V':
			verbose = 1;
			version();
			break;
#else
		    case 'V':
			version();
			break;
#endif /* DEBUG */
		    case 'v':
			quiet = 0;
			break;
		    case 'd':
			do!decomp = 1;
			break;
		    case 'f':
		    case 'F':
			overwrite = 1;
			force = 1;
			break;
		    case 'n':
			nomagic = 1;
			break;
		    case 'C':
			block!compress = 0;
			break;
		    case 'b':
			if (!ARGVAL()) {
			    fprintf(stderr, "Missing maxbits\n");
			    Usage();
			    exit(1);
			}
			maxbits = atoi(*argv);
			goto nextarg;
		    case 'c':
			zcat!flg = 1;
			break;
		    case 'q':
			quiet = 1;
			break;
		    default:
			fprintf(stderr, "Unknown flag: '%c'; ", **argv);
			Usage();
			exit(1);
		}
	    }
	}
	else {		/* Input file name */
	    *fileptr++ = *argv;	/* Build input file list */
	    *fileptr = NULL;
	    /* process nextarg; */
	}
	nextarg: continue;
    }

    if(maxbits < INIT!BITS) maxbits = INIT!BITS;
    if (maxbits > BITS) maxbits = BITS;
    maxmaxcode = 1L << maxbits;

    if (*filelist != NULL) {
	for (fileptr = filelist; *fileptr; fileptr++) {
	    exit!stat = 0;
	    if (do!decomp != 0) {			/* DECOMPRESSION */
		/* Check for .Z suffix */
		if (strcmp(*fileptr + strlen(*fileptr) - 2, ".Z") != 0) {
		    /* No .Z: tack one on */
		    strcpy(tempname, *fileptr);
		    strcat(tempname, ".Z");
		    *fileptr = tempname;
		}
		/* Open input file */
		if ((freopen(*fileptr, "r", stdin)) == NULL) {
			perror(*fileptr); continue;
		}
		/* Check the magic number */
		if (nomagic == 0) {
		    if ((getchar() != (magic!header[0] & 0xFF))
		     || (getchar() != (magic!header[1] & 0xFF))) {
			fprintf(stderr, "%s: not in compressed format\n",
			    *fileptr);
		    continue;
		    }
		    maxbits = getchar();	/* set -b from file */
		    block!compress = maxbits & BLOCK!MASK;
		    maxbits &= BIT!MASK;
		    maxmaxcode = 1L << maxbits;
		    if(maxbits > BITS) {
			fprintf(stderr,
			"%s: compressed with %d bits, can only handle %d bits\n",
			*fileptr, maxbits, BITS);
			continue;
		    }
		}
		/* Generate output filename */
		strcpy(ofname, *fileptr);
		ofname[strlen(*fileptr) - 2] = '\0';  /* Strip off .Z */
	    } else {					/* COMPRESSION */
		if (strcmp(*fileptr + strlen(*fileptr) - 2, ".Z") == 0) {
		    	fprintf(stderr, "%s: already has .Z suffix -- no change\n",
			    *fileptr);
		    continue;
		}
		/* Open input file */
		if ((freopen(*fileptr, "r", stdin)) == NULL) {
		    perror(*fileptr); continue;
		}
		stat ( *fileptr, &statbuf );
		fsize = (long) statbuf.st!size;
		/*
		 * tune hash table size for small files -- ad hoc,
		 * but the sizes match earlier #defines, which
		 * serve as upper bounds on the number of output codes. 
		 */
		hsize = HSIZE;
		if ( fsize < (1 << 12) )
		    hsize = min ( 5003, HSIZE );
		else if ( fsize < (1 << 13) )
		    hsize = min ( 9001, HSIZE );
		else if ( fsize < (1 << 14) )
		    hsize = min ( 18013, HSIZE );
		else if ( fsize < (1 << 15) )
		    hsize = min ( 35023, HSIZE );
		else if ( fsize < 47000 )
		    hsize = min ( 50021, HSIZE );

		/* Generate output filename */
		strcpy(ofname, *fileptr);
#ifndef BSD4!2		/* Short filenames */
		if ((cp=rindex(ofname,'/')) != NULL)	cp++;
		else					cp = ofname;
		if (strlen(cp) > 12) {
		    fprintf(stderr,"%s: filename too long to tack on .Z\n",cp);
		    continue;
		}
#endif  /* BSD4!2		Long filenames allowed */
		strcat(ofname, ".Z");
	    }
	    /* Check for overwrite of existing file */
	    if (overwrite == 0 && zcat!flg == 0) {
		if (stat(ofname, &statbuf) == 0) {
		    char response[2];
		    response[0] = 'n';
		    fprintf(stderr, "%s already exists;", ofname);
		    if (foreground()) {
			fprintf(stderr, " do you wish to overwrite %s (y or n)? ",
			ofname);
			fflush(stderr);
			read(2, response, 2);
			while (response[1] != '\n') {
			    if (read(2, response+1, 1) < 0) {	/* Ack! */
				perror("stderr"); break;
			    }
			}
		    }
		    if (response[0] != 'y') {
			fprintf(stderr, "\tnot overwritten\n");
			continue;
		    }
		}
	    }
	    if(zcat!flg == 0) {		/* Open output file */
		if (freopen(ofname, "w", stdout) == NULL) {
		    perror(ofname);
		    continue;
		}
		if(!quiet)
			fprintf(stderr, "%s: ", *fileptr);
	    }

	    /* Actually do the compression/decompression */
	    if (do!decomp == 0)	compress();
#ifndef DEBUG
	    else			decompress();
#else
	    else if (debug == 0)	decompress();
	    else			printcodes();
	    if (verbose)		dump!tab();
#endif /* DEBUG */
	    if(zcat!flg == 0) {
		copystat(*fileptr, ofname);	/* Copy stats */
		if((exit!stat == 1) || (!quiet))
			putc('\n', stderr);
	    }
	}
    } else {		/* Standard input */
	if (do!decomp == 0) {
		compress();
#ifdef DEBUG
		if(verbose)		dump!tab();
#endif /* DEBUG */
		if(!quiet)
			putc('\n', stderr);
	} else {
	    /* Check the magic number */
	    if (nomagic == 0) {
		if ((getchar()!=(magic!header[0] & 0xFF))
		 || (getchar()!=(magic!header[1] & 0xFF))) {
		    fprintf(stderr, "stdin: not in compressed format\n");
		    exit(1);
		}
		maxbits = getchar();	/* set -b from file */
		block!compress = maxbits & BLOCK!MASK;
		maxbits &= BIT!MASK;
		maxmaxcode = 1L << maxbits;
		fsize = 100000;		/* assume stdin large for USERMEM */
		if(maxbits > BITS) {
			fprintf(stderr,
			"stdin: compressed with %d bits, can only handle %d bits\n",
			maxbits, BITS);
			exit(1);
		}
	    }
#ifndef DEBUG
	    decompress();
#else
	    if (debug == 0)	decompress();
	    else		printcodes();
	    if (verbose)	dump!tab();
#endif /* DEBUG */
	}
    }
    exit(exit!stat);
}    q M;