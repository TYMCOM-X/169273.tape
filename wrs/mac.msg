Received: from tymix.Tymnet by B39.Tymnet; Mon, 26 Oct 87 13:21:23 PST
Received: by tymix.Tymnet (5.51/4.7) id AA04672; Mon, 26 Oct 87 13:22:42 PST
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA28425; Mon,
	26 Oct 87 13:22:36 PST
Received: by tymix.Tymnet (5.51/4.7) id AA04668; Mon, 26 Oct 87 13:22:13 PST
Received: by oliveb.ATC.OLIVETTI.COM (5.51/smail2.2/09-17-87) id AA00419; Mon,
	26 Oct 87 11:02:56 PST
Received: by oliven.ATC.OLIVETTI.COM (5.51/smail2.2/09-17-87) id AA28930; Mon,
	26 Oct 87 11:02:37 PST
Return-path: <oliveb!gnome@tymix.Tymnet> 
From: gnome@oliveb.ATC.OLIVETTI.COM (Gary) 
Date: Mon, 26 Oct 87 11:02:37 PST 
To: oliveb!tymix!wrs 
Message-id: <8710261902.AA28930@oliven.ATC.OLIVETTI.COM> 
Subject: Macpaint file format 


Macpaint: a 512-byte header (conaining patterns and printer records)
followed by 720 compressed scan lines.
here is a program that unpacks those bits:
#ifndef lint
static	char sccsid[] = "@(#)paintimp.c	1.4 12/20/84 (UT)";
#endif

/*
 * paintimp -- read macpaint document and output impress document 
 *
 * Dave Johnson, Brown University Computer Science
 *
 * (c) 1984 Brown University 
 * may be used but not sold without permission
 *
 * created ddj 5/06/84 for sun's rasterfile.h format (macfilter.c)
 * revised ddj 7/29/84 -- generate Impress format glyphs and data
 **** mods made by Ron Hitchens @ University of Texas CS
 * overhauled Rh 12/6/84 -- added @document headers
			open a pipe to lpr if stdout not redirected
			rework options, assume printer path from resolution
			added options for specifying scale and origin
			added whitespace compression scheme
			added informative messages to the user
			misc cleanup for lint
 * revised ddj 12/10/84-- added blank space and line suppression code
 * revised ddj 1/06/84 -- reworked page layout code:
			print in landscape mode if bitmap is wider than tall
			compute scale from bitmap size if not specified
			verify bitmap will fit in image space
			added verbose option to print user messages
			cleaned up code organization
 *
 * modifications made by Thomas Petsche @ Princeton University
 * revised tp 8/23/85 -- modified to allow use of smoothing filter written
 *			by Kaare Christian.  Added "-f" flag to specify
 *			smoothing.  Mods made to main, paintfilter,
 *			setup_scale, also changed default printer to
 *			imagen.   Connection to the filter is through
 * 			put_filter_image (part of filter file); the
 *			filter uses putbits for output.
 */

#include <stdio.h>

#define MACPAINT_HDRSIZE	512
#define MACRASTER_BITWIDTH	576

#define	MACPAINT_BITWIDTH	576
#define	MACPAINT_BITHEIGHT	720

#define	MACSCREEN_BITWIDTH	512
#define	MACSCREEN_BITHEIGHT	342

#define RES_240 0
#define RES_300 1
#define RES_480 2

#define DEF_RES 300	/* set what's convenient for your installation */

#define EOL -2		/* ala stdio EOF, must be passed as int */
#define BOP -3		/* ditto, for beginning of page */

typedef struct _point {
	int h;
	int v;
} point;

int filter = 0;
int smoothval = 6;
int twoup = 0;
int landscape = 0;
int screendump = 0;
int verbose = 0;
int res = DEF_RES;
int res_ind;
int scale = 0;
int width;
int height;
int glyph_width = 8;
point origin;
char *lp = NULL;

char usage[] =
"paintimp [-res] [-v] [-d] [-S] [-s n] [-x n] [-y n] [-f[sval]][-Pprinter] file ...";

char badeof[] = "Unexpected EOF. Going on to next image.\n";
char scalemsg[] = "Only scale = 2 or 4 possible with smoothing filter.\n";

main(ac, av)
char **av;
{
	char *name;

	origin.h = -1;  origin.v = -1;
	ac--; av++;
	while (ac && av[0][0] == '-' && av[0][1]) {
		switch (av[0][1]) {
		case '2':		/* kinda cheesy but it works */
		case '3':
		case '4':
			res = atoi(&av[0][1]);
			break;

		case 'S':
			screendump = 1;	/* image is macscreen sized */
			break;

		case 'd':
			twoup = 1;	/* set duplex layout */
			break;

		case 'v':
			verbose = 1;	/* set verbose mode */
			break;

		case 's':
			ac--;  av++;
			if (!ac) goto bad_usage;
			scale = atoi (av [0]);
			if( filter && scale != 4  && scale != 2 )
				{
				fprintf( stderr, scalemsg );
				exit(1);
				}
			break;

		case 'f':
			filter = 1;
			if( av[0][2] != 0 )
				smoothval = atoi(&av[0][2]);
			if( smoothval < 3 || smoothval > 16 )
				smoothval = 6;
			if( scale != 2 && scale != 4 )
				scale = 2;
			break;

		case 'x':
			ac--;  av++;
			if (!ac) goto bad_usage;
			origin.h = atoi (av [0]);
			break;

		case 'y':
			ac--;  av++;
			if (!ac) goto bad_usage;
			origin.v = atoi (av [0]);
			break;

		case 'P':
			lp = av[0];	/* propogate on to lpr filter */
			break;
			
		default:
			goto bad_usage;
		}
		ac--; av++;
	}

	switch (res)
		{
		case 240:
			res_ind = RES_240;
			break;
		case 300:
			res_ind = RES_300;
			break;
		case 480:
			res_ind = RES_480;
			break;
		default:
			fprintf(stderr, "illegal resolution %d;", res);
			fprintf(stderr, " use {240,300,480}\n");
			goto bad_usage;
		}
	if (ac == 0) {
		fprintf(stderr, "no files specified; use '-' for stdin\n");
		goto bad_usage;
	}

	write_imphdr(av[0]);	/* also starts spooling to lpr */
	get_sizes();
	setup_images();
	setup_scale();
	if (verbose) inform_user();
	while (ac) {
		name = av[0];
		if (verbose) fprintf(stderr, "Converting %s\n", name);
		paintfilter(name);
		ac--; av++;
	}
	finish_images();
	if (verbose) fprintf(stderr, "Finished\n");
	exit(0);
bad_usage:
	fprintf(stderr, "usage: %s\n", usage);
	exit(1);
}

/* print out vital statistics */
inform_user()
{
	fprintf(stderr, "Resolution=%d", res);
	if (twoup) fprintf(stderr, ", duplex (side-by-side) mode");
	fprintf(stderr, "\nScaling=%d\n", scale);
	fprintf(stderr, "Origin: x=%d, y=%d\n", origin.h, origin.v);
}

/* covert from macpaint to impress */

paintfilter(name)
char *name;
{
	register int x, y;
	FILE *fp;
	int c;

	if (name[0] == '-')
		fp = stdin;
	else
		fp = fopen(name, "r");
	if (fp == NULL) {
		perror(name);
		return;
	}

	(void) fseek(fp, (long)MACPAINT_HDRSIZE, 0);
	begin_image();				/* begin a new page */


	if( filter )
		put_filter_image(fp);

	else
		{
		for (y = 0; y < height; y++)
			{
			for (x = 0; x < MACRASTER_BITWIDTH/8; x++)
				{
				if ((c = getbits(fp)) == EOF)
					{
					fprintf(stderr, "Unexpected EOF, stopped.\n");
					y = height;
					break;
					}
				putbits(c);
				}
			putbits(EOL);
			}
		}

	end_image();				/* end the page */
	(void) fclose(fp);			/* finished with this input */
}

/* macpaint input routines */

get_sizes()
{
	if (screendump) {
		width = MACSCREEN_BITWIDTH;
		height = MACSCREEN_BITHEIGHT;
	}
	else {
		width = MACPAINT_BITWIDTH;
		height = MACPAINT_BITHEIGHT;
	}
	if (width > height)
		landscape = 1;
}

getbits(fp)
FILE *fp;
{
	/* This routine expands the packbits encoded MacPaint file,
	   delivering one byte per call */

	static int count, rep, chr;
	int c;

	if (rep) {
		rep--;
		return chr;
	}
	if (count) {
		count--;
		return getc(fp);
	}
	c = getc(fp);
	if (c & 0x80) {			/* repeated character count */
		rep = 0x100 - c;	/* byte length 2's comp + 1 */
					/* allow for this call */
		chr = getc(fp);		/* character to repeat */
		return chr;
	}
	else {
		count = c;		/* already counted this char */
		return getc(fp);
	}
}

/* impress output routines */

write_imphdr(firstname)
char *firstname;
{
	char *getenv();
	char *strcpy(), *ctime(), cimptime[26];
	char lpr [64], lpname[16];
	long time(), imptime;
	FILE *popen();

	/* if stdout is going to the terminal, call lpr to spool the
	   output, otherwise assume it's being piped somewhere else
	   or directed into a file */
	if (isatty (fileno (stdout)))
	{
		if (!lp) {
			/* assumes printer is named imagen */
			(void) sprintf (lpname, "-Pimagen");
			lp = lpname;
		}
		(void) sprintf (lpr, "lpr %s", lp);
		/* disconnect stdout from the tty and pipe it to lpr */
		(void) fclose (stdout);
		if (popen (lpr, "w") == NULL) {
			fprintf(stderr, "%s failed, giving up\n", lpr);
			exit(2);
		}
		if( verbose )fprintf(stderr, "Spooling to %s\n", lp+2);
	}

	/* imPress job command options */
	imptime = time(0l);			/* get time in internal form */
	(void) strcpy (cimptime, ctime(&imptime));/* put time in string form */
	cimptime[24] = '\0';			/* nullify \n in time string */
	printf("@document(language impress, pagereversal off");
		/* no reversal, that's a lot of stuff to buffer up */
	if (firstname[0] == '-')
		printf(", name \"MacPaint Document\"");
	else
		printf(", name \"%s\"", firstname);
	printf(", owner \"%s\"", getenv("USER"));
	printf(", spooldate \"%s\")", cimptime);
}

#define IMP_SP		128
#define IMP_CRLF	197
#define IMP_SET_FAMILY	207

#define IMP_SET_ABS_H	135
#define IMP_SET_REL_H	136
#define IMP_SET_ABS_V	137
#define IMP_SET_REL_V	138
#define IMP_BGLY	199

#define IMP_SET_HV_SYSTEM 205
#define IMP_SET_IL	208
#define IMP_SET_BOL	209
#define IMP_SET_SP	210
#define IMP_ENDPAGE	219
#define IMP_EOF		255

putbits(c)
int c;
{
	static int spaces = 0, lines = 0;

	switch (c) {
	case BOP:
		lines = 0;
		spaces = 0;
		break;

	case EOL:
		lines++;
		spaces = 0;
		break;
	
	case 0:			/* blank */
		spaces++;
		break;

	default:
		if (lines) {
			putlines(lines);
			lines = 0;
		}
		if (spaces) {
			putspaces(spaces);
			spaces = 0;
		}
		putglyph(c);
		break;
	}
}

#define FAMILY_MASK 0x80
#define MEMBER_MASK 0x7f
#define BYTE_MASK 0xff

putlines(lines)
register int lines;
{
	if (lines < 5) {
		while (lines--)
			putchar(IMP_CRLF);
	}
	else {
		putchar(IMP_CRLF);
		set_rel_v((lines - 1) * scale);
	}
}

putspaces(spaces)
register int spaces;
{
	if (spaces < 4) {
		while (spaces--)
			putchar(IMP_SP);
	}
	else {
		set_rel_h(spaces * glyph_width );
	}
}

/* this routine outputs a byte to the imagen, switching glyph
   families if necessary */

putglyph(g)
int g;
{
	static int curfam = 0;	/* family of previous glyph */
	int family;

	family = ((g & FAMILY_MASK) != 0);	/* family of g */
	if (curfam != family) {
		curfam = family;
		putchar(IMP_SET_FAMILY);
		putchar(family & BYTE_MASK);
	}
	putchar(g & MEMBER_MASK);	/* lower 7 bits of the char */
}

#define ORG_CURRENT 3
#define AXES_NOP 0
#define ORIENT_NORMAL 4
#define ORIENT_ROTATE 5
#define ROUND32(n) (((n)/32)*32)

static point image_space;
int rotated = 0;
static int imageno = 0;

/* determine orientation and available image size on output page */
/* pixel size rounded down to multiple of 32 for imagen bitmaps */

setup_images()
{
	int hv_system;
	point o;

	if (twoup != landscape)
		rotated = 1;
	if (rotated) {
		/*	 h->
		    +-----------+
		   v|\org |     |
		   ||           |
		   V|     |     |
		    +-----------+
		 */
		image_space.v = ROUND32((17 * res) / 2);/* 8.5 inches */
		image_space.h = ROUND32(11 * res);	/* 11 inches */
		if (twoup) {
			image_space.h /= 2;		/* 5.5 inches */
		}

		o.v = 0;		/* new origin in old coordinates */
		o.h = image_space.v;	/* 8.5 inches */
		set_abs_pos(o);

		hv_system = (ORG_CURRENT<<5) | (AXES_NOP<<3) | ORIENT_ROTATE;
		putchar(IMP_SET_HV_SYSTEM);
		putchar(hv_system);

		o.h = 0;	/* goto new origin in new coordinates */
		set_abs_pos(o);
	}
	else {
		/*   h->
		    +-------+
		   v|\org   |
		   ||	    |
		   V| - - - |
		    |	    |
		    |	    |
		    +-------+
		 */
		image_space.h = ROUND32((17 * res) / 2);/* 8.5 inches */
		image_space.v = ROUND32(11 * res);	/* 11 inches */
		if (twoup) {
			image_space.v /= 2;		/* 5.5 inches */
		}

		o.h = 0;
		o.v = 0;
		set_abs_pos(o);

		hv_system = (ORG_CURRENT<<5) | (AXES_NOP<<3) | ORIENT_NORMAL;
		putchar(IMP_SET_HV_SYSTEM);
		putchar(hv_system);
	}
}

/* select best scale if not given, perform scale-dependent initializations */
/* each MacPaint dot becomes an scale-square rectangle of imagen dots */

setup_scale()
	{
	int tmp;

	/* start large and cut it down if it won't fit */
	/* low level routines are limited to scale of 8 */
	if (scale <= 0) scale = 8;		/* let's be reasonable... */
	if (scale > 8) scale = 8;

	tmp = image_space.h / width;
	if (tmp < scale)
		scale = tmp;
	tmp = image_space.v / height;
	if (tmp < scale)
		scale = tmp;
	if (scale <= 0) {
		fprintf(stderr, "bitmap too large for page\n");
		exit(3);
	}
	/* bitmap will now fit in image_space */

	if( filter )
		{
		if( scale == 4 )
			write_filter_glyphs();
		else
			write_filter_glyphs2();
		glyph_width = 3 * scale;
		}
	else
		{
		write_glyphs();
		glyph_width = scale * 8;
		}

	putchar(IMP_SET_SP);
	putshort(glyph_width);
	putchar(IMP_SET_IL);
	putshort(scale);

				/* center bitmap */
				/* unless valid origin specified by user */
	if (origin.h < 0 || (origin.h + width * scale) > image_space.h)
		origin.h = (image_space.h - width * scale) / 2;
	if (origin.v < 0 || (origin.v + height * scale) > image_space.v)
		origin.v = (image_space.v - height * scale) / 2;
	}

begin_image()
{
	point o;

	imageno++;		/* count pictures */
	o.h = origin.h;
	o.v = origin.v;
	if (twoup && !(imageno & 1)) {
		/* shift second image down or over by half page */
		if (landscape)
			o.v += image_space.v;
		else
			o.h += image_space.h;
	}
	putbits(BOP);		/* prime putbits to reset spaces, lines */
	putchar(IMP_SET_BOL);
	putshort(o.h);
	set_abs_pos(o);
}

end_image()
{
	if (twoup && (imageno & 1))
		return;
	putchar(IMP_ENDPAGE);
}

finish_images()
{
	if (twoup && (imageno & 1))
		putchar(IMP_ENDPAGE);
	putchar(IMP_EOF);
}

set_abs_pos(pt)
point pt;
{
	putchar(IMP_SET_ABS_H);
	putshort(pt.h);
	putchar(IMP_SET_ABS_V);
	putshort(pt.v);
}

set_rel_v(incr)
int incr;
{
	putchar(IMP_SET_REL_V);
	putshort(incr);
}

set_rel_h(incr)
int incr;
{
	putchar(IMP_SET_REL_H);
	putshort(incr);
}

write_glyphs()		/* send the dot pattern glyphs to the imagen */
{
	register int glyph;

	if( verbose ) fprintf (stderr, "Loading dot patterns\n");
	for (glyph = 1; glyph <= 255; glyph++) {
		write_glyph(glyph);
	}
}

char bits[8];		/* code assumes scale <= 8 */

write_glyph(g)
register int g;
{
	register int i, j;
	register int b, xpos;
	int bit;

	if (rotated) {
		putchar(IMP_BGLY);
		g |= (1 << 14);		/* rotate 90 degrees clockwise */
		putshort(g);		/* family & member to define */
		putshort(scale*8);	/* advance width in h diretion */
		putshort(scale);	/* x bit width of glyph */
		putshort(0);		/* left offset */
		putshort(scale*8);	/* y bit height of glyph */
		putshort(0);		/* top offset */
		for (b = 0x80; b; b >>= 1) {
			bits[0] = 0;
			bit = g & b;
			for (i = 0; i < scale; i++)
				if (bit) setbit(i);
			for (i = 0; i < scale; i++)
				putchar(bits[0] & BYTE_MASK);
		}
	}
	else {
		putchar(IMP_BGLY);
		putshort(g);		/* family & member to define */
		putshort(scale*8);	/* advance width in h direction */
		putshort(scale*8);	/* x bit width of glyph */
		putshort(0);		/* left offset */
		putshort(scale);	/* y bit height of glyph */
		putshort(0);		/* top offset */
		xpos = 0;
		for (i = 0; i < scale; i++)
			bits[i] = 0;
		for (b = 0x80; b; b >>= 1) {
			bit = g & b;
			for (i = 0; i < scale; i++, xpos++)
				if (bit) setbit(xpos);
		}
		for (j = 0; j < scale; j++) {
			for (i = 0; i < scale; i++)
				putchar(bits[i] & BYTE_MASK);
		}
	}
}

setbit(x)
int x;
{
	int bitno;
	char *bitp;
	
	bitno = 7 - (x % 8);
	bitp = &bits[x / 8];
	*bitp |= (1 << bitno);
}

putshort(s)
int s;
{
	putchar((s >> 8) & BYTE_MASK);
	putchar(s & BYTE_MASK);
}

Screenfiles:
a data fork consisting of just 342 scan lines of 512 bits. No compression.

SuperPaint:
unknown

Pict files (use Save As Pict in MacDraw)
512 bytes of header, followed by a very complex encoded representation of
a list of quickdraw calls.  Easiest way to decode it is to use a Mac, I've
written a Mac program that translates black and white PICTs to lisp procedure
calls. (The complete documentation on PICTs is scattered across 3 reference
works and is about 30 pages long.)

MacDraw files:
I can dig this up if you really need it.


Could you do me the following favor?
I need:
1.) a list of what zip codes are in what congressional districts
2.) an explanation of what U.S. Representatives run in what year
i.e. do they get only a 2 year term? do they all run at once?
3.) do committee re-assignments happen only after elections?

1 is of course, the one I really need

Received: from tymix.Tymnet by B39.Tymnet; Mon, 26 Oct 87 20:07:14 PST
Received: by tymix.Tymnet (5.51/4.7) id AA05465; Mon, 26 Oct 87 20:11:07 PST
Received: from tymix.Tymnet by hobbes.Tymnet.com (3.2/SMI-3.2) id AA08313; Mon,
	26 Oct 87 20:11:02 PST
Received: by tymix.Tymnet (5.51/4.7) id AA05458; Mon, 26 Oct 87 20:10:59 PST
Received: by oliveb.ATC.OLIVETTI.COM (5.51/smail2.2/09-17-87) id AA05444; Mon,
	26 Oct 87 16:49:47 PST
Received: by oliven.ATC.OLIVETTI.COM (5.51/smail2.2/09-17-87) id AA01098; Mon,
	26 Oct 87 16:49:39 PST
Return-path: <oliveb!gnome@tymix.Tymnet> 
From: gnome@oliveb.ATC.OLIVETTI.COM (Gary) 
Date: Mon, 26 Oct 87 16:49:39 PST 
To: oliveb!tymix!wrs 
Message-id: <8710270049.AA01098@oliven.ATC.OLIVETTI.COM> 
Subject: More Mac/Sun stuff from Dave... 

From oster@soe.berkeley.edu Mon Oct 26 16:36:24 1987
From: oster@soe.berkeley.edu (David Phillip Oster)
Message-Id: <8710270007.AA00775@dewey.soe.berkeley.edu>
Subject: MacGraphics on Sun
Status: R

Turns out that a Mac bitmap is almost identical to a Sun PixRect
bitmap: the origin is in the upper left corner, bytes increase across
the scan line, 0 is white, 1 is black, bitMaps/pixRect are always
padded out to a number of pixels that is a multiple of 16. To turn on
the very first pixel of a sca line, the very first byte should be 0x80
(all this is the same between pixrect and QuickDraw.) The padding
issue is handled slightly differently between the two: 

On the Mac, the fundamental data structure is the bitmap, it has a
data array, a rowbytes field that says how many bytes per scan,
and a bounding rectangle that gives the bitmap its own coordinate
system, and specifies how much of the data array is actually being
used. (Remember that the data is padded to a multiple of 16 pixels,
and not all uses of bitmaps fit exactly.) The total size of the data
array is (bound.bottom-bound.top)*rowBytes.

The sun dispenses with the rowbytes and the rectangle, and just has an
x width and a y width. The coordinate system always has its origin at
(0,0), and the rowbytes is derivable from the x-width. The total size of the
data array is y_size* 16 * ((x_size + 15) integerdivide 16).

--- David Phillip Oster            --A Sun 3/60 makes a poor Macintosh II.
Arpa: oster@dewey.soe.berkeley.edu --A Macintosh II makes a poor Sun 3/60.
Uucp: {uwvax,decvax,ihnp4}!ucbvax!oster%dewey.soe.berkeley.edu



 6