Received: from B39.Tymnet by D35.Tymnet; Mon, 19 Dec 88 11:40:17 PST
Received: from tymix.Tymnet by B39.Tymnet; Mon, 19 Dec 88 10:41:04 PST
Received: by tymix.Tymnet (5.51/4.7) id AA11233; Mon, 19 Dec 88 10:33:55 PST
Received: by antares.Tymnet.com (3.2/SMI-3.2) id AA19791; Sun, 18 Dec 88
	23:23:22 PST
Return-path: <cbaltru@antares> 
From: cbaltru@antares (carl baltrunas) 
Date: Sun, 18 Dec 88 23:23:22 PST 
To: carl@d35 
Message-id: <8812190723.AA19791@antares.Tymnet.com> 
Subject: source 

Path: antares!tymix!oliveb!ames!xanth!nic.MR.NET!hal!ncoast!allbery
From: doug@letni.UUCP (Doug Davis at letni.UUCP)
Newsgroups: comp.sources.misc
Subject: v05i082: rsed -- simple in-core editor
Message-ID: <00001977-881112081327-346@letni.UUCP>
Date: 14 Dec 88 03:01:09 GMT
Sender: allbery@ncoast.UUCP
Reply-To: doug@letni.UUCP (Doug Davis at letni.UUCP)
Lines: 1477
Approved: allbery@ncoast.UUCP

Posting-number: Volume 5, Issue 82
Submitted-by: "Doug Davis at letni.UUCP" <doug@letni.UUCP>
Archive-name: rsed

The attached shar file is source of a simple user-friendly(?) text file
editor that I was commissioned to write for a bunch of computer illiterate's
to edit mail messages.  The editor makes use of an in-memory linked list
for storing each line, this gives it the ability to edit any size text
file that a machine has user process space for.  I'm sending it to
you to post, mostly because it shows a good simple example of using a linked
list, as well as in the hopes that someone will take it further along 
the evolutionary scale, and perhaps turn it into a real editor.

As usual I will gladly accept all of patches for suggested changes
or porting problems. Flames however will cheerfully be ignored.

Doug Davis
Lawnet
1030 Pleasent Valley Lane
Arlington Texas 76015
(817)-467-3740

{ killer!texbell, sys1.tandy.com, lawnet, motown!sys1 } letni!doug


#! /bin/sh
# This is a shell archive.  Remove anything before this line, then unpack
# it by saving it into a file and typing "sh file".  To overwrite existing
# files, type "sh file -c".  You can also feed this as standard input via
# unshar, or by typing "sh <file", e.g..  If this archive is complete, you
# will see the following message at the end:
#		"End of archive 1 (of 1)."
# Contents:  README rsedhelp Makefile defs.h externs.h command.c edit.c
#   find.c globals.c input.c main.c mem.c multi.c stuff.c
# Wrapped by doug@letni on Mon Dec 12 02:07:29 1988
PATH=/bin:/usr/bin:/usr/local/bin:/usr/ucb ; export PATH
if test -f 'README' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'README'\"
else
echo shar: Extracting \"'README'\" \(3961 characters\)
sed "s/^X//" >'README' <<'END_OF_FILE'
X
Real Simple Ed 
X
Rsed is a real simple editor program designed for people who are calling
into a machine to be able to edit files on a variaty of differen't 
terminals and/or computers WITHOUT haveing to learn an editor.  Rsed
is very easy to use and tries to be as "user friendly" as possable.
X
Defines:
WSEDHELP          Where the help file resieds.
MAXREPLACE        malloc size for the replacement string
BEGINDOTS         size of file to warrent nifty-userfriendly dots to show
X                  that we are doing something.
X
Options:
X    -a ##	load file and append after line ##
X    -w 		write the file out when you quit the editor (silent)
X    -D		debug (used for fiddling with the linked list)
X
X
Porting:
X    rsed usese curses just for turning off echo and setting crmod, 
X    if you don't want to fool with that, or don't have curses you will
X    have to fix up your own terminal handler. The following are the
X    function calls to worry about.
X    	savetty() save the current modes of the terminal for later recall
X    	initscr() sets up curses.
X    	crmode()  puts the terminal into One Charicter At A Time mode.
X    	resetty() restores the previously saved tty information.
X	
X    rsed also uses getopt() most later version unix's have this, if
X    yours does not, a public domain version of it is available thru
X    comp.sources.unix.
X    
X    Othewise rsed has been known to compile without changes on 4.1 BSD (vax),
X    4.2 BSD (vax) 4.3 BSD (vax), Ultrix (vax) sco (%d86) micro, Tandy 6000,
X    Masscomp, NCR tower, ATT 3b2 and a logic processes Mpulse. So it ought
X    to compile on yours.  If your compiler can't handle macros you will
X    need to work on the RANGE marco in multi.c.  Otherwise
X    please let me know of any problems and may the source be with you %s.
X
X
Author:
X    Douglas L. Davis
X    (Lawnet, Texas R&D office)
X    1030 Pleasent Valley Lane
X    Arlington Texas  76015
X    (817)-467-3740
X    { sys1 || texbell || lawnet }!letni!doug
X    
X
XFunction call        Description
main            Main routine, parses args, calls edit.
add             adds a char array below line number specified.
append          asks user for lines to add().
boom            debug sigint routine.
change          asks user for a replacement line.
clear_stuff     frees the entire linked list.
command         does the command prompting
edit            if nessasary creates the file, then loads it.
find            returns a line with a pre-specified string
getline         returns a LINE pointer for the specified line number.
getlineno       gets a line number from the tty.
getstring       gets a string from the tty.
global_replace  replaces a specified string with another full file.
go_fish         asks user for a search string then calls find with it.
go_global       asks user for a search string and a replacement string.
help            concatinate the rsedhelp file.
insert          asks user for a line.
loadem          reads a file into the editor buffer.
makenum         convert a string to a numeric entry.
printline       prints a line out of the editor.
replace         replace the text of a specified line with a user buf
reset_find      clears the global search stuff.
saveem          saves the editor buf to a file.
setfind         sets up the global search stuff.
setline         set the current line to a specified line number.
srch_n_replace  do a search / replace within a (LINE *) struct
srchrep         user interface for srch_n_replace.
strappend       like strcat() but returns then end of it's string.
strindex        returns a pointer to a string found in another string.
subtract        removes a spacified line from the buffer.
yesno           asks for a yes or no answer from the user.
multi_print     ask for starting line / ending line then for()'s printline()
multi_delete    ask for starting line / ending line then for()'s subtract()
multi_change    ask for starting line / ending line then for()'s change()
END_OF_FILE
if test 3961 -ne `wc -c <'README'`; then
    echo shar: \"'README'\" unpacked with wrong size!
fi
# end of 'README'
fi
if test -f 'rsedhelp' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'rsedhelp'\"
else
echo shar: Extracting \"'rsedhelp'\" \(1086 characters\)
sed "s/^X//" >'rsedhelp' <<'END_OF_FILE'
X
X  Keys             Action
X ------          ---------
X [0-9]  .........change the current line.
X   $    .........go to the bottom of the file.
X   ^    .........go to the top of the file.
X+ or ;  .........advance to the next line.
X   -    .........backup to the previous line.
X   a    .........append text after the current line.
X   c    .........change (i.e. replace) the current line.
X   d    .........delete (i.e. remove) the current line.
f or /  .........find and advance to a line with a specified string.
X   g    .........global replacement (entire file) of a string with another.
X   i    .........insert text before the current line.
X   n    .........advance to the next occurence of a string (re: f option)
X   p    .........print (i.e. display) the current line.
X   q    .........quit the editor.
X   s    .........search and replace a string on the current line.
X   w    .........write out all changes.
X   P    .........asks for and then prints a range of lines.
X   ?    .........display this file.
X
Note: input(s) can be ended or aborted by entering a '.' on a line by itself.
END_OF_FILE
if test 1086 -ne `wc -c <'rsedhelp'`; then
    echo shar: \"'rsedhelp'\" unpacked with wrong size!
fi
# end of 'rsedhelp'
fi
if test -f 'Makefile' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'Makefile'\"
else
echo shar: Extracting \"'Makefile'\" \(960 characters\)
sed "s/^X//" >'Makefile' <<'END_OF_FILE'
X# in an '86 environment this better be compiled in large model
SHELL=/bin/sh
X
X# defines for curses that needs termcap BSD,  xenix,  etc....
X# CURSNAME  = -lcurses -ltermlib
X# CURSNAME	= -lcurses -ltermcap
X# defines for sysV curses needs cbreak() function call.
X# if yours has crmode() instead, you will have to hack defs.h
X# CURSNAME	= -lcurses 
X
X# if your machine has strchr
X# DEFINES=-DSTRCHR=strchr
X# if your machine has index / rindex
X# DEFINES=-DSTRCHR=index
X
X 
X# for 286 uncomment the line below
X# CFLAGS	=	-O -AL $(DEFINES)
X# LDFLAGS =	-AL -i
X# for most other systems
X# CFLAGS	=	-O $(DEFINES)
X# LDFLAGS	=	-n
OBJECTS =	globals.o main.o mem.o command.o edit.o input.o stuff.o \
X		find.o multi.o
CC=/bin/cc
X
all: rsed
X
rsed:	${OBJECTS}
X		$(CC) ${LDFLAGS} ${OBJECTS} -o rsed $(CURSNAME)
X
tags: *.h *.c
X	ctags *.h *.c
X
shar:
X	shar -n 1 -e 1 -t 'Now read the README file and edit the Makefile for your system' \
X		README rsedhelp Makefile *.h *.c > rsed.shar
END_OF_FILE
if test 960 -ne `wc -c <'Makefile'`; then
    echo shar: \"'Makefile'\" unpacked with wrong size!
fi
# end of 'Makefile'
fi
if test -f 'defs.h' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'defs.h'\"
else
echo shar: Extracting \"'defs.h'\" \(984 characters\)
sed "s/^X//" >'defs.h' <<'END_OF_FILE'
X/*
X * This code copyright 1988 by Doug Davis (doug@letni.lawnet.com) 
X *  You are free to modify, hack, fold, spindle, or mutlate this code in
X *  any maner provided you give credit where credit is due and don't pretend
X *  you wrote it.
X *  If you do my lawyers (and I have a lot of lawyers) will teach you a lesson
X *  in copyright law that you will never ever forget.
X */
X#include <sys/types.h>
X#include <signal.h>
X#include <fcntl.h>
X#include <ctype.h>
X#include <sys/stat.h>
X#include <stdio.h>
X#include <errno.h>
X#include "curses.h"
X
X#define WSEDHELP	"/usr/lib/rsedhelp";
X#define	MAXREPLACE	4096 /* biggest tmp line size for global replacement */
X#define	BEGINDOTS	24L * 1024L /* size of file to warrent dots to show
X					that we are doing something */
X
typedef struct line {
X	struct line *previous;
X	struct line *next;
X	char text[1]; /* has to be end */
X} LINE;
X
X#ifndef MSDOS
X	#ifdef CBREAK
X	#define One_Char()	crmode();
X	#else
X	#define One_Char()	cbreak();
X	#endif
X#endif MSDOS
END_OF_FILE
if test 984 -ne `wc -c <'defs.h'`; then
    echo shar: \"'defs.h'\" unpacked with wrong size!
fi
# end of 'defs.h'
fi
if test -f 'externs.h' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'externs.h'\"
else
echo shar: Extracting \"'externs.h'\" \(1116 characters\)
sed "s/^X//" >'externs.h' <<'END_OF_FILE'
X
X/*
X * This code copyright 1988 by Doug Davis (doug@letni.lawnet.com) 
X *  You are free to modify, hack, fold, spindle, or mutlate this code in
X *  any maner provided you give credit where credit is due and don't pretend
X *  you wrote it.
X *  If you do my lawyers (and I have a lot of lawyers) will teach you a lesson
X *  in copyright law that you will never ever forget.
X */
extern	FILE	*EditFile;
extern	LINE	*current;
extern	LINE	*find_work;
extern	LINE	*getline();
extern	LINE	*l_end;
extern	LINE	*l_start;
extern	LINE	*setline();
extern	char	*Bottom;
extern	char	*Eof;
extern	char	*HelpName;
extern	char	*Progname;
extern	char	*STRCHR();
extern	char	*Top;
extern	char	*find_string;
extern	char	*getstring();
extern	char	*malloc();
extern	char	*strappend();
extern	char	*strindex();
extern	char	Dots;
extern	char	WriteOnQuit;
extern	char	buf[BUFSIZ];
extern	char	yesno();
extern	int	Debug;
extern	int	errno;
extern	int	getopt();
extern	long	CurrentLine;
extern	long	NumberLines;
extern	long	atol();
extern	long	find();
extern	long	find_line;
extern	long	getlineno();
extern	long	loadem();
extern	long	makenum();
END_OF_FILE
if test 1116 -ne `wc -c <'externs.h'`; then
    echo shar: \"'externs.h'\" unpacked with wrong size!
fi
# end of 'externs.h'
fi
if test -f 'command.c' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'command.c'\"
else
echo shar: Extracting \"'command.c'\" \(4136 characters\)
sed "s/^X//" >'command.c' <<'END_OF_FILE'
X
X/*
X * This code copyright 1988 by Doug Davis (doug@letni.lawnet.com) 
X *  You are free to modify, hack, fold, spindle, or mutlate this code in
X *  any maner provided you give credit where credit is due and don't pretend
X *  you wrote it.
X *  If you do my lawyers (and I have a lot of lawyers) will teach you a lesson
X *  in copyright law that you will never ever forget.
X */
X#include "defs.h"
X#include "externs.h"
X
command(file, sline)
char *file;
char *sline;
X{
X	char c;
X	long start;
X	char changed='N';
X	if (*sline != '\0') {
X		start=makenum(sline);
X		fputs("appending ", stdout);
X		if (start >= NumberLines) 
X			fputs("to the bottom", stdout);
X		else 
X			printf("below line %ld", start);
X		puts(", enter text now:");
X		append(start <= NumberLines ? start : NumberLines);
X		start=NumberLines;
X	} else
X		start = 1L;
X	
X	if (NumberLines > 0L)
X		setline(start);
X	for (;;) {
X		if (NumberLines < 1L || CurrentLine < 1L) {
X			puts("Buffer empty, begin entering new text.");
X			append(0L);
X			continue;
X		}
X		if (Debug) {
X			printf("address             l_start: 0x%lx\n",
X				l_start);
X			printf("address             current: 0x%lx\n",
X				current);
X			printf("address               l_end: 0x%lx\n",
X				l_end);
X			printf("address l_start -> previous: 0x%lx\n", 
X				l_start->previous);
X			printf("address       l_end -> next: 0x%lx\n",
X				l_end->next);
X		}
X		printf("Line %ld>", CurrentLine);
X#ifdef MSDOS
X		c=getch();
X#else 
X		c=getchar();
X#endif MSDOS
X		switch(c) {
X			case '0':
X			case '1':
X			case '2':
X			case '3':
X			case '4':
X			case '5':
X			case '6':
X			case '7':
X			case '8':
X			case '9':
X				fputs("Enter new line :", stdout);
X				getstring(buf, 10, c);
X				if ((start=makenum(buf)) > 0L && 
X						start <= NumberLines) {
X					if (setline(start) == (LINE *) NULL) 
X						puts("Sorry..");
X				}
X				break;
X			case '+':
X			case ';':
X				putchar('\n');
X				if (CurrentLine < NumberLines) {
X					CurrentLine++;
X					printline(CurrentLine);
X				} else
X					puts(Bottom);
X				break;
X			case '-':
X				putchar('\n');
X				if (CurrentLine > 1) {
X					CurrentLine--;
X					printline(CurrentLine);
X				} else 
X					puts(Top);
X				break;
X			case '$':
X				putchar('\n');
X				CurrentLine = NumberLines;
X				setline(CurrentLine);
X				puts(Bottom);
X				break;
X			case '^':
X				putchar('\n');
X				CurrentLine = 1L;
X				setline(CurrentLine);
X				puts(Top);
X				break;
X			case 'C':
X				puts("Change lines.");
X				multi_change();
X				changed = 'Y';
X				break;
X			case 'D':
X				puts("Delete lines.");
X				multi_delete();
X				changed = 'Y';
X				break;
X			case 'P':
X				puts("Print.");
X				multi_print();
X				break;
X			case 'a':
X				printf("Appending after line # %ld\n", CurrentLine);
X				append(CurrentLine);
X				changed = 'Y';
X				break;
X			case 'c':
X				printf("Change line # %ld\n", CurrentLine);
X				change(CurrentLine);
X				changed = 'Y';
X				break;
X			case 'd':
X				printf("Deleted line # %ld\n", CurrentLine);
X				subtract(CurrentLine);
X				changed = 'Y';
X				break;
X			case 'f':
X			case '/':
X				puts("Find string.");
X				go_fish(CurrentLine);
X				break;
X			case 'g':
X				puts("Global.");
X				go_global(CurrentLine);
X				changed = 'Y';
X				break;
X			case 'i':
X				printf("Inserting before line %ld.\n", CurrentLine);
X				insert(CurrentLine);
X				changed = 'Y';
X				break;
X			case 'n': /* next occurence */
X				puts("Next.");
X				find('v');
X				break;
X			case 'p':
X				puts("print.");
X				printline(CurrentLine);
X				break;
X			case 'q':
X				puts("quit.");
X				if (WriteOnQuit == 'Y') {
X					if(saveem(file) == OK) {
X						return(OK);
X					} else {
X						fprintf(stderr, "%s: Write on quit set, but save failed.\n", Progname);
X						WriteOnQuit='N';
X						break;
X					}
X					
X				}
X				if (changed == 'Y') {
X					fputs("File changed, are you sure (y/n) ? ", stdout);
X					c=yesno();
X					if (c == 'N') 
X						break;
X				}
X				return(OK);
X				break;
X			case 's':
X				puts("search/replace");
X				srchrep(CurrentLine);
X				changed = 'Y';
X				break;
X			case 'w':
X				puts("Saveing.");
X				saveem(file);
X				changed = 'N';
X				break;
X			case '?':
X				puts("Help.");
X				help();
X				break;
X			default:
X				puts("? - for help");
X				break;
X				
X		}
X	}
X}
END_OF_FILE
if test 4136 -ne `wc -c <'command.c'`; then
    echo shar: \"'command.c'\" unpacked with wrong size!
fi
# end of 'command.c'
fi
if test -f 'edit.c' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'edit.c'\"
else
echo shar: Extracting \"'edit.c'\" \(2656 characters\)
sed "s/^X//" >'edit.c' <<'END_OF_FILE'
X
X/*
X * This code copyright 1988 by Doug Davis (doug@letni.lawnet.com) 
X *  You are free to modify, hack, fold, spindle, or mutlate this code in
X *  any maner provided you give credit where credit is due and don't pretend
X *  you wrote it.
X *  If you do my lawyers (and I have a lot of lawyers) will teach you a lesson
X *  in copyright law that you will never ever forget.
X */
X#include "defs.h"
X#include "externs.h"
X
append(number)
long number;
X{
X	puts("Enter a s period on a blank line to return to the editor");
X	for (;;number++) {
X		getstring(buf, BUFSIZ, '\0');
X		if (*buf == '.' && strlen(buf) == 1) 
X			return(OK);
X		add(buf, number);
X	}
X}
X		
change(number)
long number;
X{
X	printf("Currently line %ld looks like:\n", number);
X	printline(number);
X	printf("Type the new line %ld\n", number);
X	getstring(buf, BUFSIZ, '\0');
X	if (*buf == '.' && strlen(buf) == 1)  {
X		puts("Aborted.");
X		return(!OK);
X	}
X	return(replace(number, buf));
X}
srchrep(number)
long number;
X{
X	char old[81], new[81], c='\0';
X	printf("Line %ld looks like :\n", number);
X	printline(number);
X	fputs("     Search string: ", stdout);
X	if (getstring(old, 81, '\0') == NULL)
X		return(!OK);
X	fputs("Replacement string: ", stdout);
X	if (getstring(new, 81, '\0') == NULL)
X		return(!OK);
X	while (c != 'N' && c != 'Y') {
X		fputs("Global replacement? ", stdout);
X		c=getchar();
X		if (islower(c)) 
X			c=toupper(c);
X		if (c == '\n') 
X			c='N';
X		switch(c) {
X			case 'N':
X				puts("No.");
X			break;
X			case 'Y':
X				puts("Yes.");
X			break;
X			case 'Q':
X				return(!OK);
X			break;
X			default:
X				fputs("\007<Y>es, <N>o or <Q>uit please.\n", stdout);
X			break;
X		}
X	}
X	srch_n_replace(number, old, new, c);
X	printline(number);
X	return(OK);
X}
go_fish(number)
long number;
X{
X	char sstring[81];
X	fputs("String to search for : ", stdout);
X	if(getstring(sstring, 80, '\0') == NULL)
X		return(!OK);
X	if (setfind(number, sstring) == OK) 
X		return(find('v') > 0L ? OK : !OK);
X	return(!OK);
X}
go_global(number)
long number;
X{
X	char old[81], new[81];
X	fputs("     Search string: ", stdout);
X	if (getstring(old, 80, '\0') == NULL)
X		return(!OK);
X	fputs("Replacement string: ", stdout);
X	if (getstring(new, 80, '\0') == NULL)
X		return(!OK);
X	return(global_replace(number, old, new));
X}
global_replace(number, old, new)
long number;
char *old, *new;
X{
X	long line, count=0L;
X	if (setfind(number, old) == OK) {
X		while ((line=find('n')) > 0L)  {
X			if (srch_n_replace(line, old, new, 'Y') == OK) 
X				count++;
X		}
X		printf("Replaced \"%s\" with \"%s\" %ld times.\n",
X			old, new, count);
X
X		return(count > 0L ? OK : !OK);
X	}
X	return(!OK);
X}
insert(number)
long number;
X{
X	number--;
X	return(append(number));
X}
END_OF_FILE
if test 2656 -ne `wc -c <'edit.c'`; then
    echo shar: \"'edit.c'\" unpacked with wrong size!
fi
# end of 'edit.c'
fi
if test -f 'find.c' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'find.c'\"
else
echo shar: Extracting \"'find.c'\" \(1349 characters\)
sed "s/^X//" >'find.c' <<'END_OF_FILE'
X
X/*
X * This code copyright 1988 by Doug Davis (doug@letni.lawnet.com) 
X *  You are free to modify, hack, fold, spindle, or mutlate this code in
X *  any maner provided you give credit where credit is due and don't pretend
X *  you wrote it.
X *  If you do my lawyers (and I have a lot of lawyers) will teach you a lesson
X *  in copyright law that you will never ever forget.
X */
X#include "defs.h"
X#include "externs.h"
X
X#define __FILE__	"find.c"
X
long
find(c)
char c;
X{
X	if (find_work == (LINE *) NULL || find_line >= NumberLines) {
X		puts(Eof);
X		reset_find();
X		return(-1L);
X	}
X	while (find_work != (LINE *) NULL) {
X		if (strindex(find_work->text, find_string) != NULL) {
X			CurrentLine=find_line;
X			if (c == 'v') printline(CurrentLine);
X			find_work = find_work -> next;
X			find_line++;
X			return(CurrentLine);
X		}
X		find_work = find_work -> next;
X		find_line++;
X	}
X	puts(Eof);
X	return(-1L);
X}
reset_find()
X{
X	find_work = l_start;
X	find_line = 1L;
X}
setfind(number, string)
long number;
char *string;
X{
X	if (find_string != NULL)
X		free(find_string);
X	find_string = malloc(strlen(string) + 1);
X	if (find_string == NULL) {
X		fprintf(stderr, "%s: malloc(%d) failed\n", __FILE__,
X				strlen(string));
X		return(!OK);
X	}
X	strcpy(find_string, string);
X	find_line = number;
X	find_work = setline(find_line);
X	return(find_work != (LINE *) NULL ? OK : !OK);
X}
END_OF_FILE
if test 1349 -ne `wc -c <'find.c'`; then
    echo shar: \"'find.c'\" unpacked with wrong size!
fi
# end of 'find.c'
fi
if test -f 'globals.c' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'globals.c'\"
else
echo shar: Extracting \"'globals.c'\" \(834 characters\)
sed "s/^X//" >'globals.c' <<'END_OF_FILE'
X
X/*
X * This code copyright 1988 by Doug Davis (doug@letni.lawnet.com) 
X *  You are free to modify, hack, fold, spindle, or mutlate this code in
X *  any maner provided you give credit where credit is due and don't pretend
X *  you wrote it.
X *  If you do my lawyers (and I have a lot of lawyers) will teach you a lesson
X *  in copyright law that you will never ever forget.
X */
X#include "defs.h"
X
XFILE	*EditFile;
LINE	*current	= (LINE *) NULL;
LINE	*find_work	= (LINE *) NULL;
LINE	*l_end		= (LINE *) NULL;
LINE	*l_start	= (LINE *) NULL; 
char	*Bottom="Bottom of file.";
char	*Eof="Search ending, bottom of file.";
char	*Progname;
char	*Top="Top of file.";
char	*find_string = NULL;
char	Dots		= 'N';
char	WriteOnQuit	= 'N';
char	buf[BUFSIZ];
int	Debug;
long	CurrentLine;
long	NumberLines;
long	find_line = 1L;
char	*HelpName=WSEDHELP;
END_OF_FILE
if test 834 -ne `wc -c <'globals.c'`; then
    echo shar: \"'globals.c'\" unpacked with wrong size!
fi
# end of 'globals.c'
fi
if test -f 'input.c' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'input.c'\"
else
echo shar: Extracting \"'input.c'\" \(1511 characters\)
sed "s/^X//" >'input.c' <<'END_OF_FILE'
X
X/*
X * This code copyright 1988 by Doug Davis (doug@letni.lawnet.com) 
X *  You are free to modify, hack, fold, spindle, or mutlate this code in
X *  any maner provided you give credit where credit is due and don't pretend
X *  you wrote it.
X *  If you do my lawyers (and I have a lot of lawyers) will teach you a lesson
X *  in copyright law that you will never ever forget.
X */
X#include "defs.h"
X#include "externs.h"
X
long
getlnum()
X{
X	getstring(buf, 10, '\0');
X	return(makenum(buf));
X}
X
char
yesno()
X{
X	char c;
X	for (;;) {
X#ifdef MSDOS
X		c=getch();
X#else MSDOS
X		c=getchar();
X#endif MSDOS
X		if (islower(c))
X			c=toupper(c);
X		switch (c) {
X			case 'Y':
X				puts("Yes.");
X				return(c);
X			break;
X			case 'N':
X				puts("No.");
X				return(c);
X			break;
X			default:
X				puts("<Y>es or <N>o please.");
X			break;
X		}
X	}
X}
X/*
X *	This routine gets a string..
X *
X */
char *
getstring(str, len, first)
char	*str;
int	len;
char	first;
X{
X	char *p, c;
X	p = str;
X	if (first != '\0')  {
X		*(p++) = first;
X		putchar(first);
X	}		
X	for ( ;; ) {
X#ifdef MSDOS
X		c = getch();
X#else
X		c = getchar();
X#endif MSDOS
X		switch(c) {
X			case '\b': {
X				if (p > str) {
X					*p='\0';
X					fputs("\010 \010", stdout);
X					*(p--)='\0';
X				} else {
X					putchar('\007');
X					}
X				}
X				break;
X			case '\n':
X			case '\r': {
X				*p='\0';
X				putchar('\n');
X				return(*str != '\0' ? str : NULL);
X				} break;
X			default: {
X				if (p < str+len) {
X					*(p++) = c;
X					putchar(c);
X				} else { 
X					putchar('\007');
X				}
X			} break;
X		}
X	}
X}
END_OF_FILE
if test 1511 -ne `wc -c <'input.c'`; then
    echo shar: \"'input.c'\" unpacked with wrong size!
fi
# end of 'input.c'
fi
if test -f 'main.c' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'main.c'\"
else
echo shar: Extracting \"'main.c'\" \(2419 characters\)
sed "s/^X//" >'main.c' <<'END_OF_FILE'
X
X/*
X * This code copyright 1988 by Doug Davis (doug@letni.lawnet.com) 
X *  You are free to modify, hack, fold, spindle, or mutlate this code in
X *  any maner provided you give credit where credit is due and don't pretend
X *  you wrote it.
X *  If you do my lawyers (and I have a lot of lawyers) will teach you a lesson
X *  in copyright law that you will never ever forget.
X */
X#include "defs.h"
X#include "externs.h"
X
main(argc, argv)
int argc;
char **argv;
X{
X	int c;
X	char start_line[80];
X	extern char *optarg;
X	extern int optind, opterr;
X	opterr=1;
X	Progname = *argv;
X#ifndef MSDOS
X	savetty();
X	initscr();
X#endif MSDOS
X	signal(SIGINT, SIG_IGN);
X	while ((c=getopt(argc, argv, "Dwa:")) != EOF) {
X		switch (c) {
X		case 'D': Debug = 1;			break;
X		case 'a': strcpy(start_line,optarg);	break;
X		case 'w': WriteOnQuit='Y';		break;
X		}
X	}
X	if (Debug) printf("extra args %d\n", optind);
X	if (argc < 2) {
X		fprintf(stderr, "%s: usage: %s [options] file [ file file]\n",
X				Progname, Progname);
X		exit(-1);
X	}
X	for (;optind < argc; optind++) {
X		errno=0;
X		edit(argv[optind], start_line);
X		*start_line = '\0';
X	}
X	resetty();
X	exit(errno);
X}
X
edit(file, sline)
char *file;
char *sline;
X{
X	struct stat stbuff, *s;
X	char c;
X	int new=!OK;
X	s=(struct stat *) &stbuff;
X	clear_stuff();
X	noecho();
X	One_Char();
X	if ((EditFile=fopen(file, "r")) == (FILE *) NULL) {
X		if (errno == ENOENT) {
X			printf("File %s doesn't exist, create (y/n) ? ",
X				file);
X			c=yesno();
X			switch(c) {
X				case 'Y':
X					close(creat(file, 0666));
X					if ((EditFile=fopen(file, "r")) == (FILE *) NULL) {
X						/* give up... */
X						fprintf(stderr, "%s: could not create %s\n", Progname, file);
X						return(!OK);
X					}
X					new=OK;
X					break;
X				case 'N':
X				default:
X					return(OK);
X					break;	
X			}
X		} else {
X		fprintf(stderr, "%s: Could not read %s",
X			Progname, file);
X			perror("");
X			return(!OK);
X		}
X	}
X	if (fstat(fileno(EditFile), s) < 0) {
X		fprintf(stderr, "%s: fstat(%d) failed",
X			Progname, fileno(EditFile));
X			perror("");
X			return(!OK);
X		}
X	if (s->st_size > BEGINDOTS)
X		Dots = 'Y';
X	if (new != OK && s->st_size > 0L)  {
X		if (loadem(EditFile, 0L) > 0L) {
X			printf("File: \"%s\"   lines=%ld, charicters=%ld\n",
X				file, NumberLines, s->st_size);
X			fclose(EditFile);
X			command(file, sline);
X		}
X	} else {
X		printf("File: \"%s\" is new, begin entering new text\n", file);
X		append(0L);
X		command(file, "\0");
X	}
X	return(OK);
X}
END_OF_FILE
if test 2419 -ne `wc -c <'main.c'`; then
    echo shar: \"'main.c'\" unpacked with wrong size!
fi
# end of 'main.c'
fi
if test -f 'mem.c' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'mem.c'\"
else
echo shar: Extracting \"'mem.c'\" \(5810 characters\)
sed "s/^X//" >'mem.c' <<'END_OF_FILE'
X	
X/*
X * This code copyright 1988 by Doug Davis (doug@letni.lawnet.com) 
X *  You are free to modify, hack, fold, spindle, or mutlate this code in
X *  any maner provided you give credit where credit is due and don't pretend
X *  you wrote it.
X *  If you do my lawyers (and I have a lot of lawyers) will teach you a lesson
X *  in copyright law that you will never ever forget.
X */
X#include "defs.h"
X#include "externs.h"
X
X#define __FILE__	"mem.c"
X
clear_stuff()
X{
X	LINE *temp, *next;
X	if (l_start != (LINE *) NULL) {
X		/* something inside, clear 'em out */
X		for (temp = l_end; temp != l_start ;) {
X			next = temp->previous;
X			free(temp);
X			temp = next;
X		}
X	}
X	l_start = (LINE *) malloc(sizeof(LINE) + 2);
X	l_start -> previous 	= (LINE *) NULL;
X	l_start -> next		= (LINE *) NULL;
X	l_end 			= l_start;
X	current			= l_start;
X	CurrentLine		= 0L;
X	NumberLines		= 0L;
X}
long
loadem(inputfile, place)
XFILE *inputfile;
long place;
X{
X	while ((fgets(buf, BUFSIZ, inputfile)) != NULL) {
X		if(add(buf, place++) != OK) {
X			fprintf(stderr, "%s:%d loadem() add() failure\n", 
X				__FILE__, __LINE__);
X			return(!OK);
X		}
X		if (Dots == 'Y' || Debug) {
X			putchar('.');
X			fflush(stdout);
X		}
X	}
X	putchar('\n');
X	return(NumberLines);
X}
LINE *
setline(number)
long number;
X{
X	LINE *tmp;
X	tmp=getline(number);
X	if (tmp != (LINE *) NULL)  {
X		CurrentLine = number;
X		current = tmp;
X	}
X
X	return(tmp);
X}
LINE *
getline(number)
long number;
X{
X	LINE *inc;
X	long i;
X	if (number > NumberLines) {
X		printf("%s-%s: getline(%ld) > NumberLines == %ld\n",
X			Progname, __FILE__, number, NumberLines);
X		return((LINE *)NULL);
X	}
X	if (number == 0L) 
X		return(l_start);
X	for (i=1L, inc=l_start; i<number ; i++, inc=inc->next);
X	return(inc);
X}
printline(number)
long number;
X{
X	LINE *tmp;
X	if ((tmp=setline(number)) != (LINE *) NULL) {
X		fputs(tmp->text, stdout);
X		fflush(stdout);
X		return(OK);
X	}
X	fprintf(stderr, "%s: printline(%ld) Bad line number\n", __FILE__, number);
X	fflush(stderr);
X	return(!OK);
X}
X
X
X
add(buf, number)
char *buf;
long number;
X{
X	LINE *new, *temp;
X	if (Debug) printf("add(%ld)\n", number);
X	new = (LINE *) malloc(sizeof(LINE) + strlen(buf));
X	if (number > 0L) {
X		temp=getline(number);
X		if (temp == (LINE *) NULL)  {
X			fprintf(stderr, "%s: add(%ld) bad line number\n",
X				__FILE__, number);
X			return(!OK);
X		}
X		new -> previous = temp;
X		if (temp->next != (LINE *) NULL)  {
X			temp->next->previous = new;
X			new -> next = temp->next;
X		} else {
X			new->next = (LINE *) NULL;
X			l_end = new;
X		}
X		temp -> next = new;
X	} else {
X		new -> previous = (LINE *) NULL;
X		if (NumberLines < 1L) {
X			new -> next = (LINE *) NULL;
X			l_end = new;
X		} else 
X			new -> next = l_end;
X
X		/* Now, blow away anything nasty, still hanging around */
X		if (l_start != (LINE *) NULL)
X			free(l_start); 
X
X		l_start = new;
X		current = new;
X	}
X	/* make sure line has a newline on the end.. */
X	if (buf[strlen(buf)-1] != '\n')
X		strcat(buf, "\n");
X	strcpy(new->text, buf);
X	NumberLines++;
X	current=new;
X	CurrentLine=NumberLines;
X	return(OK);
X}
subtract(number)
long number;
X{
X	LINE *tmp;
X	tmp = getline(number);
X	if (tmp == (LINE *)NULL) {
X		fprintf(stderr, "%s: subtract(%ld) bad line number\n", 
X			__FILE__, number);
X		return(!OK);
X	}
X	if (l_end == tmp) {
X		l_end = l_end -> previous;
X	}
X	if (tmp -> previous != (LINE *) NULL) 
X		tmp -> previous -> next = tmp -> next; 
X	 else {
X		if (tmp -> next != (LINE *) NULL) {
X			tmp -> next -> previous = (LINE *) NULL;
X			l_start = tmp -> next;
X		}
X	}
X
X	if (tmp -> next != (LINE *) NULL)
X		tmp -> next -> previous  = tmp -> previous;
X	 else {
X		if (tmp -> previous != (LINE *) NULL) {
X			tmp -> previous -> next = (LINE *) NULL;
X			l_end = tmp -> previous;
X		}
X	}
X	if (NumberLines-- > 1L)
X		free(tmp);
X	current=setline(number < NumberLines ? number : NumberLines);
X	return(current != (LINE *) NULL ? OK : !OK);
X}
X	
replace(number, buf)
char *buf;
long number;
X{
X	LINE *old, *new;
X	old=getline(number);
X	if (old == (LINE *) NULL)  {
X		fprintf(stderr, "%s: replace(%ld) bad line number\n", __FILE__, number);
X		return(!OK);
X	}
X	new = (LINE *) malloc(sizeof(LINE) + strlen(buf));
X	new -> previous		= old -> previous;
X	new -> next		= old -> next;
X	if (new -> previous != (LINE *) NULL)  
X		new -> previous -> next = new;
X	else 
X		l_start = new;
X
X	if (new -> next != (LINE *) NULL)  
X		new -> next -> previous = new;
X	else 
X		l_end = new;
X
X	if (STRCHR(buf, '\n') == NULL)
X		strcat(buf, "\n");
X	strcpy(new->text, buf);
X	free(old);
X	current=setline(number);
X	return(current != (LINE *) NULL ? OK : !OK );
X}
srch_n_replace(number, old_str, new_str, glob)
long number;
char *old_str, *new_str;
char glob;
X{
X	LINE *work;
X	char *p, *b, *buff;
X	int len_old, cnt=0;
X	work = getline(number);
X	if (work == (LINE *) NULL) {
X		fprintf(stderr, "%s: srch_n_replace(%ld) bad line number\n",
X			__FILE__, number);
X		return(!OK);
X	}
X	len_old = strlen(old_str);
X	buff = malloc(MAXREPLACE);
X	if (buff == NULL)
X		return(!OK);
X	p=work->text;
X	b=buff;
X	do {
X		cnt++;
X		if (*p == *old_str)  {
X			if (!strncmp(p, old_str, len_old)) {
X				p += (len_old-1); /* advance past string */
X				b = strappend(b, new_str);
X				if (glob == 'N')  {
X					p++;
X					strcat(b, p);
X					break;
X				}
X			} else
X			*(b++) = *p;
X		} else 
X			*(b++) = *p;
X	} while ((*(p++) != '\0') && cnt < MAXREPLACE);
X	replace(number, buff);
X	free(buff);
X	return(OK);
X}
saveem(file)
char *file;
X{
X	LINE *work;
X	int out;
X	long count=0L;
X	out = open(file, O_WRONLY | O_CREAT | O_TRUNC, 0666);
X	if (out < 0) {
X		fprintf(stderr, "%s: Could not open \"%s\" for writing",
X			Progname, file);
X		perror("");
X		return(!OK);
X	}
X	work = l_start;
X	while (work != (LINE *) NULL && count < NumberLines)  {
X		count++;
X		write(out, work->text, strlen(work->text));
X		work = work-> next;
X		if (WriteOnQuit != 'Y') putchar('.');
X	}
X	close(out);
X	putchar('\n');
X	return(OK);
X}
END_OF_FILE
if test 5810 -ne `wc -c <'mem.c'`; then
    echo shar: \"'mem.c'\" unpacked with wrong size!
fi
# end of 'mem.c'
fi
if test -f 'multi.c' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'multi.c'\"
else
echo shar: Extracting \"'multi.c'\" \(1593 characters\)
sed "s/^X//" >'multi.c' <<'END_OF_FILE'
X
X/*
X * This code copyright 1988 by Doug Davis (doug@letni.lawnet.com) 
X *  You are free to modify, hack, fold, spindle, or mutlate this code in
X *  any maner provided you give credit where credit is due and don't pretend
X *  you wrote it.
X *  If you do my lawyers (and I have a lot of lawyers) will teach you a lesson
X *  in copyright law that you will never ever forget.
X */
X#include "defs.h"
X#include "externs.h"
X
X#define RANGE(x)	(x > 0 && x <= NumberLines)
static char *Start="Starting line number ";
static char *End=  "  Ending line number ";
multi_change()
X{
X	long start, end, count;
X	char *string="for changes :";
X	fputs(Start, stdout);
X	fputs(string, stdout);
X	start=getlnum();
X	if (!RANGE(start)) 
X		return(!OK);
X	fputs(End, stdout);
X	fputs(string, stdout);
X	end=getlnum();
X	if (!RANGE(end))
X		return(!OK);
X	for(count = start ; count <= end ; count++) 
X		change(count);
X	return(OK);
X}
multi_delete()
X{
X	long start, end, count;
X	char *string="to delete : ";
X	fputs(Start, stdout);
X	fputs(string, stdout);
X	start=getlnum();
X	if (!RANGE(start)) 
X		return(!OK);
X	fputs(End, stdout);
X	fputs(string, stdout);
X	end=getlnum();
X	if (!RANGE(end))
X		return(!OK);
X	for(count = end ; count >= start ; count--) 
X		subtract(count);
X	return(OK);
X}
multi_print()
X{
X	long start, end, count;
X	char *string="to be printed : ";
X	fputs(Start, stdout);
X	fputs(string, stdout);
X	start=getlnum();
X	if (!RANGE(start)) 
X		return(!OK);
X	fputs(End, stdout);
X	fputs(string, stdout);
X	end=getlnum();
X	if (!RANGE(end))
X		return(!OK);
X	for(count = start ; count <= end ; count++) 
X		printline(count);
X	return(OK);
X}
END_OF_FILE
if test 1593 -ne `wc -c <'multi.c'`; then
    echo shar: \"'multi.c'\" unpacked with wrong size!
fi
# end of 'multi.c'
fi
if test -f 'stuff.c' -a "${1}" != "-c" ; then 
  echo shar: Will not clobber existing file \"'stuff.c'\"
else
echo shar: Extracting \"'stuff.c'\" \(1773 characters\)
sed "s/^X//" >'stuff.c' <<'END_OF_FILE'
X/*
X * This code copyright 1988 by Doug Davis (doug@letni.lawnet.com) 
X *  You are free to modify, hack, fold, spindle, or mutlate this code in
X *  any maner provided you give credit where credit is due and don't pretend
X *  you wrote it.
X *  If you do my lawyers (and I have a lot of lawyers) will teach you a lesson
X *  in copyright law that you will never ever forget.
X */
X#include "defs.h"    /* just for the #define of a NULL */
X#include "externs.h" /* for the strings things. */
X
X/* append  s2 unto s1, returning a pointer to the new "end" of s1 */
char *
strappend(s1, s2)
char *s1, *s2;
X{
X	register char *p, *r;
X	p = s1;
X	r = s2;
X	do {
X		*(p++) = *r;
X	} while ( *(r++) != '\0');
X	p--;
X	return(p);
X}
X/* return a pointer to the first occurence of s2 in s1 otherwise NULL */
char *
strindex(s1, s2)
char *s1, *s2;
X{
X	register char *p, *s;
X	register int len;
X	p=s1;
X	len = strlen(s2);
X	/* quick test to blow away short lines */
X	if (len > strlen(s1))
X		return(NULL);
X	while ((s=STRCHR(p, *s2)) != NULL) {
X		if (!strncmp(s, s2, len))  {
X			return(s);
X		}
X		p= (++s);
X	}
X	return(NULL);
X}
long
makenum(buf)
char *buf;
X{
X	switch (*buf) {
X		case '\0':
X			return(-1L);
X		break;
X		case '$':
X			return(NumberLines);
X		break;
X		case '^':
X			return(1L);
X		break;
X		case '0':
X		case '1':
X		case '2':
X		case '3':
X		case '4':
X		case '5':
X		case '6':
X		case '7':
X		case '8':
X		case '9':
X			return(atol(buf) > 1L ? atol(buf) : 1L);
X		break;
X		default:
X			return(-1L);
X	}
X	return(-1L);
X}
help()
X{
X	register int input, amount;
X	if ((input = open (HelpName, O_RDONLY)) < 0) {
X		fprintf(stderr, "%s: Could not open file \"%s\"",
X		    Progname, HelpName);
X		perror("");
X		return(!OK);
X	}
X	while((amount=read(input, buf, BUFSIZ)) > 0)
X		write(1, buf, amount);
X	close(input);
X	return(OK);
X}
END_OF_FILE
if test 1773 -ne `wc -c <'stuff.c'`; then
    echo shar: \"'stuff.c'\" unpacked with wrong size!
fi
# end of 'stuff.c'
fi
echo shar: End of archive 1 \(of 1\).
cp /dev/null ark1isdone
MISSING=""
for I in 1 ; do
    if test ! -f ark${I}isdone ; then
	MISSING="${MISSING} ${I}"
    fi
done
if test "${MISSING}" = "" ; then
    echo You have the archive.
    echo "Now read the README file and edit the Makefile for your system"
    rm -f ark[1-9]isdone
else
    echo You still need to unpack the following archives:
    echo "        " ${MISSING}
fi
##  End of shell archive.
exit 0
 AL