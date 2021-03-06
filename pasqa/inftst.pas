(*
 *  TEST PROGRAM FOR 'PASINF' ROUTINES - PASCAL CALLABLE
 *  ENVIRONMENTAL ENQUIRY ROUTINES.
 *)

PROGRAM INFTST;

$INCLUDE INFPAC.INC

VAR
   TESTNUM: 400000000000B..377777777777B;

PROCEDURE GETSEGINFO;

   VAR
      SEGSTUFF: SEGRECD;

   BEGIN
      SEGINFO(SEGSTUFF);

      WITH SEGSTUFF DO
      BEGIN
         WRITE(TTY,'LOWLEN:',LOWLEN);
         WRITE(TTY,'     HIGHLEN:',HIGHLEN);
         WRITE(TTY,'     RDONLY:',RDONLY);
         WRITELN(TTY,'     SHARABLE:',SHARABLE);
         BREAK(TTY)
      END

   END (* PROCEDURE GETSEGINFO *) ;

PROCEDURE GETUSRSTAT;

   VAR
      USRSTATS: USTATREC;

   BEGIN
      USRSTAT(USRSTATS);

      WITH USRSTATS DO
      BEGIN
         WRITE(TTY,'CRUS60:',CRUS60);
	writeln(tty,'   DISKRDS:',DISKRDS);
	write(tty, 'DISKWRS:',DISKWRS);

         WRITE(TTY,'     RUNTIME:',RUNTIME);
         WRITELN(TTY,'     ELAPTIME:',ELAPTIME);
         BREAK(TTY)
      END

   END  (* PROCEDURE GETUSRSTAT *);

PROCEDURE GETJOBINFO;

   VAR
      JOBSTUFF: JOBREC;

   BEGIN
      JOBINFO(JOBSTUFF);
      WITH JOBSTUFF DO
      BEGIN
         WRITE(TTY,'HSEGPPN:',HSEGPPN);
         WRITELN(TTY,'     HSEGNAM:',HSEGNAM);
         WRITE(TTY,'LSEGPPN:',LSEGPPN);
         WRITELN(TTY,'     LSEGNAM:',LSEGNAM);
         WRITE(TTY,'PROGDIR: ',PROGDIR);
         WRITELN(TTY,'     JOBNUM: ', JOBNUM);
	 writeln(tty,'Project id: [', projectid, ']');
         BREAK(TTY)
      END

   END (* PROC GETJOBINFO *);


PROCEDURE GETSYSSTAT;

   VAR
      SYSSTATS: SSTATREC;

   BEGIN
      SYSSTAT(SYSSTATS);
      WITH SYSSTATS DO
      BEGIN
         WRITE(TTY,'NLOGIN:',NLOGIN);
         WRITELN(TTY,'     MAXCOR:', MAXCOR);
         BREAK(TTY)
      END

   END (* PROC GETSYSSTAT *);


BEGIN
   OPEN(TTY);
   REWRITE(TTY);
   LOOP
      WRITE(TTY,'ENTER TEST NUMBER:');
      BREAK(TTY);
      READ(TTY,TESTNUM);
      EXIT IF TESTNUM <= 0;

      CASE TESTNUM OF

         1:
            GETSEGINFO;

         2:
            GETUSRSTAT;

         3:
            GETJOBINFO;

         4:
            GETSYSSTAT

      END (* CASE *)

   END (* LOOP *)

END.
 