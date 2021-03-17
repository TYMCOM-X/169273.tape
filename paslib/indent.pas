$LENGTH 44
$TITLE INDENT Main program for INDENT
$HEADER INDENT.HDR
program INDENT;

$INCLUDE COROUT.INC


external procedure GETCMD;		(* for setting up a run *)
external procedure INDINP;
external procedure INDFOR;
external procedure INDSPL;
external procedure INDOUT;

public var
  READER,
  SPLITR,
  FORMAT,
  WRITER: ENVIRONMENT;


external var
  OUTFILE,				(* the output and input files *)
  INFILE: text;
$PAGE WRAPPER for creating and invoking coroutines

procedure WRAPPER;

(* WRAPPER creates the coroutine environments for the four INDENT
   coroutines. By doing so, the return of WRAPPER destroys the
   instantiations of the coroutines, so they can start clean on
   the next input file to be processed. *)

  begin
  FORMAT := create(INDFOR, 300);
  SPLITR := create(INDSPL, 300);
  READER := create(INDINP, 400);
  WRITER := create(INDOUT, 300);

  call(READER)			(* here we go... *)
  end (* procedure WRAPPER *);
$PAGE INDENT the mainline


(* The main command loop calls GETCMD, which will not return until
   input and output files have been set up. The loop then calls
   WRAPPER to initiate processing of the source file by the coroutines,
   and then closes the input and output files. This loop also restores
   the default state of options before parsing the next command line. *)

  begin					(* mainline *)
  open(tty); rewrite(ttyoutput);

    loop
      GETCMD;

      WRAPPER;				(* start the processing *)
      close (OUTFILE);
      close (INFILE);
    end
  end (* mainline and program *).
