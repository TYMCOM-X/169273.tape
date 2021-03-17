!! title COMM9 - Kill Buffer related stuff ;
! *************************************************************************
*									  *
*				   COMM9				  *
*                  Commands dealing with the Kill Buffer                  *
*									  *
***************************************************************************


	   This file is required as a source!file in COMAND.SAI.


**************************************************************************;
!! title Rtn KB.Set.To.Write ;
! 
;

simple procedure KB.Set.To.Write (boolean Append);
begin "KB.Set.To.Write"
  if (Append) then
  begin
    if (KB.State neq KB.OUTPUT) then
    begin
      if (KB.Size <= KB.BUF.CHARS) then
        KB.Left.In.Core _ KB.Size
      else
      begin
        useto(KB.Chan, KB.Size div CHARS.PER.BLOCK + 1);
        KB.Left.In.Core _ KB.Size mod CHARS.PER.BLOCK;
        arryin(KB.Chan, KB.Buffer[0], ceiling(KB.Left.In.Core, 5));
      end;
    end;
  end
  else
  begin
    KB.Size         _ 0;
    KB.Left.In.Core _ 0;
    useto(KB.Chan, 1);
  end;

  KB.BP    _ Develop.Ptr(location(KB.Buffer[0]), KB.Left.In.Core, true);
  KB.State _ KB.OUTPUT;
end "KB.Set.To.Write";
!! title Rtn KB.Set.To.Read ;
! 
;

simple procedure KB.Set.To.Read;
begin "KB.Set.To.Read"
  if (KB.Size > KB.BUF.CHARS) then
  begin
    if (KB.State = KB.OUTPUT and KB.Left.In.Core > 0) then
    begin
      arryout(KB.Chan, KB.Buffer[0], ceiling(KB.Left.In.Core, 5));
      KB.Left.In.Core _ 0;
    end;

    useti(KB.Chan, 1);
  end
  else
    KB.Left.In.Core _ KB.Size;

  KB.BP      _ Develop.Ptr(location(KB.Buffer[0]), 0, true);
  KB.Unread  _ KB.Size;
  KB.State   _ KB.INPUT;
end "KB.Set.To.Read";
!! title Rtn KB.GetC ;
! 
;

simple integer procedure KB.GetC;
begin "KB.GetC"
  integer
    Ch;

  if (KB.State neq KB.INPUT) then
    KB.Set.To.Read;

  if (KB.Left.In.Core <= 0 and KB.Size > KB.BUF.CHARS) then
  begin
    arrclr(KB.Buffer);
    arryin(KB.Chan, KB.Buffer[0], KB.BUF.WORDS);
    KB.Left.In.Core _ KB.Unread min KB.BUF.CHARS;
    KB.BP           _ Develop.Ptr(location(KB.Buffer[0]), 0, true);
  end;

  if (KB.Unread > 0) then
  begin
    Ch _ ildb(KB.BP);
    decr(KB.Left.In.Core);
    decr(KB.Unread);
  end
  else
    Ch _ -1;

  return(Ch);
end "KB.GetC";
!! title Rtn KB.PutC ;
! 
;

simple procedure KB.PutC (integer Ch);
begin "KB.PutC"
  if (KB.State neq KB.OUTPUT) then
    KB.Set.To.Write(G!AppendKillP);

  if (KB.Left.In.Core >= KB.BUF.CHARS) then
  begin
    arryout(KB.Chan, KB.Buffer[0], ceiling(KB.Left.In.Core, 5));
    arrclr(KB.Buffer);
    KB.BP           _ Develop.Ptr(location(KB.Buffer[0]), 0, true);
    KB.Left.In.Core _ 0;
  end;

  idpb(ch, KB.BP);
  incr(KB.Left.In.Core);
  incr(KB.Size);
end "KB.PutC";
!! title Rtn KB.Init ;
!  Initialize the Kill Buffer by establishing the file to be used. This
!  routine should be called by the instantiation dependent initialization
!  code in the command module.
;

simple procedure KB.Init;
begin "KB.Init"
  KB.Name _ cvs(1000 + calli(0, calli!pjob))[2 for 3] & "PKB.TMP";
  open(KB.Chan _ getchan, "DSK", '17, 0, 0, 0, 0, KB.Eof _ -1);

  if (KB.Eof) then
  begin
    print(crlf, "Error opening channel for Kill Buffer.", xwdstr(KB.Eof), crlf);
    return;
  end;

  lookup(KB.Chan, KB.Name, KB.Eof _ -1);

  if (KB.Eof and rh(KB.Eof) neq 0) then
  begin
    print(crlf, "Error in lookup for Kill Buffer.", xwdstr(KB.Eof), crlf);
    release(KB.Chan);
    return;
  end;

  enter(KB.Chan, KB.Name, KB.Eof _ -1);

  if (KB.Eof) then
  begin
    print(crlf, "Error in enter for Kill Buffer.", xwdstr(KB.Eof), crlf);
    release(KB.Chan);
    return;
  end;

  KB.Set.To.Write(false);
end "KB.Init";
!! title Rtn C!DelToEOL ;
!  Delete to the end of the Line, copying text into the kill buffer.
;

forward simple procedure C!DelEOL;
forward simple procedure C!Wipe;
forward simple procedure C!Copy;

simple procedure C!DelToEOL;
begin
  own integer
    Temp1;
  define
    Eol = {Eol};

  if (B!Lock or B!EndP) then
    PuntCommand;

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  KB.Set.To.Write(G!AppendKillP or
      C!LastAddr = location(C!DelToEOL) or
      C!LastAddr = location(C!Wipe) or
      C!LastAddr = location(C!Copy) or
      C!LastAddr = location(C!DelEOL));

  clear(G!AppendKillP);
  C!ArgV _ C!ArgV max 1;

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  if (not B!EndP) then
  begin
    while (C!ArgV) do
    begin "argv times"
      if (not B!EndP) then
      begin "delete"
	Temp1 _ B!GetC;

	if (Temp1 = FF) then
	  done "argv times";

	if (AtEOL) then
	begin "EOL"
          KB.PutC(CR);
          B!Delete(FORWARDS);
          KB.PutC(LF);
          B!Delete(FORWARDS);
	end "EOL"
	else
	begin "char"
	  while (not B!EndP and not AtEOL) do
	  begin
	    Temp1 _ B!GetC;
	    KB.PutC(Temp1);
            B!Delete(FORWARDS);
	  end;
	end "char";
      end "delete"
      else
	done "argv times";

      decr(C!ArgV);
    end "argv times";
  end;

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  W!Msg(null);
  W!FixS;
  C!ArgV _ 0;
end;
!! title Rtn C!DelEOL ;
!  Delete to and including the end of the line, copying text into the kill
!  buffer.
;

simple procedure C!DelEOL;
begin
  own integer
    Temp1;
  define
    Eol = {Eol};

  if (B!Lock or B!EndP) then
    PuntCommand;

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  KB.Set.To.Write(G!AppendKillP or
      C!LastAddr = location(C!DelToEOL) or
      C!LastAddr = location(C!Wipe) or
      C!LastAddr = location(C!Copy) or
      C!LastAddr = location(C!DelEOL));

  clear(G!AppendKillP);
  C!ArgV _ C!ArgV max 1;

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  if (not B!EndP) then
  begin
    while (C!ArgV) do
    begin "argv times"
      while (not B!EndP) do
      begin "delete"
	Temp1 _ B!GetC;

	if (AtEOL and Temp1 neq FF) then
	begin "EOL"
          KB.PutC(CR);
          B!Delete(FORWARDS);
          KB.PutC(LF);
          B!Delete(FORWARDS);
	  done "delete";
	end "EOL"
	else
	begin
	  KB.PutC(Temp1);
          B!Delete(FORWARDS);

          if (Temp1 = FF) then
            done "delete";
	end;
      end "delete";

      decr(C!ArgV);
    end "argv times";
  end;

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  W!Msg(null);
  W!FixS;
  C!ArgV _ 0;
end;
!! title Rtn C!Copy ;
!  "Copy" text from the point to the mark.
;

simple procedure C!Copy;
begin
  integer
    Mark,
    HowMany,
    SavePt;

  Mark   _ C!ArgV;
  C!ArgV _ 0;

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  if ((Mark < 0) or (Mark > 9)) then
  begin
    W!Msg("Bad Mark Number - " & cvs(Mark));
    T!Bell;
    return;
  end;

  if (B!GetM(Mark) < 0) then
  begin
    W!Msg("Mark " & cvs(Mark) & " not set");
    T!Bell;
    return;
  end;

  if (B!GetM(Mark) = B!GetP) then
  begin
    W!Msg("The Point and Mark " & cvs(Mark) & " are in the same place");
    T!Bell;
    return;
  end;

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  KB.Set.To.Write(G!AppendKillP or
      C!LastAddr = location(C!DelToEOL) or
      C!LastAddr = location(C!Wipe) or
      C!LastAddr = location(C!Copy) or
      C!LastAddr = location(C!DelEOL));

  clear(G!AppendKillP);

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  W!Msg("Copy  [Working]");
  W!Disp(true);

  HowMany _ abs(B!GetP - B!GetM(Mark));
  SavePt  _ B!GetP;			! save the point ;

  if (B!GetM(Mark) < B!GetP) then	! set it to mark if needed ;
    B!SetP(B!GetM(Mark));

  while (HowMany > 0) do
  begin
    KB.PutC(B!GetC);
    B!Move(FORWARDS);
    decr(HowMany);
  end;

  B!SetP(SavePt);

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  W!Msg("Copy  [Complete]");
end;
!! title Rtn C!Wipe ;
!  "Wipe" text from between the point and the mark.  The text will be
!   removed from the lower end of the region forward.
;

simple procedure C!Wipe;
begin "C!Wipe"
  integer
    Mark,
    HowMany;


  if (B!Lock) then
    PuntCommand;

  Mark   _ C!ArgV;
  C!ArgV _ 0;

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  if ((Mark < 0) or (Mark > 9)) then
  begin
    W!Msg("Bad Mark Number - " & cvs(Mark));
    T!Bell;
    return;
  end;

  if (B!GetM(Mark) < 0) then
  begin
    W!Msg("Mark " & cvs(Mark) & " not set");
    T!Bell;
    return;
  end;

  if (B!GetM(Mark) = B!GetP) then
  begin
    W!Msg("The Point and Mark " & cvs(Mark) & " are in the same place");
    T!Bell;
    return;
  end;

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  KB.Set.To.Write(G!AppendKillP or
      C!LastAddr = location(C!DelToEOL) or
      C!LastAddr = location(C!Wipe) or
      C!LastAddr = location(C!Copy) or
      C!LastAddr = location(C!DelEOL));

  clear(G!AppendKillP);

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  W!Msg("Wipe  [Working]");
  W!Disp(true);

  HowMany _ abs(B!GetP - B!GetM(Mark));

  if (B!GetP > B!GetM(Mark)) then
    B!SetP(B!GetM(Mark));

  while (HowMany > 0) do
  begin
    KB.PutC(B!GetC);
    B!Delete(FORWARDS);
    decr(HowMany);
  end;

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

  W!Msg("Wipe  [Complete]");
end "C!Wipe";
!! title Rtn C!Yank ;
! "Yank" text out of the kill buffer and insert it at the mark.  The kill
!  buffer is not affected.
;

procedure C!Yank;
begin "C!Yank"
  integer
    SavePt;

  SavePt _ B!GetP;
  W!Msg("Yank  [Working]");
  W!Disp(true);

  if (not C!ArgV) then
    C!ArgV _ 1;

  while (C!ArgV) do
  begin
    integer
      Ch;

    KB.Set.To.Read;

    while ((Ch _ KB.GetC) >= 0) do
      B!Insert(Ch);

    decr(C!ArgV);
  end;

  if (not C!SPMRegion) then
    B!SetP(SavePt);

  W!Msg("Yank  [Complete]");
end "C!Yank";


! ************************** End of COMM9.REQ ****************************;
    