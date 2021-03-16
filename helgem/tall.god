begin "TALL"

require "(HELGEM)AUX" load!module;
require "(SAILIB)sail.def" source!file;

define  nhosts = 50;
boolean File,
	det,
	BlankLine;
integer RecFilChan,
	Eof,
	BK,
	CmdFileChan,
	DSKIN,
	foo,
	h,
	PortNumber,
	lasthost;
string  UserName,
	line,
	commands;

external integer procedure auxcre(string username);
external procedure auxzap(integer port);
external procedure auxout(integer port; string text);
external integer procedure auxinc(integer port);
integer array hosts[0:nhosts-1];

string procedure outstr(string s);
begin
	integer c;
if File then
  out (RecFilChan,S);
while (c _ lop(s)) neq null do
  auxclv(-1,c,3);
end;

string procedure prompt(string msg);
begin
	string S;
	integer SKIP;
while true do
  begin
    if DSKIN then
      begin
        S _ input(CmdFileChan,1);
        SKIP _ !SKIP!;
	if EOF then
	  begin
	    DSKIN _ false;
	    continue;
          end;
      end
    else 
      begin
        outstr (MSG);
        S _ inchwl;
        SKIP _ !SKIP!;
        if !SKIP! = #esc  then
          begin "password"
            auxclr(-1,memory['200],'64);
	    S _ S & #esc  & inchwl;
	    SKIP _ !SKIP!;
	    outstr(crlf);
	    auxclr(-1,memory['000],'64);
	  end "password";
      end;
    if equ(S[1 to 2],"@@") then
      begin
        !SKIP! _ SKIP;
        return(S[2 to inf]);
      end;
    if S="@" then
      begin
	release(CmdFileChan);
	open(CmdFileChan_getchan,"DSK",1,1,0, 256,!SKIP!,EOF);
	lookup(CmdFileChan,S[2 to inf],EOF_0);
	if EOF then
	  lookup(CmdFileChan,S[2 to inf]&".MHX",EOF_0);
	if EOF then
	  begin
	    outstr("?file "&S[2 to INF]&" not found"&crlf);
	    DSKIN _ false;
	    continue;
	  end;
	DSKIN _ true;
	continue;
      end;
    !SKIP! _ SKIP; 
    return(S);
  end;
end;

procedure gethosts;
begin
	integer h,
		OneHost,
                HostFilChan,
		eof,
		HostFilLnBrk;
	string  line, 
		savline;
while true do
  begin
    outstr("hosts: ");
    Line_inchwl;
    if line[1 for 1] = "?" then 
      begin
	outstr("enter either: "&crlf);
        outstr("- host numbers, separated by commas"&crlf);
        outstr("- 10s (to get all PDP10s)"&crlf);
      end
    else
      if equ(line,"10s") or equ(line,"10S") then 
        begin
  	  setbreak(HostFilLnBrk_GetBreak,#lf,#cr,"I");
	  open(HostFilChan_getchan,"DSK",0,2,0,500,0,Eof);
	  lookup(HostFilChan,"(helgem)TENS.lst",Eof); 
          if Eof then
            usererr(0,0,"Can't read (helgem)Tens.lst","X")
          else
            begin ! Get hosts from file;
              Line_input(HostFilChan,HostFilLnBrk);
              for h _ 0 step 1 until nhosts-1 do
                begin
                  lasthost _ h-1;
                  if (savline_line) = null then
                    done;
                  OneHost _ intscan(line,foo);
                  if foo = -1 then
                    begin
                      print(savline&"? -- "
                            &"type ? for help"&crlf);
                    end;
                  hosts[h] _ OneHost;
                end;
	      return;
	    end
        end
      else 
        begin
        for h _ 0 step 1 until nhosts-1 do
          begin
            lasthost _ h-1;
            if (savline_line) = null then
              done;
            OneHost _ intscan(line,foo);
            if foo = -1 then
               begin
                 print(savline&"? -- "
                       &"type ? for help"&crlf);
               end;
             hosts[h] _ OneHost;
           end;
             return;
	end;
	if lasthost = -1 then 
          begin
           outstr("no hosts? -- type ? for help"&crlf);
          end;
  end;
end;

string procedure doesc(string s); 
begin
	string  res;
	integer c;
res _ null;
while c _ lop(s) do
  if c = "$" then
    begin
      if "$" = (c _ lop(s)) then
        res_res&"$"
      else
        res_res&(c land '37);
    end
  else
    res_res&c;
return(res);
end;

procedure getcommands;
begin
	string cl,fs;
	external integer !skip!;
commands _ null;
fs _ "commands (end with ctrl-D):"&13&10;
while true do
  begin
    commands _ commands & (cl _ doesc(prompt(fs)));
    fs _ null;
    case !SKIP! of 
      begin
        [4] begin
	      if length(cl) then commands_commands & #cr;
              done;
            end;
	[#lf] commands_commands&#cr;
	else commands_commands& !SKIP!
      end;
  end;
end;

procedure getfilestuff;
begin
ttyup(true);
setbreak(1,#lf&#eot,#cr,"SINK");
File _ (prompt("record? ") = "Y");
if File then
  begin
    open(RecFilChan_getchan,"DSK",0,0,4, 0,0,Eof);
    enter(RecFilChan,prompt("output file: "),Eof);
    if Eof then
      usererr(Eof,2,"Enter failure: ","X");
    det _ (prompt("detach? ") = "Y");
  end
else
  det _ false;
end;



Procedure GoTohosts;
begin 
	integer ch;
for h _ 0 step 1 until lasthost do 
  begin "hostloop"
    outstr(crlf&cvs(hosts[h])&":");
    PortNumber _ auxcre(UserName&":"&cvs(hosts[h]));
    if PortNumber = -1 then 
      begin
        outstr(" cannot build circuit"&crlf);
        continue;
      end;
    outstr(crlf);
    auxout(PortNumber,#cr&#cr); ! proj code/attach msgs;
    auxout(PortNumber,";("&#cr); ! begin marker;
    auxout(PortNumber,commands);
    auxout(PortNumber,";)"&#cr); ! end marker;
    auxout(PortNumber,"exit"&crlf);
  
    ! search for begin marker;
    while true do 
      begin
        ch _ auxinc(PortNumber); ! catches echo;
        if ch = -1 then 
          begin
            outstr("Not valid on this host."&crlf);
            continue "hostloop";
          end;
        if ch = ";" then
          begin "BeginMarkFound"
            ch _ auxinc(PortNumber);
            if ch = -1 then
              begin
                outstr("*** fail B "&crlf);
                continue "hostloop";
              end;
            if ch = "(" then
              begin
                ch_auxinc(PortNumber);
                if ch = -1 then
                  begin
                    outstr("*** fail C "&crlf);
                    continue "hostloop";
                  end;
                done;
              end;
          end "BeginMarkFound" ;
      end;
  
    ! copy command output until end marker;
    BlankLine _ true;
    while true do
      begin "Respond"
             ! loops in one char at a time as they ;
             ! come in through the pipeline        ;
        ch _ auxinc(PortNumber);
        if ch = -1 then
          begin
            outstr("*** fail D "&crlf);
            continue "hostloop";
          end;
        if ch > '40 then
          BlankLine _ false;
        if ch = ";" then
          begin "EndMarkFound"
            ch _ auxinc(PortNumber);
            if ch = -1 then
              begin
                outstr("*** fail E "&crlf);
                continue "hostloop";
              end;
            if ch = ")" then
              done;
            outstr(";"&ch);
          end "EndMarkFound"
        else
          if (ch neq #lf) or not BlankLine then
            ! Print whatever comes back ;
            outstr(ch);  
        if ch = #lf then
          BlankLine _ true;
      end; "Respond"
    ! copy characters until zapped;
    if (ch _ auxinc(PortNumber)) neq -1 then
      outstr(crlf);
    while (ch _ auxinc(PortNumber)) neq -1 do ;
    auxzap(PortNumber); ! kill the circuit ;
  end;  "hostloop"
end;

!  *** main *** ;

UserName _ cv6str(calli(-1 lsh 18 lor -'22, '41))
	 & cv6str(calli(-1 lsh 18 lor -'21, '41));
setbreak(1,#lf&#eot,#cr,"SINF");

gethosts;
getcommands;
getfilestuff;

if det then
  begin
    print("Job ",call(0,"PJOB")," detaching...");
    calli(0,-6);
  end;

Gotohosts;

close(RecFilChan);
release(RecFilChan);

if det then
  calli(0,-'130);	! LOGOFF;
call(0,"EXIT");

end "TALL"
  