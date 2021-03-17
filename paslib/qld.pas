(*   +--------------------------------------------------------------+
     I                                                              I
     I                        L D P A R S                           I
     I                        - - - - - -                           I
     I                                                              I
     +--------------------------------------------------------------+

     MDSI, COMPANY CONFIDENTIAL

     STARTED:  4-Aug-77

     PURPOSE: This package  contains  procedures  QLDPARSE,  QLDEVAL,
	and QLDDISPOSE,  for use in QED-type packages.

     USAGE:
	CALLING SEQUENCES:
		QLDPARSE(LINE : CMDLINE;
			 var IDX : CMDLINEIDX;
			 var NLD : LDCOUNT;
			 var LD : LDCHAIN;
			 var ERR : QERRCODE) ;
		QLDEVAL( var BUF : QBUFFER;
			 LD : LDCHAIN;
			 SEARCHRANGE : LDRANGE;

			 var RANGE : LDRANGE;
			 var ERR : QERRCODE);
		QLDDISPOSE (LD : LDCHAIN);

     REQUIREMENTS: These  routines  need the SPRED and SPAT packages,
	from QSPRED     and QSPATP (arse).  Note that  QLDPARSE
	may   change   the  line  index  on  an  unsuccessful  parse.
	QLDDISPOSE users should NIL the head  of  the  LDCHAIN  after
	calling.

     EFFECTS: QLDPARSE  returns an LDCHAIN suitable for evaluation by
	QLDEVAL and disposal  by  QLDDISPOSE.  This  package  is  the
	highest  in  the LD parsing hierarchy,  and needs INCLUDEs at
	compile time and the other LD parsing packages at link time.

     ALGORITHM: 


     RESPONSIBLE: Software Tools

     The following error codes may be returned by QLDPARSE:
	QOFFIRST -- Signed offset found as first part of LA
	QNONUMF  -- No numeric field after '+' or '-'
	QONLYFIRST- '*', '$', or '.' valid only as first token of LA
	QNOPREVIOUS '*' valid only in second LA
	QNO2NDLA -- No valid LA found after comma
     QLDPARSE can also return any of the codes returned by SPREDPARSE
     or SPATPARSE.

     The following error codes may be returned by QLDEVAL:
	QOUTRANGE-- Evaluated LD is out of buffer or special range
	QSPNOTFND-- String predicate not found by search algorithm


     CHANGES:
        12/14/78   smr   Changed error codes returned by QEDEVAL to
			 indicate which la was in error.

	7/30/79    P.Lee  Changed this module to parse and evaluate
			  backward search predicates.

     ---------------------------------------------------------------- *)
module qld
  options special;
external function qtokenget(line : cmdline; var idx : cmdlineidx) : boolean;
(* in QSPAT *)
$PAGE qlddispose
public procedure qlddispose(ld : ldchain);
begin
if ld <> nil then
     begin
     qlddispose(ld^.next);
     if (ld^.ldkind = forward_ld) orif (ld^.ldkind = backward_ld) then
	  spreddispose(ld^.pred);
     dispose(ld)
     end
end;
$PAGE qldparse
public procedure qldparse ( line : cmdline;
			    var idx : cmdlineidx;
			    var nld : ldcount;
			    var ld : ldchain;
			    wildswitch : boolean;
			    var err : qerrcode)   ;


var
  firstld: ldchain;				(* where we build new chain *)
  lastld, ldtemp: ldchain;
  idxtemp: cmdlineidx;
  backward: boolean;                            (* flag for backward searches *)

(*and now for a few helpers*)

label   1;

     procedure error(derr : qerrcode);
     begin
     err := derr;
     qlddispose(firstld);
     nld := 0;
     goto 1
     end;



     function getint ( line : cmdline; var idx : cmdlineidx) : qlnoffset;
     var     okflag : boolean;
     begin
     getint := 0;
	  loop
	  okflag := false;
	  if idx <= length(line) then
	       if line[idx] in ['0'..'9'] then
		     begin
		     if (getint*10) > maximum(qlineno) then error(qtoobig);
		     getint := getint*10 + ( ord(line[idx]) - ord('0') );
		     idx := idx + 1;
		     okflag := true
		     end;
	  exit if not okflag
	  end
     end;


     procedure chain ( var one, two : ldchain);
     begin
     if one <> nil then
	  one^.next := two;
     one := two;
     two^.next := nil
     end;


     function laparse (line : cmdline;
			      var idx : cmdlineidx;
			      var first, last : ldchain  ): boolean;

     var
	  pred: spred;
	  firstpass, kickout: boolean;
	  offsetsign: -1..1;
	  linetmp: qlineno;
	  temprec: ldchain;

     begin
     pred := nil;
     firstpass := true;
     kickout := false;
     err := qok;
     offsetsign := 1;
	  loop
	  if not qtokenget(line, idx) then
	       kickout := true
	  else case line[idx] of

     '0'..'9', '+', '-'      :begin
			      if line[idx] in ['+','-'] then
				   begin
				   if firstpass then error(qoffirst);
				   if line[idx] = '-' then
					offsetsign := -1;
				   idx := idx + 1;
				   if (not qtokenget(line,idx))  orif
				      (not (line[idx] in ['0'..'9'])) then
					error(qnonumf)
				   end;
			      linetmp := getint(line, idx);
			      new(temprec, num_ld);
			      temprec^.offset := linetmp * offsetsign;
			      offsetsign := 1
			      end;

     '*', '$', '.'           :begin
			      if not firstpass then error(qonlyfirst);
			      case line[idx] of
	     '*'                     :begin
				      if nld = 0 then
					   error(qnoprevious);
				      new(temprec, star_ld)
				      end;
	     '$'                     :new(temprec, dollar_ld);
	     '.'                     :new(temprec, dot_ld)
				  end;		(*little case*)
			      idx := idx + 1
			      end;

     others                  :begin
                              if (line[idx] = '^') then begin
                                backward := true;
                                idx := idx + 1;
                                if not qtokenget(line, idx) then
                                  kickout := true
                                else kickout := not spredparse
                                  (line, idx, pred, wildswitch, err)
                                end
                              else begin
                                backward := false;
                                kickout := not spredparse
                                  (line, idx, pred, wildswitch, err)
                                end;
			      if err <> qok then error(err);
			      if not kickout then
				   begin
                                   if backward then new (temprec, backward_ld)
                                               else new (temprec, forward_ld);
				   temprec^.pred := pred;
				   pred := nil
				   end
			      end

			 end;			(*case*)
	  exit if kickout;
	  chain(last, temprec);
	  if first = nil then
	       first := temprec
	  else if firstpass then
		    first^.next := temprec;
	  firstpass := false
	  end;					(*loop*)
     laparse := not firstpass
     end;
$PAGE mainline for qldparse
begin						(*mainline for QLDPARSE*)
err := qok;    nld := 0;   firstld := nil;  lastld := nil;
idxtemp := idx;
if laparse(line, idx, firstld, lastld) then
begin
     nld := nld + 1;
     if qtokenget (line, idx) andif (line[idx] = ',') then
	  begin
	  new(ldtemp, comma_ld);
	  ldtemp^.ldkind := comma_ld;
	  chain(lastld, ldtemp);
	  idx := idx + 1;
	  if not laparse(line, idx, lastld, ldtemp)
	       then error (qno2ndla)
	       else nld := nld + 1
	  end;
     ld := firstld
     end
else begin
     ld := nil;
     idx := idxtemp
     end;
1: end;
$PAGE qldeval
public procedure qldeval ( var buf : qbuffer;
			   ld : ldchain;
			   searchrange : ldrange;
			   var range : ldrange;
			   var err : qerrcode);

label   1;

	procedure error(derr : qerrcode);
	begin
	err := derr;
	goto 1
	end;

	function search (pred : spred;
			 range : ldrange;
			 var buf : qbuffer;
			 var where : qlineno) : boolean;
	var     found : qlineno;
		linetemp : cmdline;
                increment : -1..1;
                backward : boolean;

	function within_limit : boolean;   (* function to check limit in search *)
          begin
            if backward then within_limit := found > range.hbound
                        else within_limit := found < range.hbound
          end;   (* function within_limit *)

	begin
	search := false;
	backward := range.lbound > range.hbound;
	if not backward then increment := 1
			else increment := -1;
	found := range.lbound;
	while (not search) and within_limit do
	     begin
	     found := found + increment;
	     linetemp := qgetline (buf, found, err);
	     if err <> qok then error(err);
	     search := spredmatch (linetemp, pred, err);
	     if err <> qok then error(err)
	     end;
	if search then where := found
	end;


var     firsttoken, lowbdalso : boolean;
	ldtemp : ldchain;
	range_limit, predrange, addrrange : ldrange;
        notfinderr, outrangeerr: qerrcode;

begin
ldtemp := ld;
err := qok;
range := searchrange;
range_limit.lbound := qfirst_val (buf);	(* allowed search limits- this narrows *)
range_limit.hbound := searchrange.hbound;	(* down as the search continues *)
addrrange.lbound := 0;	(* limits for non-predicate addressing *)
addrrange.hbound := qdollar_val (buf);
predrange.lbound := buf.curlineno;	(* line before search beginning *)
firsttoken := true;
lowbdalso := true;
notfinderr := qla1notfnd;
outrangeerr := qla1outrange;
while ldtemp <> nil do
     begin
     case ldtemp^.ldkind of

forward_ld,
backward_ld     :begin
		 if ldtemp^.ldkind = backward_ld then begin
		   predrange.hbound := range_limit.lbound; (* last search line *)
		   range_limit.hbound := predrange.lbound - 1 (* limits narrow down! *)
		   end
		 else begin
		   predrange.hbound := range_limit.hbound; (* like above *)
		   range_limit.lbound := predrange.lbound + 1
		   end;
		 if search(ldtemp^.pred, predrange, buf, range.hbound) then
		      begin
		      predrange.lbound := range.hbound
		      end
		 else begin
		      if not(firsttoken and lowbdalso) or
			    (searchrange.lbound=searchrange.hbound) then
			   error(notfinderr);
		      if ldtemp^.ldkind = forward_ld then begin
			predrange.lbound := qfirst_val (buf) - 1;
			range_limit.lbound := predrange.lbound;  (* search limits change *)
			range_limit.hbound := buf.curlineno + 1; (* on wrap-around *)
			predrange.hbound := range_limit.hbound
		      end
		      else begin
			predrange.lbound := searchrange.hbound + 1;
			range_limit.lbound := buf.curlineno - 1;
			predrange.hbound := range_limit.lbound;
			range_limit.hbound := predrange.lbound
		      end;
		      if not search(ldtemp^.pred, predrange, buf, range.hbound) then
			   error(notfinderr);
		      predrange.lbound := range.hbound
		      end;
		 firsttoken := false
		 end;

comma_ld        :begin
		 lowbdalso := false;
                 notfinderr := qla2notfnd;
                 outrangeerr := qla2outrange;
		 firsttoken := true;
		 range_limit.lbound := range.lbound;
		 range_limit.hbound := searchrange.hbound;
		 predrange.lbound := range.lbound;
		 predrange.hbound := searchrange.hbound;
		 addrrange.hbound := searchrange.hbound
		 end;

others          :begin
		 case ldtemp^.ldkind of
	dollar_ld       :range.hbound := qdollar_val (buf);
	star_ld         :range.hbound := range.lbound;
	dot_ld          :range.hbound := buf.curlineno;
	num_ld          :if firsttoken then
			      range.hbound := ldtemp^.offset
			 else if (range.hbound + ldtemp^.offset > addrrange.hbound)
			      or (range.hbound + ldtemp^.offset < addrrange.lbound) 
			      then
			      error(outrangeerr)
			      else range.hbound := range.hbound+ldtemp^.offset
		     end;			(*little case*)
		 if range.hbound = 0 then
		      predrange.lbound := 1
		 else if range.hbound >= maximum(qlineno) then error(outrangeerr)
		      else predrange.lbound := range.hbound ;
		 firsttoken := false
		 end
	end;					(*big case*)
     if lowbdalso then
	  range.lbound := range.hbound;
     if (range.hbound < addrrange.lbound) or
	(range.hbound > addrrange.hbound) then
          if lowbdalso or (ldtemp^.ldkind = comma_ld) then
            error(qla1outrange)
          else error(qla2outrange);
     ldtemp := ldtemp^.next
     end;					(*while*)
if (range.lbound < searchrange.lbound) then error(qla1outrange)
else if (range.hbound > searchrange.hbound) then error(qla2outrange);
1: end.
   