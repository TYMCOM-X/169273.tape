$TITLE SCNFIL -- SCANNR File Module

module scnfil;

$INCLUDE scannr.typ

public var
    trace_file: text;

var trace_open: boolean := false;
$PAGE open_trace

(*  OpenTrace makes sure that the trace file is open for output.  *)

public procedure open_trace;

begin
  if not trace_open then begin
    rewrite (trace_file, 'SCANNR.TRC');
    trace_open := true;
  end;
end.
