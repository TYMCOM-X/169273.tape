PROGRAM search;
(*simple graph search, version 3, zw*)

CONST
  max_num_nodes = 100;

TYPE
  node = 1 .. max_num_nodes;
  arc_link = ^arc;
  arc = RECORD into: node; next: arc_link END;
  frontier_index = 1 .. max_num_nodes;
  zero_or_frontier_index = 0 .. max_num_nodes;

VAR
  reached: ARRAY[node] OF BOOLEAN;
  top: zero_or_frontier_index;
  frontier: ARRAY[frontier_index] OF node;
  s, f, x, y: node;
  a: arc_link;
  first_arc_from: ARRAY[node] OF arc_link;

PROCEDURE input_graph;
VAR from, into: 0 .. max_num_nodes; a: arc_link; x: node;
BEGIN
  FOR x := 1 TO max_num_nodes DO first_arc_from[x] := NIL;
  WRITELN('Enter "from, to" pairs.  End with "0,0".');
  BREAK; READLN; READ(from, into);
  WHILE (from <> 0) AND (into <> 0) DO BEGIN
    NEW(a); a^.into := into;
    a^.next := first_arc_from[from]; first_arc_from[from] := a;
    BREAK; READLN; READ(from, into)
    END
  END;

BEGIN
  OPEN(INPUT, 'TTY:'); REWRITE(OUTPUT, 'TTY:'); TTYOUTPUT := OUTPUT;
  WRITELN('Graph Search, Version 3'); input_graph;
  REPEAT
    WRITE('Enter "start, finish" nodes -> '); BREAK; READLN; READ(s, f);
    top := 1; frontier[top] := s;
    FOR x := 1 TO max_num_nodes DO reached[x] := FALSE;
    WHILE NOT reached[f] AND (top <> 0) DO BEGIN
      x := frontier[top]; top := top - 1; a := first_arc_from[x];
      WHILE a <> NIL DO BEGIN
        y := a^.into; a := a^.next;
	IF NOT reached[y] THEN BEGIN
	  reached[y] := TRUE; top := top + 1; frontier[top] := y
	  END
        END
      END;
    IF reached[f] THEN WRITELN('There is a path.')
    ELSE WRITELN('There is not a path.')
    UNTIL FALSE
  END.
   