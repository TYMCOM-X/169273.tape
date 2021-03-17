PROGRAM search;
(*simple graph search, version 1, zw*)

CONST
  max_num_nodes = 100; max_num_arcs = 100;

TYPE
  node = 1 .. max_num_nodes;
  arc =  1 .. max_num_arcs;
  node_set = SET OF node;
  arc_set = SET OF arc;
  arc_node_map = ARRAY[arc] OF node;

VAR
  reached, reachable: node_set;
  changed: BOOLEAN;
  s, f: node;
  nodes: node_set;
  arcs: arc_set;
  arc_from_node, arc_into_node: arc_node_map;

PROCEDURE input_graph;
VAR from, into: 0 .. max_num_nodes; arc_num: 0 .. max_num_arcs;
BEGIN
  nodes := []; arcs := [];
  WRITELN('Enter "from, to" pairs.  End with "0,0".');
  BREAK; READLN; READ(from, into); arc_num := 0;
  WHILE (from <> 0) AND (into <> 0) DO BEGIN
    nodes := nodes + [from, into];
    arc_num := arc_num + 1; arcs := arcs + [arc_num];
    arc_from_node[arc_num] := from; arc_into_node[arc_num] := into;
    BREAK; READLN; READ(from, into)
    END
  END;

FUNCTION directly_reachable(reached: node_set): node_set;
VAR arc_index: arc; reachable: node_set;
BEGIN
  reachable := [];
  FOR arc_index := 1 TO max_num_arcs
  DO IF arc_index IN arcs THEN BEGIN
    IF arc_from_node[arc_index] IN reached
    THEN reachable := reachable + [arc_into_node[arc_index]]
    END;
  directly_reachable := reachable
  END;

BEGIN
  OPEN(INPUT, 'TTY:'); REWRITE(OUTPUT, 'TTY:'); TTYOUTPUT := OUTPUT;
  WRITELN('Graph Search, Version 1'); input_graph;
  REPEAT
    WRITE('Enter "start, finish" nodes -> '); BREAK; READLN; READ(s, f);
    reached := directly_reachable([s]); changed := reached <> [];
    WHILE NOT (f IN reached) AND changed DO BEGIN
      reachable := directly_reachable(reached);
      changed := reached <> (reached + reachable);
      reached := reached + reachable
      END;
    IF f IN reached THEN WRITELN('There is a path.')
    ELSE WRITELN('There is not a path.')
    UNTIL FALSE
  END.
