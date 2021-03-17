module print_clusters;

$include clustr.typ
$PAGE external declarations
external procedure trace_direction( way  : direction;
                                    proc : string[30] );

external var  trace    : boolean;
              level    : integer;
              opt_list : opt_ptr;




$PAGE print_sep_list_nodes
(* PRINT_SEP_LIST_NODES will print the sep node pointed to by pointer,
   if necessary, and then print all the children of the node pointed
   to by pointer, if necessary.  If DEPTH does not equal zero, then
   the node pointed to by pointer, and all its clustered children, have
   already been printed and therefore need not be printed again.  The
   separate children of the node pointed to by pointer have not been
   printed as yet, and therefore will be printed.                     *)


procedure print_clus_list_nodes( pointer : list_ptr;
                                 depth   : integer ); forward;


procedure print_sep_list_nodes( pointer : list_ptr;
                                depth   : integer );

  begin
    if trace  then trace_direction( enter_proc, 'print_sep_list_nodes' );


    if pointer = nil


      then (* nothing to print *) begin
	if trace  then trace_direction( exit_proc, 'print_sep_list_nodes' );
        return
      end


      else (* print what has not been printed yet *) begin


	(* if depth =0 then this node and its clustered children *)
	(*             need to be printed -- print them.         *)

	if depth = 0   then begin

	  write( output, '  ', pointer^.entity_num:0 );

	  print_clus_list_nodes( pointer^.clus_children, 1 );

	end (* then begin *);


	(* print the children connected separately to this node *)

	print_sep_list_nodes( pointer^.sep_children, 0 );

	print_sep_list_nodes( pointer^.next_kid, depth );


    end (* else begin *);


    if trace  then trace_direction( exit_proc, 'print_sep_list_nodes' );
  end (* print_sep_list_nodes *);
$PAGE print_clus_list_nodes
(* PRINT_CLUS_LIST_NODES prints the node pointed to by pointer, and
   then prints all nodes that are clustered with this node (down
   as many levels as necessary).  After all the clustered nodes are 
   printed, all nodes connected seperately to a clustered child node
   are printed, and if at cluster depth 1, then all the nodes
   connected separately to this node, and any node connected
   seprarately to a node below this node, will be printed.         *)

procedure print_clus_list_nodes( pointer : list_ptr;
				 depth   : integer );

  begin
    if trace  then trace_direction( enter_proc, 'print_clus_list_nodes' );


    if pointer = nil


      then (* nothing to print *) begin
	if trace  then trace_direction( exit_proc, 'print_clus_list_nodes' );
	return
      end


      else (* print all the clustered nodes below *) begin


	if depth = 1  then write( output, '(' );  (* first in cluster *)

	write( output, '  ', pointer^.entity_num:0 );


        (* get all other nodes that are clustered with this one *)

	print_clus_list_nodes( pointer^.clus_children, (depth+1) );

        print_clus_list_nodes( pointer^.next_kid, (depth+1) );





        (* if depth = 1  then all nodes in the cluster have been *)
        (*               found -- print all connected separately *)

        if depth = 1  then begin

          write( output, ')' );   (* close clustering *)

          print_sep_list_nodes( pointer^.sep_children, 0 );

          print_sep_list_nodes( pointer^.clus_children, depth );

          print_sep_list_nodes( pointer^.next_kid, depth );

        end (* then *);


    end (* else begin *);


    if trace  then trace_direction( exit_proc, 'print_clus_list_nodes' );
  end (* print_clus_list_nodes *);
$PAGE print_sep_cost_nodes
(* PRINT_SEP_LIST_NODES will print the sep node pointed to by pointer,
   if necessary, and then print all the children of the node pointed
   to by pointer, if necessary.  If DEPTH does not equal zero, then
   the node pointed to by pointer, and all its clustered children, have
   already been printed and therefore need not be printed again.  The
   separate children of the node pointed to by pointer have not been
   printed as yet, and therefore will be printed.                     *)

procedure print_sep_cost_nodes( pointer : cost_ptr;
                                depth   : integer );

  begin
    if trace  then trace_direction( enter_proc, 'print_sep_cost_nodes' );


    if pointer = nil


      then (* nothing to print *) begin
	if trace  then trace_direction( exit_proc, 'print_sep_cost_nodes' );
	return
      end


      else (* print what has not been printed yet *) begin


	(* if depth =0 then this node and its clustered children *)
	(*             need to be printed -- print them.         *)

	if depth = 0   then begin

	  write( output, '  ', pointer^.cost_owner^.entity_num:0 );

	  print_clus_list_nodes( pointer^.clus_children, 1 );

	end (* then begin *);





	(* print the children connected separately to this node *)

	print_sep_list_nodes( pointer^.sep_children, 0 );

	print_sep_cost_nodes( pointer^.join_separate, depth );


    end (* else begin *);


    if trace  then trace_direction( exit_proc, 'print_sep_cost_nodes' );
  end (* print_sep_cost_nodes *);
$PAGE print_clus_cost_nodes
(* PRINT_CLUS_COST_NODES prints the node pointed to by pointer, and
   then prints all nodes that are clustered with this node (down
   as many levels as necessary).  After all the clustered nodes are 
   printed, all nodes connected seperately to a clustered child node
   are printed, and if at cluster depth 1, then all the nodes
   connected separately to this node, and any node connected
   seprarately to a node below this node, will be printed.         *)

procedure print_clus_cost_nodes( pointer : cost_ptr;
				 depth   : integer );

  begin
    if trace  then trace_direction( enter_proc, 'print_clus_cost_nodes' );


    if pointer = nil


      then (* nothing to print *) begin
	if trace  then trace_direction( exit_proc, 'print_clus_cost_nodes' );
	return
      end


      else (* print all the clustered nodes below *) begin


	if depth = 1  then write( output, '(' );  (* first in cluster *)

	write( output, '  ', pointer^.cost_owner^.entity_num:0 );


        (* get all other nodes that are clustered with this one *)

	print_clus_list_nodes( pointer^.clus_children, (depth+1) );

	print_clus_cost_nodes( pointer^.join_clustered, (depth+1) );





        (* if depth = 1  then all nodes in the cluster have been *)
        (*               found -- print all connected separately *)

        if depth = 1  then begin

          write( output, ')' );   (* close clustering *)

          print_sep_list_nodes( pointer^.sep_children, 0 );

          print_sep_list_nodes( pointer^.clus_children, depth );

          print_sep_cost_nodes( pointer^.join_clustered, depth );

        end (* then *);


    end (* else begin *);


    if trace  then trace_direction( exit_proc, 'print_clus_cost_nodes' );
  end (* print_clus_cost_nodes *);
$PAGE print_cluster_lists
(* PRINT_CLUSTER_LISTS prints all the cluster lists produced by
   combining a parent and its children in all the possible
   combinations.  Two procedure calls  to two recursive procedures
   handle the printing of one clustering.                          *)

public procedure print_cluster_lists( cur_parent : entity_ptr );

   var  p    : cost_ptr;  (* moving pointer to clusterings *)
        opt  : opt_ptr;   (* pointer into optimizing list  *)
        dist : integer;   (* used to get integer distance *)

  begin
    if trace  then trace_direction( enter_proc, 'print_cluster_lists' );


    p   := cur_parent^.cluster_list;   (* start with the first ones *)
    opt := opt_list;


    (* print a title on a new page *)

    page (output);
    writeln( output );
    writeln( output, '  Cluster list costs for parent ', cur_parent^.entity_num:0 );
    writeln( output );
    write( output, '        D(I,I)  COST(', cur_parent^.entity_num:2, ')' );
    write( output, '      Clustering' );
    writeln( output );



    (* print out all the cluster lists for cur_parent *)

    while p <> nil  do begin

      writeln( output );

      dist := trunc( p^.d_of_ii );
      write( output, '   ', dist:10, p^.cost:10:2 );

      write( output, '    -- ', cur_parent^.entity_num:0 );

      print_clus_cost_nodes( p^.join_clustered, 1 );

      print_sep_cost_nodes(  p^.join_separate, 0 );

      if ( opt <> nil )  andif  ( opt^.keeper = p )
        then begin
          write( output, ' ...KEEP' );
          opt := opt^.next; (* pt to next on list *)
        end (* then begin *)
        else write( output, ' ...ELIMINATE' );

      p := p^.next_choice;   (* move to next one *)

    end (* while p <> nil *);


    if trace  then trace_direction( exit_proc, 'print_cluster_lists' );
  end (* print_cluster_lists *).
    