module free_up_space;

$include clustr.typ
$PAGE external declarations
external procedure trace_direction( way  : direction;
                                    proc : string[30] );

external var  trace    : boolean;
              level    : integer;
              opt_list : opt_ptr;





$PAGE dispose_list_node
(* DISPOSE_LIST_NODE will dispose of a list node and any other
   list_nodes that it supports.                                 *)

procedure dispose_list_node( var   node : list_ptr );

  begin
    if trace  then trace_direction( enter_proc, 'dispose_list_node' );


    if node <> nil  then begin


      (* dispose of all supported list_nodes *)

      dispose_list_node( node^.next_kid );
      dispose_list_node( node^.sep_children );
      dispose_list_node( node^.clus_children );


      (* dispose of this node *)

      dispose( node );
      node := nil;

    end (* then begin *);


    if trace  then trace_direction( exit_proc, 'dispose_list_node' );
  end (* dispose_list_node *);
$PAGE dispose_cost_node
(* DISPOSE_COST_NODE will dispose of a cost node and any other
   cost or list nodes that it supports                          *)

procedure dispose_cost_node( var   node : cost_ptr );

  begin
    if trace  then trace_direction( enter_proc, 'dispose_cost_node' );


    if node <> nil  then begin


      (* dispose of all supported cost nodes *)

      dispose_cost_node( node^.join_separate );
      dispose_cost_node( node^.join_clustered );


      (* dispose of all supported list_nodes *)

      dispose_list_node( node^.sep_children );
      dispose_list_node( node^.clus_children );



      (* dispose of this node *)

      dispose( node );
      node := nil;

    end (* then begin *);


    if trace  then trace_direction( exit_proc, 'dispose_cost_node' );
  end (* dispose_cost_node *);
$PAGE dispose_of_cluster_list
(* DISPOSE_OF_CLUSTER_LIST is a recursive procedure which disposes
   of the clusterings on the cluster_list from the bottom up.       *)

procedure dispose_of_cluster_list( var   clustering : cost_ptr );

  begin
    if trace  then trace_direction( enter_proc, 'dispose_of_cluster_list' );


    (* if there are more clusterings below this one *)
    (*    then dispose of them first                *)

    if clustering^.next_choice <> nil
      then dispose_of_cluster_list( clustering^.next_choice );


    (* now get rid of this node's kid nodes *)

    dispose_cost_node( clustering^.join_separate );
    dispose_cost_node( clustering^.join_clustered );


    (* finally dispose of this node *)

    dispose( clustering );
    clustering := nil;


    if trace  then trace_direction( exit_proc, 'dispose_of_cluster_list' );
  end (* dispose_of_cluster_list *);
$PAGE eliminate_nodes
(* ELIMINATE_NODES will take those nodes which were flagged by the
   OPTIMIZE procedure and remove them from the cluster_list        *)

public procedure eliminate_nodes( var   parent : entity_ptr );

   var  p         : cost_ptr;  (* moving pointer to clusterings *)
        opt       : opt_ptr;   (* pointer into optimizing list  *)
        killer    : cost_ptr;  (* pointer to node to be eliminated *)

  begin
    if trace  then trace_direction( enter_proc, 'eliminate_nodes' );


    (* initialize some pointers to do the processing *)

    opt := opt_list;   (* start at the top of the opt_list *)
    p   := parent^.cluster_list;   (* and at top of clister_list *)


    (* do special case for first node in cluster_list *)

    while p <> opt^.keeper  do begin
      killer := p;  (* node is not on opt_list - dispose of it *)
      parent^.cluster_list := p^.next_choice; (* new first node *)
      p := p^.next_choice;
      dispose_cost_node( killer );
    end (* while do begin *);


    (* process until opt_list is exausted *)

    while opt <> nil  do begin

      if p = opt^.keeper
        then (* move onto next nodes -- this one stays *) begin
          p := p^.next_choice;
          opt := opt^.next;
        end (* then begin *)

        else (* dispose of this clustering -- not on opt_list *) begin
          killer := p;
          opt^.pred^.keeper^.next_choice := p^.next_choice;
          p := p^.next_choice;
          dispose_cost_node( killer );
        end (* else begin *);

    end (* while opt <> nil *);


    (* the rest of the cluster list is not on the opt_list *)
    (*              get rid of those clusters              *)

    if p <> nil  then begin

      opt := opt_list;  (* need to find last node on opt_list *)

      while opt^.next <> nil
	do opt := opt^.next;

      opt^.keeper^.next_choice := nil;  (* last node on reduced list *)

      dispose_of_cluster_list( p );

    end (* then begin *);


    if trace  then trace_direction( exit_proc, 'eliminate_nodes' );
  end (* eliminate_nodes *);
$PAGE dispose_of_useless
(* DISPOSE_OF_USELESS will eliminate the clusters in the parent's
   cluster list that were flagged for elimination, and it will
   dispose of all the parent's children's cluster_lists since
   they will be no longer needed.                                  *)

public procedure dispose_of_useless( var   parent : entity_ptr );

   var  kid : child_ptr;  (* used to access the parent's kids *)

  begin
    if trace  then trace_direction( enter_proc, 'dispose_of_useless' );


    (* get rid of the flagged nodes *)

    eliminate_nodes( parent );


    (* now get rid of the kid's cluster_lists *)

    kid := parent^.first_child;

    while kid <> nil  do begin  
      dispose_of_cluster_list( kid^.ij_ptr^.cluster_list );
      kid := kid^.next_child;
    end (* while kid <> nil *);


    if trace  then trace_direction( exit_proc, 'dispose_of_useless' );
  end (* dispose_of_useless *).
   