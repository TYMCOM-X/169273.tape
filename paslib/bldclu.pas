module build_clusters;

$include clustr.typ
$PAGE globals
  external procedure print_cluster_lists( parent : entity_ptr );

  external procedure eval_costs( parent : entity_ptr );

  external procedure optimize( parent : entity_ptr );

  external procedure dispose_of_useless( var  parent : entity_ptr );


  external var  root  : child_ptr;
                ebks  : real;


  var  process_list : process_ptr:= nil;  (* pointer to the list used in
					     merging child clusters 
					     together to form a new parent
					     cluster.                     *)

       current_cost_node : cost_ptr := nil;

$ifnot trace    trace : boolean := false;
$if    trace    trace : boolean := true;



$PAGE trace_direction
(* TRACE_DIRECTION is a debugging aid used to trace the flow
   of the program.  It is turned on or off by conditionally
   compiling the build module with the trace switch enabled,
   to turn the trace on, or with the trace switch disabled,
   to turn the trace off.                                      *)

public var  level : integer := 0;

public procedure trace_direction( way  : direction;
				  proc : string[30] );
  var  i : integer;

  begin
    writeln( tty );

    case way of
      enter_proc:  begin
		     level := level + 5;  (* increase indentation *)
		     for i:=1 to level  
		       do write( tty, '.' );
		     write( tty, ' Enter ', proc, ' -- ', level:0 );
		   end (* enter_proc *);

      exit_proc:   begin
		     for i:=1 to level  
		       do write( tty, '.' );
		     write( tty, ' Exit  ', proc );
		     level := level - 5;  (* decrease indentation *)
		   end (* exit_proc *);
    end (* case *);

    break( tty );

  end (* trace direction *);
$PAGE copy_list_nodes
(* COPY_LIST_NODES will make a copy of all the list_nodes that are
   accessable by "old_ptr" and have "new_ptr" point to the new copy *)

procedure copy_list_nodes(      old_list : list_ptr;
                           var  new_list : list_ptr );

  begin
    if trace  then trace_direction( enter_proc, 'copy_list_nodes' );


    (* if old_list = nil, then no list to copy *)

    if old_list = nil  then new_list := nil


      else begin

	(* create a new node *)

	new( new_list );


	(* copy in the necessary information *)

	with new_list^  do begin
	  entity_num := old_list^.entity_num;
	  copy_list_nodes( old_list^.sep_children, sep_children );
	  copy_list_nodes( old_list^.clus_children, clus_children );
	  copy_list_nodes( old_list^.next_kid, next_kid );
	end (* with new_list^ *);

      end (* else begin *);


    if trace  then trace_direction( exit_proc, 'copy_list_nodes' );
  end (* copy_list_nodes *);
$PAGE list_separate
(* LIST_SEPARATE will convert the cost_node pointed to by "old_ptr" into
   a list_node (most of the information in the cost node is no longer
   needed), and will also make a copy of all the nodes accessable
   through "old_ptr". "new_ptr" will point to the converted node.       *)

procedure list_separate(        old_ptr : cost_ptr;
                           var  new_ptr : list_ptr );

  begin
    if trace  then trace_direction( enter_proc, 'list_separate' );


    (* if old_ptr = nil, then no list to copy *)

    if old_ptr = nil  then new_ptr := nil


      else begin

	(* create a new node *)

	new( new_ptr );


	(* copy in the necessary information *)

	with new_ptr^  do begin
          entity_num := old_ptr^.cost_owner^.entity_num;
	  copy_list_nodes( old_ptr^.sep_children, sep_children );
	  copy_list_nodes( old_ptr^.clus_children, clus_children );
	  list_separate( old_ptr^.join_separate, next_kid );
	end (* with new_ptr^ *);

      end (* else begin *);


    if trace  then trace_direction( exit_proc, 'list_separate' );
  end (* list_separate *);
$PAGE list_clustered
(* LIST_SEPARATE will convert the cost_node pointed to by "old_ptr" into
   a list_node (most of the information in the cost node is no longer
   needed), and will also make a copy of all the nodes accessable
   through "old_ptr". "new_ptr" will point to the converted node.       *)

procedure list_clustered(       old_ptr : cost_ptr;
                           var  new_ptr : list_ptr );

  begin
    if trace  then trace_direction( enter_proc, 'list_clustered' );


    (* if old_ptr = nil, then no list to copy *)

    if old_ptr = nil  then new_ptr := nil


      else begin

	(* create a new node *)

	new( new_ptr );


	(* copy in the necessary information *)

	with new_ptr^  do begin
          entity_num := old_ptr^.cost_owner^.entity_num;
	  copy_list_nodes( old_ptr^.sep_children, sep_children );
	  copy_list_nodes( old_ptr^.clus_children, clus_children );
	  list_clustered( old_ptr^.join_clustered, next_kid );
	end (* with new_ptr^ *);

      end (* else begin *);


    if trace  then trace_direction( exit_proc, 'list_clustered' );
  end (* list_clustered *);
$PAGE copy
(* COST is used in building a clustering from a parent's childrens'
   clustering list.  COST makes a copy of a cost_node (and every thing 
   connected to the cost_node) of a child, and returns a pointer
   to that node for a parent to attach to.  This allows the parent
   to build up a cluster combination.                               *)

function copy( this_node : cost_ptr ) : cost_ptr;

  var  that_node : cost_ptr;  (* ptr to the new cost_node *)

  begin
    if trace  then trace_direction( enter_proc, 'copy' );


    (* get a new cost_node *)

    new( that_node );


    (* copy all necessary info from "this_node" to "that_node" *)

    with this_node^  do begin
      that_node^.cost_owner     := cost_owner;
      that_node^.d_of_ii        := d_of_ii;
      that_node^.pba            := pba;
      that_node^.cost           := cost;
      that_node^.join_separate  := nil;
      that_node^.join_clustered := nil;
      that_node^.next_choice    := nil;
    end (* with this_node^ *);


    (* convert children cost_nodes into list_nodes *)
    (*     (  cost info is no longer needed  )     *)

    list_separate( this_node^.join_separate, that_node^.sep_children );
    list_clustered( this_node^.join_clustered, that_node^.clus_children );

    copy := that_node;  (* return pointer to new cost_node *)

    if trace  then trace_direction( exit_proc, 'copy' );
  end (* copy *);
$PAGE join_nodes
(* JOIN_NODES is used in joining "(last-first+1)" child cost_nodes
   to their parent's cost_node in the way specified by "how_to_join".
   When a child's cost_node is joined with a parent's, a copy of the
   child's cost_node is actually copied to the parent's cost_node
   along with any lists that are attached to the child's cost_node.  *)

procedure join_nodes(      how_to_join : join_type;
                           first       : integer;    (* ordinal starting node *)
                           last        : integer;    (* ordinal ending node *)
                      var  last_sep    : cost_ptr;   (* last joined separately *)
                      var  last_clus   : cost_ptr;   (* last joined clustered *)
                      var  cur_child   : process_ptr (* child to add *) );

  var  count : integer;  (* used in for loop *)

  begin
    if trace  then trace_direction( enter_proc, 'join_nodes' );


    (* for all those child cost_nodes to be added on *)

    for count := first to last  

      do case how_to_join of

        separate  :  (* join separately from parent *) begin
		       last_sep^.join_separate:=copy(cur_child^.cur_cluster);
		       last_sep := last_sep^.join_separate;
		       cur_child := cur_child^.next_node;
                     end (* separate case *);

        clustered :  (* join clustered from parent *) begin
		       last_clus^.join_clustered:=copy(cur_child^.cur_cluster);
		       last_clus := last_clus^.join_clustered;
		       cur_child := cur_child^.next_node;
                     end (* clustered case *);

      end (* case how_to_join *);


    if trace  then trace_direction( exit_proc, 'join_nodes' );
  end (* join_nodes *);
$PAGE set_up_for_new
(*  SET_UP_FOR_NEW creates a copy of the current parent's cost_node
   and attaches it to the last cost_node on the parent's
   cluster_list.  Pointers are initialized appropriately.           *)

procedure set_up_for_new( var  parent : entity_ptr;
                          var  cur_node : cost_ptr;  (* current cost_node *)
                          var  sep_ptr  : cost_ptr;
                          var  clus_ptr : cost_ptr );

  begin
    if trace  then trace_direction( enter_proc, 'set_up_for_new' );


    if parent^.cluster_list = nil


      then (* list is empty - need to start one *) begin
        new( parent^.cluster_list );
        cur_node := parent^.cluster_list;  (* new node is now current node *)
      end (* then begin *)


      else (* add on a new node to existing list *) begin
	new( cur_node^.next_choice );
	cur_node := cur_node^.next_choice; (* new node is current node *)
      end (* else begin *);


    (* initialize the new cost_nodes fields *)

    with cur_node^  do begin
      cost_owner     := parent;
      join_separate  := nil;
      join_clustered := nil;
      sep_children   := nil;
      clus_children  := nil;
      next_choice    := nil;
    end (* with cur_node *);




    (* set up pointers to the node where a node should be added *)
    (*     if added sep(arately) or if added clus(tered).       *)

    sep_ptr  := cur_node;
    clus_ptr := cur_node;


    if trace  then trace_direction( exit_proc, 'set_up_for_new' );
  end (* set_up_for_new *);
$PAGE empty_process_list
(* EMPTY_PROCESS_LIST is called before FORM_PROCESS_LIST to empty the
   list of process nodes that may be on the list from the last parent
   processed.  It, like many other procedures, is recursive, and goes
   down to the bottom of the list initially and then DISPOSES of all
   connected nodes leaving the process_list empty.                    *)

procedure empty_process_list( var  list : process_ptr );

  begin
    if trace  then trace_direction( enter_proc, 'empty_process_list' );


    if list = nil  then return

      else begin


        (* empty everything below this level *)

        empty_process_list( list^.next_node );


        (* now dispose of the "next_node" *)

        dispose( list );



      end (* else begin *);

    if trace  then trace_direction( exit_proc, 'empty_process_list' );
  end (* empty_process_list *);
$PAGE form_process_list
(* FORM_PROCESS_LIST will add on another process node to the process
   list.  This process list will be used while processing the clustering
   combinations of a parent node. The list will consist of a linked list
   of information that is kept for each child of the parent being
   processed.  For each child the following information is kept:

         - a pointer to the child's entity node;
         - a pointer to the child's cluster being currently used;
         - a pointer to the next "separate" node of the current
             cluster to go on the cluster being formed;
         - a pointer to the next "clustered" node of the current
             cluster to go on the cluster being formed.

   FORM_PROCESS_LIST only adds to the process list, it is left up
   to INIT_PROCESS_LIST and UPDATE_PROCESS_LIST to manipulate the
   values contained in the list.                                     *)

function form_process_list( child : child_ptr ) : process_ptr;

  var  list : process_ptr;  (* used in forming a new node *)

  begin
    if trace  then trace_direction( enter_proc, 'form_process_list' );


    if child = nil  then form_process_list := nil

      else (* form a new process node *) begin

        new( list );    (* get a new node for the list *)

        list^.entity := child^.ij_ptr;  (* entity points to the child node *)
        list^.cur_cluster := nil;       (* just to be safe *)
        list^.next_node := form_process_list( child^.next_child );

        form_process_list := list;   (* connect the list *)

      end (* else begin *);


    if trace  then trace_direction( exit_proc, 'form_process_list' );
  end (* form_process_list *);
$PAGE init_process_list
(* INIT_PROCESS_LIST will initialize the process list so that each
   process node's cur_cluster pointer will point to the first
   cluster in the entity's cluster list, and the next pointers
   will have the same value as the join pointers of the cluster node. *)

procedure init_process_list( list : process_ptr );

  var  pointer : process_ptr;

  begin
    if trace  then trace_direction( enter_proc, 'init_process_list' );


    pointer := list;  (* start at the top *)


    while pointer <> nil  do with pointer^  do begin

      cur_cluster := entity^.cluster_list;  (* start w/ first clustering *)

      pointer := pointer^.next_node;  (* move on to next in list *)

    end (* while  do with ptr^ *);


    if trace  then trace_direction( exit_proc, 'init_process_list' );
  end (* init_process_list *);
$PAGE update_process_list
(* UPDATE_PROCESS_LIST is used to adjust the list in order to build the
   next set of clusters using the next combination of already existing
   children clusters. To get all possible combinations,
   UPDATE_PROCESS_LIST will proceed as illustrated by the following
   example:

     ( numbers are ordinal values for clusters in cluster_list X )
     (    each cluster_list has two clusterings in its list      )

     cluster_list:     A          B          C   returned to orig caller

     initialized to    1          1          1    (by INIT_PROCESS_LIST)
     update 1:         2          1          1            TRUE
     update 2:         1          2          1            TRUE
     update 3:         2          2          1            TRUE
     update 4:         1          1          2            TRUE
     update 5:         2          1          2            TRUE
     update 6:         1          2          2            TRUE
     update 7:         2          2          2            TRUE
     update 8:         ?          ?          ?            FALSE

   UPDATE_PROCESS_LIST checks the child's cluster list pointed to by
   proc_list and if the list has been exausted, it then calls itself
   to check the next child's cluster list.  This process continues
   until either a combibation is found that has not yet been used,
   or until all combinations have been exausted and thus returns
   FALSE.                                                            *)

$PAGE update_process_list
function update_process_list( proc_list : process_ptr ) : boolean;

  begin
    if trace  then trace_direction( enter_proc, 'update_process_list' );


    with proc_list^  do begin

      if cur_cluster^.next_choice <> nil


        then (* move to next choice *) begin
	  cur_cluster := cur_cluster^.next_choice;
	  update_process_list := true; (* everything OK *)
	end (* then begin *)


        else if next_node <> nil

          then (* update next node's choice *) begin
            cur_cluster := entity^.cluster_list; (* restart this at top *)
            update_process_list := update_process_list( next_node );
          end (* then begin *)

          else update_process_list := false; (* all combinations used *)


    end (* with proc_list^ *);


    if trace  then trace_direction( exit_proc, 'update_process_list' );
  end (* update_process_list *);
$PAGE join_all_separate
(* JOIN_ALL_SEPERATE will handle the special case of joining all the
   children of a parent separately (ie. not clustered together) in
   a clustering list.                                                 *)

procedure join_all_separate(      parent   : child_ptr;
                             var  cur_clus : cost_ptr; (* last node added *) 
                                  num      : integer   (* num children *) );

  var  sep_ptr  : cost_ptr;  (* ptr to node to add next node separately *)
       clus_ptr : cost_ptr;  (* ptr to node to add next node clustered  *)
       kid_ptr  : process_ptr;  (* ptr to process list *)

  begin
    if trace  then trace_direction( enter_proc, 'join_all_separate' );


    init_process_list( process_list );


    repeat (* for all combinations of child list clusters *)

      set_up_for_new( parent^.ij_ptr, cur_clus, sep_ptr, clus_ptr );

      kid_ptr := process_list;


      (* find all clusters using current child clusters *)

      join_nodes( separate, 1, num, sep_ptr, clus_ptr, kid_ptr );


    until  not update_process_list( process_list );


    if trace  then trace_direction( exit_proc, 'join_all_separate' );
  end (*join_all_separate *);
$PAGE combinations
(* COMBINATIONS will put together all the possible clustering
   combinations for the given parent, and link them to the
   parent's cluster_list.                                       *)

procedure combinations(      parent   : child_ptr;
                        var  cur_clus : cost_ptr;  (* last cost_node added *)
                             num      : integer ); (* number of children   *)

  var  n_p_c    : integer;   (* number of nodes per cluster *)
       start    : integer;   (* ordinal child to start clustering *)
       last     : integer;   (* ordinal child to stop clustering  *)
       i        : integer;
       sep_ptr  : cost_ptr;  (* next node to add separately to *)
       clus_ptr : cost_ptr;  (* next node to add clustered to  *)
       kid_ptr  : process_ptr; 

  begin
    if trace  then trace_direction( enter_proc, 'combinations' );

    init_process_list( process_list );  (* initialize processing list *)

    repeat (* for all possible child clustering combinations *)

      for n_p_c := 1 to num  do  (* from 1 to num per cluster *)

        for start := 1 to (num-n_p_c+1)  do begin

          set_up_for_new( parent^.ij_ptr, cur_clus, sep_ptr, clus_ptr );

	  last := ( start + n_p_c ) - 1 ;

          kid_ptr := process_list;  (* start with first k

	  (* do separates up to cluster *)
          join_nodes( separate, 1, (start-1), sep_ptr, clus_ptr, kid_ptr );

	  (* do cluster *)
          join_nodes( clustered, start, last, sep_ptr, clus_ptr, kid_ptr );

	  (* do separates after cluster *)
          join_nodes( separate, (last+1), num, sep_ptr, clus_ptr, kid_ptr );

        end (* for start := 1 *);

    until  not update_process_list( process_list );

    if trace  then trace_direction( exit_proc, 'combinations' );
  end (* combinations *);
$PAGE not_a_parent
(* NOT_A_PARENT is called when a node is found not to have any
   children, and thus terminates the recursive processing.
   This procedure puts on a clustering node on the entity node
   passed in.                                                   *)

procedure not_a_parent(      child : entity_ptr;
                        var  clus  : cost_ptr );

  begin
    if trace  then trace_direction( enter_proc, 'not_a_parent' );


    (* get a cluster block for the information *)

    new( clus );


    (* make the initial assignments to the block *)

    with clus^  do begin
      cost_owner     := child;
      d_of_ii        := child^.srs;
      pba            := min( (child^.srs/ebks), 1 );
      cost           := child^.ii_freq * pba;
      join_clustered := nil;
      join_separate  := nil;
      sep_children   := nil;
      clus_children  := nil;
      next_choice    := nil;
    end (* with clus^ *);


    if trace  then trace_direction( exit_proc, 'not_a_parent' );
  end (* not_a_parent *);
$PAGE process_tree
(* PROCESS_TREE does everything needed to take the existing tree
   representation of the hierarchical database representation and
   1) form all possible clusters, 2) find the cost of each cluster,
   and 3) optimize the list of clusters by eliminating those 
   clusters which are obviously not as good as some other clusters. 
   PROCESS_TREE is the main calling procedure which calls the other
   procedures, including itself, to do the above functions.          *)

public procedure process_tree( parent : child_ptr );

  var  p            : child_ptr;       (* used as a moving pointer *)
       num_children : integer ;    (* counts parent's children *)

  begin
    if trace  then trace_direction( enter_proc, 'process_tree' );

    if parent^.ij_ptr^.first_child <> nil
      then (* process this parents children *) begin

        p := parent^.ij_ptr^.first_child;  (* start w/ first child *)
        num_children := 0;

        while p <> nil  do begin   (* for all children do *)
          process_tree( p );
          p := p^.next_child;
          num_children := num_children + 1;
        end (* while do *);

        empty_process_list( process_list );

        process_list := form_process_list( parent^.ij_ptr^.first_child );

	join_all_separate( parent, current_cost_node, num_children );

        combinations( parent, current_cost_node, num_children );

        eval_costs( parent^.ij_ptr );

        optimize( parent^.ij_ptr );

        print_cluster_lists( parent^.ij_ptr );

        dispose_of_useless( parent^.ij_ptr );

      end (* then do *)

      else not_a_parent( parent^.ij_ptr, parent^.ij_ptr^.cluster_list );

    if trace  then trace_direction( exit_proc, 'process_tree' );
  end (*process_tree *).
     ?X{