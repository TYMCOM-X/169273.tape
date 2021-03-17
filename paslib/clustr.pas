(* This program is an implementation of the Schrolnick
   clustering algorithm.                               *)

program cluster;

$include clustr.typ

  public var
       root : child_ptr;          (* ptr to the child node which pts
                                     to the root entity node         *)

       ebks : real;               (* effective block size *)



$PAGE  build_hierarchy algorithm
 
  (* build_hierarchy reads in the data and builds the hierarchical
     database tree.  The following algorithm will be used to read
     in the database tree's entity input lines:

     1) read an entity's (i,i) transition frequency;

     2) read the entity's "srs" and "nr";

     3) read in the parent-child transition frequencies beginning
        with the left most child and moving right, and at the same
	time create entity nodes for the children (a blank should
        seperate values on the input line);

     4) A zero ("0") in the input line terminates the input for that 
        parent, and the cur_ptr is set to either the next node at
        the same level, or to the first node of the next lower level
        if the current node is the right most node for that level.

     5) Go to 3) until "num_entities" entity input lines are read.


     NOTE:  Database tree's entity input lines should be stored as
            illustrated below:
            
                                   A
                       ____________|____________
                       |      |         |      |
                       B      C         D      E
                    ___|___   |     ____|____
                    |     |   |     |   |   |
                    F     G   H     I   J   K
                              |
                              |
                              L


     Entity input line order: A, B, C, D, E, F, G, H, I, J, K, L.

                                                                    *)
$PAGE  build_hierarchy
  procedure build_hierarchy ( size : integer;
                              var data : text  );

     var
         cur_ptr     : child_ptr;  (* "pts" to the parent node whose
				      children are being added on    *)

         cur_parent  : child_ptr;  (* "pts" to the parent of the 
				      cur_ptr entity              *)

         left_parent : child_ptr;  (* "pts" to the left most parent
				      at the cur_parent level       *)

         (*  NOTE: "pts" indicates the pointer is actually pointing
                   to the child node whose ij_ptr field is pointing
                   to the actual entity node being refered to.      *)



     (* get_entity will get a new entity node to add to the hierarchy
        tree and initialize its pointer fields to nil.                *)

     procedure get_entity( var  p : entity_ptr );
       begin
         new( p );
         p^.sibling_ptr  := nil;
         p^.first_child  := nil;
         p^.cluster_list := nil;
       end;

$PAGE  add_a_child
     (* add_a_child is a recursive procedure that is used to read in 
        parent-child transaction frequencies and add an entity node
        to the tree corresponding to the frequency.  It calls itself
        to continue adding children until a zero is found in the
        terminate the input line.                                     *)

     procedure add_a_child( var  current_child : child_ptr );

       var  freq : integer;

       begin
       writeln( tty, '         Enter add_a_child' ); break( tty );

         (* read the next parent-child transition frequency *)

         read( data, freq );
         write( output, freq );  (* echo *)
	 writeln( tty, '         ', freq ); break( tty );


         (* if the frequency is zero  then return
                                      else add on the new child  *)
         if freq = 0  then return
           else begin

             new( current_child );  (* get a child node for the freq *)

             with current_child^  do begin
               ij_freq := freq;
               get_entity( ij_ptr );
               next_child := nil;
             end  (* with *);

             add_a_child( current_child^.next_child );  (* check for more *)

           end  (* else begin *);

       writeln( tty, '         Exit  add_a_child' ); break( tty );
       end  (* add_a_child *);
$PAGE  build_hierarchy body

    var  count : integer;

    begin  (* build_hierarchy *)
    writeln( tty, '      Enter build_hierarchy' ); break( tty );

      (* set up some initial pointers and the root of the tree *)

      new( root );
      root^.ij_freq := -1;
      get_entity( root^.ij_ptr );
      root^.next_child := nil;
      cur_ptr := root;


      (* read in "size" entity input lines *)

      for count := 1 to size  do begin

        with cur_ptr^.ij_ptr^  do begin

          (* read in parent's SRS, NR, and F(i,i) *)

	  readln( data );
          read( data, srs, nr, ii_freq );
          entity_num := count;   (* assign entity number *)


          (* echo the data to the output file *)

          writeln( output );
          write( output, srs:5, nr:5, ii_freq:5 );


          (* get the parent's children *)
     
          add_a_child( first_child );

        end  (* with cur_ptr^.ij_ptr^ *);
$PAGE

	(* all children added - adjust the tree pointers *)

	if cur_ptr^.next_child = nil

          then (* end of child list *) begin

	    if cur_ptr = root

              then (* root is first parent generation *) begin
                 cur_parent := root;
                 left_parent := cur_parent;  (* set left most parent *)
              end (* then *)

	      else if cur_parent^.next_child = nil

		then (* end of parent row *) begin
		  cur_parent := left_parent^.ij_ptr^.first_child;
		  left_parent := cur_parent;
		end (* then begin *)

		else cur_parent := cur_parent^.next_child; (* next parent *)

	    cur_ptr := cur_parent^.ij_ptr^.first_child; (* first child *)

	  end (* then *)

	  else (* join siblings and move to next child *) begin
	    cur_ptr^.ij_ptr^.sibling_ptr := cur_ptr^.next_child^.ij_ptr;
	    cur_ptr := cur_ptr^.next_child;
	  end  (* else begin *)

      end (* for loop *);

    writeln( tty, '      Exit  build_hierarchy' ); break( tty );
    end  (* build_hierarchy *);
$PAGE  print_hierarchy
  (* print_hierarchy will print the database tree by printing higher
     nodes more left and lower nodes more to the right. Siblings of
     a parent node will be printed on the same line seperated by comas. *)

  procedure print_hierarchy;

    const blank = chr(32);

    (* print_sub_tree prints all the children node of the given ptr. *)

    procedure print_sub_tree( indent  : integer;
                              pointer : child_ptr );
      var  ind, i : integer;
           ptr    : child_ptr;

      begin
      writeln( tty, '         Enter print_sub_tree' ); break( tty );
 
        ind := indent + 10;   (* indent next level *)
        ptr := pointer ;      (* save original pointer *)

	writeln( output );
	for i:=1 to ind  do write( output, blank );

        while ptr <> nil  do begin

	  with ptr^.ij_ptr^  do begin

	    write( output, entity_num );  (* write out entity number *)

	    if sibling_ptr <> nil  then write( output, ', ' );

            if first_child <> nil
              then begin
                print_sub_tree( ind, first_child );
		for i:=1 to ind  do write( output, blank );
              end (* then begin *);

	  end (* with *);

          ptr := ptr^.next_child;  (* move to next sibling *)

	end (* while *);
        writeln( output );
      writeln( tty, '         Exit  print_sub_tree' ); break( tty );
      end (* print_sub_tree *);

$PAGE  print_hierarchy body
    begin  (* print_hierarchy main body *)
    writeln( tty, '      Enter print_hierarchy' ); break( tty );
      page( output );
      writeln( output, 'Database Hierarchy "Tree"' );
      writeln( output );
      print_sub_tree( 0, root );
    writeln( tty, '      Exit  print_hierarchy' ); break( tty );
    end (* print_hierarchy *);
$PAGE  load_hierarchy

  (* load_hierarchy will read in the hierarchy from a text file
     and build the hierarchal tree from the given data.         *)

  procedure load_hierarchy;

    var  data_file    : string[ 10 ];
	 data         : text;
         num_entities : integer;


    begin
    writeln( tty, '   Enter load_hierarchy' ); break( tty );

      (* get the data file name *)
      
      write( tty, 'Enter data file name: ');
      break( tty );
      readln( tty );
      read( tty, data_file );
      rewrite( output, data_file || '.OUT' );  (* open output file *)


      (* open the data file and read the number of
	 entities, and read the effective block size *)

      reset( data, data_file );
      read( data, num_entities, ebks );  (* read only - NO READLN *)


     (* read in the data *)

     build_hierarchy( num_entities, data );


     (* write out the hierarchy *)

     print_hierarchy;

    writeln( tty, '   Exit  load_hierarchy' ); break ( tty );
    end  (* load_hierarchy *) ;
$PAGE  cluster - main

(*  the main for the clustering program simply loads the
    hierarchical "database", and then calls the processing
    routine to do the clustering                           *)

external procedure process_tree( parent : child_ptr );

begin

  rewrite( tty );
  writeln( tty, 'Culstering Program      February 19, 1982');
  writeln( tty );
  break( tty );
  open( tty );

  load_hierarchy;

  process_tree( root );


end.   (* cluster *)
