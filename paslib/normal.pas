$TITLE Third Normal Form Program
program normal;

(*




                    Third Normal Form Program                      




  Description: This program accepts as input a set of relational
	       dependencies between a set of items (attributes),
	       and outputs a revised set of relational dependencies
	       which are in third normal form. 


*)
$PAGE
(*




 Program Terminology:

    item             --  a field or attribute of the database;

    key item         --  an item which has other item(s) functionally
			dependent  on it;

    dependent item   --  the item which is functionally dependent
		         on a key item;

    whole key        --  all the key items for which a dependent item
                         is functionally dependent;

    relation         --  the combination of a whole key and all of the
                         dependent items of that whole key;

    relation array   --  an array holding unique whole keys 
                         (ie. relation keys);

    relation number  --  the index of the array corresponding to the
                         current whole key;

    func_dep_matrix  --  a (K,N) matrix holding the relationships
                         between the current relations, and the
                         attributes. If item J is functionally
                         dependent on key K, then the (K,J)
                         elenemt of the matrix will have the
                         value "dependent".

*)
$PAGE
(*



  Algorithm: The algorithm used in producing the final set of relations
             is taken from "Computer Data-base Organization" by
             James Martin (Chaps. 13-15).  It primarily uses the merging
             procedure outlined in Chapter 15 and is as follows:

               FOR each input line...
                 1) extract the key items from the input line;
                 2) add whole key to relations array if not found in it;
                 3) extract the dependent items from the input line;
                 4) fill proper key-dependent matrix coordinates
                    with "dependent";
                 5) check for non-full functional dependencies and
                    adjust if found;
                 6) check for transitive dependencies and
                    adjust if found;
                 7) output the relations as they currently stand.

*)
$PAGE
(*


  Input: The input of the program is a set of relational dependencies
	 in the following form:   

	    <key items> --> <dependent items>

	 where both <key items> and <dependent items> are lists in
	 the following form:  

            <item name>[ <item name>[ <item name>...]].

         The input file should have one relational dependency per
         input line, and <item name>s should be limited to twenty
         characters.




  Output: The output from the program is a set of relational
          dependencies, with the same number of total unique items,
          in third normal form.  The output will be output in the
          following form:
 
            { <key items> --> <dependent items> }

          where the item name(s) in upper case are the key(s) for
          the relation, and those in lower case are the dependent
          items.


*)
$PAGE 3NF Program
$include normal.inc
$PAGE program_init
(*  PROGRAM_INIT initializes some of the variables to initial states
    and also gets the input file from the user.                      *)

procedure program_init;

  var  input_file : string[10];
       i          : rel_index;
       j          : item_index;
 
  begin


    (* init the number of items currently in the data *)
    (*      and the number of current relations       *)

    num_items := 0;
    num_rels  := 0;


    (* the functional dependency matrix should contain *)
    (*      all zeros at the start of the program      *)

    for i:= 1  to  max_rels   do
      for j:= 1  to  max_items   do
        func_dep_matrix [ i, j ] := non_dependent;


    (* get the input file from the user *)

    rewrite( ttyoutput );
    writeln( ttyoutput );
    writeln( ttyoutput, 'Third Normal Form Program' );
    writeln( ttyoutput );
    write( ttyoutput, '  Enter input filename: ' );
    break( ttyoutput );
    open( tty );
    readln( tty );
    read( tty, input_file );





    (* open the input file, and open the output file *)

    reset( input, input_file );
    rewrite( output, input_file || '.3NF' );
    writeln( output, 'Third Normal Form Program Output' );
    writeln( output );


  end (* program_init *);
$PAGE get_token
(*  GET_TOKEN will 'copy' the next token from the input line
    It begins parsing at ind, ignores leading blanks, retreives
    the next token, and leaves ind pointing to the character
    after the token.                                             *)

function get_token(      line : string[255];
                    var  ind  : integer;
                    var  item : item_name   ) : boolean;

  var  start    : integer;
       line_len : integer;

  begin

    start    := ind;             (* we start from ind *)
    line_len := length( line );  (* we need the line length *)


    (* ignore leading blanks *)

    while ( ind <= line_len ) and ( line[ start ] = ' ' )
      do start := start +1;


    (* is there a token left on this line ??? *)

    if start <= line_len


      then (* there is one *) begin

	(* move ind to last character of item *)

        ind := start;

	while ( ind <= line_len ) and ( line[ ind ] <> ' ' )
	  do ind := ind +1;


	(* copy the token to item *)

        item := uppercase( substr( line, start, (ind-start) ) );
        get_token := true;

      end (* then begin *)

      else get_token := false;  (* nothing left *)

  end (* get_token *);
$PAGE add_to_item_names
(*  ADD_TO_ITEM_NAMES will search the item_names array for the
    name passed into the procedure.  If the name is found
    already in the array, ADD_TO_ITEM_NAMES will return its
    array index.  If the name is not found in the array, then
    it is put in the array and its index is returned.            *)


function add_to_item_names( name : item_name ) : item_set;

  var  ind : item_index;  (* used to search item_names *)
 
  begin

    (* initialize ind *)

    ind := 1;


    (* loop through list until name is found or no names left *)

    while ( item_names[ ind ] <> name ) and ( ind <= num_items )
      do ind := ind + 1;  (* try the next one *)


    (* if the name was not found, then add it to the list *)

    if ind > num_items
      then begin
        num_items := num_items + 1;
        item_names[ num_items ] := name;
      end (* then begin *);


    (* assign function value *)

    add_to_item_names := [ ind ];


end (* add_to_item_names *);
$PAGE add_to_relation_keys
(*  ADD_TO_RELATION_KEYS will check the relations array to see
    if there is already a relation in the relation array with
    the same key item(s).  If so, then it will set the rel_index
    to be the index to that relation.  If not, it will add the
    new key(s) to the relation array and return its new index    *)


procedure add_to_relation_keys( new_key : item_set );

  var  ind : rel_index;  (* scanning index into relation_keys *)
       i   : item_index; (* used to find set elements *)
       count : 0..max_rels;

  begin

    (* initialize ind *)

    ind := 1;


    (* loop through relation_keys until key is found, or none left *)

    while ( ind <= num_rels ) and
	  ( relation_keys[ ind ].items <> new_key )
      do ind := ind+1;


    (* if key not found, then add it to the array *)

    if ind > num_rels  then begin

      num_rels := num_rels + 1;
      relation_keys[ num_rels ].items := new_key;

      count := 0;
      for i:=1 to num_items 
        do if ( i in new_key )
          then begin
            count := count +1;
            func_dep_matrix[ num_rels, i ] := self;  (* mark items *)
          end (* then begin *);

      if count = 1
        then relation_keys[ num_rels ].key_type := singular
        else relation_keys[ num_rels ].key_type := concatenated;

      relation_keys[ num_rels ].num_in_key := count;

    end (* then begin *);

    rel_ind := ind;  (* assign current relation index *)

end (* add_to_relation_keys *);
$PAGE get_input_line
(*  GET_INPUT_LINE reads the next input line from the input
    file.  In doing this it parses the different items within
    the input line, adds them to the item_names list if not
    already in it, and puts their index into the name list
    into either the key_set or the dep_set.  If EOF is
    found, GET_INPUT_LINE returns the value false.             *)

function get_input_line( var  key_set   : item_set;
                         var  dep_set   : item_set ) : boolean;

  var  item_type  : ( key, dep );  (* input items type *)
       line       : string[255];   (* reads input line *)
       ind        : integer;       (* input line index *)
       input_item : item_name;     (* name of item input *)

  begin

    if not eof( input )

      then begin

	readln( input, line );  (* get next input line *)


        (* echo input line *)

        writeln( output, 'Input Line: "', line, '"' );
        get_input_line := true;


        (* some parsing initializations *)

        item_type := key;  (* key(s) come first *)
        ind       := 1;    (* start with the first char *)
        key_set   := [];   (* start with the empty set  *)
        dep_set   := [];   (* start also empty *)






	(* get all the input items *)

        while get_token( line, ind, input_item )

          do if input_item = '-->'

            then (* all key items collected *) begin

              add_to_relation_keys( key_set );
 
              item_type := dep;  (* now get dependents *)

            end (* then begin *)


            else (* add item to proper set *) 

              case item_type of

                key:  key_set := key_set + add_to_item_names( input_item );

                dep:  dep_set := dep_set + add_to_item_names( input_item );

              end (* case and else *);

      end (* then begin *)


      else get_input_line := false;  (* End Of File found *)


  end (* get_input_line *);
$PAGE add_rel_to_matrix
(*  ADD_REL_TO_MATRIX will add the input dependencies to the
    func_dep_matrix by assigning the value of "dependent" to the
    (K,j)th element, where K is the key input, and j is an item
    dependent on K.                                                   *)

procedure add_rel_to_matrix( rel_ind    : rel_index;
                             candidates : item_set );

  var  index : 0..max_items;  (* used for finding items in candidates *)
       temp_set : item_set;

  begin


    index := num_items;  (* start from the bottom since its more *)
                         (* likely the candidates are new items   *)
    temp_set := candidates;


    while temp_set <> []  do begin


      if ( index in temp_set)  then begin

        func_dep_matrix[ rel_ind, index ] := dependent; 

        temp_set := temp_set - [ index ]; (* remove index from set *)

      end (* then begin *);


      index := index -1;  (* move on to check next item *)


    end (* while do *);


end (* add_rel_to_matrix *);
$PAGE special_non_full_dep
(*
(*  SPECIAL_NON_FULL_DEP will remove special non full functional
    dependencies from the func_dep_matrix.  For example, the
    following change would be made:

	       -----
	      |  a--|---> c           e <--- a ---> c
	e <---|     |         ====>
	      |  b--|---> d           e <--- b ---> d
	       -----

        non-full func dep              full func dep             *)


procedure special_non_full_dep;

  var  rel_1, rel_2 : rel_index;
       ind          : item_index;
       temp_key     : rel_set;
       depen_set      : item_set;  (* holds dependent items of a relation *)

  begin

    (* check all concatenated relations *)

    for rel_1 := 1  to  num_rels  do
      if relation_keys[ rel_1 ].key_type = concatenated  then begin


      (* see if the key is the union of other keys *)

      temp_key := relation_keys[ rel_1 ].items;
      for rel_2 := 1  to num_rels  do
	if ( relation_keys[ rel_2 ].items <= temp_key ) and ( rel_1 <> rel_2 )
          then temp_key := temp_key - relation_keys[ rel_2 ].items;

*)
$PAGE
(*

	if temp_key = []  then begin

	  (* key is union of other keys -- eliminate it, and    *)
	  (* put dependent items in each of the other relations *)

	  depen_set := [];  (* collect the dependents of this key *)

	  for ind := 1  to  num_items   do
	    if func_dep_matrix[ rel_1, ind ] = dependent
              then begin
                depen_set := depen_set + [ ind ];
                func_dep_matrix[ rel_1, ind ] := non_dependent;
              end;

	  relation_keys[ rel_1 ].num_in_key := 0;


	  (* find which other relations they go to *)

	  for rel_2 := 1  to  (rel_1-1)  do
	    if relation_keys[ rel_2 ].items <= relation_keys[ rel_1 ].items 
	      then add_rel_to_matrix( rel_2, depen_set );

	  for rel_2 := (rel_1+1)  to  num_rels  do
	    if relation_keys[ rel_2 ].items <= relation_keys[ rel_1 ].items 
	      then add_rel_to_matrix( rel_2, depen_set );

	end (* if temp_key = []  then begin *);

      end (* then begin *);

  end (* special_non_ful_dep *);
*)
$PAGE elim_trans_dependencies
(*  ELIM_TRANSITIVE_DEPENDENCIES will check the current relations
    for transitive dependencies and eliminate them if found.    *)

procedure elim_transitive_dependencies;

  var  j_self, j_attr : item_index;
       rel_i          : rel_index;

  begin

    (* for every existing relation, check to see if any *)
    (*    transitive dependencies exsist in the matrix  *)
 
    for rel_ind := 1  to  num_rels  do 

      if ( relation_keys[ rel_ind ].key_type = singular )  and
         ( relation_keys[ rel_ind ].items <> [] )  then begin

        (* get key's item number -- j_self *)

        j_self := 1;
        while [ j_self ] <> relation_keys[ rel_ind ].items
          do j_self := j_self +1;


        (* is this item an attribute of another relation ??? *)

        for rel_i := 1 to num_rels  do 
          if func_dep_matrix[ rel_i, j_self ] = dependent  then begin

	    (* if both the "rel_ind" key and the "rel_i" key have a   *)
	    (* dependent attribute in common, then the dependency     *)
	    (* to the "rel_i" key is transitive and may be eliminated *)
            (*                                                        *)
            (*          rel_i  --->  rel_ind  ---> j_attr             *)
            (*            |                           ^               *)
            (*            |___________________________| ( remove )    *)

	    for j_attr := 1  to  num_items   do 




	      if ( func_dep_matrix[ rel_ind, j_attr ] = dependent ) and
		 ( func_dep_matrix[ rel_i, j_attr ] = dependent )
		then func_dep_matrix[ rel_i, j_attr ] := non_dependent

                else if ( func_dep_matrix[ rel_ind, j_attr ] = dependent) and
                        ( func_dep_matrix[ rel_i, j_attr ] = self )

                  then (* j_attr in rel_i is redundant -- remove it *) begin
                    func_dep_matrix[ rel_i, j_attr ] := non_dependent;
                    with relation_keys[ rel_i ]  do begin
                      items := items - [ j_attr ];
                      num_in_key := num_in_key -1;
                      if num_in_key = 1  then key_type := singular;
                    end (* with *);
                  end (* then *);

          
          end (* then begin *)


          else if func_dep_matrix[ rel_i, j_self ] = self  then

            for j_attr := 1  to  num_items  do
              if ( func_dep_matrix[ rel_ind, j_attr ] = dependent ) and
                 ( func_dep_matrix[ rel_i, j_attr ] = self )
                  then (* j_self in rel_i is redundant -- remove it *) begin
                    func_dep_matrix[ rel_i, j_self ] := non_dependent;
                    with relation_keys[ rel_i ]  do begin
                      items := items - [ j_self ];
                      num_in_key := num_in_key -1;
                      if num_in_key = 1  then key_type := singular;
                    end (* with *);
                  end (* then *);


      end (* then begin *);

  end (* elim_transitive_dependencies *);
$PAGE print_relations
(*  PRINT_RELATIONS simply goes down the func_dep_array and
    prints the relations key in caps, and then prints any
    item related to that key ( ie. f_d_m(K,j) = dependent ). *)

procedure print_relations;

  var  i     : rel_index;  (* loops through relations *)
       j     : item_index; (* loops through items *)
       count : 0..max_rels; (* counts items from rel_sets *)

  begin

    writeln( output );
    writeln( output );
    writeln( output, 'Third Normal Form Relations:' );


    (* loop throug entire func_dep_matrix *)

    for i:=1 to num_rels  (* loop through relations *) do 
      if relation_keys[ i ].num_in_key > 0  then begin

	writeln( output );
	write( output, '    {' );

	(* ready to print items in relation's key *)

	count := relation_keys[ i ].num_in_key;
	j := 1;

	repeat  (* until no more items left in key *)

	  if ( j in relation_keys[ i ].items )  then begin
	    write( output, '  ', uppercase( item_names[ j ] ) );
	    count := count - 1;  (* decrement count *)
	  end (* then begin *);

	  j := j + 1;

	until count = 0;

	write( output, '   --> ' );


	(* now write the attributes of the relation *)

	for j:=1 to num_items  
	  do if func_dep_matrix[ i, j ] = dependent
	       then write( output, '  ', lowercase( item_names[ j ] ) );

	writeln( output, ' }' );

      end (* then begin *);

    writeln( output );

end (* print_relation
$PAGE matrix_dump
(*  MATRIX_DUMP writes out the item_names array, and then the
    func_dep_matrix matrix as it currently stands.            *)

procedure matrix_dump( var  out_file : text );

  var  i,j : item_index;

  begin

    (* write the list of item names *)

    writeln( out_file );
    writeln( out_file, ' ITEM_NAME list dump:' );
    for i:=1 to num_items
      do writeln( out_file, '    ', i:3, '-- ', item_names[ i ] );


    (* write out relation keys *)

    writeln( out_file );
    writeln( out_file );
    writeln( out_file, '  RELATION_KEYS dump:' );
    writeln( out_file );
    for i:=1 to num_rels  do begin
      write( out_file, '    ', i:2 );
      for j:=1 to num_items
        do if ( j in relation_keys[ i ].items )
          then write( out_file, '  ', item_names[ j ] );
      writeln( out_file );
    end (* for begin *);


    (* write out the functional dependencies matrix *)

    writeln( out_file );
    writeln( out_file );
    writeln( out_file, '  FUNC_DEP_MATRIX dump:' );
    writeln( out_file );
    write( out_file, '      ' );
    for i:=1 to num_items
      do write( out_file, i:4 );
    writeln( out_file );
    for i:=1 to num_rels  do begin
      writeln( out_file );
      write( out_file, '    ', i:2 );
      for j:=1 to num_items  do
        write( out_file, ord( func_dep_matrix[ i, j ] ):4 );
    end (* for begin *);

    page( out_file );

end (* matrix_dump *);
$PAGE normal main body
begin  (* NORMAL main body *)


  (* program initialization and file open *)

  program_init;


  (*  the following loop processes one input line for each pass *)

  while get_input_line( key_set, dep_set )  do begin


    add_rel_to_matrix( rel_ind, dep_set );

(*
    special_non_full_dep;
*)

    elim_transitive_dependencies;

    print_relations;

    matrix_dump( output );

  end (* while begin *);


  (* print final relations *)

  writeln( output, 'FINAL OUTPUT:' );
  writeln( output );
  print_relations;



end (* program normal *).
    ]Kv