$TITLE optimize_plot
$WIDTH (107)
  
(* OPTIMIZE_PLOT:

      Optimize Plot accepts a list of line segments constituting a "plot", and
      attempts to reorder the list in such a fashion that it would take less
      time to plot than the original ordering.

      This is a straight forward application of Dr. John Holland's theory.  For
      information on his theory in general see Holland: Adaptation in Natural
      and Artificial Systems.  For a detailed explanation of this particular
      application, see Wilson: "Plot Optimization: An application of Dr. John
      Holland's Theory of Adaptive Systems to an artificial system".

   Brief explanation:

      For a list of N segments, we are searching a space of N! permutations
      (assuming that for a given permutation we can determine the optimal
      direction in which to traverse each segment - an assumption not made by
      the earlier version of this program described in the paper cited above).
      At each instant in time, t, there will be a population A(t), a subset of
      possible structures A (where the size of A is N!).  The elements of A
      can be enumerated, and given a number, I, the Ith element of A can be
      constructed.  This is particularly easy if I is expressed in a mixed
      radix system where the digits are coefficients of factorials:

	   Cn (n-1)! + Cn-1 (n-2)! + ... + C3 (2)! + C2

      Therefore the most convenient representation for the elements of A(t)
      is to store the index within A of each element.  In particular, since
      converting to and from the mixed radix form is difficult, we will simply
      represent each element by the list of coefficients C2, ..., Cn.

      The process of generating population A(t+1) from A(t) is the heart of the
      application.  The method used here is distinct crossover and reproduction
      phases.  A temporary population A'(t) is constructed by reproducing each
      element of A(t) in proportion to its relative "fitness".  Then A(t+1)
      is built by randomly choosing pairs of elements from A(t) (without re-
      placement) and crossing them.  This is the most straightforward method
      of applying Dr. Holland's Theory.  More sophisticated, and potentially
      better performing, methods exist and will "someday" be applied.		*)
$PAGE declarations
program optimize_plot;
  
   const
      max_line_segments = 128;			(* maximum size of plot I'm prepared to handle *)
      max_width         =   7;			(* log base 2 of max_line_segments *)
      max_pop_size      = 400;			(* maximum size of population I can handle *)
      num_of_best       =   5;			(* number of best structures saved *)
      crossover_prob    = 1.0;			(* probability of crossing a selected pair *)
 
   type
      segment_coords = record
         x1, y1,
	 x2, y2: real
      end;
      structure_type = record
         cells:  packed array [2..max_line_segments] of 0..max_line_segments-1;
	 payoff: real
      end;
      direction_type = (forwards, backwards);
  
   const
      start_end_coordinates:	segment_coords:= (0.0, 0.0, 0.0, 0.0);
      direction_label:		array [direction_type] of string[3] := ('for', 'bac');
  
   var
      num_lines:		0..max_line_segments;
      coords:			array [0..max_line_segments+1] of segment_coords;
      answer:			string[3];
  
      pop_size,
      original_pop_size:	0..max_pop_size;
      population,
      temp_population:		array[1..max_pop_size] of structure_type;
 
      exploitation_coef,
      mutation_prob,
      best, worst, average:	real;
      enough:			boolean;
      gen_number:		0..10000;
      
      best_structure:		array [1..num_of_best] of structure_type;
      permutation:		array [-2..max_line_segments+1] of 0..max_line_segments;
      cell_width:		array [2..max_line_segments] of 1..max_width;
      sum_cell_width:		0..max_width * max_line_segments;
      chosen_directions:	array [0..max_line_segments+1] of direction_type;
$PAGE pen_up_distance
procedure pen_up_distance (var structure: structure_type);
  
   var
      i:		0..max_line_segments;
      j:		1..max_line_segments;
      prev_x, prev_y,
      lev1_x1, lev1_y1,
      lev1_x2, lev1_y2,
      lev2_x1, lev2_y1,
      lev2_x2, lev2_y2,
      lev3_x1, lev3_y1,
      lev3_x2, lev3_y2,
  
      to_1_forwards, to_1_backwards,
      from_1_for_to_2_for,
      from_1_for_to_2_bac,
      from_1_bac_to_2_for,
      from_1_bac_to_2_bac,
      from_2_for_to_3_for,
      from_2_for_to_3_bac,
      from_2_bac_to_3_for,
      from_2_bac_to_3_bac:	real;
  
   begin
      
      (* Construct the permutation from the member structure.  Permutation [num_lines]
	 will be the number in the coords list of the first segment to plot, and
	 permutation[1] that of the last.
	 
	 See R. Sedgewick: "Permutation Generation Methods"  in ACM Computing Surveys 9:2  *)
  
      permutation[1] := 1;
      with structure do
	 for i := 2 to num_lines do begin
	    for j := 1 to i-1 do
	       if permutation[j] >= (cells[i] + 1) then
		  permutation[j] := permutation[j] + 1;
	    permutation[i] := cells[i] + 1
	 end;
      
      
      (* Run through the plot's line segments in the order specified by permutation, and add up
         the pen-up distances getting from the end of each segment to the beginning or the next.
  
         On each pass we are calculating the distance from where the pen was left after the
         previous segment to the start of the current segment.  Since the first segment
         (specified by permutation [num_lines]) has no predecessor, we faked one by setting
         permutation [num_lines + 1] to specify coords [0].  Coords [0] was initialized to
         look like a zero-length segment starting and ending at the point where we require
         the pen to begin and finish at.  Similarly, to take care of the distance from the
         last segment (specified by permutation [1]) back to that point, the loop runs
         num_lines + 1 times rather than num_lines, and permutations [0] was set to specify
         coords [num_lines + 1] which was initialized identically with coords [0].   *)
  
      structure.payoff := 0.0;
  
      for i := num_lines downto 0 do begin
  
         (* Choose the best direction in which to traverse the current segment, so that we know
            which endpoint to use in calculating the pen-up distance from the previous segment
            to the current one.  This is done by a three level lookahead, "hard coded" here due to
            the extremely heavy use made of this code sequence.  So that we can "look ahead" from
            the last segment (specified by permutation [1]) and from the one faked by
            permutation [0], we utilize the additional identically faked segments specified
            by permutations [-1] and [-2].
  
            When the best direction for the current segment has been chosen, it is recorded
            in the vector chosen_directions, indexed the same as permutation (i.e. backwards).  *)
  
	 with coords [permutation [i+1]] do
	    if chosen_directions [i+1] = forwards then begin
	       prev_x := x2;
	       prev_y := y2
	    end
	    else begin
	       prev_x := x1;
	       prev_y := y1
	    end;
  
	 with coords [permutation [i]] do begin
	    lev1_x1 := x1;
	    lev1_y1 := y1;
	    lev1_x2 := x2;
	    lev1_y2 := y2
	 end;
	 with coords [permutation [i-1]] do begin
	    lev2_x1 := x1;
	    lev2_y1 := y1;
	    lev2_x2 := x2;
	    lev2_y2 := y2
	 end;
	 with coords [permutation [i-2]] do begin
	    lev3_x1 := x1;
	    lev3_y1 := y1;
	    lev3_x2 := x2;
	    lev3_y2 := y2
	 end;
  
	 to_1_forwards := sqrt (sqr (lev1_x1 - prev_x) + sqr (lev1_y1 - prev_y));
	 to_1_backwards := sqrt (sqr (lev1_x2 - prev_x) + sqr (lev1_y2 - prev_y));
  
	 from_1_for_to_2_for := sqrt (sqr (lev2_x1 - lev1_x2) + sqr (lev2_y1 - lev1_y2));
	 from_1_for_to_2_bac := sqrt (sqr (lev2_x2 - lev1_x2) + sqr (lev2_y2 - lev1_y2));
	 from_1_bac_to_2_for := sqrt (sqr (lev2_x1 - lev1_x1) + sqr (lev2_y1 - lev1_y1));
	 from_1_bac_to_2_bac := sqrt (sqr (lev2_x2 - lev1_x1) + sqr (lev2_y2 - lev1_y1));
  
	 from_2_for_to_3_for := sqrt (sqr (lev3_x1 - lev2_x2) + sqr (lev3_y1 - lev2_y2));
	 from_2_for_to_3_bac := sqrt (sqr (lev3_x2 - lev2_x2) + sqr (lev3_y2 - lev2_y2));
	 from_2_bac_to_3_for := sqrt (sqr (lev3_x1 - lev2_x1) + sqr (lev3_y1 - lev2_y1));
	 from_2_bac_to_3_bac := sqrt (sqr (lev3_x2 - lev2_x1) + sqr (lev3_y2 - lev2_y1));
  
	 if min (to_1_forwards + from_1_for_to_2_for + from_2_for_to_3_for,
		 to_1_forwards + from_1_for_to_2_for + from_2_for_to_3_bac,
		 to_1_forwards + from_1_for_to_2_bac + from_2_bac_to_3_for,
		 to_1_forwards + from_1_for_to_2_bac + from_2_bac_to_3_bac)
	     <=
	    min (to_1_backwards + from_1_bac_to_2_for + from_2_for_to_3_for,
		 to_1_backwards + from_1_bac_to_2_for + from_2_for_to_3_bac,
		 to_1_backwards + from_1_bac_to_2_bac + from_2_bac_to_3_for,
		 to_1_backwards + from_1_bac_to_2_bac + from_2_bac_to_3_bac) then begin
	    chosen_directions [i] := forwards;
	    structure.payoff := structure.payoff + to_1_forwards
         end
	 else begin
	    chosen_directions [i] := backwards;
	    structure.payoff := structure.payoff + to_1_backwards
	 end
      end (* for i *);
   end (* pen_up_distance *);
$PAGE read_and_initialize
procedure read_and_initialize;

   var
      filename:		string[30];
      i:		1..max_line_segments;
      member,
      preload:		1..max_pop_size;
      pos,
      span:		1..max_line_segments;
      with_pen_up,
      with_pen_down:	real;
      ibest:		1..num_of_best;
      log:		1..max_width;

   begin
  
      write (ttyoutput, 'file containing coordinates-- ');
      break (ttyoutput);
      readln (tty);
      read (tty, filename);

      writeln (ttyoutput);
      write (ttyoutput, 'initial population size-- ');
      break (ttyoutput);
      readln (tty);
      read (tty, pop_size);
      original_pop_size := pop_size;

      writeln (ttyoutput);
      write (ttyoutput, 'exploitation coefficient-- ');
      break (ttyoutput);
      readln (tty);
      read (tty, exploitation_coef);

      writeln (ttyoutput);
      write (ttyoutput, 'mutation probability-- ');
      break (ttyoutput);
      readln (tty);
      read (tty, mutation_prob);
  
      (* Read segment endpoint coordinates into the coords array.  Note that the real coordinate
	 pairs will be in coords[1] through coords[num_lines].  Coords[0] and coords[num_lines+1]
	 will be set equal to ((0.0, 0.0), (0.0, 0.0)) corresponding to starting the pen at,
	 and returning it to, (0.0, 0.0).  This is convenient in the pen-up-distance calculations,
	 and will make it easy to handle start-end points other than (0.0, 0.0) in the future. *)
  
      reset (input, filename);
      num_lines := 0;
      coords [0] := start_end_coordinates;
      while not eof(input) do begin
	 if num_lines = max_line_segments then begin
	    writeln (ttyoutput, '?Too many line segments - maximum is', max_line_segments:5,'.');
	    stop
	 end;
	 num_lines := num_lines + 1;
	 with coords [num_lines] do
	    readln (input, x1, y1, x2, y2)
      end;
      coords [num_lines + 1] := start_end_coordinates;
      close (input);
  
      (* Construct a "base structure" corresponding to performing the plot just as
         it was presented.  *)
  
      with population [1] do
	 for i := 2 to num_lines do
	    cells [i] := 0;
      pen_up_distance (population [1]);

      page (ttyoutput);
      writeln (ttyoutput, 'The plot to be optimized:');
      writeln (ttyoutput,
		    '                                                            directions');
      writeln (ttyoutput,
		    'segment      start point            end point           given       "best"');
      writeln (ttyoutput);
      for i := 1 to num_lines do
	 with coords[i] do
	    writeln (ttyoutput, i:4, '  (',x1:9:4, ',', y1:9:4,')  (',x2:9:4, ',', y2:9:4,')',
			        direction_label [forwards]:10,
				direction_label [chosen_directions [num_lines - i + 1]]:12 );
      writeln (ttyoutput);
 
  
      with_pen_down := 0.0;
      with_pen_up := sqrt ( sqr (coords[1].x1 - coords[0].x2) + sqr (coords[1].y1 - coords[0].y2));
      for i := 1 to num_lines do
         with coords[i] do begin
            with_pen_down := with_pen_down + sqrt ( sqr(x2 - x1) + sqr(y2 - y1) );
	    with_pen_up := with_pen_up + sqrt( sqr(coords[i+1].x1 - x2) + sqr(coords[i+1].y1 - y2))
	 end;
      writeln (ttyoutput);
      writeln (ttyoutput, 'The distances are:                       pen up:  ',
					       with_pen_up:12:2, population [1].payoff:12:2);
      writeln (ttyoutput, '                                         pen down:',
					       with_pen_down:12:2, with_pen_down:12:2);
      writeln (ttyoutput, '                                                  ',
					       '  ----------', '  ----------');
      writeln (ttyoutput, '                                         total:   ',
					       with_pen_down + with_pen_up:12:2,
					       with_pen_down + population [1].payoff:12:2);
      writeln (ttyoutput);
      writeln (ttyoutput, 'initial population size: ', pop_size:4);
      writeln (ttyoutput, 'exploitation coefficient: ', exploitation_coef:5:1);
      writeln (ttyoutput, 'crossover probability: ', crossover_prob:6:2);
      writeln (ttyoutput, 'mutation probability: ', mutation_prob:6:2);
   
  
      (* Preload the population with a few copies of the "base structure", and then
	 construct the rest of the population randomly. *)
  
      preload := max (trunc (0.05 * pop_size + 0.5), 1);
      for member := 2 to preload do
         population [member] := population [1];
  
      for member := preload + 1 to pop_size do begin
         with population [member] do
	    for i := 2 to num_lines do
	       cells[i] := trunc (random * i);
	 pen_up_distance (population [member])
      end;
  
      for ibest := 1 to num_of_best do		(* initialize table of best structures seen *)
	 best_structure [ibest] := population [1];
  
  
      (* Create table of actual utilized bit widths of cells.  Cell_width[i] will be number 
	 of bits actually used in cell[i] where 2 <= i <= num_lines.	*)
  
      log := 1;					(* number of bits used in current "span" of cells*)
      span := 1;				(* number of cells that each utilize "log" bits *)
      pos := 1;					(* last cell in previous "span" of cells *)
      loop
         for i := pos + 1 to pos + span do	(* fill in next "span" of cells *)
            cell_width [i] := log;
         pos := pos + span;			(* advance to end of current span *)
      exit if pos >= num_lines;
         log := log + 1;			(* each cell in next span requires one bit more *)
         span := span + span			(* next span will be twice as wide *)
      end;
  
      sum_cell_width := 0;
      for i := 2 to num_lines do		(* add up number of bits really in use *)
         sum_cell_width := sum_cell_width + cell_width [i];
      
  
      (* Setup some things for convenience within pen_up_distance.  See that
         routine for a detailed explanation.  *)
  
      permutation [num_lines + 1] := 0;			(* pen is to start off at coords [0] *)
      permutation [0] := num_lines + 1;			(* pen ends up at coords [num_lines+1] *)
      permutation [-1] := num_lines + 1;		(* to avoid having to special case ... *)
      permutation [-2] := num_lines + 1;		(* ... lookahead from last two segments *)
      chosen_directions [num_lines + 1] := forwards;	(* for convenience *)
  
   end; (* read_and_initialize *)
$PAGE payoff_stats
procedure payoff_stats (var best, worst, average: real);
  
   var 
      member:	1..max_pop_size;
      total:	real;
      i,
      index:	1..num_of_best + 1;
  
   procedure check_best;
      begin
	 index := num_of_best + 1;
	 while (index > 1) andif (best <= best_structure [index - 1].payoff) do
	    index := index - 1;
	 if (index = num_of_best + 1) orif (best = best_structure [index].payoff) then return;
	 if index < num_of_best then
	    for i := num_of_best downto index + 1 do
	       best_structure [i] := best_structure [i-1];
	 best_structure [index] := population [member]
      end;
  
   begin
      best := population [1].payoff;
      worst := population [1].payoff;
      total := 0;
      for member := 1 to pop_size do
	 with population [member] do begin
	    total := total + payoff;
	    if payoff < best then begin
	       best := payoff;
	       check_best
	    end
	    else if payoff > worst then
	       worst := payoff
	 end;
      average := total / pop_size
   end;
$PAGE next_generation
procedure next_generation;
 
   var
      member,
      first_pick,
      second_pick:	1..max_pop_size;
      offspring,
      cent_force:	real;
      temp_pop_size:	0..max_pop_size;
      taken:		array [1..max_pop_size] of boolean;
      i,
      cell_before:	1..max_line_segments;
      bit_in_cell:	0..max_width-1;
      xpoint,
      bits_before:	0..max_width * max_line_segments;
      new_cell,
      source_cell: record
	 case boolean of
	    true:  (cell_val: 0..max_line_segments-1);
	    false: (bit: packed array [1..36] of 0..1)
      end;
  
  
   function picked_member: 1..max_pop_size;
  
      var
         pick, member, count: 0..max_pop_size;
  
      begin
         pick := trunc (random * temp_pop_size + 1);	(* choose one of remaining structures *)
  
         member := 0;
         count := 0;
         repeat
            member := member + 1;
            if not taken[member] then
	       count := count + 1			(* count through remaining structures *)
	 until count = pick;
  
         picked_member := member;
         taken[member] := true;  			(* sampling is without replacement *)
         temp_pop_size := temp_pop_size - 1
      end (* picked_member *);
  
  
   procedure cross_in_cell (target_mem: 1..max_pop_size;
			    source_val: 0..max_line_segments-1;
			    cross_bit:  1..max_width-1);
      var
         ibit: 37-maximum(cross_bit)..36;
  
      begin
	 source_cell.cell_v source_val;
	 new_cell.cell_val := population [target_mem].cells [cell_before + 1];
	 for ibit := 36 downto 37 - cross_bit do
	    new_cell.bit [ibit] := source_cell.bit [ibit];
	 if new_cell.cell_val <= cell_before then	(* check range *)
	    population [target_mem].cells [cell_before + 1] := new_cell.cell_val
      end (* cross_in_cell *);
  
  
      procedure mutate (target_mem: 1..max_pop_size);
  
         var
            mut_point: 1..max_width * max_line_segments;
            mut_bit:   37-max_width..36;
            cell:      1..max_line_segments;
  
         begin
	    mut_point := trunc ((random * sum_cell_width) + 1); (* select bit to hit *)
	    cell := 2;
	    bits_before := 0;
	    while (bits_before + cell_width[cell]) < mut_point do begin
	       bits_before := bits_before + cell_width[cell];
	       cell := cell + 1
	    end;
	    source_cell.cell_val := population[target_mem].cells[cell];
	    mut_bit := 37 - (mut_point - bits_before);
	    source_cell.bit[mut_bit] := (source_cell.bit[mut_bit] + 1) mod 2;
	    if source_cell.cell_val < cell then begin (* check range *)
	       population[target_mem].cells[cell] := source_cell.cell_val;
	    end
	 end (* mutate *);
$PAGE
   begin (* next generation *)
  
  
      (* reproduction phase *)
  
      temp_pop_size := 0;
      cent_force := (original_pop_size - pop_size) / original_pop_size * 0.5;
							(* centering force on pop. size *)
      for member := 1 to pop_size do begin
  
         (* If the object was to maximize payoff (pen-up distance), offspring = payoff / average
            would be the desired function.  But since the object here is to minimize payoff,
            imagine the payoff function inverted about the line y = average, and then compute
            offspring to maximize this new "payoff" function.  *)
  
         offspring := 2 - population [member].payoff / average;
  
         offspring := 1.0 + exploitation_coef * (offspring - 1.0);
							(* emphasize distance from 1.0 *)
         offspring := offspring + cent_force;		(* and apply centering force *)
  
         while offspring >= 1.0 do begin
	    temp_pop_size := temp_pop_size + 1;
	    temp_population [temp_pop_size] := population [member];
	    offspring := offspring - 1.0
	 end;
	 if offspring > 0.0 then
	    if random <= offspring then begin
	       temp_pop_size := temp_pop_size + 1;
	       temp_population [temp_pop_size] := population [member]
	    end
  
      end;
$PAGE
      (* cross over phase *)
    
      pop_size := 0;
      for member := 1 to temp_pop_size do
	 taken [member] := false;
  
      while temp_pop_size > 1 do begin
  
         first_pick := picked_member;
         second_pick := picked_member;
  
         population [pop_size + 1] := temp_population [first_pick];
         population [pop_size + 2] := temp_population [second_pick];
  
	 if random <= crossover_prob then begin
	    xpoint := trunc (random * (sum_cell_width - 1) + 1);
	    cell_before := 2;
	    bits_before := cell_width [2];
	    while ((bits_before + cell_width [cell_before + 1]) <= xpoint)
		   and (cell_before < num_lines)  do begin
	       cell_before := cell_before + 1;
	       bits_before := bits_before + cell_width [cell_before]
	    end;
	    for i := num_lines downto (cell_before + 1) do begin
	       population [pop_size + 1].cells[i] :=
		  temp_population [second_pick].cells[i];
	       population [pop_size + 2].cells[i] :=
		  temp_population [first_pick].cells[i]
	       end;
	       bit_in_cell := xpoint - bits_before;
	       if bit_in_cell > 0 then begin
		  cross_in_cell (pop_size + 1,
				 temp_population [first_pick].cells [cell_before + 1],
				 bit_in_cell);
		  cross_in_cell (pop_size + 2,
				 temp_population [second_pick].cells [cell_before + 1],
				 bit_in_cell)
	       end
	 end;
	 if random <= mutation_prob then
	    mutate (pop_size + 1);
	 if random <= mutation_prob then
	    mutate (pop_size + 2);
	 pen_up_distance (population [pop_size + 1]);
	 pen_up_distance (population [pop_size + 2]);
	 pop_size := pop_size + 2
      
      end; (* while *)
  
      if temp_pop_size > 0 then begin
	 first_pick := picked_member;
	 pop_size := pop_size + 1;
	 population [pop_size] := temp_population [first_pick];
	 pen_up_distance (population [pop_size])
      end
  
   end (* next generation *);
$PAGE dump_best_seen
procedure dump_best_seen;
  
   var
      ibest:	1..num_of_best;
      p1,
      p2:	1..max_line_segments;
  
   begin
  
      writeln (ttyoutput);
      writeln (ttyoutput, 'Best structures seen:');
  
      for ibest := 1 to num_of_best do begin
  
	 permutation [1] := 1;
	 with best_structure [ibest] do
	    for p1 := 2 to num_lines do begin
	       for p2 := 1 to p1 - 1 do
		  if permutation [p2] >= (cells[p1] + 1) then
		     permutation [p2] := permutation [p2] + 1;
	       permutation [p1] := cells[p1] + 1
	    end;
  
	 writeln (ttyoutput);
	 write (ttyoutput, best_structure [ibest].payoff:8:2);
	 for p1 := num_lines downto 1 do
	    write (ttyoutput, permutation[p1]:4);
  
	 pen_up_distance (best_structure [ibest]);	(* recalculate to get directions *)
	 writeln (ttyoutput);
	 write (ttyoutput, ' ':8);
	 for p1 := num_lines downto 1 do
	    write (ttyoutput, direction_label [chosen_directions [p1]]:4);
	 writeln (ttyoutput)
  
      end
  
   end;
$PAGE mainline
begin
   open (tty);					(* open terminal for input ... *)
   rewrite (ttyoutput);				(* ... and for output          *)
   read_and_initialize;
  
   payoff_stats (best, worst, average);
   gen_number := 0;
   writeln (ttyoutput);
   writeln (ttyoutput, 'generation   0: best, worst, ave. =', best:8:2, worst:8:2, average:8:2);
  
   repeat
  
      page (ttyoutput);
      writeln (ttyoutput, 'generation      best       worst     average    pop. size');
      writeln (ttyoutput, '----------      ----       -----     -------    ---------');
  
      repeat					(* run through ten generations *)
	 break (ttyoutput);
	 next_generation;
	 payoff_stats (best, worst, average);
	 gen_number := gen_number + 1;
         writeln (ttyoutput, gen_number:7, best:13:2, worst:12:2, average:12:2, pop_size:10)
      until (gen_number mod 10) = 0;
  
      dump_best_seen;
  
      if best = worst then
	 enough := true				(* population has converged *)
      else begin
	 writeln (ttyoutput);
	 write (ttyoutput, 'stop? ');
	 break (ttyoutput);
	 readln (tty);
	 read (tty, answer);
	 enough := uppercase (answer[1]) = 'Y'
      end
  
   until enough;
   page (ttyoutput)
end.
 Ah4y