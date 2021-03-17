$TITLE SCNFSA -- SCANNR FSA Builder

module scnfsa

    options special (ptr);

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         S C N F S A                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     MDSI, Company Confidential
     
     SYSTEM:  SCANNR Lexical Scanner Builder
     
     STARTED:  17 October 1979
     
     PURPOSE:  This module builds the finite state  automata  at  the
        various stages of processing.
     
     ENTRY POINTS:
     
        MAKE_FSA (tm, av)
                    is called by the main program to build the finite
                    state   automaton   for   the   pattern   regular
                    expressions.  On  return, TM will be a transition
                    matrix pointer, and  AV  will  be  an  acceptance
                    vector pointer.
     
     ALGORITHM:  The computation of FSA's is based on the isomorphism
        between  the  states  of  an  FSA  and  the  values  of  some
        characteristic  function.  The  central FSA builder is called
        with a characteristic function value for the initial state of
        the   FSA   to   be  built,  a  function  which  can  compare
        characteristic function values,  and  a  transition  function
        which,  given  the  characteristic  function value C(S) for a
        state S, and a symbol  A,  will  compute  the  characteristic
        function  value  C(T)  for  the state T such that there is an
        A-transition from state S to state T.
     
        The construction algorithm is straightforward:
     
        Create the initial state S0
        C(S0) := the initial characteristic
        UnprocessedStates := {S0}
        While UnprocessedStates <> {} do
            Remove a state S from UnprocessedStates
            For each input symbol A do
                C' := Transition (C(S), A)
                If there is a state S' in States with C(S') = C'
                    Then Delta(S, A) := S'
                    Else Create a new state S'
                         Delta (S, A) := S'
                         C(S') := C'
                         Add S' to UnprocessedStates
                Fi
            Od
        Od
     
     ---------------------------------------------------------------- *)
$PAGE declarations

$SYSTEM scannr.typ
$SYSTEM scnfsa.typ

$SYSTEM scnnam
$SYSTEM scnpat
$SYSTEM scnrea
$SYSTEM scnreu
$SYSTEM scnlst


type
    characteristic = ptr;

    characteristic_vector = ^ packed array [1..*] of characteristic;
$PAGE build_fsa

(*  BUILD FSA is called with:

      - The characteristic function of the initial state.

      - A test function to compare characteristic function values.

      - A transition function, which gives a new characteristic function value
	for each old characteristic function value and input symbol.

      - A predicate which tests whether a characteristic belongs to an accept
	state.

      - A procedure for discarding redundant characteristic function values.

    It returns:

      - A transition matrix for an FSA.

      - A list of characteristics function values for the states of the FSA.  *)


procedure build_fsa 

	  (   init_ch:		 characteristic;
	      ch_compare:	 function ( characteristic; characteristic ): relation;
	      transition:	 function ( characteristic; number ): characteristic;
	      accepting:	 function ( characteristic ): boolean;
	      discard:		 procedure ( characteristic );
	  var trans_mat:	 transition_matrix;
	  var ch_vec:		 characteristic_vector   );
$PAGE build_fsa - declarations

type
    state = ^ state_node;
    state_list = ^ state_list_node;

    state_node = packed record
	index: number; (* The state number. *)
	dead: boolean; (* No transition to an accept state. *)
	next: state; (* Chains together all the states in the FSA. *)
	left, right: state; (* States are sorted in a binary tree. *)
	ch: characteristic; (* The characteristic function of the state. *)
	preds: state_list; (* All predecessor states. *)
	delta: packed array [0..*] of state; (* The transition function. *)
    end;

    state_list_node = packed record
	next: state_list;
	st: state;
    end;

var first_state: state;
    nsymbols: number; (* The number of relevant input symbols. *)
    nstates: number; (* The number of live states in the FSA. *)
$PAGE create_the_states

(*  CREATE THE STATES builds the internal form of the FSA.  When this routine
    is finished, FirstState will point to the head of a list of states.  Each
    will have its final Next, Ch and Preds field values.  Delta will be the
    state transition function.  All states will be marked as dead.  *)

procedure create_the_states;

var last_state: state;
$PAGE new_state - in create_the_states - in build_fsa

(*  NEW STATE is called with a characteristic function value.  It creates
    a new state with that characteristic function.  *)

function new_state ( new_ch: characteristic ): state;

begin
  new (new_state, nsymbols);
  with new_state^ do begin
    dead := true; (* Dead until proven otherwise. *)
    next := nil;
    left := nil;
    right := nil;
    ch := new_ch;
    preds := nil; (* No known predecessors. *)
  end;
  if last_state <> nil then
    last_state^.next := new_state;
  last_state := new_state;
end (* new_state *);
$PAGE add_state - in create_the_states - in build_fsa

(*  ADD STATE is called with a characteristic function value.  It searches the
    state tree to see if there is already a state with that characteristic
    function.  If so, it returns that state.  Otherwise, it creates a new
    state.  *)

function add_state ( new_ch: characteristic ): state;

var rel: relation;

begin
  add_state := first_state;
  repeat
    rel := ch_compare (new_ch, add_state^.ch);
    case rel of

      lss:
	if add_state^.left <> nil then
	  add_state := add_state^.left
	else begin
	  add_state^.left := new_state (new_ch);
	  add_state := add_state^.left;
	  rel := eql;
	end;

      gtr:
	if add_state^.right <> nil then
	  add_state := add_state^.right
	else begin
	  add_state^.right := new_state (new_ch);
	  add_state := add_state^.right;
	  rel := eql;
	end;

      eql:
	discard (new_ch); (* Characteristic already has a state. *)

    end (* case ch_compare *);
  until rel = eql;
end (* add_state *);
$PAGE create_the_states - main routine

var st, next_st: state;
    a: number;
    pred: state_list;

begin
  last_state := nil;
  first_state := new_state (init_ch);

  st := first_state;
  while st <> nil do begin
    for a := min_symbol to max_symbol do begin
      next_st := add_state (transition (st^.ch, a));
      st^.delta[a - min_symbol] := next_st;
      if (next_st^.preds = nil) orif (next_st^.preds^.st <> st) then begin
	new (pred); (* Make St a predecessor of NextSt. *)
	pred^.st := st;
	pred^.next := next_st^.preds;
	next_st^.preds := pred;
      end;
    end;
    st := st^.next;
  end (* while st <> nil *);
end (* create_the_states *);
$PAGE find_the_dead_states

(*  FIND THE DEAD STATES will determine the live and dead states of the FSA.
    The rule is that an accepting state is live; any state with a transition
    to a live state is live; all other states are dead.  At entry, all the
    states of the FSA have been marked as dead.  This routine first finds
    all accepting states, marks them as alive, and pushes them on a stack.
    It then repeatedly pops the stack, marking and pushing any dead prede-
    cessors of each popped state.  *)

procedure find_the_dead_states;

var live_stack, live_state, pred: state_list;
    st: state;

begin
  live_stack := nil;
  st := first_state;
  while st <> nil do begin

    (*  If the state is accepting, then mark it and push it on the stack.  *)

    if accepting (st^.ch) then begin
      st^.dead := false;
      new (live_state);
      live_state^.st := st;
      live_state^.next := live_stack;
      live_stack := live_state;
    end;
    st := st^.next;
  end (* while st <> nil *);

  (*  LiveStack now contains all the accepting states.  *)

  while live_stack <> nil do begin

    (*  Pop a live state off the stack.  *)

    pred := live_stack^.st^.preds;
    live_stack^.st^.preds := nil;
    live_state := live_stack;
    live_stack := live_stack^.next;
    dispose (live_state);

    (*  Mark and push all of its dead predecessors.  *)

    while pred <> nil do begin
      live_state := pred;
      pred := pred^.next;
      if live_state^.st^.dead then begin
	live_state^.st^.dead := false;
	live_state^.next := live_stack;
	live_stack := live_state;
      end
      else
	dispose (live_state);
    end;
  end (* while live_stack <> nil *);

  (*  All unmarked states are dead.  Dispose of their predecessor lists,
      and discardd their characteristic function values.  *)

  st := first_state;
  while st <> nil do begin
    if st^.dead then begin
      while st^.preds <> nil do begin
	pred := st^.preds;
	st^.preds := st^.preds^.next;
	dispose (pred);
      end;
      discard (st^.ch);
    end;
    st := st^.next;
  end;

end (* find_dead_states *);
$PAGE number_the_states - in build_fsa

(*  NUMBER THE STATES will set the Index field of each live state according
    to its order in the state list, and will zero the Index field of each
    dead state.  *)

procedure number_the_states;

var st: state;

begin
  nstates := 0;
  st := first_state;
  while st <> nil do begin
    if st^.dead then
      st^.index := 0
    else begin
      nstates := nstates + 1;
      st^.index := nstates;
    end;
    st := st^.next;
  end;
end (* number_the_states *);
$PAGE construct_the_output_structures - in build_fsa

(*  CONSTRUCT THE OUTPUT STRUCTURES will create the transition matrix and
    characteristic vector output parameters from the FSA which has previously
    been constructed.  *)

procedure construct_the_output_structures;

var st: state;
    a: number;

begin
  new (ch_vec, nstates);
  new (trans_mat, nstates);
  st := first_state;
  while st <> nil do begin
    with st^ do begin
      if index <> 0 then begin
	ch_vec^[index] := ch;
	new (trans_mat^[index], nsymbols);
	for a := 0 to nsymbols do
	  trans_mat^[index]^[a] := delta[a]^.index;
      end;
      st := next;
    end;
  end (* while st <> nil *);
end (* construct_the_output_structures *);
$PAGE dispose_of_the_fsa - in build_fsa

(*  DISPOSE OF THE FSA will discard the internal FSA, which is no longer needed.  *)

procedure dispose_of_the_fsa;

var st, st_next: state;
    pred: state_list;

begin
  st := first_state;
  while st <> nil do begin
    st_next := st^.next;
    dispose (st);
    st := st_next;
  end (* while st <> nil *);
end (* dispose_of_the_fsa *);
$PAGE build_fsa - main routine

begin
  nsymbols := max_symbol - min_symbol;
  create_the_states;
  find_the_dead_states;
  number_the_states;
  construct_the_output_structures;
  dispose_of_the_fsa;
end (* build_fsa *);
$PAGE make_fsa

(*  MAKE FSA is called by the main program to construct the transition matrix
    and acceptance vector of an FSA to recognize all the defined patterns in
    parallel.  *)

public procedure make_fsa ( var tm: transition_matrix;
			    var av: acc_pat_vector );

var tr_matrices: ^ array [1..*] of transition_matrix;
    accept: ^ array [1..*] of acc_vector;
$PAGE make_recognizer - in make_fsa

(*  For this construction, the characteristic function value for an FSA state
    is a regular expression representing the set of strings which will be
    accepted by the FSA, starting in that state.  The characteristic regular
    expression for the initial state is obviously the regular expression to
    be recognized.  Any state whose characteristic regular expression includes
    the null string (lambda) is an accepting state of the recognizer.  *)

procedure make_recognizer ( ipat: number; (* The pattern number. *)
			var tm: transition_matrix;
			var av: acc_vector );

var char_re: re_vector; (* The characteristic regular expressions. *)
    is: number;

begin
  build_fsa ( pat_re (ipat), (* The initial regular expression. *)
	      re_compare, (* The regular expression comparison function. *)
	      derivative, (* The transition function. *)
	      re_null, (* The accepting state predicate. *)
	      test_re, (* The regular expression "discard" procedure. *)
	      tm, (* The result transition matrix. *)
	      char_re ); (* The characteristic regular expression vector. *)
  new (av, upperbound (char_re^)); (* Create the acceptance vector. *)
  for is := 1 to upperbound (char_re^) do
    av^[is] := re_null (char_re^[is]);
  list_recognizer (ipat, tm, char_re, av); (* Print the recognizer. *)
  for is := 1 to upperbound (char_re^) do (* Free up the characteristic regular *)
    test_re (char_re^[is]); (*   expressions, if possible. *)
  dispose (char_re); (* We're done with this. *)
end (* make_recognizer *);
$PAGE make_scanner - in make_fsa

(*  For this construction, the characteristic function value for an FSA state
    is a vector of the states that the individual recognizers would be in
    after some input string.  The initial state characteristic vector is
    <1, 1, ..., 1>, except that any recognizers with no accepting states
    will have a 0 instead of a 1 in the vector.  A state with characteristic
    vector <s1, s2, ..., sn> is an accepting state for the first pattern i
    such that si is an accepting state of the recognizer for pattern i.  *)

procedure make_scanner;
$PAGE state vector functions - in make_scanner - in make_fsa

(*  The state vector comparison function.  *)

function st_compare (st1, st2: states ): relation;
var i: number;
begin
  st_compare := eql;
  for i := 1 to npatterns do
    exit if st1^[i] <> st2^[i] do
      if st1^[i] < st2^[i]
	then st_compare := lss
	else st_compare := gtr;
end;


(*  The state vector transition function.  *)

function st_transition ( st: states; a: number ): states;
var i: number;
begin
  new (st_transition, npatterns);
  for i := 1 to npatterns do
    if st^[i] = 0
      then st_transition^[i] := 0
      else st_transition^[i] := tr_matrices^[i]^[st^[i]]^[a - min_symbol];
end;


(*  The state vector acceptance test.  *)

function st_accept ( st: states ): boolean;
var i: number;
begin
  st_accept := false;
  for i := 1 to npatterns do
    exit if (st^[i] <> 0) andif accept^[i]^[st^[i]] do
      st_accept := true;
end;


(*  The state vector discard function.  *)

procedure st_discard ( st: states );
begin
  dispose (st);
end;
$PAGE make_scanner - main routine - in make_fsa

var start_st: states;
    i, j: number;
    char_st: st_vector;

begin
  new (start_st, npatterns);
  for i := 1 to npatterns do begin
    if upperbound (accept^[i]^) = 0
      then start_st^[i] := 0
      else start_st^[i] := 1;
  end;
  build_fsa ( start_st,
	      st_compare,
	      st_transition,
	      st_accept,
	      st_discard,
	      tm,
	      char_st );
  new (av, upperbound (char_st^));
  for i := 1 to upperbound (char_st^) do begin
    av^[i] := 0;
    for j := 1 to npatterns do begin
      exit if (char_st^[i]^[j] <> 0) andif accept^[j]^[char_st^[i]^[j]] do
	av^[i] := j;
    end;
  end;
  list_scanner (tm, char_st, av);
  for i := 1 to upperbound (char_st^) do
    dispose (char_st^[i]);
  dispose (char_st);
end (* make_scanner *);
$PAGE make_fsa - main routine

var ipat, is: number;

begin

  (*  Construct recognizer FSAs for the individual pattern regular expressions.  *)

  new (tr_matrices, npatterns);
  new (accept, npatterns);
  for ipat := 1 to npatterns do
    make_recognizer (ipat, tr_matrices^[ipat], accept^[ipat]);
  make_scanner;
  for ipat := 1 to npatterns do begin
    dispose (accept^[ipat]);
    for is := 1 to upperbound(tr_matrices^[ipat]^) do
      dispose (tr_matrices^[ipat]^[is]);
    dispose (tr_matrices^[ipat]);
  end;
  dispose (accept);
  dispose (tr_matrices);
end (* make_fsa *).
    