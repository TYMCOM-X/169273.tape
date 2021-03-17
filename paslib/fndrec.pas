$TITLE FNDREC -- FIND Recognizer Module

module fndrec;
$PAGE pattern manipulation
type
    pattern = ^ pattern_node;
    relation = ( lss, eql, gtr );

external var phi: pattern;

external function re_compare ( pattern; pattern ): relation;
external function lithead ( pattern; var char; var char; var pattern ): boolean;
external function derivative ( pattern; char ): pattern;
external function re_null ( pattern ): boolean;

external procedure use_re ( pattern );
external procedure free_re ( pattern );
$PAGE declarations
(*  A recognizer comprises a transition matrix, a boolean vector indicating
    which states are live states, and a boolean vector indiicating which
    states are accepting states.  *)

type
    stnumber = 0 .. 4095; (* State numbers. *)

var recognizer: ^ array [1..*] of packed record
	delta: packed array [char] of stnumber;
	alive, accept: boolean;
    end;

    current_pattern: pattern; (* Pattern of the current recognizer. *)
$PAGE clr_recognizer
(*  CLR RECOGNIZER merely sets TransitionMatrix to Nil as an indicator that
    no recognizer has been defined.  *)

public procedure clr_recognizer;

begin
  recognizer := nil;
end;
$PAGE make_recognizer
(*  MAKE RECOGNIZER constructs a finite-state recognizer for the pattern Pat.
    It does this by associating a pattern with each state of the recognizer,
    so that, if S is a state, A is a character, Delta is the transition
    function, D is the pattern derivative function, and Ch is a function
    which returns the characteristic pattern of each state, then

	Ch(Delta(S,A)) = D(Ch(S),A) .					*)

procedure make_recognizer ( pat: pattern );

type
    state = ^ state_node;
    state_list = ^ state_list_node;

    state_node = packed record
	index: stnumber; (* The state number. *)
	dead: boolean; (* This is a dead state. *)
	accepting: boolean; (* This is an accepting state. *)
	next: state; (* Chains together all the states in the FSA. *)
	left, right: state; (* States are sorted in a binary tree. *)
	ch: pattern; (* The characteristic pattern of the state. *)
	preds: state_list; (* All predecessor states. *)
	delta: packed array [char] of state; (* The transition function. *)
    end;

    state_list_node = packed record
	next: state_list;
	st: state;
    end;

var first_state: state;
    nstates: stnumber; (* The number of live states in the FSA. *)
$PAGE create_the_states
(*  CREATE THE STATES builds the internal form of the FSA.  When this routine
    is finished, FirstState will point to the head of a list of states.  Each
    will have its final Next, Ch and Preds field values.  Delta will be the
    state transition function.  All states will be marked as dead.  *)

procedure create_the_states;

var last_state: state;
$PAGE new_state
(*  NEW STATE creates a new state with a specified characteristic pattern.  *)

function new_state ( new_ch: pattern ): state;

begin
  new (new_state);
  with new_state^ do begin
    dead := true; (* Dead until proven otherwise. *)
    accepting := re_null (new_ch);
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
$PAGE add_state
(*  ADD STATE searches the state tree to see if there is already a state for
    a specified pattern.  If so, it returns that state.  Otherwise, it creates
    a new state.  *)

function add_state ( new_ch: pattern ): state;

var rel: relation;

begin
  use_re (new_ch);
  add_state := first_state;
  repeat
    rel := re_compare (new_ch, add_state^.ch);
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
	free_re (new_ch); (* Pattern already has a state. *)

    end (* case re_compare *);
  until rel = eql;
end (* add_state *);
$PAGE set_predecessor
(*  SET PREDECESSOR records the relation that state PS is a predecessor of
    state S.  *)

procedure set_predecessor ( s, ps: state );

var pred: state_list;

begin
  if (s^.preds = nil) orif (s^.preds^.st <> ps) then begin
    new (pred);
    pred^.st := ps;
    pred^.next := s^.preds;
    s^.preds := pred;
  end;
end (* set_predecessor *);
$PAGE create_the_states - main routine
var st, next_st, phi_st: state;
    a: char;
    minlit, maxlit: char;
    rest: pattern;

begin
  last_state := nil;
  first_state := new_state (pat);
  use_re (pat);

  st := first_state;
  while st <> nil do begin
    if lithead (st^.ch, minlit, maxlit, rest) then begin
      next_st := add_state (rest);
      if (minlit > minimum (char)) or (maxlit < maximum (char)) then
	phi_st := add_state (phi);
      for a := minimum (char) to maximum (char) do begin
	if a in [minlit..maxlit]
	  then st^.delta[a] := next_st
	  else st^.delta[a] := phi_st;
      end;
      set_predecessor (next_st, st);
      if (minlit > minimum (char)) or (maxlit < maximum (char)) then
	set_predecessor (phi_st, st);
    end
    else begin
      for a := minimum (char) to maximum (char) do begin
	next_st := add_state (derivative (st^.ch, a));
	st^.delta[a] := next_st;
	set_predecessor (next_st, st);
      end;
    end;
    st := st^.next;
  end (* while st <> nil *);
end (* create_the_states *);
$PAGE find_the_dead_states
(*  FIND THE DEAD STATES will determine the live and dead states of the FSA.
    The rule is that a state S is live if it has a successor SS such that S
    is an accepting state and SS is not, or SS is an accepting state and S is
    not, or SS is a live state.  At entry, all of the states of the FSA have
    been marked as dead.  This routine first finds all the explicitly live
    states, marks them as alive, and pushes them on a stack.  It then pops
    the stack repeatedly, marking and pushing any dead predecessors of each
    live state.  *)

procedure find_the_dead_states;

var live_stack, live_state, pred: state_list;
    st: state;

begin
  live_stack := nil;
  st := first_state;
  while st <> nil do begin

    (*  If the state has a predecessor of different type, the mark the
	predecessor and push it on the stack.  *)

    pred := st^.preds;
    while pred <> nil do begin
      if pred^.st^.accepting <> st^.accepting then begin
	pred^.st^.dead := false;
	new (live_state);
	live_state^.st := pred^.st;
	live_state^.next := live_stack;
	live_stack := live_state;
      end;
      pred := pred^.next;
    end (* while pred <> nil *);
    st := st^.next;
  end (* while st <> nil *);

  (*  LiveStack now contains all the explicitly live states.  *)

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

  (*  Dispose of any remaining predecessor lists.  *)

  st := first_state;
  while st <> nil do begin
    while st^.preds <> nil do begin
      pred := st^.preds;
      st^.preds := st^.preds^.next;
      dispose (pred);
    end;
    st := st^.next;
  end;

end (* find_dead_states *);
$PAGE number_the_states
(*  NUMBER THE STATES will set the Index field of each live state according
    to its order in the state list.  All accepting (non-accepting) dead
    states will get the state number of the first accepting (non-accepting)
    dead state in the list.  *)

procedure number_the_states;

var st: state;
    dead_states: array [boolean] of stnumber;

begin
  nstates := 0;
  dead_states := ( 0, 0 );
  st := first_state;
  while st <> nil do begin
    with st^ do begin
      if dead then begin
	if dead_states [accepting] = 0 then begin
	  nstates := nstates + 1;
	  index := nstates;
	  dead_states [accepting] := nstates;
	end
	else
	  index := dead_states [accepting];
      end
      else begin
	nstates := nstates + 1;
	index := nstates;
      end;
      st := next;
    end (* with st^ *);
  end (* while st <> nil *);
end (* number_the_states *);
$PAGE construct_the_matrix
(*  CONSTRUCT THE MATRIX will create the transition matrix and accept and
    live vectors from the previously constructed FSA.  *)

procedure construct_the_matrix;

var st: state;
    a: char;

begin
  new (recognizer, nstates);
  st := first_state;
  while st <> nil do begin
    with st^ do begin
      for a := minimum (char) to maximum (char) do
	recognizer^[index].delta[a] := delta[a]^.index;
      recognizer^[index].accept := accepting;
      recognizer^[index].alive := not dead;
      st := next;
    end;
  end (* while st <> nil *);
end (* construct_the_matrix *);
$PAGE discard_the_fsa
(*  DISCARD THE FSA will discard the internal FSA, which is no longer needed.  *)

procedure discard_the_fsa;

var st, st_next: state;
    pred: state_list;

begin
  st := first_state;
  while st <> nil do begin
    free_re (st^.ch);
    st_next := st^.next;
    dispose (st);
    st := st_next;
  end (* while st <> nil *);
end (* discard_the_fsa *);
$PAGE make_recognizer - main routine
begin
  create_the_states;
  find_the_dead_states;
  number_the_states;
  construct_the_matrix;
  discard_the_fsa;
end (* make_recognizer *);
$PAGE set_recognizer
(*  SET RECOGNIZER will test whether the current recognizer is for a specified
    pattern.  If not, then it will replace it with one that is.  *)

public procedure set_recognizer ( pat: pattern );

begin
  if recognizer = nil then begin
    make_recognizer (pat);
    current_pattern := pat;
    use_re (current_pattern);
  end
  else if re_compare (pat, current_pattern) <> eql then begin
    dispose (recognizer);
    free_re (current_pattern);
    make_recognizer (pat);
    current_pattern := pat;
    use_re (current_pattern);
  end;
end (* set_recognizer *);
$PAGE match
(*  MATCH will test whether a given string is matched by the current recognizer.  *)

public function match ( str: packed array [1..*] of char ): boolean

  options optimize;

var state: stnumber;
    i: integer;

begin
  state := 1;
  i := 1;
  while recognizer^[state].alive and (i <= length (str)) do begin
    state := recognizer^[state].delta[str[i]];
    i := i + 1;
  end;
  match := recognizer^[state].accept;
end (* match *).
 