$TITLE SCNREA - SCANNR Regular Expression Allocation
$LENGTH 43

(*   +--------------------------------------------------------------+
     |                                                              |
     |                         S C N R E A                          |
     |                         - - - - - -                          |
     |                                                              |
     +--------------------------------------------------------------+
     
     MDSI, Company Confidential
     
     SYSTEM:  SCANNR Lexical Scanner Builder
     
     STARTED:  23 June 1978
     
     PURPOSE:  This module is responsible for the creation of regular
        expression  nodes,  for  maintaining the references counts in
        them,  and  for  deleting  them  when  they  are  no   longer
        referenced.  All regular expression nodes contain a reference
        count.  This count is set to zero when the node  is  created.
        Whenever a pointer to the node is stored somewhere (either in
        a variable or  in  another  node),  the  reference  count  is
        incremented.  When  such  a pointer is changed, the reference
        count is decremented.  When the count reaches zero, the  node
        is automatically deleted.
     
     ENTRY POINTS:
     
        new_re      returns  a  pointer  to  a  newly created regular
                    expression node with a  specified  operator.  The
                    reference count of the node is zero, and the l_in
                    field of the node (which  indicates  whether  the
                    regular  expression  contains lambda) is set to a
                    specified value.
     
        use_re      takes a pointer to a node, and returns a  pointer
                    to  the  same node.  However, the reference count
                    in the node will have been incremented.  Whenever
                    a  pointer  to  a node is stored in a variable or
                    another node, use_re should be called.
     
        free_re     is called when a pointer to a  node  is  changed.
                    It  decrements  the  reference count in the node.
                    If the refernce count thus becomes zero, then the
                    last  reference to the node has just been erased,
                    and the node may be disposed.
     
        test_re     is called when a pointer to  a  node,  which  may
                    just  have been created, is NOT stored somewhere.
                    If the reference count of the node is zero,  then
                    it is deleted directly.
     
        del_re      is  called  to  dispose  of  a regular expression
                    node, without looking at its operands.
     
     NOTES:   The  following  conditional  compilation  switches  are
        defined:
        CHAR  controls  the basic data type of the regular expression
             type.  It is used in SCNRE.TYP.
     
        TRACING causes a record to be written to the file  SCANNR.DMP
             every  time  one  of  these routines is called.  This is
             purely a debugging option.
     
        TESTING causes a summary  of  the  allocator  actions  to  be
             written  to the terminal when ReaFinish is called.  This
             is also a debuging option.
     
        CHECKING causes counts of the various allocator actions to be
             kept,  and generates an assertion in ReaFinish about the
             relations between those counts.  This option may be used
             in  production  code, or at least semi-debugged code, to
             detect errors  in  the  use  of  the  allocator  module.
             Actually tracking down those bugs generally requires the
             use of the TESTING or TRACING options.
     
     ---------------------------------------------------------------- *)
$PAGE declarations

$INCLUDE scannr.typ
$INCLUDE scnre.typ



$IFANY (testing, checking)
  var n_allocated,
      n_freed,
      use_calls,
      free_calls,
      test_calls,
      del_calls: integer;
$ENDIF
$PAGE open_trace

(*  OPEN TRACE is called to make sure that the trace file is open.  *)

$IF tracing

var trace_file: text := nilf;

procedure open_trace;

begin
  if trace_file = nilf then
    rewrite (trace_file, 'SCANNR.TRC');
end;

$ENDIF
$PAGE rea_init

(*  ReaInit initializes the module.  It only does something if the TRACING
    or TESTING flag is enabled.  *)

public procedure rea_init;

begin

$IFANY (testing, checking)
  n_allocated := 0;
  n_freed := 0;
  use_calls := 0;
  free_calls := 0;
  test_calls := 0;
  del_calls := 0;
$ENDIF

$IF tracing
  open_trace;
$ENDIF

end (* rea_init *);
$PAGE rea_finish

(*  ReaFinish terminates the module.  It only has an effect if the TESTING
    flag is enabled.  *)

public procedure rea_finish;

begin

$IF testing
  writeln (tty);
  writeln (tty, 'Allocator Counts:');
  writeln (tty, '  ', n_allocated, ' regular expressions created');
  writeln (tty, '  ', n_freed, ' regular expressions released');
  writeln (tty, '  USE  called ', use_calls, ' times');
  writeln (tty, '  FREE called ', free_calls, ' times');
  writeln (tty, '  TEST called ', test_calls, ' times');
  writeln (tty, '  DEL  called ', del_calls, ' times');
  writeln (tty);
$ENDIF

$IF checking  assert (n_allocated = n_freed);

end (* rea_finish *);
$PAGE new_re

(*  All regular expression nodes are created by NewRe.  The type of expression
    and the value of the l_in field must be specified.  *)

public function new_re ( kind: reg_ops; lambda_in: boolean ): reg_exp;

$IF tracing
  const re_names: array [reg_ops] of string [6] :=
      (  'LAMBDA', 'PHI', 'LIT', 'LITRNG', 'STAR', 'NOT', 'CAT', 'OR', 'AND'  );
$ENDIF

begin

$IFANY (testing, checking)  n_allocated := n_allocated + 1;

  case kind of
    star_op:	   new ( new_re, star_op );
    not_op:	   new ( new_re, not_op );
    or_op:	   new ( new_re, or_op );
    and_op:	   new ( new_re, and_op );
    cat_op:	   new ( new_re, cat_op );
    literal_op:	   new ( new_re, literal_op );
    lit_range_op:  new ( new_re, lit_range_op );
    lambda_op:	   new ( new_re, lambda_op );
    phi_op:	   new ( new_re, phi_op )
  end;

$IF tracing  writeln (trace_file, 'NewRe :  Node at ', ord (new_re):6:o, ' created:  ', re_names[kind]);

  with new_re^ do begin
    l_in := lambda_in;
    refs := 0;
  end;
end (* new_re *);
$PAGE use_re

(*  Whenever a pointer to a regular expression node is stored somewhere,
    UseRe must be called to record the fact that there is now another
    reference to the node in existence.  *)

public function use_re ( re: reg_exp ): reg_exp;

begin

$IF testing  use_calls := use_calls + 1;

  use_re := re;
  with re^ do
    refs := refs + 1;

$IF tracing  writeln (trace_file, 'UseRe :  Node at ', ord (re):6:o, ' used');

end (* use_re *);
$PAGE free_re

(*  Whenever a stored pointer to a regualr expression node is erased, FreeRe
    must be called to record the fact that there is now one less reference
    to the node, and to dispose of the node entirely if it is no longer
    needed.  *)

public procedure free_re ( re: reg_exp );

begin

$IF testing  free_calls := free_calls + 1;
$IF tracing  writeln (trace_file, 'FreeRe:  Node at ', ord (re):6:o, ' freed');

  with re^ do begin
    assert (refs <> 0);
    refs := refs - 1;
    if refs = 0 then begin

$IFANY (testing, checking)  n_freed := n_freed + 1;
$IF tracing  writeln (trace_file, 'FreeRe:  Node at ', ord (re):6:o, ' released');

      case reg_op of
	star_op, not_op:
	  free_re (operand);
	cat_op, or_op, and_op:
	  begin
	    free_re (leftop);
	    free_re (rightop);
	  end;
	others:
	  (* no operands *)
      end;
      dispose ( re );
    end;
  end;
end (* free_re *);
$PAGE test_re

(*  A node which is created and immediately passed as an operand to one of the
    SCNREU constructor routines may never be stored anywhere, and consequently
    may never be USEd and then FREEd.  When this is possible, such a node is
    passed to TestRe, which will delete it if there are no references to it.  *)

public procedure test_re ( re: reg_exp );

begin

$IF testing  test_calls := test_calls + 1;
$IF tracing  writeln (trace_file, 'TestRe:  Node at ', ord (re):6:o, ' tested');

  re^.refs := re^.refs + 1;
  free_re (re);
end;
$PAGE del_re

(*  If an allocated regular expression node is to be freed, and its operands
    have never been properly initialized, then it may be passed to DelRe, which
    simply disposes it.  *)

public procedure del_re ( re: reg_exp );

begin

$IF testing  del_calls := del_calls + 1;
$IFANY (testing, checking)  n_freed := n_freed + 1;
$IF tracing  writeln (trace_file, 'DelRe :  Node at ', ord (re):6:o, ' disposed');

  assert (re^.refs = 0);
  dispose (re);
end.
  