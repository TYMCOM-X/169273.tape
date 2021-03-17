$TITLE LEXGEN: lexical analyzer generator
$WIDTH (107)
$SOURCE
  
  
(*      LEXGEN  -  lexical analyzer generator
  
        Ralph D. Jeffords,  University of Mississippi,  April 1979
        Version 2  (first distribution)
  
  
        Modifications by Dave Wilson:
  
            - Totally reformatted and cleaned up for readability (according to my private
              prejudices).
   
            - Minimization always done, not optional, since minimization takes only a small
              fraction of the running time.  Unminimized DELTA is not printed at all.
    
            - Representation of DFA states changed from packed arrays of booleans to arrays of
              sets, since sets are operated on a word at a time, rather than bit by bit.  Speed
              improvement was a factor of 100 on a large example.
    
            - Intermediate-backup node numbering changed to not be wasteful, as recommended in the
              implementation notes.
    
            - DELTA rows allocated as needed, and packed.  The packing saves a great deal of core
              with no apparent loss of efficiency, while allocating rows means only large DFAs
              need use a lot of core for DELTA.
    
            - Added routine to print the scanner program from Appendix B of the LEXGEN user
              manual, filling in all the constants and tables that LEXGEN has information about. 
              The additional code and tables required for the lookahead feature are included just
              if they are required.
    
            - The two options for printing NFA diagrams and graphs have been combined.
    
            - INTEGER types have been eliminated, and tag fields added to record variants that
              didn't have them, so that the "special" compiler option isn't required.
    
            - When handling semantics of productions that build terminal sets, the sets are
              kept on the stack, rather than carried "off the side", resulting in consistent
              handling of semantic information.
    
            - Ascii "delete" character used as internal end-of-line mark rather than backslash to
              avoid any interference with use of the entire printable set of characters.
    
            - MINIMIZE_DFA changed to add DFA states to end of equivalence class member lists
              rather than at the beginning, making reindexing unnecessary.
  
            - Implemented transition table compaction technique described in Aho & Ullman.  The
              user is given the choice of having the compacted or uncompacted form of DELTA
              incorporated in the output scanner program.
  
            - Bug in minimization algorithm removed.
  
          --------------------
  
            - Representation of DFA states further changed from arrays of sets to just sets
              to take advantage of the "long" set capability of the MDSI compiler.

            - The setup of nextif, the transition function of the NFAs, was leaving the
              entries for the final and intermediate-backup states unitialized, but
              was accessing those entries (unnecessarily) when performing the breadth
              first search.  It worked on the DEC 10, but not on other machines.

            - Altered scanning of input to eliminate any restriction on input line lengths.

            - Exhaustively reworked the entire program.
*)
$PAGE constant definitions
program lexgen
      options storage (8192), xref, symbols, calls;
  
const 
      maxsymlen     =   25;           (* the maximum length of a symbol *)
      maxcols       =   24;           (* max # of columns across page for delta *)
      lambda        =    0;           (* code for null *)
      maxnonter     =   40;           (* max # of nonterminals for input grammar *)
      maxtermsets   =   20;           (* max # of sets of terminals for input grammar *)
      min_term      = -107;           (* minimum code for terminals for input grammar *)
      maxifstates   =  287;           (* max # of important/backup/final states *)
      dead          =    0;           (* dead state of DFA *)
      maxdfastates  =  150;           (* max # of DFA states *)
      firstblock    =    0;           (* first equivalence class *)
      maxblocks     = maxdfastates;   (* max possible equivalence classes *)
      max_com_tab   =  750;           (* max size of compacted tables *)
      word_size     =   36;           (* bits per word *)

      (* constants used by parser: *)

      maxparsest    =   18;           (* last state of parser *)
      minparcode    = -521;           (* smallest code for parsing table *)

      meta_lbrack   =  -13;           (*     ]     *)
      meta_rbrack   =  -12;           (*     [     *)
      meta_or       =  -11;           (*     !     *)
      meta_rparen   =  -10;           (*     )     *)
      meta_lparen   =   -9;           (*     (     *)
      terminal      =   -8;           (*  ' ... '  *)
      meta_plus     =   -7;           (*     +     *)
      meta_star     =   -6;           (*     *     *)
      setname       =   -5;           (*  " ... "  *)
      meta_slash    =   -4;           (*     /     *)
      meta_define   =   -3;           (*    ::=    *)
      nonterminal   =   -2;           (*  < ... >  *)
      meta_end      =   -1;           (*     ;     *)
      eodata        =    0;           (*     $     *)

      terminalhead  =    7;           (* last nonterminal code for parser *)
$PAGE type definitions
type 
     term_range    = min_term..lambda;               (* range of terminals in input *)
     ifstateset    = set of 0..maxifstates;
     dfastaterange = dead..maxdfastates;             (* range of DFA states *)
     errstatus     = (ok, warning, recover);  (* indicators of (nonfatal) error status for lexgen *)
  
     nfa_node = 
        record
           arc:  min_term..maxtermsets;       (* arc label *)
           ptr1: ^nfa_node;                   (* major pointer to another node *)
           case node_kind: (skeletal_node, normal_node,
                            important_node, inter_backup_node, final_node) of
              skeletal_node,                     (* initially given to all nodes *)
              normal_node:                      (* after "fix" *)
                   (ptr2: ^nfa_node;          (* secondary pointer used by union and closures *)
                    skeletal_form: (ordinary, union, concatenation,
                                    kleene_closure, positive_closure);
                    scanned: ^nfa_node);      (* special marker to prevent infinite search in NFA *)
              important_node,                 (* important state *)
              inter_backup_node,              (* intermediate-backup state (lookahead feature using
                                                 "/") *)
              final_node:                    (* final state *)
                   (ifcode: 1..maxifstates)  (* index this important/backup/final ("ibf") state *)
        end;
  
     delta_row = packed array [min_term..-1] of dfastaterange;
     blockrange = firstblock..maxblocks;     (* range of blocks *)
     member = record
                 state: dfastaterange;
                 next_mem: ^member
              end;
  
     stackrange = 1..maximum (integer);               (* range of parser stack *)
     elementrange = minparcode..maxparsest;  (* range of parse table entries *)
  
     stacktype = ^array [minimum (stackrange) .. *] of
        record
           stackstate: 1..maxparsest;                     (* parser states *)
           case variant: (symbol_variant, nfa_variant, set_variant) of
              symbol_variant: (symbol: min_term..maxnonter);  (* term.<0, lambda=0, 0<nonterm. *)
              nfa_variant:    (initial, final: ^nfa_node);  (* point to initial,final nodes of NFA *)
              set_variant:    (term_set: set of lambda .. -min_term) (* set of terminals *)
        end;
$PAGE variable declarations
var 
     p: array [1..maxparsest, meta_lbrack..terminalhead]
           of elementrange;  (* parsing action/goto table *)
     lhs: array [-21..-2] of 1..terminalhead;  (* left hand sides of parser productions *)
     rhslength: array [-21..-2] of 0..5;  (* length of right hand side of each parser production *)
     st: 1..maxparsest;
     la: meta_lbrack..terminalhead;
   
     t: record   (* terminal symbol table *)
           symmax:   term_range;
           symtable: array [term_range] of
                        record
                           sym:     string [maxsymlen]
                        end
        end;
     
     s: record   (* terminal set symbol table *)
           symmax:   0..maxtermsets;
           symtable: array [1..maxtermsets] of
                        record
                           sym:     string [maxsymlen];
                           defined: boolean;
                           term_set: set of lambda .. -min_term
                        end
        end;
  
     n: record   (* nonterminal symbol table *)
           symmax:   1..maxnonter;
           symtable: array [1..maxnonter] of
                        record
                           sym:     string [maxsymlen];
                           initial,           (* pointer to initial node of associated NFA *)
                           intermed,          (*    "    "  interm.-backup node of assoc. NFA *)
                           final:   ^nfa_node (*    "    "  final node of associated NFA *)
                        end
        end;
  
     errors:        errstatus;
     last_int_back: 1..maxifstates;           (* actual index of last intermediate-backup state *)
     which_nfa:     packed array [1..maxifstates]
                       of 1..maxnonter;       (* cross ref. backup states to NFAs *)
     last_nfa:      1..maxnonter;             (* actual index of last final state *)
     last_ibf:      1..maxifstates;           (* actual last ibf state *)
     last_dfa:      dfastaterange;            (* actual last DFA state *)
     delta:         array [dfastaterange]
                       of ^delta_row;         (* transition table for DFA *)
     dfastate:      array [dfastaterange]
                       of ifstateset;         (* DFA states repr. as sets of ibf states *)
  
     whichblock: array [dfastaterange] of blockrange; (* xref DFA states to equivalence classes *)
     block: array [blockrange] of
               record
                  element_cnt: dfastaterange; (* no. of members in block *)
                  first_mem,
                  last_mem: ^member   (* ^ chain of member DFA states in this equivalence class *)
               end;
     last_block: blockrange;          (* actual last equivalence class of minimized DFA *)
     la_feature: boolean;             (* flag indicating use of the lookahead feature *)
$PAGE semantics declarations
procedure semantics (var stack:     stacktype;
                         action:    elementrange; 
                         tos:       stackrange;
                     var print_nfa: boolean); 

   var 
      temp_initial, temp_final: ^nfa_node;
      tsym: term_range;
      tsetsym: 1..maxtermsets;

  
  
   procedure newnode (var n: ^nfa_node);   

      begin
         new (n, skeletal_node);   (* allocate node with skeletal variant *)
         n^ := (lambda, nil, skeletal_node, nil, ordinary, nil)
      end;
  
  
  
   procedure errout (mtype: errstatus;
                     msg:   packed array [1..*] of char);

      begin
         write ('     *****      ');
         if errors < mtype then             (* most serious error so far *)
            errors := mtype;
         case mtype of
            warning:
               writeln (msg);
            recover:
               writeln (msg, ' is undefined - will use null string nfa and continue.')
         end
      end;
$PAGE copy   in semantics
   procedure copy (var i, f:   ^nfa_node;
                       ti, tf: ^nfa_node);
  
   (* copy the skeletal NFA (ti,tf) to (i,f)  *)

      begin
         newnode (i);
         i^.arc := ti^.arc;
         i^.skeletal_form := ti^.skeletal_form;
  
         case i^.skeletal_form of
            ordinary: begin                              (* terminal or terminal-set NFA *)
               newnode (f);
               i^.ptr1 := f;
               i^.ptr2 := nil
            end;

            union: begin
               newnode (f);
               copy (i^.ptr2, f^.ptr2, ti^.ptr2, tf^.ptr2);          (* dupl. 2nd component NFA *)
               copy (i^.ptr1, f^.ptr2^.ptr1, ti^.ptr1, tf^.ptr2^.ptr1);
                                                                     (*  "    1st     "      "  *)
               f^.ptr2^.ptr1^.ptr1 := f
            end;

            concatenation: begin
               copy (i^.ptr1, i^.ptr2, ti^.ptr1, ti^.ptr2);          (* dupl. 1st component NFA *)
               copy (i^.ptr2^.ptr1, f, ti^.ptr2^.ptr1, tf)           (*  "    2nd     "      "  *)
            end;

            kleene_closure, positive_closure: begin
               newnode (f);
               copy (i^.ptr1, f^.ptr2, ti^.ptr1, tf^.ptr2);          (* dupl. component NFA *)
               i^.ptr2 := f;
               f^.ptr2^.ptr1 := f
            end
         end
      end;
$PAGE print_term_nfa, print_termset_nfa, print_union_nfa   in semantics
   procedure print_term_nfa (symbol: term_range);    (* print terminal NFA *)

      begin
         writeln;
         writeln;
         writeln (' ':21, '''', t.symtable [symbol].sym, '''');
         writeln (tos:3, ':      O-------------------------------->(O)')
      end;


   procedure print_termset_nfa (symbol: 1..maxtermsets);    (* print terminal set NFA *)

      begin
         writeln;
         writeln;
         writeln (' ':21, '"', s.symtable [symbol].sym, '"');
         writeln (tos:3, ':      O-------------------------------->(O)')
      end;

  
   procedure print_union_nfa; (* print union NFA *)

      begin
         writeln;
         writeln;
         writeln ('                    ___________');
         writeln ('                   /           \');
         writeln ('                  /             \');
         writeln ('               ->( 0    ', tos:3, '    0 )____');
         writeln ('              /   \             /     \');
         writeln ('             /     \___________/       \');
         writeln ('            /                           !');
         writeln ('           /                            V');
         writeln (tos:3, ':      O                            (O)');
         writeln ('           \                           /\');
         writeln ('            \       ___________        /');
         writeln ('             \     /           \      /');
         writeln ('              \   /             \    /');
         writeln ('               ->( 0    ', tos+2:3, '    0 )__/');
         writeln ('                  \             /');
         writeln ('                   \___________/')
      end;
$PAGE print_closure_nfa, print_concat_nfa   in semantics
   procedure print_closure_nfa; (* print closure NFA *)

      begin
         writeln;
         writeln;
         writeln ('                    ____________');
         writeln ('                   /            \');
         writeln ('                  !              \');
         writeln ('                  ! ___________   !');
         writeln ('                  V/           \  !');
         writeln ('                  /             \/');
         writeln ('               ->( 0    ', tos:3, '    0 )____');
         writeln ('              /   \             /     \');
         writeln ('             /     \___________/       \');
         writeln ('            /                           !');
         writeln ('           /                            V');
         writeln (tos:3, ':      O                            (O)');

         if action = -10  (* kleene closure *) then
            begin
               writeln ('           \                           /\');
               writeln ('            \                          /');
               writeln ('             \________________________/')
            end
      end;

  
  
   procedure print_concat_nfa; (* print concatenation NFA *)

      begin
         writeln;
         writeln;
         writeln ('                  ___________            ___________');
         writeln ('                 /           \          /           \');
         writeln ('                /             \        /             \');
         writeln (tos:3, ':      O--->( 0    ', tos:3, '    0 )----->( 0   ', tos+1:3, '   (O) )');
         writeln ('                \             /        \             /');
         writeln ('                 \___________/          \___________/')
      end;
$PAGE semantics
   begin (* semantics *)

      case action of
  
         -3, (* <tokens> ::= 'nonterminal' *)
         -4: (* <tokens> ::= <tokens> 'nonterminal' *)
      
         (* The form of the productions ensures that this list of nonterminals to
            be recognized is completely processed before the first regular expression
            is parsed.  Therefore we can use LAST_NFA to mark where the nonterminal
            symbol table ended after this list is seen, and any later nonterminals
            encountered must be merely intermediate names.  *)
  
         begin
            if last_nfa+1 <> n.symmax then      (* if nonterm. new, FIND just put it in table *)
               errout (warning, 'WARNING: Duplicate token.');
            last_nfa := n.symmax
         end;

  
  
         -7: (* <production> ::= 'nonterminal' '::=' <expression> *)
  
         with n.symtable [stack^[tos].symbol] do begin
            if initial <> nil then              (* already NFA associated with nonterminal? *)
               errout (warning, 'WARNING: <' || sym || '> reassigned.');
            ini:= stack^[tos+2].initial;   (* identify NFA with nonterminal *)
            final := stack^[tos+2].final;
            intermed := nil;                    (* no lookahead nfa *)
            if print_nfa then begin
               writeln;
               writeln;
               writeln ('   <', sym, '> ******* IS ASSIGNED THE NFA ****', tos+2:3)
            end
         end;
$PAGE
         -8: (* <production> ::= 'nonterminal' '::=' <expression> '/' <expression> *)
  
         with n.symtable [stack^[tos].symbol] do begin
            if initial <> nil then              (* already NFA associated with nonterminal? *)
               errout (warning, 'WARNING: <' || sym || '> reassigned.');

            (* concat. the two NFAs and identify resulting lookahead NFA with nonterm. *)

            intermed := stack^[tos+2].final;    (* token actually ends with end of expr1 *)
            newnode (initial);                  (* initial node of concatenation NFA *)
            initial^.skeletal_form := concatenation;
            initial^.ptr1 := stack^[tos+2].initial; (* arc to expr1 NFA's initial node *)
            initial^.ptr2 := intermed;          (* hook new initial to expr1 final (for copying) *)
            stack^[tos+2].final^.ptr1 := stack^[tos+4].initial;
                                                (* from token NFA to lookahead NFA *)
            final := stack^[tos+4].final;
            if print_nfa then begin
               writeln;
               writeln;
               writeln ('   <', sym, '> **** IS ASSIGNED THE NFA ****', tos+2:3, '//', tos+4:3)
            end
         end;

  
  
         -9: (* <production> ::= 'setname' '::=' <terminalset> *)
  
         with s.symtable [stack^[tos].symbol] do begin
            if defined then                     (* already set associated with setname? *)
               errout (warning, 'WARNING: "' || sym || '" reassigned.');
            term_set := stack^[tos+2].term_set;
            defined := true
         end;
$PAGE
         -10, (* <expression> ::= <expression> '*' *)
         -11: (* <expression> ::= <expression> '+' *)
  
         begin
            newnode (temp_initial);                       (* initial node of closure NFA *)
            newnode (temp_final);                         (* final    "   "     "    "   *)
            with temp_initial^ do
               begin
                  if action = -10 then
                     skeletal_form := kleene_closure
                  else
                     skeletal_form := positive_closure;
                  ptr1 := stack^[tos].initial;            (* arc from new initial to old
                                                             init. node *)
                  temp_final^.ptr2 := stack^[tos].final;  (* connect new final to old
                                                             (for copying) *)
                  ptr2 := temp_final;                     (* arc from initial to final node *)
                  stack^[tos].final^.ptr1 := temp_final;  (* from old final to new final *)
                  stack^[tos].initial := temp_initial;    (* identify new NFA with expr. *)
                  stack^[tos].final := temp_final;        (*    "      "   "    "    "   *)
                  if print_nfa then
                     print_closure_nfa
               end
         end;

  
  
         -12: (* <expression> ::= <expression><expression> *)
  
         begin
            newnode (temp_initial);                       (* initial node of concat. NFA *)
            temp_initial^.skeletal_form := concatenation;
            temp_initial^.ptr1 := stack^[tos].initial;    (* arc to expr1 NFA's init. node *)
            temp_initial^.ptr2 := stack^[tos].final;      (* conn. new init. to expr1 final
                                                             (for copying)  *)
            stack^[tos].final^.ptr1 := stack^[tos+1].initial; (* from expr1 final to expr2 init. *)
            stack^[tos].initial := temp_initial;          (* identify new NFA with expr. *)
            stack^[tos].final := stack^[tos+1].final;     (*    "      "   "    "    "   *)
            if print_nfa then
               print_concat_nfa
         end;
$PAGE
         -14: (* <expression> ::= 'nonterminal' *)
  
         with n.symtable [stack^[tos].symbol] do begin
            stack^[tos].variant := nfa_variant;
            if initial = nil then begin       (* undefined - recover by assuming null string NFA *)
               errout (recover, '<' || sym || '>');
               newnode (stack^[tos].initial);
               newnode (stack^[tos].final);
               stack^[tos].initial^.ptr1 := stack^[tos].final;
               if print_nfa then
                  print_term_nfa (lambda)
            end
            else begin   (* yes - copy NFA from nonterminal *)
               copy (stack^[tos].initial, stack^[tos].final, initial, final);
               if print_nfa then begin
                  writeln;
                  writeln;
                  writeln (tos:3, ': ****** IS COPIED FROM ***** <', sym, '>')
               end
            end
         end;
  

         -16: (* <expression> ::= '(' <expression> ')' *)
  
         begin
            stack^[tos].variant := nfa_variant;
            stack^[tos].initial :=  stack^[tos+1].initial;
            stack^[tos].final := stack^[tos+1].final;
            if print_nfa then begin
               writeln;
               writeln;
               writeln (tos:3, ': ******* IS THE NEW LABEL FOR ****', tos+1:3)
            end
         end;
$PAGE
         -17: (* <expression> ::= <expression> '!' <expression> *)
  
         begin
            newnode (temp_initial);                      (* initial node of union NFA *)
            temp_initial^.skeletal_form := union;
            temp_initial^.ptr1 := stack^[tos].initial;   (* arc to expr1 NFA's initial node *)
            temp_initial^.ptr2 := stack^[tos+2].initial; (*  "   " expr2   "      "     "   *)
            newnode (temp_final);                        (* final node of union NFA *)
            stack^[tos].final^.ptr1 := temp_final;       (* arc from expr1 final to new final *)
            temp_final^.ptr2 := stack^[tos+2].final;     (* connect new final to expr2 final
                                                            (for copying)  *)
            stack^[tos+2].final^.ptr1 := stack^[tos].final;
                                                         (* conn. expr2 final to expr1 (for
                                                            copying) *)
            stack^[tos].initial := temp_initial;         (* identify new NFA with expression *)
            stack^[tos].final :=  temp_final;            (*     "     "   "   "        "     *)
            if print_nfa then
               print_union_nfa
         end;
  
  
         -18, (* <expression> ::= '' *)
         -13: (* <expression> ::= 'terminal' *)
  
         begin
            tsym := stack^[tos].symbol;              (* save the symbol from rhs *)
            stack^[tos].variant := nfa_variant;
            newnode (stack^[tos].initial);               (* initial node of NFA *)
            newnode (stack^[tos].final);                 (* final node of NFA *)
            with stack^[tos].initial^ do begin
               arc := tsym;                              (* label arc with terminal (codes<=0) *)
               ptr1 := stack^[tos].final;                (* arc from initial to final node *)
               if print_nfa then
                  print_term_nfa (arc)
            end
         end;


         -15: (* <expression> ::= 'setname'  *)
  
         begin
            tsetsym := stack^[tos].symbol;              (* save the symbol from rhs *)
            stack^[tos].variant := nfa_variant;
            newnode (stack^[tos].initial);               (* initial node of NFA *)
            newnode (stack^[tos].final);                 (* final node of NFA *)
            with stack^[tos].initial^ do begin
               if not s.symtable [tsetsym].defined then begin
                  errout (recover, '"' || s.symtable [stack^[tos].symbol].sym || '"');
                  arc := lambda
               end
               else
                  arc := tsetsym;                           (* label arc with terminal (codes<=0) *)
               ptr1 := stack^[tos].final;                (* arc from initial to final node *)
               if print_nfa then
                  print_termset_nfa (arc)
            end
         end;

         -20: (* <terminalhead> := '[' *)
  
         begin
            stack^[tos].variant := set_variant;
            stack^[tos].term_set := []                   (* initialize set to be empty *)
         end;

  
   
         -21: (* <terminalhead> ::= <terminalhead> 'terminal' *)
  
            stack^[tos].term_set := stack^[tos].term_set + [ -stack^[tos+1].symbol];
                                                         (* add terminal to set *)

  
         others: (* -2: <complete> ::= <tokens> ';' <prodset>
                    -5: <prodset> ::= <production>
                    -6: <prodset> ::= <prodset> ';' <production>
                   -19: <terminalset> ::= <terminalhead> ']'     *)

                         (*  --- no semantics ---  *)
  
      end (* case action *)
   end (* semantics *);
$PAGE parse_regular_expressions  declarations
procedure parse_regular_expressions;     (* parse input and create NFA's *)
  
   const 
      accept     =  -1;   (* code for accept action *)
      errorstate =   0;   (* code for parsing error *)
      lr0reduce  =-500;   (* addend to indicate special code for replaced lr0-reduce state *)
      shift      =   0;   (* code for shift *)
      eod_term   =  -1;   (* code for end-of-data terminal *)
      stack_allocation_increment = 25;

   var 
      stack:        stacktype;
      currentstate: 1..maxparsest;
      tos:          stackrange;
      action, oldaction: elementrange;
      print_nfa:    boolean;
      curr_nfa:     1..maxnonter;
      tsym:         term_range;


  
   procedure newnode (var n: ^nfa_node);

      begin
         new (n, skeletal_node);   (* allocate node with skeletal variant *)
         n^ := (lambda, nil, skeletal_node, nil, ordinary, nil)
      end;
$PAGE next_lexeme  in parse_regular_expressions
   (* NEXT LEXEME obtains the next lexeme from the input file.  NEWSYM_TYPE will
      be set to indicate the lexeme, and in the case of terminal, terminal-set,
      or nonterminal, NEWSYM will be the actual character string.  BUFFER and
      BUFFER_POS preserve the state of the input scanning from one call to the next. *)

   const
      buffer_allocation_increment = 100;
   
   type
      input_text_string = ^string [*];

   var
      buffer: input_text_string;
      buffer_pos: 0..maximum (integer);
   
   procedure init_next_lexeme;
      begin
         new (buffer, buffer_allocation_increment);
         buffer^ := '';
         buffer_pos := 0
      end;

   procedure term_next_lexeme;
      begin
         dispose (buffer)
      end;

   procedure next_lexeme (var newsym_type: meta_lbrack..eodata; var newsym: string [maxsymlen]);

      var 
         truncated, lexeme_found: boolean;
         end_char: char;
         scan_state: (init, got_end_char, end_of_line, done);
         temp_buffer: input_text_string;
         next_line_seg: string [buffer_allocation_increment];

      const
         char_to_lexeme_table: array [meta_lbrack..eodata] of char :=
               ('[', ']', '!', ')', '(', '''', '+', '*', '"', '/', ':', '<', ';', chr (127));


      procedure next_line;
         begin
	    if not eof (input) then begin
	       read (input, buffer^);
	       while not eoln (input) do begin
		  read (input, next_line_seg);
		  new (temp_buffer, upperbound (buffer^) + buffer_allocation_increment);
		  temp_buffer^ := buffer^ || next_line_seg;
		  dispose (buffer);
		  buffer := temp_buffer
	       end;
	       readln (input);
	       writeln ('INPUT:   ', buffer^)
	    end
	    else
	       buffer^ := '';
	    buffer^ := buffer^ || chr (127);        (* internal end-of-line marker *)
	    buffer_pos := 0
         end;

      procedure errout (mtype: errstatus; msg: packed array [1..*] of char);

         begin
            write ('     *****      ');
            if (errors < mtype) then                    (* most serious error so far? *)
               errors := mtype;
            writeln (msg)
         end (* errout *);


      procedure insertch; (* insert char into sym *)

         begin
            if length (newsym) < upperbound (newsym) then
               newsym := newsym || buffer^ [buffer_pos]
            else begin
               if not truncated (* had already run out of room? *) then
                  errout (warning, 'WARNING: Symbol too long - had to be truncated.');
               truncated := true                     (* remember warning already given *)
            end
         end;
$PAGE
      begin (* next_lexeme *)

         repeat   (* until lexeme found *)
            lexeme_found := false;

            if buffer_pos >= length (buffer^) then       (* has buffer's contents been used up? *)
               repeat
                  next_line                              (* setup buffer with next input line *)
               until buffer^ [1] <> '*';                 (* until not a comment line *)
            repeat 
               buffer_pos := buffer_pos + 1
            until buffer^ [buffer_pos] <> ' ';

            newsym_type := lowerbound (char_to_lexeme_table);
            repeat
             exit if buffer^ [buffer_pos] = char_to_lexeme_table [newsym_type];
               newsym_type := newsym_type + 1
            until newsym_type = upperbound (char_to_lexeme_table);

            if buffer^ [buffer_pos] <> char_to_lexeme_table [newsym_type] then
               errout (recover, 'Illegal character ''' || buffer^ [buffer_pos] || '''.')
            else if buffer^ [buffer_pos] in ['"', '''', '<'] then begin  (* language symbols *)
               newsym := '';
               if buffer^ [buffer_pos] = '<' then
                  end_char := '>' (* reading nonterminal name *)
               else
                  end_char := buffer^ [buffer_pos]; (* reading terminal or terminal-set name *)
               truncated := false;
               scan_state := init;
               repeat
                  buffer_pos := buffer_pos + 1;
                  case scan_state of
                     init:
                          if buffer^ [buffer_pos] = end_char then
                             scan_state := got_end_char
                          else if buffer^ [buffer_pos] = chr (127) then
                             scan_state := end_of_line
                          else
                             insertch;
                     got_end_char:
                          if buffer^ [buffer_pos] = end_char then begin (* doubled? *)
                             insertch;
                             scan_state := init
                          end
                          else begin
                             scan_state := done; (* was really end *)
                             buffer_pos := buffer_pos - 1 (* back up one char *)
                          end
                  end
               until (scan_state = done) or (scan_state = end_of_line);

               if scan_state = end_of_line then
                  errout (recover, 'Symbol continues across end of line.')
               else if (newsym_type <> terminal) and (newsym = '') then
                  errout (recover, '<> or "" invalid.')
               else
                  lexeme_found := true
            end (* language symbols *)

            else if buffer^ [buffer_pos] = ':' then begin
               lexeme_found := true;
               if ((length (buffer^) - buffer_pos) < 3) orif (buffer^ [buffer_pos:3] <> '::=') then
                  errout (recover, '::= mistyped.');
               if buffer^ [buffer_pos + 1] in [':', '='] then  (* allow for slight scrambling *)
                  buffer_pos := buffer_pos + 1;
               if buffer^ [buffer_pos + 1] in [':', '='] then
                  buffer_pos := buffer_pos + 1
            end

            else if (newsym_type <> eodata) or eof (input) then
               lexeme_found := true
         until lexeme_found

      end (* next_lexeme *);
$PAGE find  in parse_regular_expressions
   procedure find (newsym_type: meta_lbrack..eodata; newsym: string [maxsymlen]);   

      procedure tablefull;

         begin
            case newsym_type of
               terminal:    write (ttyoutput, 'TERMINAL');
               setname:     write (ttyoutput, 'SETNAME');
               nonterminal: write (ttyoutput, 'NONTERMINAL')
            end;
            writeln (ttyoutput, ' SYMBOL TABLE OVERFLOW');
            stop
         end (* tablefull *);


      begin (* find *)
  
         stack^[tos].variant := symbol_variant;
         case newsym_type of
  
            terminal:
               with t do begin
                  if symmax <= min_term then
                     tablefull;                         (* no room to put symbol I'm looking for *)
                  stack^[tos].symbol := lambda;
                  symtable [symmax-1].sym := newsym;    (* insert at end of table *)
                  while (length (symtable [stack^[tos].symbol].sym) <> length (newsym)) or
                        (symtable [stack^[tos].symbol].sym <> newsym) do   (* search table *)
                     stack^[tos].symbol := stack^[tos].symbol - 1;
                  if stack^[tos].symbol < symmax then   (* it wasn't in table? *)
                     symmax := symmax - 1               (* now it is *)
               end;

            setname:
               with s do begin
                  if symmax >= maxtermsets then
                     tablefull;                         (* no room to put symbol I'm looking for *)
                  stack^[tos].symbol := 1;
                  symtable [symmax+1].sym := newsym;    (* insert at end of table *)
                  while (symtable [stack^[tos].symbol].sym <> newsym) do   (* search table *)
                     stack^[tos].symbol := stack^[tos].symbol + 1;
                  if stack^[tos].symbol > symmax then begin (* it wasn't in table? *)
                     symmax := symmax + 1;              (* now it is ... *)
                     symtable [symmax].defined := false (* ... but it hasn't any definition yet *)
                  end
               end;

            nonterminal:
               with n do begin
                  if symmax >= maxnonter then
                     tablefull;                         (* no room to put symbol I'm looking for *)
                  stack^[tos].symbol := 1;
                  symtable [symmax+1].sym := newsym;    (* insert at end of table *)
                  while (symtable [stack^[tos].symbol].sym <> newsym) do   (* search table *)
                     stack^[tos].symbol := stack^[tos].symbol + 1;
                  if stack^[tos].symbol > symmax then begin (* it wasn't in table? *)
                     symmax := symmax + 1;              (* now it is ... *)
                     symtable [symmax].initial := nil;  (* ... but it has no associated nfa *)
                     symtable [symmax].intermed := nil;
                     symtable [symmax].final := nil
                  end
               end
         end
  
      end (* find *);
$PAGE pushsymbol   in parse_regular_expressions
   procedure pushsymbol (action: elementrange;
                         newsym_type: meta_lbrack..eodata;
			 newsym: string [maxsymlen]);    (* push symbol onto parser stack *)

      var
         tempstack: stacktype;
         stackindex: stackrange;

      begin
         tos := tos + 1;
         if tos > upperbound (stack^) then begin
            new (tempstack, upperbound (stack^) + stack_allocation_increment);
            for stackindex := lowerbound (stack^) to upperbound (stack^) do
               tempstack^[stackindex] := stack^[stackindex];
            dispose (stack);
            stack := tempstack
         end;
         if action = shift then begin
            if (newsym_type = terminal) or
                 (newsym_type = nonterminal) or
                   (newsym_type = setname) then
               find (newsym_type, newsym)     (* put symbol table index of new symbol onto stack *)
         end
         else
            semantics (stack, action, tos, print_nfa)
      end;
$PAGE printnode  in parse_regular_expressions
   procedure printnode (i: ^nfa_node);    (* print a node of NFA *)

      begin
         write (ord (i):10:o, ':');
         if i^.arc > lambda then
            with s.symtable [i^.arc] do
               write ('"', sym, '"', ' ':maxsymlen-length (sym)+2)
         else
            with t.symtable [i^.arc] do
               write ('''', sym, '''', ' ':maxsymlen-length (sym)+2);
         if i^.ptr1 = nil then
            write ('  - NIL -   ')
         else
            write (ord (i^.ptr1):10:o, '  ');
         if i^.ptr2 = nil then
            writeln ('  - NIL -  ')
         else
            writeln (ord (i^.ptr2):10:o)
      end;
$PAGE fix  in parse_regular_expressions
   procedure fix (var i, f: ^nfa_node);   (* fix up NFA: convert NFA from skeletal to final form *)

      begin
         case i^.skeletal_form of
            union: begin
               fix (i^.ptr1, f^.ptr2^.ptr1);
               if print_nfa then
                  printnode (f^.ptr2^.ptr1);
               fix (i^.ptr2, f^.ptr2);
               f^.ptr2^.ptr1 := f;
               if print_nfa then
                  printnode (f^.ptr2);
               f^.ptr2 := nil
            end;

            concatenation: begin
               fix (i^.ptr2^.ptr1, f);
               fix (i^.ptr1, i^.ptr2);
               if print_nfa then
                  printnode (i^.ptr2);
               i^.ptr2 := nil
            end;

            kleene_closure, positive_closure: begin
               fix (i^.ptr1, f^.ptr2);
               f^.ptr2^.ptr2 := i^.ptr1;
               if print_nfa then
                  printnode (f^.ptr2);
               f^.ptr2 := nil;
               if i^.skeletal_form = positive_closure then
                  i^.ptr2 := nil
            end;

            ordinary: (* no fixing required *)
         end;
 
         i^.node_kind := normal_node;
         f^.node_kind := normal_node;

         if print_nfa then
            printnode (i)               (* print initial node - caller will print final node *)
      end;
$PAGE parse_regular_expressions
   var
      fname: file_name;
      newsym:       string [maxsymlen];
      newsym_type:  meta_lbrack..eodata;

   begin (* parse_regular_expressions *)

      (* query user *)

      rewrite (ttyoutput);
      open (tty);
      repeat
         writeln (ttyoutput);
         write (ttyoutput, 'INPUT FILE-- ');
         break (ttyoutput);
         readln (tty);
         read (tty, fname);
         reset (input, fname);
         if eof (input) then
            writeln (ttyoutput, '?INPUT FILE EMPTY OR NONEXISTENT')
      until not eof (input);
  
  
      write (ttyoutput, 'OUTPUT FILE-- ');
      break (ttyoutput);
      readln (tty);
      read (tty, fname);
      rewrite (output, fname);
  
      repeat
         write (ttyoutput, 'PRINT NFA GRAPH (S)? ');
         break (ttyoutput);
         readln (tty);
         tty^ := uppercase (tty^);
         print_nfa := (tty^ = 'Y');
         if (tty^ <> 'Y') and (tty^ <> 'N') then
            writeln (ttyoutput, '?"YES" OR "NO", PLEASE.')
      until (tty^ = 'Y') or (tty^ = 'N');
$PAGE
      (* initialize nonterminal symbol table and create NFA for eod *)

      n.symmax := 1;                           (* table starts with eod *)
      with n.symtable [1] do begin
         sym := '*****END OF DATA*****';
         newnode (initial);                    (* initial node of NFA for eod *)
         newnode (final);                      (* final     "  "   "   "   "  *)
         initial^.ptr1 := final;               (* arc from initial to final node *)
         initial^.arc := eod_term;             (* label exiting arc *)
         intermed := nil
      end;
      last_nfa := 1; (* initialize *)

  
      (* initialize terminal symbol table *)

      t.symmax := eod_term;                    (* table starts with lambda and eod *)
      t.symtable [lambda].sym := '';
      t.symtable [eod_term].sym := '*****END OF DATA*****';

  
      (* initialize terminal setname symbol table *)

      s.symmax := 0;                           (* table is empty *)
$PAGE
      (* parse input  (see Appendix B of LALR1 user manual) *)

      errors := ok;
      init_next_lexeme; (* initialize the input scanner *)
      new (stack, stack_allocation_increment);
      tos := lowerbound (stack^);   (* initial top of stack *)
      currentstate := 1;
      next_lexeme (newsym_type, newsym);

      repeat
         stack^[tos].stackstate := currentstate;   (* push current state onto parser stack *)
         action := p [currentstate, newsym_type];

         (* shift *)
  
         if action > errorstate then begin
            pushsymbol (shift, newsym_type, newsym);
            currentstate := action;
            next_lexeme (newsym_type, newsym)
         end

         (* reduce *)
  
         else if action < accept then begin
            if action < lr0reduce then begin       (* eliminated lr0 reduce? *)
               pushsymbol (shift, newsym_type, newsym);
               next_lexeme (newsym_type, newsym);
               action := action - lr0reduce        (* modify to prod. # *)
            end;

            repeat
               tos := tos - rhslength [action];    (* pop parser stack *)
               oldaction := action;
               action := p [stack^[tos].stackstate, lhs [action]];
               pushsymbol (oldaction, newsym_type, newsym)
            until  action >= accept;
  
            if action >= 1 then
               currentstate := action
         end;

         (* error *)
  
         if action = errorstate then begin
            writeln ('*****SYNTAX ERROR:WILL FLUSH TO NEXT ";" OR EOF AND CONTINUE');
            errors := recover;
            if newsym_type <> eodata then begin
               while (newsym_type <> eodata) and (newsym_type <> meta_end) do
                  next_lexeme (newsym_type, newsym);
               if newsym_type = meta_end then begin
                  next_lexeme (newsym_type, newsym);     (* get symbol after *)
                  action := 1                      (* dummy up for continuing *)
               end;

               (* reinitialize stack and currentstate *)
               stack^[2].stackstate := 2;
               tos := 3;
               currentstate := 3
            end
         end
      until (action = errorstate) or (action = accept);
      term_next_lexeme; (* terminate the input scanner *)
  
  

      (* convert skeletal NFA's to final form and (optionally) print them *)

      for curr_nfa := 1 to last_nfa do
         with n.symtable [ curr_nfa ] do
            if initial <> nil then begin
               if print_nfa then begin
                  writeln;
                  writeln;
                  writeln ('*** NFA <', sym, '>:');
                  writeln
               end;
               fix (initial, final);
               if print_nfa then
                  printnode (final)
            end
            else
               writeln ('     *****     WARNING: NO NFA <', sym, '>');


      (*  print codes for terminals,  and for nonterminals to be recognized *)

      page;
      writeln ('****** CODES FOR TERMINAL SYMBOLS ******');
      writeln;
      writeln ('          CODE   SYMBOL');
      for tsym := t.symmax to -1 do
         writeln ('          ', tsym:3, '   ''', t.symtable [tsym].sym, '''');

      writeln;
      writeln;
      writeln ('****** CODES FOR NONTERMINALS TO BE RECOGNIZED ******');
      writeln;
      writeln ('          CODE   NONTERMINAL');
      for curr_nfa := 1 to last_nfa do
         writeln ('          ', curr_nfa:3, '   ''', n.symtable [curr_nfa].sym, '''')
  
   end (* parse_regular_expressions *);
$PAGE create_dfa  declarations
procedure create_dfa;
  
   type 
      queuenode =      (* queue to save important states for later processing to create nextif *)
         record
            imp:    ^nfa_node;        (* ^ important state's successor *)
            ifcode: 1..maxifstates; (* index of important state *)
            next:   ^queuenode
         end;

   var 
      tdfastate: array [min_term..-1]
                    of ifstateset;  (* temporary DFA states for each possible transition *)
      notnull:   array [min_term..-1] of boolean;     (* indicates if transition exists *)
  
      (* nextif[x] has nextset = set of all ibf states reachable from x under a transition via
                   symbol. (Note that each NFA node has at most one non-lambda exiting arc due
                   to the construction method utilizing lambda transitions)                     *)
  
      nextif: array [1..maxifstates] of
                 record
                    symbol:  min_term..maxtermsets;
                    nextset: ifstateset
                 end;

      tsym:         min_term..-1;
      nsym:         1..maxnonter;
      currdfa,
      nextdfa:      dfastaterange;
      ifstate:      1..maxifstates;
      finalfound:   boolean;
      headqueue,
      tailqueue,
      queue:        ^queuenode;
      oldi:         ^nfa_node;
$PAGE createnextif  in create_dfa
   procedure createnextif (var i: ^nfa_node; var iff: ifstateset);
  
      (* find all ibf states reachable under lambda transition from I and add to IFF *)

      begin 
         if i <> nil then
  
            case i^.node_kind of
  
               normal_node:
                  if i^.arc <> lambda then begin    (* important (i.e. has non-lambda exiting
                                                       arc) not yet marked *)
                     i^.node_kind := important_node;
                     if last_ibf = maxifstates then begin
                        writeln (tty, 'TOO MANY I-F STATES');
                        stop
                     end;
                     last_ibf := last_ibf + 1;
                     i^.ifcode := last_ibf;
                     iff := iff + [i^.ifcode];      (* put into set *)

                     with nextif [i^.ifcode] do begin
                        symbol := i^.arc;
                        if symbol > lambda then     (* terminal set? *)
                           if lambda in s.symtable [symbol].term_set then  (* lambda in set? *)
                              createnextif (i^.ptr1, iff)
                     end;
                     new (tailqueue^.next);
                     tailqueue := tailqueue^.next;
                     tailqueue^.imp := i^.ptr1;     (* add i^.ptr1 to queue of nodes whose
                                                       arcs haven't been explored yet      *)
                     tailqueue^.ifcode := i^.ifcode;
                     tailqueue^.next := nil
                  end

                  else if i^.scanned <> oldi then begin (* nonfinal with arc = lambda *)
                     i^.scanned := oldi;            (* mark to avoid infinite loops *)
                     createnextif (i^.ptr1, iff);
                     createnextif (i^.ptr2, iff)
                  end;

  
               important_node,  (* important already marked *)
               final_node:  (* final state *)
                  iff := iff + [i^.ifcode];         (* put into set *)

               inter_backup_node: begin  (* intermediate-backup state *)
                  createnextif (i^.ptr1, iff);      (* will always have single lambda transition *)
                  iff := iff + [i^.ifcode]          (* put into set *)
               end
            end
      end (* createnextif *);
$PAGE duplicate  in create_dfa
   function duplicate: boolean;
  
      (* compare tdfastate[tsym] with all DFA states already generated and assign
         delta[currdfa,tsym] the one which matches (or nextdfa if none matches) *)

      var 
         st:   dfastaterange;
         found_dupl: boolean;

      begin
         st := 1;
         while st < nextdfa do begin
            found_dupl := true;
            if tdfastate [tsym] <> dfastate [st] then
               found_dupl := false;
            if found_dupl then begin
               delta [currdfa]^[tsym] :=  st;
               st := nextdfa (* force exit *)
            end;
            st := st + 1
         end;

         if not found_dupl then
            delta [currdfa]^[tsym] := nextdfa;
         duplicate := found_dupl
      end;
$PAGE create_dfa
   begin (* create_dfa *)
  
      (* first, number the final and intermediate-backup nodes in the NFAs *)
   
      last_int_back := last_nfa;     (* nos. for inter.-backup nodes will follow final node nos. *)
      for nsym := 1 to last_nfa do
         with n.symtable [nsym] do
            if final <> nil then begin
               final^.node_kind := final_node;
               final^.ifcode := nsym;                   (* no. for final node = nonter. code *)
               if intermed <> nil then begin
                  intermed^.node_kind := inter_backup_node;
                  last_int_back := last_int_back+1;     (* allocate a no. for backup node *)
                  intermed^.ifcode := last_int_back;
                  which_nfa [last_int_back] := nsym     (* cross ref. backup node to NFA *)
               end
            end;
  
      new (delta [dead]);
      for tsym := -1 downto t.symmax do
         delta [dead]^[tsym] := dead;  (* initialize dead state to point to itself *)
      dfastate [dead] := [];           (* no NFA states in DFA dead state *)
      last_ibf := last_int_back;       (* numbers for important nodes (i.e.nodes with non-lambda
                                          exiting arcs) will follow the intermediate-backup nodes*)
      new (headqueue);                 (* init. queue with an unused entry so that createnextif
                                          need not handle special case of adding to empty queue *)
      tailqueue := headqueue;
      tailqueue^.next := nil;

  
      (* put all ibf states of all NFA's initially reachable by lambda transitions
         into the initial DFA state *)

      dfastate [1] := [];
      for nsym := 1 to last_nfa do begin
         oldi := n.symtable [nsym].initial;
         createnextif (oldi, dfastate [1])
      end;
$PAGE
      (* build nextif - the set-valued transition function of the NFAs *)

      queue := headqueue^.next;
      while queue <> nil do begin          (* while there are nodes whose exiting arcs haven't been
                                              explored *)
         nextif [queue^.ifcode].nextset := [];
         oldi := queue^.imp;
         createnextif (oldi, nextif [queue^.ifcode].nextset);
         queue := queue^.next
      end;
  
  
      (* create DFA by breadthfirst search of the NFA's transition function nextif *)
  
      currdfa := 1;
      nextdfa := 2;

      repeat
         new(delta [currdfa]);                         (* allocate a row for currdfa *)
         for tsym := -1 downto t.symmax do begin
            delta [currdfa]^[tsym] := dead;            (* initially trans. from currdfa are dead *)
            notnull [tsym] := false;                   (* assume no transitions from currdfa *)
            tdfastate [tsym] := []                     (* all temporary DFA states set to null *)
         end;

         (* find all transitions from currdfa *)
  
         for ifstate := last_int_back + 1 to last_ibf do  (* for each NFA state ...         *)
            if ifstate in dfastate [currdfa] then         (* ... in the current DFA state: *)
               with nextif [ifstate] do
                  if symbol < lambda then begin            (* terminal? *)
                     notnull [symbol] := true;         (* transition from currdfa under symbol *)
                     tdfastate [symbol] := tdfastate [symbol] + nextset
                                                       (* add NFA states reachable under symbol *)
                  end
                  else (* terminal set - process transitions for each terminal in it *)
                     for tsym := -1 downto t.symmax do
                        if -tsym in s.symtable [symbol].term_set then begin
                           notnull [tsym] := true;     (* transition under tsym *)
                           tdfastate [tsym] := tdfastate [tsym] + nextset
                                                       (* add NFA states reachable under tsym *)
                        end;

         (* add all DFA state transitions from currdfa to delta *)

         for tsym := -1 downto t.symmax do
            if notnull [tsym] then
               if not duplicate then begin
                  dfastate [nextdfa] := tdfastate [tsym];
                  if nextdfa = maxdfastates then begin
                     writeln (ttyoutput, 'TOO MANY DFA STATES');
                     stop
                  end;
                  nextdfa := nextdfa + 1
               end;

         currdfa := currdfa + 1
      until currdfa = nextdfa;                         (* no more new states *)
      last_dfa := currdfa - 1;
  
  
      (* clean up each DFA state in preparation for minimization - only the highest precedence
         final NFA state and the backup states are required to determine the initial partition *)

      for currdfa := 1 to last_dfa do begin
         finalfound := false;
         for ifstate := 1 to last_nfa do
            if ifstate in dfastate [currdfa] then begin
               if finalfound then   
                  dfastate [currdfa] := dfastate [currdfa] - [ifstate];
               finalfound := true                   (* found highest prec. final - kill the rest *)
            end;
         for ifstate := last_int_back + 1 to last_ibf do
            dfastate [currdfa] := dfastate [currdfa] - [ifstate]
      end
  
   end (* create_dfa *);
$PAGE minimize_dfa declarations
procedure minimize_dfa;

   var 
      first_state,
      currdfa:        dfastaterange;
      currblock:      blockrange;
      prev_mem,
      curr_mem:       ^member;
      match,
      newblock,
      nochange,
      nomore_deletes: boolean;
      tsym:           term_range;
      lb, ub:         min_term..0;
      ifstate:        1..maxifstates;


  
   procedure insert (newstate: dfastaterange; x: blockrange);  

      begin
         with block [x] do begin
            if first_mem = nil then begin    (* equivalence class empty? *)
               new (first_mem);               (* newstate is only member *)
               last_mem := first_mem
            end
            else begin
               new (last_mem^.next_mem);      (* add newstate to end of list *)
               last_mem := last_mem^.next_mem
            end;
            last_mem^.state := newstate;     (* fill in the next member *)
            last_mem^.next_mem := nil;       (* its the last member *)
            element_cnt := element_cnt + 1
         end
      end;
$PAGE minimize_dfa body
   begin (* minimize_dfa *)
  
  
      (* initialize *)
  
      for currblock := firstblock to maxblocks do begin
         block [currblock].element_cnt := 0;
         block [currblock].first_mem := nil
      end;
  

  
      (* find initial partition: each distinct DFA state (as determined by intermediate-backup
         states and highest precedence final state in it) is assigned a different block      *)

  
      insert (dead, firstblock);              (* dead belongs to firstblock.  initially, all other
                                                 DFA states not containing final or backup states
                                                 will be put there too. *)
      whichblock [dead] := firstblock;       (* cross reference *)
      last_block := firstblock;              (* so far, only have the one block *)
  
      for currdfa := 1 to last_dfa do begin
         currblock := firstblock;

         repeat
            match := true;
            if dfastate [currdfa] <> dfastate [block [currblock].first_mem^.state] then
               match := false;               (* DFA state is distinct from those in curr. block *)
            if not match then
               currblock := currblock + 1    (* keep looking *)
         until match or (currblock > last_block);

         whichblock [currdfa] := currblock;   (* indicate currdfa's block *)
         if currblock = last_block+1 then    (* distinct DFA state? *)
            begin
               last_block := last_block + 1; (* allocate new block *)
               insert (currdfa, last_block)    (* put state into new block *)
            end
         else
            insert (currdfa, currblock)        (* put state into block with similar states *)
      end;
$PAGE
      (* Find equivalent states by refining the partition until each block contains only DFA
         states that are equivalent to each other.  At worst, each block will contain only one
         DFA state.  *)

      repeat
         nochange := true;
         currblock := firstblock;
  
         while currblock <= last_block do begin
            tsym := t.symmax;

            while (block [currblock].element_cnt > 1) and (tsym <= -1) do begin
               newblock := false;
               first_state := delta [ block [currblock].first_mem^.state ]^[tsym];
  
               repeat   (* until no more DFA states are removed from currblock *)
                  nomore_deletes := true;
                  prev_mem := block [currblock].first_mem;  (* get first consecutive pair *)
                  curr_mem := prev_mem^.next_mem;

                  while curr_mem <> nil do   (* compare members of the current block *)
                     if whichblock [ delta [curr_mem^.state]^[tsym] ]
                              <> whichblock [first_state] then begin
                        (* if delta(first_mem,tsym) isn't in same block as
                           delta(curr_mem,tsym), curr_mem isn't equivalent to first_mem *)
                        whichblock [curr_mem^.state] := last_block + 1;
                        nochange := false;
                        newblock := true;
                        insert (curr_mem^.state, last_block+1);
                        prev_mem^.next_mem := curr_mem^.next_mem;   (* delete from currblock *)
                        nomore_deletes := false;
                        block [currblock].element_cnt := block [currblock].element_cnt - 1;
                        curr_mem := curr_mem^.next_mem
                     end
                     else begin
                        prev_mem := curr_mem;   (* shift over to next pair in current block *)
                        curr_mem := curr_mem^.next_mem
                     end
               until nomore_deletes;

               if newblock then
                  last_block := last_block + 1;
               tsym := tsym + 1
            end;

            currblock := currblock + 1
         end
      until nochange;
  
  
      (* print minimized delta *)

      lb := t.symmax;
      repeat
         if (lb + maxcols - 1) <= -1 then   (* need full width of page? *)
            ub := lb + maxcols - 1
         else
            ub := -1;   (* only need portion of width of this page *)
         page;
         writeln ('****** MINIMIZED DFA NEXT STATE TABLE ******');
         writeln;
         write ('      ');
         for tsym := lb to ub do
            write (tsym:3, ' ');
         writeln;
         write ('    __');
         for tsym := lb to ub do
            write ('____');
         writeln;
         for currblock := 1 to last_block do begin
            writeln ('    !');
            write (currblock:3, ' ! ');
            for tsym := lb to ub do
               write (whichblock [delta [block [currblock].first_mem^.state]^[tsym]]:3, ' ');
            writeln
         end;
         lb := ub + 1
      until lb > -1;
      writeln;
      writeln ('****** START STATE: ', whichblock [1]:4);
$PAGE
      (* print intermediate-backup and high precedence final states in each DFA state *)

      page;
      writeln ('****** FINAL/BACKUP STATES IN MINIMIZED DFA ******');
      writeln;
      writeln ('          DFA STATE   FINAL/BACKUP STATE (S)');
      writeln ('            0      *<**ERROR**>');
      la_feature := false;                           (* assume lookahead feature isn't required *)
      for currblock := 1 to last_block do
         with block [currblock] do begin
            write ('          ', currblock:3, '      ');
            for ifstate := 1 to last_int_back do
               if ifstate in dfastate [first_mem^.state] then
                  if ifstate <= last_nfa then
                     with n.symtable [ifstate] do
                        write ('*<', sym, '> ')  (* final state *)
                  else begin
                     with n.symtable [which_nfa [ifstate]] do
                        write (' <', sym, '> '); (* intermediate-backup state *)
                     la_feature := true              (* lookahead feature really is required *)
                  end;
            writeln
         end;
      page
  
   end (* minimize_dfa *);
$PAGE print_program declarations and local proc. compact
procedure print_program;
  
   type
      com_tab_range = 0..max_com_tab;
      bit_range     = 0..10;
   
   var
      line_width:  0..80;
      width:       0..5;
      currblock:   blockrange;
      tsym:        min_term..-1;
      use_compacted, finalfound, needed_la: boolean;
      ifstate:     1..maxifstates;
      token_la:    packed array [1..maxnonter] of boolean;
      default:     packed array [1..maxblocks] of blockrange;
      base:        array [1..maxblocks] of com_tab_range;
      next,
      check:       array [1..max_com_tab] of blockrange;
      last_next_check,
      temp_base:   com_tab_range;
      per_word, per_word2: 1..word_size;
  
  
   procedure compact;
     
      var
         pagenum:   1..10;
         col, cols: 1..4;
         chosen_base, next_free, line, ub:  com_tab_range;
         currblock, best_state, temp_state:  blockrange;
         curr_row:  array [term_range] of blockrange;
         num_same, best_same, first_in_template:  0..-min_term;
         conflict, found_free:  boolean;
         template:  array [term_range] of boolean;
     
     
      begin (* compact *)
  
         (* put row of delta for first state directly into table *)
  
         default [1] := 0;   (* no default required - all entries will be entered explicitly *)
         base [1] := 0;      (* first state will use positions 1 to -t.symmax in next and check *)
         for tsym := -1 downto t.symmax do begin   (* put row into next and check *)
            next [-tsym] := whichblock [delta [block [1].first_mem^.state]^[tsym]];
            check [-tsym] := 1
         end;
         last_next_check := -t.symmax;
     
  
         (* fill in remaining states, taking advantage of similar states already in the table
            whenever possible.  (see Aho & Ullman ch. 3 for explanation of compaction method) *)
  
         for currblock := 2 to last_block do begin
  
            for tsym := -1 downto t.symmax do      (* make copy of current row (for efficiency) *)
               curr_row [tsym] := whichblock [delta [block [currblock].first_mem^.state]^[tsym]];

            best_same := 0;
            for temp_state := 1 to currblock-1 do begin   (* search for similar state *)
               num_same := 0;
               for tsym := -1 downto t.symmax do
                  if whichblock [delta [block [temp_state].first_mem^.state]^[tsym]]
                        = curr_row [tsym] then
                     num_same := num_same + 1;
               if num_same > best_same then begin      (* a new "best"? *)
                  best_same := num_same;
                  best_state := temp_state    (* new candidate *)
               end
            end;

            if best_same = 0 then begin   (* don't have a "similar" state? *)
               (* do not have a similar state - enter current state directly into table *)
               default [currblock] := 0;
               base [currblock] := last_next_check;
               if last_next_check - t.symmax <= max_com_tab then
                  last_next_check := last_next_check - t.symmax
               else begin
                  writeln (ttyoutput, 'COMPACTED TABLE OVERFLOW');
                  stop
               end;
               for tsym := -1 downto t.symmax do begin
                  next [base [currblock]-tsym] := curr_row [tsym];
                  check [base [currblock]-tsym] := currblock
               end
            end

            else begin   (* I have a similar state *)
               default [currblock] := best_state;   (* default to the similar state *)
               first_in_template := 0;
               for tsym := -1 downto t.symmax do   (* construct template of positions where curr.
                                                      state doesn't match the similar one *)
                  if whichblock [delta [block [best_state].first_mem^.state]^[tsym]]
                         <> curr_row [tsym] then begin
                     template [tsym] := true;
                     if first_in_template = 0 then
                        first_in_template := -tsym   (* remember position of first non-match *)
                  end
                  else
                     template [tsym] := false;
               next_free := 0;
               if first_in_template = 0 then   (* state exactly matches similar state? *)
                  chosen_base := 0 (* align with 1st state - default (the similar state)
                                      always taken *)
               else                (* have to find place for non-matching entries *)
                  repeat           (* slide template down table looking for first fit *)
                     found_free := false;
                     repeat        (* find next position that might fit *)
                        next_free := next_free + 1;
                        if next_free > last_next_check then
                           found_free := true   (* anything past current end of table is free *)
                        else if check [next_free] = 0 then
                           found_free := true
                     until found_free;
                     chosen_base := next_free - first_in_template;
                     conflict := false;
                     if first_in_template < -t.symmax then
                        for tsym := -first_in_template-1 downto t.symmax do
                           if template [tsym] then                (* if I need this position ... *)
                              if chosen_base - tsym <= last_next_check then
                                 if check [chosen_base-tsym] > 0 then (* ... but its not free ...*)
                                    conflict := true           (* ... then template doesn't fit *)
                  until not conflict;
               base [currblock] := chosen_base;
               if chosen_base - t.symmax > last_next_check then  (* need to expand table? *)
                  if chosen_base - t.symmax <= max_com_tab then begin
                     for temp_base := last_next_check+1 to chosen_base-t.symmax do
                        begin
                           next [temp_base] := 0;
                           check [temp_base] := 0
                        end;
                     last_next_check := chosen_base - t.symmax
                  end
                  else begin
                     writeln (ttyoutput, 'COMPACTED TABLE OVERFLOW');
                     stop
                  end;
               for tsym := -1 downto t.symmax do
                  if template [tsym] then begin
                     next [chosen_base-tsym] := curr_row [tsym];
                     check [chosen_base-tsym] := currblock
                  end
            end
         end (* for currblock *);
  
  
         (* print the compacted tables *)
  
         writeln ('****** COMPACTED VERSION OF MINIMIZED DFA NEXT STATE TABLE ******');
         writeln;
  
         for pagenum := 1 to (last_block+199) div 200 do begin (* fit 4 columns of 50 to a page *)
            if 200 + (pagenum-1)*200 <= last_block then        (* need to fill this page? *)
               ub := 200 + (pagenum-1)*200                     (* last entry on this page *)
            else
               ub := last_block;                               (* only need part of this page *)
            cols := (ub - (pagenum-1)*200 + 49) div 50;
            for col := 1 to cols do
               write ('             DEFAULT  BASE');
            writeln;
            writeln;
            if ub > 50 + (pagenum-1)*200 then
               ub := 50 + (pagenum-1)*200;   (* last entry in left column on this page *)
            for line := 1 + (pagenum-1)*200 to ub do begin
               for col := 1 to cols do
                  if line+(col-1)*50 <= last_block then
                     write (line+(col-1)*50:12, default [line+(col-1)*50]:7,
                                                base [line+(col-1)*50]:7);
                  writeln
            end;
            page
         end;
  
         for pagenum := 1 to (last_next_check+199) div 200 do begin  (* fit 4 col of 50 per page *)
            if 200 + (pagenum-1)*200 <= last_next_check then   (* need to fill this page? *)
               ub := 200      + (pagenum-1)*200   (* last entry on this page *)
            else
               ub := last_next_check;             (* only need part of this page *)
            cols := (ub - (pagenum-1)*200 + 49) div 50;
            for col := 1 to cols do
               write ('              NEXT  CHECK');
            writeln;
            writeln;
            if ub > 50 + (pagenum-1)*200 then
               ub := 50 + (pagenum-1)*200;        (* last entry in left column on this page *)
            for line := 1 + (pagenum-1)*200 to ub do begin
               for col := 1 to cols do
                  if line + (col-1)*50 <= last_next_check then
                     write (line+(col-1)*50:12, next [line+(col-1)*50]:6,
                                                check [line+(col-1)*50]:7);
                  writeln
            end;
            page
         end
  
      end (* compact *);
$PAGE print_program local procedures
   function bits (maxval: com_tab_range): bit_range;
  
      var
         temp_val: com_tab_range;
  
      begin
         bits := 0;
         temp_val := maxval;
         repeat
            bits := bits + 1;
            temp_val := temp_val div 2
         until temp_val = 0
      end;
  
  
   procedure print (val: com_tab_range);
  
      var
         temp_val: com_tab_range;
  
      begin
         temp_val := val;
         width := 0;
         repeat                                  (* calculate required field width *)
            temp_val := temp_val div 10;
            width := width + 1
         until temp_val = 0;
         if (line_width + width + 1) > 75 then begin   (* need new line? *)
            line_width := width + 7;
            writeln;
            write ('      ')
         end;
         line_width := line_width + width + 1;
         write (val:width)
      end;
$PAGE print_program body
   begin (* print_program *)
  
      (* compact the minimized version of delta *)
  
      compact;
  
      (* let user choose between compacted and uncompacted froms *)
  
      per_word := word_size div bits (last_block);
      per_word2 := word_size div bits (last_next_check);
      writeln (ttyoutput);
      writeln (ttyoutput, 'THE UNCOMPACTED FORM OF DELTA REQUIRES',
               (-t.symmax*last_block + per_word - 1) div per_word:6, ' WORDS FOR STORAGE');
      writeln (ttyoutput, 'THE COMPACTED FORM REQUIRES',
            (last_block + per_word - 1) div per_word                        (* for default      *)
          + (last_block + per_word2 - 1) div per_word2                      (*  "  base         *)
          + 2*((last_next_check + per_word - 1) div per_word):17, ' WORDS'); (*  "  next & check *)
      writeln (ttyoutput);
      repeat
         write (ttyoutput, 'DO YOU WANT THE COMPACTED FORM? ');
         break (ttyoutput);
         readln (tty);
         tty^ := uppercase (tty^);
         use_compacted := (tty^ = 'Y');
         if (tty^ <> 'Y') and (tty^ <> 'N') then
            writeln (ttyoutput, '?"YES" OR "NO", PLEASE.')
      until (tty^ = 'Y') or (tty^ = 'N');
  
  
      (* print scanner program with appropriate constants, tables, and code *)
  
      writeln ('(********************************************)');
      writeln ('(*   SCANNER ALGORITHM CREATED BY LEXGEN    *)');
      if la_feature then
         writeln ('(*    INCORPORATES THE LOOKAHEAD FEATURE    *)');
      writeln ('(********************************************)');
      writeln;
      writeln ('PROGRAM ??????;');
      writeln;
  
      writeln ('CONST');
      if la_feature then
         writeln ('   MAXSTACK    =  ???;  (* MAX INDEX IN STACK FOR LOOKAHEAD *)');
      writeln ('   MAXINDEX    =  ???;  (* MAX INDEX USED TO ACCESS BUFFER *)');
      writeln ('   BUFFERSIZE  =  ???;  (* MAXINDEX + 1 *)');
      writeln ('   MAXTOKEN    = ', last_nfa:4, ';');
      writeln ('   DFASTATE1   = ', whichblock [1]:4, ';  (* CODE FOR INITIAL STATE OF DFA *)');
      writeln ('   MAXDFASTATE = ', last_block:4, ';  (* CODE FOR MAX STATE OF DFA *)');
      writeln ('   MINTERMINAL = ', t.symmax:4, ';  (* MIN TERMINAL CODE *)');
      writeln ('   EODATA      =   -1;  (* CODE FOR END-OF-DATA *)');
      writeln;
      writeln;
  
      writeln ('TYPE');
      writeln ('   STATERANGE  = 1..MAXDFASTATE;');
      writeln ('   EXSTATERANGE= 0..MAXDFASTATE;');
      writeln ('   INDEXRANGE  = 0..MAXINDEX;');
      writeln ('   LEXTOKEN    = RECORD');
      writeln ('                    TOKEN_TYPE: ???;');
      writeln ('                    MORE: ???  (* POINTER TO SYMBOL TABLE, CODE');
      writeln ('                                  TO DIFFERENTIATE DIFFERENT SYMBOLS');
      writeln ('                                  SUCH AS RELATIONAL OPERATORS OF THE');
      writeln ('                                  SAME TOKEN_TYPE, ETC.  *)');
      writeln ('                 END;');
      writeln;
      writeln ('VAR');
  
      if use_compacted then begin
         write ('   DEFAULT: PACKED ARRAY [STATERANGE] OF EXSTATERANGE := (');
         line_width := 75;   (* force first line *)
         for currblock := 1 to last_block do begin
            print (default [currblock]);
            if currblock < last_block then
               write (',')
         end;
         writeln (');');
         writeln;
         write ('   BASE: PACKED ARRAY [STATERANGE] OF 0..', last_next_check:4, ' := (');
         line_width := 75;   (* force first line *)
         for currblock := 1 to last_block do begin
            print (base [currblock]);
            if currblock < last_block then
               write (',')
         end;
         writeln (');');
         writeln;
         writeln ('   NEXT: PACKED ARRAY [1..', last_next_check:4, '] OF EXSTATERANGE := (');
         line_width := 75;   (* force first line *)
         for temp_base := 1 to last_next_check do begin
            print (next [temp_base]);
            if temp_base < last_next_check then
               write (',')
         end;
         writeln (');');
         writeln;
         write ('   CHECK: PACKED ARRAY [1..', last_next_check:4, '] OF EXSTATERANGE := (');
         line_width := 75;   (* force first line *)
         for temp_base := 1 to last_next_check do begin
            print (check [temp_base]);
            if temp_base < last_next_check then
               write (',')
         end;
         writeln (');');
         writeln;
      end
      else begin
         write ('   DELTA: PACKED ARRAY [STATERANGE, MINTERMINAL..EODATA] OF EXSTATERANGE := (');
         line_width := 75;   (* force first line *)
         for currblock := 1 to last_block do
            for tsym := t.symmax to -1 do begin
               print (whichblock [delta [block [currblock].first_mem^.state]^[tsym]]);
               if (currblock < last_block) or (tsym < -1) then
                  write (',')
            end;
         writeln (');');
         writeln
      end;
  
      writeln ('   (* FINAL [X] = 0 IF STATE X IS NOT A FINAL STATE');
      for ifstate := 1 to last_nfa do
         with n.symtable [ifstate] do
            writeln ('               ', ifstate:3, ' IF STATE X RECOGNIZES <', sym, '>');
      writeln (' ':65, '*)');
      write ('   FINAL: PACKED ARRAY [EXSTATERANGE] OF 0..MAXTOKEN := (');
      line_width := 75;   (* force first line *)
      for currblock := firstblock to last_block do begin
         finalfound := false;
         for ifstate := 1 to last_nfa do
            if ifstate in dfastate [block [currblock].first_mem^.state] then begin
               print (ifstate);
               finalfound := true
            end;
         if not finalfound then
            print (0);
         if currblock < last_block then
            write (',')
      end;
      writeln (');');
      writeln;
  
      if la_feature then begin
         writeln ('   (* BACKUP [X] = SET OF ALL BACKUP TOKENS ASSOCIATED WITH STATE X.');
         writeln ('                  SEE "FINAL" COMMENT FOR TOKEN CODES. *)');
         writeln;
         write ('   BACKUP: ARRAY [STATERANGE] OF SET OF 1..MAXTOKEN := (');
         line_width := 75;   (* force first line *)
         for ifstate := 1 to last_nfa do
            token_la [ifstate] := false;  (* assume no tokens require lookahead to be recognized *)
         for currblock := 1 to last_block do begin
            if line_width > 60 then
               begin
                  writeln;
                  write ('      ');
                  line_width := 6
               end;
            write ('[');
            finalfound := false;
            for ifstate := last_nfa+1 to last_int_back do
               if ifstate in dfastate [block [currblock].first_mem^.state] then
                  begin
                     if finalfound then
                        write (',');
                     write (which_nfa [ifstate]:3);
                     finalfound := true;
                     line_width := line_width + 4;
                     token_la [which_nfa [ifstate]] := true  (* this token requires lookahead *)
                  end;
            write (']');
            if currblock < last_block then
               write (',');
            line_width := line_width + 2
         end;
         writeln (');');
         writeln;

         writeln ('   (* LOOKAHEADFINAL [X] = TRUE IFF LOOKAHEAD WAS REQUIRED TO RECOGNIZE');
         writeln ('          TOKEN ASSOCIATED WITH STATE X.  "FINAL" INDICATES WHICH TOKEN');
         writeln ('          THAT IS,  AND "BACKUP" IS USED TO FIGURE OUT WHERE TO BACKUP TO. *)');
         writeln;
         write ('   LOOKAHEADFINAL: PACKED ARRAY [EXSTATERANGE] OF BOOLEAN := (');
         line_width := 75;   (* force first line *)
         for currblock := firstblock to last_block do begin
            needed_la := false;
            for ifstate := 1 to last_nfa do
               if ifstate in dfastate [block [currblock].first_mem^.state] then
                  if token_la [ifstate] then
                     needed_la := true;
            if line_width + 6 > 75 then begin
               writeln;
               write ('      ');
               line_width := 6
            end;
            line_width := line_width + 6;
            write (needed_la:5);
            if currblock < last_block then
               write (',')
         end;
         writeln (');');
         writeln
      end;
  
      writeln ('   BEGIN_INDEX, END_INDEX: INDEXRANGE;');
      writeln ('   LEXEME: LEXTOKEN;');
      writeln ('   BUFFER: ARRAY [INDEXRANGE] OF MINTERMINAL..EODATA;');
      writeln;
      writeln;
  
      writeln ('PROCEDURE SCAN (VAR BEGIN_INDEX, END_INDEX: INDEXRANGE;');
      writeln ('               VAR LEXEME: LEXTOKEN);');
      writeln;
      writeln ('   VAR');
      writeln ('      NEWTOKEN:  BOOLEAN;');
      writeln ('      CURRSTATE, CURRFINAL: EXSTATERANGE;');
      writeln ('      OLDINDEX:  INDEXRANGE;');
      if la_feature then begin
         writeln ('      STACK: ARRAY [0..MAXSTACK] OF');
         writeln ('                RECORD');
         writeln ('                   INDEX: INDEXRANGE;');
         writeln ('                   STATE: STATERANGE');
         writeln ('                END;');
         writeln ('      TOS: 0..MAXSTACK;  (* CURRENT TOP OF STACK INDEX *)');
         writeln;
         writeln;
         writeln ('   PROCEDURE PUSH (ININDEX: INDEXRANGE; INSTATE: STATERANGE);');
         writeln ('      BEGIN');
         writeln ('         TOS := TOS + 1;');
         writeln ('         STACK [TOS].INDEX := ININDEX;');
         writeln ('         STACK [TOS].STATE := INSTATE');
         writeln ('      END (* PUSH *);')
      end;
      writeln;
      writeln;
  
      writeln ('   PROCEDURE GETCHAR (NEWTOKEN: BOOLEAN);');
      writeln ('      BEGIN');
      writeln ('         <  THIS PROCEDURE OBTAINS THE NEXT INPUT CHARACTER (WHICH');
      writeln ('            IS ASSUMED TO BE EODATA IF NO MORE INPUT) AND MODIFIES');
      writeln ('            BEGIN_INDEX AND END_INDEX AS NECESSARY DEPENDING ON');
      writeln ('            THE BUFFERING SCHEME SO THAT');
      writeln ('             (1) IF NEWTOKEN, THEN BEGIN_INDEX POINTS TO THE INPUT');
      writeln ('                 CHARACTER JUST OBTAINED, ELSE BEGIN_INDEX POINTS');
      writeln ('                 TO THE SAME CHARACTER IT POINTED TO BEFORE.');
      writeln ('             (2) END_INDEX IS THE INDEX OF THE NEW CHARACTER JUST');
      writeln ('                 OBTAINED.');
      writeln ('            SCAN ALLOWS FOR EITHER SEQUENTIAL OR CIRCULAR BUFFER  >');
      writeln ('      END (* GETCHAR *);');
      writeln;
      writeln;
  
      writeln ('   BEGIN (* SCAN *)');
      writeln ('      NEWTOKEN  := TRUE;');
      if la_feature then
         writeln ('      TOS := 0;');
      writeln ('      CURRSTATE := DFASTATE1;  (* START IN INITIAL STATE *)');
      writeln ('      CURRFINAL := 0;');
      writeln ('      OLDINDEX  := 0;  (* WIDTH OF LEXEME AS OF LAST FINAL STATE *)');
      writeln;
      writeln ('      WHILE CURRSTATE <> 0 DO');
      writeln ('         BEGIN');
      if la_feature then begin
         writeln ('            IF BACKUP [CURRSTATE] <> [] THEN');
         writeln ('               PUSH ((END_INDEX-BEGIN_INDEX) MOD BUFFERSIZE, CURRSTATE);')
      end;
      writeln ('            IF FINAL [CURRSTATE] <> 0 THEN');
      writeln ('               BEGIN');
      writeln ('                  CURRFINAL := CURRSTATE;');
      writeln ('                  OLDINDEX := (END_INDEX - BEGIN_INDEX) MOD BUFFERSIZE');
      writeln ('               END;');
      writeln ('            GETCHAR (NEWTOKEN);');
      writeln ('            NEWTOKEN := FALSE;');
      if use_compacted then begin
         writeln ('            WHILE CHECK [BASE [CURRSTATE]-BUFFER [END_INDEX]] <> CURRSTATE DO');
         writeln ('               CURRSTATE := DEFAULT [CURRSTATE];');
         writeln ('            CURRSTATE := NEXT [BASE [CURRSTATE]-BUFFER [END_INDEX]]')
      end
      else
         writeln ('            CURRSTATE := DELTA [CURRSTATE, BUFFER [END_INDEX]]');
      writeln ('         END;');
      if la_feature then begin
         writeln ('      IF LOOKAHEADFINAL [CURRFINAL] THEN');
         writeln ('         BEGIN');
         writeln ('            WHILE NOT (FINAL [CURRFINAL] IN BACKUP [STACK [TOS].STATE]) DO');
         writeln ('               TOS := TOS - 1;');
         writeln ('            END_INDEX := (STACK [TOS].INDEX + BEGIN_INDEX) MOD BUFFERSIZE');
         writeln ('         END');
         writeln ('      ELSE');
         write ('   ')
      end;
      writeln ('      END_INDEX := (BEGIN_INDEX + OLDINDEX) MOD BUFFERSIZE;');
      writeln;
      writeln ('       < COMPUTE LEXEME GIVEN FINAL [CURRFINAL], BEGIN_INDEX, END_INDEX, ');
      writeln ('         ETC.                                                          >');
      writeln;
      writeln ('   END (* SCAN *);');
      writeln;
      writeln;
  
      writeln ('BEGIN (* MAINLINE *)');
      writeln ('          .');
      writeln ('          .');
      writeln ('          .');
      writeln ('   SCAN (BEGIN_INDEX, END_INDEX, LEXEME);');
      writeln ('              (* AS NEEDED UNTIL END-OF-DATA LEXEME IS OBTAINED *)');
      writeln ('          .');
      writeln ('          .');
      writeln ('          .');
      writeln ('END. (* MAINLINE *)');
      writeln;
  
   end (* print_program *);
$PAGE mainline
begin (* lexgen *)

   (* initialize parsing tables *)

   for st := 1 to maxparsest do
      for la := meta_lbrack to terminalhead do
         p [st, la] := lambda;
   p [ 1,  -2] := -503;  p [ 1,   1] :=  -1;   p [ 1,   2] :=   2;
   p [ 2,  -2] := -504;  p [ 2,  -1] :=   3;
   p [ 3,  -5] :=   6;   p [ 3,  -2] :=   5;   p [ 3,   3] :=   4;   p [ 3,   4] :=  -5;
   p [ 4,  -1] :=   7;   p [ 4,   0] :=  -2;
   p [ 5,  -3] :=   8;
   p [ 6,  -3] :=   9;
   p [ 7,  -5] :=   6;   p [ 7,  -2] :=   5;   p [ 7,   4] :=  -6;
   p [ 8, -11] := -18;   p [ 8,  -9] :=  11;   p [ 8,  -8] := -513;  p [ 8,  -7] := -18;
   p [ 8,  -6] := -18;   p [ 8,  -5] := -515;  p [ 8,  -4] := -18;   p [ 8,  -2] := -514;
   p [ 8,  -1] := -18;   p [ 8,   0] := -18;   p [ 8,   5] :=  10;
   p [ 9, -13] := -520;  p [ 9,   6] :=  -9;   p [ 9,   7] :=  12;
   p [10, -11] :=  15;   p [10,  -9] :=  11;   p [10,  -8] := -513;  p [10,  -7] := -511;
   p [10,  -6] := -510;  p [10,  -5] := -515;  p [10,  -4] :=  13;   p [10,  -2] := -514;
   p [10,  -1] :=  -7;   p [10,   0] :=  -7;   p [10,   5] :=  14;
   p [11, -11] := -18;   p [11, -10] := -18;   p [11,  -9] :=  11;   p [11,  -8] := -513;
   p [11,  -7] := -18;   p [11,  -6] := -18;   p [11,  -5] := -515;  p [11,  -2] := -514;
   p [11,   5] :=  16;
   p [12, -12] := -519;  p [12,  -8] := -521;
   p [13, -11] := -18;   p [13,  -9] :=  11;   p [13,  -8] := -513;  p [13,  -7] := -18;
   p [13,  -6] := -18;   p [13,  -5] := -515;  p [13,  -2] := -514;  p [13,  -1] := -18;
   p [13,   0] := -18;   p [13,   5] :=  17;
   p [14, -11] := -12;   p [14, -10] := -12;   p [14,  -9] := -12;   p [14,  -8] := -12;
   p [14,  -7] := -511;  p [14,  -6] := -510;  p [14,  -5] := -12;   p [14,  -4] := -12;
   p [14,  -2] := -12;   p [14,  -1] := -12;   p [14,   0] := -12;   p [14,   5] :=  14;
   p [15, -11] := -18;   p [15, -10] := -18;   p [15,  -9] :=  11;   p [15,  -8] := -513;
   p [15,  -7] := -18;   p [15,  -6] := -18;   p [15,  -5] := -515;  p [15,  -4] := -18;
   p [15,  -2] := -514;  p [15,  -1] := -18;   p [15,   0] := -18;   p [15,   5] :=  18;
   p [16, -11] :=  15;   p [16, -10] := -516;  p [16,  -9] :=  11;   p [16,  -8] := -513;
   p [16,  -7] := -511;  p [16,  -6] := -510;  p [16,  -5] := -515;  p [16,  -2] := -514;
   p [16,   5] :=  14;
   p [17, -11] :=  15;   p [17,  -9] :=  11;   p [17,  -8] := -513;  p [17,  -7] := -511;
   p [17,  -6] := -510;  p [17,  -5] := -515;  p [17,  -2] := -514;  p [17,  -1] :=  -8;
   p [17,   0] :=  -8;   p [17,   5] :=  14;
   p [18, -11] := -17;   p [18, -10] := -17;   p [18,  -9] :=  11;   p [18,  -8] := -513;
   p [18,  -7] := -511;  p [18,  -6] := -510;  p [18,  -5] := -515;  p [18,  -4] := -17;
   p [18,  -2] := -514;  p [18,  -1] := -17;   p [18,   0] := -17;   p [18,   5] :=  14;

   lhs [ -2] := 1; rhslength [ -2] := 3;  (* <complete>   ::=  <tokens> ';' <prodset>            *)
   lhs [ -3] := 2; rhslength [ -3] := 1;  (* <tokens>     ::=  'nonterminal'                     *)
   lhs [ -4] := 2; rhslength [ -4] := 2;  (*                   <tokens> 'nonterminal'            *)
   lhs [ -5] := 3; rhslength [ -5] := 1;  (* <prodset>    ::=  <production>                      *)
   lhs [ -6] := 3; rhslength [ -6] := 3;  (*                   <prodset> ';' <production>        *)
   lhs [ -7] := 4; rhslength [ -7] := 3;  (* <production> ::=  'nonterminal' '::=' <expression>  *)
   lhs [ -8] := 4; rhslength [ -8] := 5;  (*                   'nonterminal' '::='
                                                                   <expression> '/' <expression> *)
   lhs [ -9] := 4; rhslength [ -9] := 3;  (*                   'setname' '::=' <terminalset>     *)
   lhs [-10] := 5; rhslength [-10] := 2;  (* <expression> ::=  <expression> '*'                  *)
   lhs [-11] := 5; rhslength [-11] := 2;  (*                   <expression> '+'                  *)
   lhs [-12] := 5; rhslength [-12] := 2;  (*                   <expression> <expression>         *)
   lhs [-13] := 5; rhslength [-13] := 1;  (*                   'terminal'                        *)
   lhs [-14] := 5; rhslength [-14] := 1;  (*                   'nonterminal'                     *)
   lhs [-15] := 5; rhslength [-15] := 1;  (*                   'setname'                         *)
   lhs [-16] := 5; rhslength [-16] := 3;  (*                   '(' <expression> ')'              *)
   lhs [-17] := 5; rhslength [-17] := 3;  (*                   <expression> '!' <expression>     *)
   lhs [-18] := 5; rhslength [-18] := 0;  (*                   ''                                *)
   lhs [-19] := 6; rhslength [-19] := 2;  (* <terminalset> ::= <terminalhead> ']'                *)
   lhs [-20] := 7; rhslength [-20] := 1;  (* <terminalhead>::= '['                               *)
   lhs [-21] := 7; rhslength [-21] := 2;  (*                   <terminalhead> 'terminal'         *)

  
  
   parse_regular_expressions;            (* process input and create NFA's *)

   create_dfa;
   minimize_dfa;
   print_program;

   writeln;
   writeln (ttyoutput);
  
   case errors of
      ok: begin
         writeln ('NO ERRORS');
         writeln (ttyoutput, 'NO ERRORS')
      end;
  
      warning: begin
         writeln ('WARNING (S) GIVEN');
         writeln (ttyoutput, 'WARNING (S) GIVEN')
      end;
  
      recover: begin
         writeln ('RECOVERY FROM MAJOR ERROR (S)');
         writeln (ttyoutput, 'RECOVERY FROM MAJOR ERROR (S)')
      end
   end

end (* lexgen *).
>D*;