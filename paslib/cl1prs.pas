$title cl/1 parsing routines.
$length 45

(*
 *  parser - module containing cl/1 recursive descent
 *  parsing routines.
 *
 *  each parsing routine obeys the following conventions.
 *  global variable 'token' is assumed to contain the current
 *  token on entry to a procedure.  each procedure is
 *  responsible for placing the next token in variable 'token'
 *  whenever the routine consumes the current token - except at 
 *  the end of a statement.
 *)

module parser;

$include cl1.inc

external var
   token: token_rec;   (* current token *)

external function scanner: token_rec;   (* the scanner *)
external procedure enter(identifier;var int; var boolean);
      (* enters id and value into symbol table *)
external procedure value_of(identifier; var int; var boolean);
      (* returns value of specified id from symbol table *)

procedure expr(var value:int; var err_code:err_types); forward;
procedure term(var value: int; var err_code:err_types);
      forward;
procedure op_term(var value:int; var err_code: err_types); forward;
procedure factor(var value:int; var err_code:err_types); forward;
procedure op_factor(var value:int; var err_code:err_types); forward;

$page assign_stmt
(*
 *  assign_stmt - procedure to parse an assignment statement.
 *  evaluates the expression to the right of the assignment
 *  operator and enters the id and expression value into the
 *  symbol table.
 *)

public procedure assign_stmt(var err_code:err_types);

var
   id_txt: identifier;   (* text of id being assigned to *)
   value: int;   (* value of expression on rhs *)
   enter_err: boolean;   (* true if symbol table error *)


begin
   id_txt := token.id_text;
   token := scanner;

   case token.t_type of

   assign_op:   (* <stmt> ::= id := <expr> eol   *)
      begin token := scanner;
         expr(value,err_code);
         if err_code = success then
         begin enter(id_txt,value,enter_err);
            if enter_err then err_code := sym_err
            else
            begin if token.t_type = error
               then err_code := lex_err
               else if token.t_type <> eol then err_code := syn_err;
            end
         end
      end;
   error:
      err_code := lex_err;
   others:
      err_code := syn_err
   end (* case *)
end;

$page expr_stmt
(*
 *  expr_stmt - procedure to parse an expression statement
 *  ( <stmt> ::=  := <expr> eol ) and print the result.
 *)

public procedure expr_stmt(var err_code: err_types);

var
   value: int;

begin
   token := scanner;
   expr(value,err_code);
   if err_code = success then
   begin writeln(tty,value);
      if token.t_type = error then err_code := lex_err
      else if token.t_type <> eol then err_code := syn_err
   end
end;

$page expr
(*
 *  expr - procedure to evaluate syntactic units corresponding
 *  to non-terminal <expr>.
 *)

procedure expr;

begin
   err_code := success;

   case token.t_type of

   id,cons,minus,l_paren:   (* <expr> ::= <term> <+-term> *)
      begin
         term(value,err_code);
         if err_code = success then
            op_term(value,err_code)
      end;
   error:
      err_code := lex_err;
   others:
      err_code := syn_err

   end (* case *)

end;

$page op_term
(*
 *  op_term - procedure to evaluate syntactic units corresponding
 *  to non-terminal <+-term>.
 *)

procedure op_term;

var
   value2:int;
   add: boolean;

begin
   err_code := success;

   case token.t_type of

   plus,minus:   (* <+-term> ::= +- <term> <+-term> *)
      begin add := token.t_type = plus;
         token := scanner;
         term(value2,err_code);
         if err_code = success then 
         begin if add then value := value + value2
            else value := value - value2;
            op_term(value,err_code)
         end
      end;
   eol,r_paren: ;  (* <+-term> ::= epsilon *)
   error:
      err_code := lex_err;
   others:
      err_code := syn_err

   end (* case *)

end;

$page term
(*
 *  term - procedure to evaluate syntactic units corresponding
 *  to non-terminal <term>.
 *)

procedure term;

begin
   err_code := success;

   case token.t_type of

      id,cons,minus,l_paren:   (* <term> ::= <factor> <* /factor>  *)
         begin
            factor(value,err_code);
            if err_code = success then
               op_factor(value,err_code)
         end;
      error:
         err_code := lex_err;
      others:
         err_code := syn_err
   end (* case *)

end;

$page op_factor
(*
 *  op_factor - procedure to evaluate syntactic units corresponding
 *  to non-terminal <* /factor>.
 *)

procedure op_factor;

var
   mult: boolean;
   value2: int;

begin
   err_code := success;

   case token.t_type of

      times,divide:   (* <* /factor> ::= <factor> <* /factor> *)
      begin
         mult := token.t_type = times;
         token := scanner;
         factor(value2,err_code);
         if err_code = success then
         begin if mult then value := value * value2
            else 
            begin
               if value2 <> 0 then value := value div value2
               else
               begin
                  err_code := div_err;
                  writeln(tty,' attempt to divide by zero');
                  break(tty)
               end
            end;
            if err_code = success then op_factor(value,err_code)
         end
      end;
      plus,minus,r_paren,eol: ;  (* <* /factor> ::= epsilon *)
      error:
         err_code := lex_err;
      others:
         err_code := syn_err
   end (* case *)

end;

$page factor
(*
 *  factor - procedure to evaluate syntactic units corresponding to 
 *  non-terminal <factor>.
 *)

procedure factor;

var
   id_txt: identifier;
   found,factor_flag: boolean;

begin
   err_code := success;
   factor_flag := false;

   case token.t_type of

      id:   (* <factor> ::= id *)
         begin
            id_txt := token.id_text;
            value_of(id_txt,value,found);
            if not found then
            begin err_code := id_err;
               writeln(tty, ' identifier ''', id_txt, ''' undefined');
               break(tty)
            end
         end;
      cons:   (* <factor> ::= cons  *)
         value := token.cons_val;
      l_paren:   (* <factor> ::= ( <expr> )  *)
         begin
            token := scanner;
            expr(value,err_code);
            if err_code = success then
               if token.t_type = error then err_code := lex_err
               else if token.t_type <> r_paren then err_code := syn_err
         end;
      minus:   (* <factor> ::= - <factor> *)
         begin
            token := scanner;
            factor(value,err_code);
            if err_code = success then value := -value;
            factor_flag := true;
         end;
      error:
         err_code := lex_err;
      others:
         err_code := syn_err


   end (* case *);

   if (err_code = success) andif not factor_flag then token := scanner

end.
    