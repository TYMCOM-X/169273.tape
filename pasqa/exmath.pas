$ENABLE MDSI_PASCAL
MODULE ex_math;
$PAGE type include files
(* EXNUM.TYP - types needed by EXNUM test program and
   EXNUM PDP-10 stubs. *)

const
  ex_num_str_length = 40;

type
  exnum_str_range = 0..41;
  ex_numb_string = string[ ex_num_str_length ];

$IF MDSI_PASCAL
  ex_numb = minimum(real)..maximum(real) prec 16; (* Always real on Dec10, to be changed on DG  *)
$ENDIF
$IF MP_PASCAL
  ex_numb = record
    r1: real;
    r2:real;
  end;
$ENDIF

  ex_status = (ex_ok, ex_lst_prec, ex_ill_fmt, ex_rng_excd); (* returned by EXCSN  *)

  (* END INCLUDE FILE EXNUM.TYP -------------------------------------- *)
$PAGE local types
$PAGE level-0 vars
$ENABLE(PUBLIC)
(* INCLUDE FILE EXMATH.VAR

Purpose:  Contains the var declarations for the math package
            error return counters.

Requires:     Nothing

Contents:
          EX_IF
         EX_OF
         EX_UF
         EX_DZ
         EX_DE

Update List:   10/27/80 D. Jeris

Conditional switches in effect at expansion:
$IF (PASCAL_10)      PASCAL-10
$IF (MP_PASCAL) MP-PASCAL
$IF (PUBLIC)      PUBLIC
$IF (EXTERNAL)  EXTERNAL

------------------------------------------------------------------------ *)
$IF (PUBLIC) public
$IF (EXTERNAL) external
var
  ex_if : integer;          (* Number of illegal extended number format errors *)
  ex_of : integer;               (* Number of overflow errors  *)
  ex_uf : integer;            (* Number of underflow errors *)
  ex_dz : integer;            (* Number of divide by zero errors *)
  ex_de : integer;               (* number of domain error on input errors *)
  ex_ls : integer;                (* number of loss of significance errors *)

(* END EXMATH.VAR ----------------------------------------------------------- *)
$DISABLE(PUBLIC)
$PAGE ex_sqrd

public function ex_sqrd ( x: ex_numb ): ex_numb;

begin
  ex_sqrd := x * x;
end;
$PAGE ex_add

(*!   Sum := addend1 + addend2   !*)

PUBLIC FUNCTION ex_add ( addend1 : ex_numb; addend2 : ex_numb ) : ex_numb;

BEGIN
  ex_add := addend1 + addend2
END;
$PAGE ex_sub

(*!  Difference := minuend - subtrahend  !*)

PUBLIC FUNCTION ex_sub ( minuend : ex_numb;
                        subtrahend : ex_numb ) : ex_numb;

BEGIN
  ex_sub := minuend - subtrahend
END;
$PAGE ex_mult

(*!  Product := factor1 * factor2   !*)

PUBLIC FUNCTION ex_mult ( factor1 : ex_numb;
                   factor2 : ex_numb ) : ex_numb;

BEGIN
  ex_mult := factor1 * factor2
END;
$PAGE ex_div

(*!   Quotient := dividend/divisor    !*)

PUBLIC FUNCTION ex_div ( dividend : ex_numb;
                     divisor : ex_numb ) : ex_numb;

BEGIN
  ex_div := dividend / divisor
END;
$PAGE ex_exp

(*!   Result := term ** exponent    !*)
(*!   on dg result := e ** exponent * ln (term)    !*)

PUBLIC FUNCTION ex_exp( term : ex_numb; exponent : ex_numb ) : ex_numb;


BEGIN
$IF MDSI_PASCAL  ex_exp := term ** exponent
$IF mp_pascal  ex_exp := exp (exponent * ln (term))
END;
$PAGE ex_sin

(*!  sine := sin(angle/360/2*pi)   !*)

PUBLIC FUNCTION ex_sin ( angle : ex_numb ) : ex_numb;

BEGIN
  ex_sin := sin( angle / 57.29577951 )
END;
$PAGE ex_cos


(*!   cosine := cos(angle/180/pi)   !*)

PUBLIC FUNCTION ex_cos( angle : ex_numb ) : ex_numb;

BEGIN
  ex_cos := cos( angle / 57.29577951 )
END;
$PAGE ex_tan

(*!   Tangent := sin(angle/180/pi)/cos(angle/180/pi)   !*)

PUBLIC FUNCTION ex_tan( angle : ex_numb ) : ex_numb;

BEGIN
  ex_tan := sin( angle / 57.29577951 ) / cos( angle / 57.29577951 )
END;
$PAGE ex_atan

(*!  Angle := atan(value)  !*)

PUBLIC FUNCTION ex_atan( value : ex_numb ) : ex_numb;

BEGIN
  ex_atan := 57.29577951 * arctan( value )
END;
$PAGE ex_asin


(*!  Angle := 180/pi * arctan(value/sqrt(1-value**2))   !*)

PUBLIC FUNCTION ex_asin( value : ex_numb ) : ex_numb;

BEGIN
  ex_asin :=
    57.29577951 * ( arctan ( value / ( sqrt( 1 - ( value * value
      ) ) ) )  )
END;
$PAGE ex_acos

(*!  Angle := pi/2 - arctan(x/sqrt(1-x**2))    !*)

PUBLIC FUNCTION ex_acos( value : ex_numb ) : ex_numb;

BEGIN
  ex_acos :=
    57.29577951 * ( ( 3.141592654 / 2 ) - ( arctan( value / ( sqrt( 1 - (
    value * value ) ) ) ) ) )
END;
$PAGE ex_ln

(*!     Value := ln(term)       !*)

PUBLIC FUNCTION ex_ln ( term : ex_numb ) : ex_numb;

BEGIN
  ex_ln := ln( term )
END;
$PAGE ex_log

(*!    Value := 0.4342945 * ln (term)    !*)


PUBLIC FUNCTION ex_log ( term : ex_numb ) : ex_numb;

BEGIN
  ex_log := 0.4342945 * ln( term )
END;
$PAGE ex_ip

(*!   value := trunc (term)     !*)

PUBLIC FUNCTION ex_ip( term : ex_numb ) : ex_numb;

BEGIN
  ex_ip := trunc( term )
END;
$PAGE ex_fp

(*!  value := term - integer part (term)   !*)

PUBLIC FUNCTION ex_fp( term : ex_numb ) : ex_numb;

BEGIN
  ex_fp := term - ex_ip ( term )
END;
$PAGE ex_rnd

(*!  Doesn't do anything but check for overflow and underflow for now  !*)

PUBLIC FUNCTION ex_rnd( term : ex_numb ) : ex_numb;

BEGIN
  ex_rnd := 0.0;
  IF abs( term ) < 10e-6 THEN
    ex_uf := ex_uf + 1
  ELSE IF abs( term ) > 999999999.0
  THEN ex_of := ex_of + 1
  ELSE 
    ex_rnd := term
END;
$PAGE ex_lt
PUBLIC FUNCTION ex_lt ( term1 : ex_numb; term2 : ex_numb ) : boolean;

BEGIN
  ex_lt := ( term1 < term2 )
END;
$PAGE ex_le
PUBLIC FUNCTION ex_le ( term1 : ex_numb; term2 : ex_numb ) : boolean;

BEGIN
  ex_le := ( term1 <= term2 )
END;
$PAGE ex_gt
PUBLIC FUNCTION ex_gt ( term1 : ex_numb; term2 : ex_numb ) : boolean;

BEGIN
  ex_gt := ( term1 > term2 )
END;
$PAGE ex_ge
PUBLIC FUNCTION ex_ge ( term1 : ex_numb; term2 : ex_numb ) : boolean;

BEGIN
  ex_ge := ( term1 >= term2 )
END;
$PAGE ex_eq
PUBLIC FUNCTION ex_eq ( term1 : ex_numb; term2 : ex_numb ) : boolean;

BEGIN
  ex_eq := ( term1 = term2 )
END;
$PAGE ex_ne
PUBLIC FUNCTION ex_ne ( term1 : ex_numb; term2 : ex_numb ) : boolean;

BEGIN
  ex_ne := ( term1 <> term2 )
END;
$PAGE excsn
PUBLIC FUNCTION excsn_cvt_str_no ( number : ex_numb_string; VAR cursor: exnum_str_range; VAR status : ex_status) : ex_numb;


  (*!     For each character in the input string:
              determine the type of the character token
             perform the action associated with the transition
             make the transition
   until arrive at error or terminal transition state              !*)


  TYPE

    parse_states = ( start,
      neg_mantissa,
      left_add,
      no_integer_part,
      right_add,
      scientific_notation,
      add_to_exponent,
      neg_exponent,
      terminal_state,
      error_state
      );

    tokens = (
      plus_minus,
      digit,
      decimal,
      e_format,
      end_of_string,
      other_token
      );


$IF (MP_PASCAL) external
$IF (MDSI_PASCAL) static

  VAR

    state_table : ARRAY[ parse_states, tokens ] OF parse_states
$IF (MDSI_PASCAL)
      :=
      ( neg_mantissa, left_add, no_integer_part, error_state,
  error_state, error_state ,
       error_state, left_add, no_integer_part, error_state, error_state,
  error_state ,
       error_state, left_add, right_add, scientific_notation,
  terminal_state, error_state ,
       error_state, right_add, error_state, error_state, error_state,
  error_state ,
       error_state, right_add, error_state, scientific_notation,
       terminal_state, error_state ,
       neg_exponent, add_to_exponent, error_state, error_state,
        error_state, error_state ,
       error_state, add_to_exponent, error_state, error_state,
    terminal_state, error_state ,
       error_state, add_to_exponent, error_state, error_state,
 error_state, error_state ,
      error_state, error_state, error_state, error_state, error_state,
    error_state,
      error_state, error_state, error_state, error_state, error_state,
  error_state )
$ENDIF
    ;

    l_digit_counter : integer;
    r_digit_counter : integer;
    r_digit_count : integer;
    exp_neg : boolean;
    temp_exponent : ex_numb;
    mant_neg : boolean;
    temp_result : ex_numb;
    tens_counter : ex_numb;
    parts_tens_counter : ex_numb;
    exponent_tens_counter : ex_numb;
    num : ex_numb;
    string_ptr : integer;
    current_state : parse_states;
    token_type : tokens;

BEGIN
(*

  string_ptr := 0;                    (* Initialize the ptr to current token  *)
  l_digit_counter := 0;
  r_digit_counter := 0;
  current_state := start;
  tens_counter := 0.0;
  parts_tens_counter := 0.0;
  temp_result := 0.0;
  temp_exponent :=0.0;
  exp_neg := false;
  mant_neg := false;
  exponent_tens_counter := 0.0;

  REPEAT                            (* For each character in NUMBER  *)

    string_ptr := string_ptr + 1;

    (*      Determine the type of the token         *)

    IF string_ptr > length( number ) 
    THEN token_type := end_of_string
    ELSE IF ( number[string_ptr] = '+' )OR( number[string_ptr] = '-' )
    THEN token_type := plus_minus
    ELSE IF ( number[string_ptr] >= '0' ) AND
      ( number[string_ptr] <= '9' )
    THEN BEGIN
    token_type := digit;
    if number[string_ptr] = '0' then num := 0.0;
    if number[string_ptr] = '1' then num := 1.0;
    if number[string_ptr] = '2' then num := 2.0;
    if number[string_ptr] = '3' then num := 3.0;
    if number[string_ptr] = '4' then num := 4.0;
    if number[string_ptr] = '5' then num := 5.0;
    if number[string_ptr] = '6' then num := 6.0;
    if number[string_ptr] = '7' then num := 7.0;
    if number[string_ptr] = '8' then num := 8.0;
    if number[string_ptr] = '9' then num := 9.0
  end
    ELSE IF number[string_ptr] = '.'
    THEN token_type := decimal
    ELSE IF ( number[string_ptr] = 'e' )OR( number[string_ptr] = 'E' )
    THEN token_type := e_format
    ELSE token_type := other_token;

    (*      Do any action associated with the transition    *)

    CASE state_table [ current_state, token_type ] OF

      neg_mantissa : mant_neg := true;
      left_add : BEGIN
                   l_digit_counter := l_digit_counter + 1;
                temp_result :=
                     num + temp_result * 10.0
               END;
      right_add : 
                 IF current_state <> left_add
                  THEN
                  BEGIN
                      r_digit_counter := r_digit_counter + 1;
                    parts_tens_counter :=
                   parts_tens_counter + 1;
                     temp_result :=
                  temp_result + num / ex_exp( 10,
                       parts_tens_counter )
              END;
      add_to_exponent : BEGIN
        temp_exponent := num + temp_exponent * 10.0
      end;
      neg_exponent : exp_neg := true;
      terminal_state : begin
        if exp_neg
        then temp_exponent := -temp_exponent;
        if mant_neg
        then temp_result := - temp_result;
        temp_result := temp_result * ex_exp (10, temp_exponent);
        if temp_result > 10e9 then
        begin
          excsn_cvt_str_no := 10e9;
          status := ex_rng_excd
        end
        else
        if temp_result < -10e9 then
        begin
          excsn_cvt_str_no := -10e9;
          status := ex_rng_excd
        end
        else begin
          excsn_cvt_str_no := temp_result;
          if (l_digit_counter + r_digit_counter <= 9) and (r_digit_counter <= 6) then
          status := ex_ok
          else status := ex_lst_prec
        end
      end;
      error_state : status := ex_ill_fmt;
      start, no_integer_part, scientific_notation :
  end;

      (*      Make the transition     *)

      current_state := state_table[ current_state, token_type ] ;

      UNTIL ( current_state = terminal_state )OR
      ( current_state = error_state );

      (****temporary code*****)

      IF current_state = error_state
      THEN writeln( tty, 'Bad number' )
      ELSE IF current_state <> terminal_state
      THEN writeln( tty, 'something strange', ord( current_state ) )


*)
    getstring( number, excsn_cvt_str_no );
    status := ex_ok;

      END;
$PAGE excns
      PUBLIC FUNCTION excns_cvt_no_str ( value : ex_numb ) :
                                                ex_numb_string ;

      BEGIN
$IF MDSI_PASCAL putstring(excns_cvt_no_str, value:40:20 );
      END;
$PAGE ex_ng
      PUBLIC FUNCTION ex_ng( value : ex_numb ) : ex_numb;

BEGIN
  ex_ng := - ( value )
END;
$PAGE ex_ird
PUBLIC FUNCTION ex_ird( value : ex_numb ) : integer;

BEGIN
  ex_ird := round( value )
END;
$PAGE ex_abs
PUBLIC FUNCTION ex_abs( value : ex_numb ) : ex_numb;

BEGIN
END.
  