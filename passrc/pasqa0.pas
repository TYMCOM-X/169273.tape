$WIDTH=100
$LENGTH=55
$TITLE PASQA0.PAS, last modified 2/1/84, zw
PROGRAM pasqa0;
(*TYM-Pascal compiler -- quality assurance driver program*)

$PAGE exception definitions

(*minimal case*)
EXCEPTION exception_0;

(*many case*)
EXCEPTION exception_1, exception_2, exception_3;

(*public case*)
PUBLIC EXCEPTION exception_5;

(*external case*)
EXTERNAL EXCEPTION exception_6;

$PAGE simple constant definitions

(*minimum case*)
CONST const_1 = 1;

(*multiple case*)
CONST const_2 = 2; const_3 = 3; const_4 = 4;

(*public case*)
PUBLIC CONST const_5: INTEGER = 5;

(*external case*)
EXTERNAL CONST const_6: INTEGER;

(*standard types case*)
CONST
const_boolean_1 = TRUE; const_boolean_2: BOOLEAN = FALSE;
const_integer_1 = 1; const_integer_2: INTEGER = 2;
const_real_1 = 1.0; const_real_2: REAL = 2.0;
const_string_1 = 'STRING 1'; const_string_2: STRING = 'STRING 2';
const_char_1 = '1'; const_char_2: CHAR = '2';
const_pointer = NIL;
const_set_1 = []; const_set_2: SET OF BOOLEAN = [TRUE, FALSE];
const_array: ARRAY [BOOLEAN] OF INTEGER = (1, 2);

$PAGE type definitions

(*minimum case*)
TYPE type_1 = INTEGER;

(*multiple case*)
TYPE type_2 = INTEGER; type_3 = INTEGER; type_4 = INTEGER;

(*standard types case*)
TYPE
type_boolean = BOOLEAN;
type_integer = INTEGER;
type_real = REAL;
type_string = STRING;
type_char = CHAR;
type_pointer = ^INTEGER;
type_set = SET OF BOOLEAN;
type_array = ARRAY [BOOLEAN] OF INTEGER;
type_packed_array = PACKED ARRAY [BOOLEAN] OF INTEGER;
type_record = RECORD variable: INTEGER END;
type_packed_record = PACKED RECORD variable: INTEGER END;
type_procedure = PROCEDURE;
type_function = FUNCTION: INTEGER;

$PAGE complex type definitions

(*general purpose value*)
type_value = INTEGER;

(*simple linear list of values*)
type_list = ^type_list_record;
type_list_record = RECORD value: type_value; next: type_list END;

(*simple binary tree of values*)
type_binary_tree = ^type_binary_tree_node;
type_binary_tree_node = RECORD
  value: type_value; left, right: type_binary_tree
END;

(*dynamic strings*)
type_generic_string = STRING[*];
type_generic_string_pointer = ^type_generic_string;
type_flex_string = PACKED ARRAY [1 .. *] OF CHAR;
type_flex_string_pointer = ^type_flex_string;

(*dynamic array of values*)
type_flex_array = ARRAY [1 .. *] OF type_value;
type_array_pointer = ^type_flex_array;

(*simple stack of values*)
type_stack = ^type_stack_record;
type_stack_record = RECORD value: type_value; previous: type_stack END;

(*simple queue of values*)
type_queue_pointer = ^type_queue_node;
type_queue = RECORD head, tail: type_queue_pointer END;
type_queue_node = RECORD
  value: type_value; next, previous: type_queue_pointer
END;

(*character and integer sets*)
type_char_set = SET OF CHAR;
type_integer_set = type_binary_tree;

(*matrices of values*)
type_matrix_1 = ARRAY [1 .. 1] OF ARRAY [1 .. 1] OF type_value;
type_matrix_2 = ARRAY [1 .. 2] OF ARRAY [1 .. 2] OF type_value;
type_matrix_3 = ARRAY [1 .. 3] OF ARRAY [1 .. 3] OF type_value;
type_matrix = ^ARRAY [1 .. *] OF ^ARRAY [1 .. *] OF type_value;

$PAGE definition of symbol table and arithmetic expression

(*symbol table*)
type_symbol = ^type_symbol_record;
type_symbol_table = ^type_symbol_table_record;
type_symbol_record = RECORD
  symbol_value: type_value;
  symbol_name: type_flex_string
END;
type_symbol_table_record = RECORD
  symbol: type_symbol;
  less, greater: type_symbol_table
END;

(*arithmetic expression*)
type_arithmetic_expression = ^type_arithmetic_expression_node;
type_arithmetic_value_type = (constant, reference);
type_arithmetic_value = RECORD
  CASE value_type: type_arithmetic_value_type OF
  constant: (constant_value: type_value);
  reference: (reference_symbol: type_symbol)
END;
type_arithmetic_node_type = (value_node, operation_node);
type_arithmetic_operator = (add, subtract, multiply, divide);
type_arithmetic_expression_node = RECORD
  CASE node_type: type_arithmetic_node_type OF
  value_node: (arithmetic_value: type_arithmetic_value);
  operation_node: (
    arithmetic_operator: type_arithmetic_operator;
    operands: ^ARRAY [1 .. *] OF type_arithmetic_expression
  );
END;

$PAGE complex constants

CONST

(*generally complex*)
complex_const_1: ARRAY [1 .. 4] OF ARRAY [1 .. 4] OF INTEGER =
((11, 12, 13, 14), (21, 22, 23, 24), (31, 32, 33, 34), (41, 42, 43, 44));
complex_const_2:
ARRAY [1 .. 4] OF RECORD variable_1: STRING; variable_2: SET OF BOOLEAN END =
(('1', []), ('2', [TRUE]), ('3', [FALSE]), ('4', [TRUE, FALSE]));

(*matrix constants*)
const_matrix_1_zero: type_matrix_1 = ((0));
const_matrix_2_zero: type_matrix_2 = ((0, 0), (0, 0));
const_matrix_3_zero: type_matrix_3 = ((0, 0, 0), (0, 0, 0), (0, 0, 0));
const_matrix_1_one: type_matrix_1 = ((1));
const_matrix_2_one: type_matrix_2 = ((1, 1), (1, 1));
const_matrix_3_one: type_matrix_3 = ((1, 1, 1), (1, 1, 1), (1, 1, 1));
const_matrix_1_left: type_matrix_1 = ((1));
const_matrix_2_left: type_matrix_2 = ((1, 0), (0, 1));
const_matrix_3_left: type_matrix_3 = ((1, 0, 0), (0, 1, 0), (0, 0, 1));
const_matrix_1_right: type_matrix_1 = ((1));
const_matrix_2_right: type_matrix_2 = ((0, 1), (1, 0));
const_matrix_3_right: type_matrix_3 = ((0, 0, 1), (0, 1, 0), (1, 0, 0));

(*character sets*)
const_alpha_char_set: type_char_set = ['A' .. 'Z'];
const_numeric_char_set: type_char_set = ['0' .. '9'];
const_symbol_char_set: type_char_set =
  const_alpha_char_set + const_numeric_char_set + ['_'];
const_special_char_set: type_char_set =
['!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_',
'-', '+', '=', '`', '~', '{', '}', '[', ']', '|', '\',
':', ';', '"', '''', '<', ',', '>', '.', '?', '/'];
const_delimiter_char_set: type_char_set =
[MINIMUM(CHAR) .. MAXIMUM(CHAR)] -
(const_symbol_char_set + const_special_char_set);

(*nulls*)
const_null_boolean: type_boolean = FALSE;
const_null_integer: type_integer = 0;
const_null_string: type_string = '';
const_null_set = [];
const_null_pointer = NIL;
const_null_list: type_list = NIL;
const_null_binary_tree: type_binary_tree = NIL;
const_null_queue: type_queue = (NIL, NIL);
const_null_symbol_table: type_symbol_table = NIL;
const_null_arithmetic_expression: type_arithmetic_expression = NIL;

$PAGE simple procedure/function definitions

(*simple no-parameter case*)
PROCEDURE procedure_1; BEGIN END;
FUNCTION function_1: INTEGER; BEGIN function_1 := 1 END;

(*simple parameter case*)
PROCEDURE procedure_2(parameter: INTEGER); BEGIN END;
FUNCTION function_2(parameter: INTEGER): INTEGER;
BEGIN function_2 := parameter END;

(*complex parameter case*)
PROCEDURE procedure_3
(VAR parameter_1: INTEGER; (*minimum VAR case*)
VAR parameter_2, parameter_3, parameter_4: INTEGER; (*multiple VAR case*)
parameter_5: INTEGER; (*minimum value case*)
parameter_6, parameter_7, parameter_8: INTEGER); (*multiple value case*)
BEGIN
  parameter_1 := parameter_5;
  parameter_2 := parameter_6;
  parameter_3 := parameter_7;
  parameter_4 := parameter_8
END;

(*public case*)
PUBLIC PROCEDURE procedure_4; BEGIN END;
PUBLIC PROCEDURE procedure_5(parameter: INTEGER); BEGIN END;
PUBLIC FUNCTION function_3: INTEGER; BEGIN function_3 := 3 END;
PUBLIC FUNCTION function_4(parameter: INTEGER): INTEGER;
BEGIN function_4 := parameter END;

(*external case*)
EXTERNAL PROCEDURE procedure_6;
EXTERNAL PROCEDURE procedure_7(INTEGER; VAR INTEGER);
EXTERNAL FUNCTION function_5: INTEGER;
EXTERNAL FUNCTION function_6(INTEGER; VAR INTEGER): INTEGER;

$PAGE recursive procedures and functions

PROCEDURE recurse_2(parameter: INTEGER); FORWARD;

PROCEDURE recurse_1(parameter: INTEGER);
BEGIN
  IF parameter > 0 THEN recurse_2(parameter - 1)
END;

PROCEDURE recurse_2(parameter: INTEGER);
BEGIN
  IF parameter > 0 THEN recurse_1(parameter - 1)
END;

FUNCTION factorial_1(x: INTEGER): INTEGER;
VAR i: INTEGER;
BEGIN (*recursive factorial*)
  IF x IN [0, 1] THEN i := 1
  ELSE i := x * factorial_1(x - 1);
  factorial_1 := i;
END;

FUNCTION factorial(x: INTEGER): INTEGER;
VAR i, j: INTEGER;
BEGIN (*non-recursive factorial*)
  j := 1;
  FOR i := 1 TO x DO j := j * i;
  factorial := j
END;

$PAGE simple list manipulation

FUNCTION list_value(l: type_list): type_value;
BEGIN (*return value part of list*)
  ASSERT(l <> const_null_list);
  list_value := l^.value
END;

FUNCTION list_next(l: type_list): type_list;
BEGIN (*return next part of list*)
  ASSERT(l <> const_null_list);
  list_next := l^.next
END;

PROCEDURE dispose_list_value(VAR l: type_list);
VAR next: type_list;
BEGIN (*dispose of list value*)
  next := list_next(l);
  DISPOSE(l);
  l := next
END;

PROCEDURE dispose_list(VAR l: type_list);
BEGIN (*dispose of list*)
  WHILE l <> const_null_list DO dispose_list_value(l)
END;

FUNCTION list_length_1(l: type_list): INTEGER;
BEGIN (*recursive list length*)
  IF l = const_null_list THEN list_length_1 := 0
  ELSE list_length_1 := list_length_1(list_next(l)) + 1
END;

FUNCTION list_length(l: type_list): INTEGER;
VAR i: INTEGER; list: type_list;
BEGIN (*non_recursive list length*)
  i := 0; list := l;
  WHILE list <> const_null_list DO BEGIN
    i := i + 1;
    list := list_next(list)
  END;
  list_length := i
END;

BEGIN
END.   