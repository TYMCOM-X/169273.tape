PROGRAM grammar;
  (*a program to manipulate a grammar*)

CONST number_of_symbols = 13;

TYPE
  symbol = 1 .. number_of_symbols;
  symbol_table = ARRAY[symbol] OF STRING[20];
  symbol_set = ARRAY[symbol] OF BOOLEAN;
  symbol_link = ^symbol_unit;
  symbol_unit = RECORD production_symbol: symbol; next: symbol_link END;
  production_link = ^production;
  production = RECORD symbol_list: symbol_link; next: production_link END;
  production_set = ARRAY[symbol] OF production_link;
  grammar_set = RECORD
    terminals: symbol_set;
    non_terminals: symbol_set;
    productions: production_set
    start_symbol: symbol
    END;

VAR grammar: grammar_set;

$PAGE symbol table utilities
PROCEDURE clear_symbol_table;
  (*clear the symbol table*)
  VAR table_index: LOWERBOUND(symbol_table) .. UPPERBOUND(symbol_table);
  BEGIN
    FOR table_index := MINIMUM(table_index) TO MAXIMUM(table_index)
    DO symbol_table[table_index] := ''
    END;

PROCEDURE load_symbol_table;
  (*load the symbol table from input*)
  VAR table_index: LOWERBOUND(symbol_table) .. UPPERBOUND(symbol_table);
  VAR name: symbol_name;
  VAR item_index: INTEGER;
  BEGIN
    READ(item_index);
    FOR item_index := item_index DOWNTO 1 DO BEGIN
      READLN(table_index, name);
      symbol_table[table_index] := name
      END
    END;

PROCEDURE store_symbol_table;
  (*store the symbol table to output*)
  VAR table_index: LOWERBOUND(symbol_table) .. UPPERBOUND(symbol_table);
  VAR item_count: INTEGER;
  BEGIN
    item_count := 0;
    FOR table_index := MINIMUM(table_index) TO MAXIMUM(table_index)
    DO IF symbol_table[table_index] <> ''
    THEN item_count := item_count + 1;
    WRITELN(item_count);
    FOR table_index := MINIMUM(table_index) TO MAXIMUM(table_index)
    DO IF symbol_table[table_index] <> ''
    THEN WRITELN(table_index, symbol_table[table_index])
    END;

$PAGE non-terminal set utilities
PROCEDURE clear_non_terminal_set
  (*clear
  BEGIN
    END;

PROCEDURE load
  (*load
  BEGIN
    END;

PROCEDURE store
  (*store
  BEGIN
    END;

PROCEDURE display
  (*display
  BEGIN
    WRITELN(TTY, '
    END;

$PAGE terminal set utilities
PROCEDURE clear
  (*clear
  BEGIN
    END;

PROCEDURE load
  (*load
  BEGIN
    END;

PROCEDURE store
  (*store
  BEGIN
    END;

PROCEDURE display
  (*display
  BEGIN
    WRITELN(TTY, '
    END;

$PAGE production set utilities
PROCEDURE clear
  (*clear
  BEGIN
    END;

PROCEDURE load
  (*load
  BEGIN
    END;

PROCEDURE store
  (*store
  BEGIN
    END;

PROCEDURE display
  (*display
  BEGIN
    WRITELN(TTY, '
    END;

$PAGE general grammar utilities

PROCEDURE clear_grammar;
  (*clear the global grammar structure*)
  BEGIN
    clear_symbol_table;
    clear_non_terminal_set;
    clear_terminal_set;
    clear_production_set
    END;

PROCEDURE load_grammar(name: FILE_NAME);
  (*load in a grammar from the specified file*)
  BEGIN
    WRITELN(TTY); WRITELN(TTY, 'Load grammar from "', name, '".');
    clear_grammar;
    RESET(INPUT, name);
    load_symbol_table;
    load_starting_symbol;
    load_non_terminal_set;
    load_terminal_set;
    load_production_set;
    CLOSE(INPUT)
    END;

PROCEDURE store_grammar(name: FILE_NAME);
  (*store a grammar to the specified file*)
  BEGIN
    WRITELN(TTY); WRITELN(TTY, 'Store grammar to "', name, '".');
    REWRITE(OUTPUT, name);
    store_symbol_table;
    store_starting_symbol;
    store_non_terminal_set;
    store_terminal_set;
    store_production_set;
    CLOSE(OUTPUT);
    clear_grammar;
    END;

PROCEDURE display_grammar;
  (*display a grammar*)
  BEGIN
    WRITELN(TTY); WRITELN(TTY, 'Display grammar.');
    display_non_terminal_set;
    display_terminal_set;
    display_production_set;
    display_starting_symbol
    END;

PROCEDURE set_up;
  (*set up global structures*)
  TYPE production_index, symbol_table_index: symbol;
  BEGIN
    OPEN(TTY); RESET(TTY);
    FOR symbol_table_index := MINIMUM(symbol_table_index)
    TO MAXIMUM(symbol_table_index)
    DO symbol_table[symbol_table_index] := '';
    FOR production_index := MINIMUM(production_index)
    TO MAXIMUM(production_index)
    DO grammar.productions[production_index] := NIL;
    clear_grammar
    END;

BEGIN
  set_up;
  load_grammar('gramma.gmr');
  display_grammar;
  store_grammar('gramma.gmr')
  END.

