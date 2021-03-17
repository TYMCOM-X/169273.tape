

external procedure definit;

external function  lookup ( s: sym_string ): definition;

external procedure define ( name: sym_string; kind: macro_kinds; text: str_pointer );

external procedure undef ( s: sym_string );

external var       lib_file_id: string [40];

external function  lib_save: boolean;

external function  lib_load: boolean;
