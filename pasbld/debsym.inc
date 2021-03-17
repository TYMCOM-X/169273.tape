(* DEBSYM.INC - include file for public routines of Pascal Debugger
   module DEBSYM.  *)

external procedure st$init;
external procedure st$open (file_name: file_name_string);
external procedure st$close;
external function  st$file: file_name_string;
external function  deref$ptr (offset: half_word): ptr;
