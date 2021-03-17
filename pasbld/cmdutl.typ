(* CMDUTL.typ - last modified 8/13/77 - type declarations for command 
   utility subroutines. *)


TYPE						(* system dependent file name types *)
  FILE_ID = STRING[75];				(* maximum TOPS-10 filename with SFD's *)
  EXTENSION = PACKED ARRAY [1..3] OF CHAR;


CONST CMDLINELEN = 254;				(* command line declaration *)
TYPE
  CMDLINE = STRING[254];			(* string itself *)
  CMDLINEIDX = 0..255;				(* index of above *)

