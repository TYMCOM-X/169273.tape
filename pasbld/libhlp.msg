TYM-Pascal File Library Utility
OPEN <library file>
(* The OPEN command opens a new/old library file using the OPEN_LIBRARY
   routine. *)
.
PACK
(* The PACK command uses the PACK routine to pack the library file *)
.
ADD <file list>
(* The ADD command adds external files to the currently open library
   using the WR_WORD routine. All disk transfers are done in one block
   chunks.  *)
.
EXTRACT <file list>
(* The EXTRACT command copies a library segment to an external file of the
   same name using the RD_WORDS routine *)
.
DELETE <file list> (requires confirmation)
(* The DELETE command deletes library segments (no packing here) by
   simply changing a segment's IN_USE flag in the directory entry *)
.
DIRECTORY [/A] <file list>
(* The DIR command uses the RD_DIR and FIND_DIR routines to read and find
   the specified directory entries and print information about them. *)
.
HELP <command>
(* Display information from help file*)
.
QUIT
(* exit LIB*)
.
   