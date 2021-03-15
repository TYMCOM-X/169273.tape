DSKB: ALTER .CTL ;Compile and add ALTER.
DSKB: ALTER .REL ;Alter-mode editing.
DSKB: ALTER .REQ ;ALTER require file.
DSKB: ALTER .SAI ;Do SOS-style altermode editing.
DSKB: BAILIN.SAI ;Include file for use when BAILing.
DSKB: BAYSAI.SAI ;Basic Sail declarations.
DSKB: CALLI .REQ ;CALLI number definitions.
DSKB: CCL   .CTL ;Compile and add CCL.
DSKB: CCL   .REL ;Concise Command Line.
DSKB: CCL   .REQ ;CCL require file.
DSKB: CCL   .SAI ;Provide standard CCL linkage.
DSKB: CHAN  .CTL ;Compile and add CHAN.
DSKB: CHAN  .REL ;Channel manipulation.
DSKB: CHAN  .REQ ;CHAN require file.
DSKB: CHAN  .SAI ;Return information about a file on a given channel.
DSKB: CMUPPN.CTL ;Compile and add CMUPPN.
DSKB: CMUPPN.REL ;Convert a CMU PPN.
DSKB: CMUPPN.REQ ;CMUPPN require file.
DSKB: CMUPPN.SAI ;Do CMU PPN mapping.
DSKB: CORSER.CTL ;Compile and add CORSER.
DSKB: CORSER.REL ;Core service.
DSKB: CORSER.REQ ;CORSER require file.
DSKB: CORSER.SAI ;Utility routines to manipulate core.
DSKB: CSWIT .SAI ;Compile switch processor.
DSKB: CURSOR.CTL ;Compile and add CURSOR.
DSKB: CURSOR.REL ;Cursor definitions.
DSKB: CURSOR.REQ ;CURSOR require file.
DSKB: CURSOR.SAI ;Cursor handling package for various terminals.
DSKB: DATE  .CTL ;Compile and add DATE.
DSKB: DATE  .REL ;Dates.
DSKB: DATE  .REQ ;DATE require file.
DSKB: DATE  .SAI ;Procedures to return date information.
DSKB: DIABLO.SAI ;Diablo driver for PUB output files.
DSKB: DSKPPN.CTL ;Compile and add DSKPPN.
DSKB: DSKPPN.REL ;Get a PPN.
DSKB: DSKPPN.REQ ;DSKPPN require file.
DSKB: DSKPPN.SAI ;Get PPN of default directory.
DSKB: EFILE .CTL ;Compile and add EFILE.
DSKB: EFILE .REL ;Easy file i/o.
DSKB: EFILE .REQ ;EFILE require file.
DSKB: EFILE .SAI ;Easy file I/O.
DSKB: FILDEF.CTL ;Compile and add FILDEF.
DSKB: FILDEF.REL ;File definition.
DSKB: FILDEF.REQ ;FILDEF require file.
DSKB: FILDEF.SAI ;File name manipulations.
DSKB: FILE  .CTL ;Compile and add FILE.
DSKB: FILE  .REL ;File i/o.
DSKB: FILE  .REQ ;FILE require file.
DSKB: FILE  .SAI ;Read or write files.
DSKB: FILE2 .CTL ;Compile and add FILE2.
DSKB: FILE2 .REL ;File i/o.
DSKB: FILE2 .REQ ;FILE2 require file.
DSKB: FILE2 .SAI ;Simple file I/O utilities from FILE.SAI.
DSKB: FSORT .HLP ;Help for file sort program.
DSKB: FSORT .REL ;Sort a file.
DSKB: FSORT .SAI ;File sort program.
DSKB: GRIPE .CTL ;Compile and add GRIPE.
DSKB: GRIPE .REL ;Post a Gripe.
DSKB: GRIPE .REQ ;GRIPE require file.
DSKB: GRIPE .SAI ;Module to mail Gripes.
DSKB: HOMFIL.CTL ;Compile and add HOMFIL.
DSKB: HOMFIL.REL ;Name of home.
DSKB: HOMFIL.REQ ;HOMFIL require file.
DSKB: HOMFIL.SAI ;Determine the file the calling program came from.
DSKB: LIBARH.CTL ;Initial creation of whole high-segment version of
                 ;library.
DSKB: LIBARH.MIC ;MICro to add a module to LIBARH.
DSKB: LIBARH.REL ;High-segment version of SAIL libary.
DSKB: LIBARY.CTL ;Initial creation of whole library.
DSKB: LIBARY.DOC ;Instructions for users of the SAI: library.
DSKB: LIBARY.MIC ;Add a module to SAI: LIBARY.
DSKB: LIBARY.REL ;Low-segment version of SAIL library.
DSKB: LIBARY.REQ ;Require file for users of the entire library.
DSKB: MAKESI.CTL ;Compile and add MAKESI.
DSKB: MAKESI.REQ ;MAKESI require file.
DSKB: MAKESI.SAI ;Generate index vectors.
DSKB: MONSYM.SAI ;Monitor symbolics.
DSKB: MONTOR.CTL ;Compile and add MONTOR.
DSKB: MONTOR.REL ;Monitor requests.
DSKB: MONTOR.REQ ;Monitor requests.
DSKB: MONTOR.SAI ;Monitor requests.
DSKB: OLDLIB.SFD ;Old CMU Sail library.
DSKB: PARSER.CTL ;Compile and add PARSER.
DSKB: PARSER.REL ;Parse filename.
DSKB: PARSER.REQ ;PARSER require file.
DSKB: PARSER.SAI ;Do switch parsing.
DSKB: PURGE .CTL ;Control file to purge bulletin board items.
DSKB: QNET  .CTL ;Compile and add QNET.
DSKB: QNET  .REL ;Post mail.
DSKB: QNET  .REQ ;QNET require file.
DSKB: QNET  .SAI ;Submit a request to QNET to mail files.
DSKB: SAIL  .DOC ;Reference manual for Sail user routine library.
DSKB: SCN7B .TEC ;Fix up SCN7B.
DSKB: SCN7C .CTL ;Compile and add SCN7C.
DSKB: STRFIL.CTL ;Compile and add STRFIL.
DSKB: STRFIL.REL ;String handling procedures.
DSKB: STRFIL.REQ ;STRFIL require file.
DSKB: STRFIL.SAI ;String handling procedures.
DSKB: SWINI .CTL ;Compile and add SWINI.
DSKB: SWINI .REL ;Switch.ini processing.
DSKB: SWINI .REQ ;Look for switches in SWITCH.INI.
DSKB: SWINI .SAI ;Look for switches in SWITCH.INI.
DSKB: TABUTL.CTL ;Compile and add TABUTL.
DSKB: TABUTL.REL ;Table utilities.
DSKB: TABUTL.REQ ;TABUTL require file.
DSKB: TABUTL.SAI ;Utilities for integer and string tables.
DSKB: TMPFIL.CTL ;Compile and add TMPFIL.
DSKB: TMPFIL.REL ;TMP: file i/o.
DSKB: TMPFIL.REQ ;TMPFIL require file.
DSKB: TMPFIL.SAI ;Work with TMP: files.
DSKB: TTYSER.CTL ;Compile and add TTYSER.
DSKB: TTYSER.REL ;TTY service.
DSKB: TTYSER.REQ ;TTYSER require file.
DSKB: TTYSER.SAI ;Simple terminal handling.
DSKB: UDATE .CTL ;Compile and add UDATE.
DSKB: UDATE .REL ;Universal date.
DSKB: UDATE .REQ ;UDATE require file.
DSKB: UDATE .SAI ;Convert between DEC and universal date/time.
DSKB: UFD   .COM ;Comment file for SAI:[5,104]. 10-Feb-79.  CMU Sail
                 ;library. Contact:  David Philllips.  Added MONTOR
                 ;for monitor calls. Converted cases with elses to
                 ;ifs.  Converted <= to leq.
DSKB: WILD  .CTL ;Compile and add WILD.
DSKB: WILD  .REL ;Wild card filenames.
DSKB: WILD  .REQ ;WILD require file.
DSKB: WILD  .SAI ;Wild card file searching.
DSKB: WILDGF.CTL ;Compile and add WILDGF.
DSKB: WILDGF.REL ;Wild card filenames.
DSKB: WILDGF.REQ ;WILDGF require file.
DSKB: WILDGF.SAI ;Package to do wild card file searching.
DSKB: XLOOK .CTL ;Compile and add XLOOK.
DSKB: XLOOK .REL ;Extended lookup.
DSKB: XLOOK .REQ ;XLOOK require file.
DSKB: XLOOK .SAI ;Procedures for extended lookup, etc.
DSKB: ZYGO  .REL ;Game?
DSKB: ZYGO  .SAI ;See the snake chase the zygotes.
