(* TEMP_FILE_NAME is given a 3 character string and returns a
   file name constructed from the 3 character string and the
   process id of the calling process.

   Currently this routine is implemented on the DEC10 and the
   VAX.  This copy of the source is used for both systems through
   use of the automatically enabled conditional compilation
   switches.

   Note that the name "TEMP_FILE_NAME" is somewhat misleading;
   whether or not a given system treats the given file name
   as a true temporary file (i.e., automatically deletes
   it at some point) is entirely system dependent.  *)

module tempfilename;

$SYSTEM jobnum.inc

public function temp_file_name ( str3: string[ 3 ] ): file_name;

begin
$IF VAX  temp_file_name := substr( jobnum, 3 ) || str3 || '.TMP';
$IF P10  temp_file_name := jobnum || str3 || '.TMP';
$IFNONE (VAX,P10)  Enable either "VAX" or "P10" switch !!!
end.
   