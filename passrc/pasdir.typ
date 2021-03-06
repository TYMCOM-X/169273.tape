(*PASDIR.TYP, last modified 11/21/83, zw*)
$IFNOT pasdirtyp
(*SYSTEM DTIME.TYP*)
(*this should eventually be renamed to TYMDIR*)

TYPE (*type definitions for directory utility*)
    dir_ext_id = STRING [80]; (*string describing directory*)
    dir_int_id = 0..15; (*internal identifier*)
    dir_fname = STRING [40]; (*file name string*)
    dir_fchar = PACKED ARRAY [1..9] OF CHAR; (* file name and extension*)
    dir_m_str = STRING [50]; (*pattern matching string*)
    dir_errors = ( (*error scalar type*)
    dir_ok, (*no error*)
    dir_no_open, (*DIR_OPEN can't open directory file*)
    dir_bad_ppn, (*invalid PPN string for DIR_OPEN*)
    dir_bad_int_id, (*invalid internal ID to DIR_NEXT*)
    dir_not_open, (*no directory open on internal ID*)
    dir_eof, (*end of file reached on directory*)
    dir_no_file (*DIR_ATTR can't find file*)
    );
    dir_attrs = RECORD
      name: dir_fchar;
      protect: PACKED ARRAY [1..3] OF CHAR; (*protection as digits*)
      size: INTEGER; (*size in words*)
      creation: dtime_int; (*day/time of creation*)
      accessed: date_int (*date of most recent access*)
    END;
$ENABLE pasdirtyp
$ENDIF
   