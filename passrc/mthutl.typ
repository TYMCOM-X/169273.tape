$PAGE UTLMTH.TYP, last modified 4/3/84, zw
$IFNOT utlmthtyp

(*The "big" integer maps into a PDP10 machine word.*)

TYPE
artificial_integer = RECORD
  CASE size: (normal, big) OF
  normal: (normal_value: INTEGER);
  big: (big_value: PACKED ARRAY [1 .. 36] OF 0 .. 1)
END;

$ENABLE utlmthtyp
$ENDIF
   