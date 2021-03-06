$WIDTH=100
$LENGTH=55
$TITLE LIST.PAS, last modified 10/14/83, zw

PROGRAM dolist;
(*list a file*)
$SYSTEM PASUTL
$SYSTEM LSTUTL

VAR
    opts: ARRAY[1 .. 14] OF int;
    optkeys: ARRAY[1 .. 14] OF wrdtyp := ('ODD', 'EVEN', 'LEGAL', 'DOUBLE',
      'BREAK', 'HEADER', 'WRAP', 'LOGICAL', 'GO', 'START', 'COUNT', 'MARGIN',
	'WIDTH', 'CLEAN');
    f: listflags;
    s, c, m, w: int;

BEGIN
  WHILE start('LIST', '') DO BEGIN
    opts := (0,0,0,0,0,0,0,0,0,0,0,0,0, 0);
    f := [];
    s := -1;
    c := -1;
    m := -1;
    w := -1;
$PAGE
    getopt(optkeys, opts);
    IF opts[1] > 0 THEN
      f := f + [listodd]
    ELSE IF opts[2] > 0 THEN
      f := f + [listeven];
    IF opts[3] > 0 THEN
      f := f + [listlegal];
    IF opts[4] > 0 THEN
      f := f + [listdouble];
    IF opts[5] > 0 THEN
      f := f + [listbreak];
    IF opts[6] > 0 THEN
      f := f + [listheader];
    IF opts[7] > 0 THEN
      f := f + [listwrap];
    IF opts[8] > 0 THEN
      f := f + [listlogical];
    IF opts[9] > 0 THEN
      f := f + [listgo];
    IF opts[10] > 0 THEN
      optnum(s, -1, opts[10]);
    IF opts[11] > 0 THEN
      optnum(c, -1, opts[11]);
    IF opts[12] > 0 THEN
      optnum(m, -1, opts[12]);
    IF opts[13] > 0 THEN
      optnum(w, -1, opts[13]);
IF opts[14] > 0 THEN f := f + [listclean];
    list(f, s, c, m, w)
  END
END.
   