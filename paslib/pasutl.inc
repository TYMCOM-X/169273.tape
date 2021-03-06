(*pasutl.inc, last modified 9/19/83, zw*)

$IFNOT pasutlinc

$INCLUDE pasutl.typ

EXTERNAL VAR
    dbg: bin;

EXTERNAL VAR
    err: bin;

EXTERNAL VAR
    tkn: STRING[tknsiz];

EXTERNAL PROCEDURE errmsg(msg: str);

EXTERNAL PROCEDURE error(msg: str);

EXTERNAL PROCEDURE fatal(msg: str);

EXTERNAL PROCEDURE chkerr(cond: bin; msg: str);

EXTERNAL PROCEDURE chkftl(cond: bin; msg: str);

EXTERNAL PROCEDURE errfil(fil: str);

EXTERNAL PROCEDURE logstr(txtstr: str);

EXTERNAL PROCEDURE loglin(txtlin: str);

EXTERNAL PROCEDURE logfil(fil: str);

EXTERNAL PROCEDURE wrstr(txtstr: str);

EXTERNAL PROCEDURE wrlin(txtlin: str);

EXTERNAL FUNCTION wrchk(len: int): bin;

EXTERNAL FUNCTION rdlin(VAR lin: str): bin;

EXTERNAL FUNCTION getcmd(prompt: str; VAR cmdlin: str): bin;

EXTERNAL PROCEDURE cmdfil(fil: str);

EXTERNAL PROCEDURE scan(tknstr: STRING[tknsiz]);

EXTERNAL FUNCTION numstr(num: int; VAR strval: str): bin;

EXTERNAL FUNCTION strnum(strval: str; VAR num: int): bin;

EXTERNAL FUNCTION abbrev(s1, s2: str): bin;

EXTERNAL FUNCTION lkpwrd(word: str; list: wrdlst): int;

EXTERNAL FUNCTION query(question: str; list: wrdlst): int;

EXTERNAL FUNCTION askyn(question: str): bin;

EXTERNAL PROCEDURE lptoff;

EXTERNAL PROCEDURE lpton;

EXTERNAL PROCEDURE ttyoff;

EXTERNAL PROCEDURE ttyon;

EXTERNAL FUNCTION popio: bin;

EXTERNAL FUNCTION pushio: bin;

EXTERNAL PROCEDURE clsfil(VAR fil: txtfil);

EXTERNAL FUNCTION opnfil(n, d: str; VAR f: txtfil; m: str): bin;

EXTERNAL PROCEDURE closio;

EXTERNAL FUNCTION openio(ifil, ofil: str): bin;

EXTERNAL PROCEDURE d(msg: str);

EXTERNAL PROCEDURE b(msg: str);

EXTERNAL PROCEDURE e(msg: str);

EXTERNAL PROCEDURE debug;

EXTERNAL PROCEDURE dbgfil(fil: str);

EXTERNAL PROCEDURE optnum(VAR opt: int; defval, argnum: int);

EXTERNAL PROCEDURE getopt (keys: wrdlst; VAR flags: ARRAY[1 .. *] OF int);

EXTERNAL FUNCTION getarg(n: int; VAR argstr: str): bin;

EXTERNAL FUNCTION start(prog, cmd: str): bin;

EXTERNAL PROCEDURE copy;

EXTERNAL PROCEDURE help(fil: str);

EXTERNAL PROCEDURE crypt(key: str);

$ENABLE pasutlinc
$ENDIF

    