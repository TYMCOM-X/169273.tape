(*pasutl.typ, last modified 9/19/83, zw*)

$IFNOT pasutltyp

CONST
    wrdlen = 20;
    tknsiz = wrdlen;
    cmdlinlen = 80;
    lstlinlen = 132;
    filstksiz = 10;
    cmdstksiz = 10;
    argflag = '/'; (*flags beginning of argument strings*)
    optflag = ':'; (*flags beginning of argument data after key*)
    eofflag = '.EOF'; (*flags end of file for INPUT*)
    cmdflag = '@'; (*flags command file name*)
    logflag = '#'; (*flags log file name*)
    filflag = '='; (*seperates output and input file names*)
    cmdext = '.CMD'; (*default command file extension*)
    logext = '.LOG'; (*default log file extension*)
    ttynam = 'TTY:'; (*file name of terminal*)
    lptnam = 'LPT:'; (*file name of printer*)



TYPE
    str = STRING[*];
    int = INTEGER;
    bin = BOOLEAN;
    txtfil = TEXT;
    filnam = FILE_NAME;
    wrdtyp = STRING[wrdlen];
    tkntyp = STRING[tknsiz];
    wrdlst = ARRAY[1 .. *] OF wrdtyp;
    cmdlintyp = STRING[cmdlinlen];
    lstlintyp = STRING[lstlinlen];

$ENABLE pasutltyp
$ENDIF
   