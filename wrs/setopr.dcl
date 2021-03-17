external record!class DBN (
    integer Next );			comment next free record;
external record!class DBH (
    integer SetopVersion;		comment author version - must be first field;
    integer RecSize;			comment size of data record;
    integer First;			comment address of first record;
    integer Last;			comment address of last record;
    integer Host0,Host1,Host2,Host3,Host4,Host5,Host6,Host7,Host8;
    integer Host9,Host10,Host11,Host12,Host13,Host14,Host15,Host16,Host17;
    integer Host18,Host19,Host20,Host21,Host22,Host23,Host24,Host25,Host26;
    integer Host27,Host28,Host29,Host30,Host31,Host32,Host33,Host34 );
external record!class DBP (
    integer EmpNo;
    integer Nam0,Nam1,Nam2,Nam3,Nam4;
    integer Tel0,Tel1,Tel2;
    integer Adr0,Adr1,Adr2,Adr3,Adr4;
    integer Creation.Date!Mgr;
    integer Transfer.Date!Mgr;
    integer Suspense.Date!Mgr;
    integer Flags!Cost );
external record!class DBL (
    integer Nam1,Nam2;
    integer Password;
    integer DOC!Dist;
    integer Proxy.R!W;
    integer DOE!Mgr0,DOE!Mgr1,DOE!Mgr2,DOE!Mgr3;
    integer Lic0,Lic1,Lic2,Lic3;
    integer Sys0,Sys1,Sys2,Sys3 );
external r!p($CLASS) $DBN,$DBH,$DBP,$DBL;
