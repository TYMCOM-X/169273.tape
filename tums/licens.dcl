
require "{}[]" delimiters;
redefine
	LC!WC=-'400000000000
,	LC!RC='200000000000
,	LC!OP='100000000000
,	LC!SY='40000000000
,	LC!GD='20000000000
,	LC!TD='10000000000
,	LC!ST='4000000000
,	LC!HF='2000000000
,	LC!JL='1000000000
,	LC!AC='400000000
,	LC!XC='200000000
,	LC!RP='04000000
,	LC!RF='10000000
,	LC!RA='14000000
,	LC!WP='1000000
,	LC!WF='2000000
,	LC!WA='3000000
;
redefine !License!Required(LIC) = {
    require "{}[]" delimiters;
    define !!LIC = cvps(LIC), !!LICS = null, !!LICM = 0;
    whilec {length(cvms(!!LIC))} doc {
	redefine !!LIC1 = cvms(!!LIC)[1 to 2], !!LIC = cvms(!!LIC)[4 to inf];
	redefine !!LIC2 = {LC!}&cvms(!!LIC1);
	redefine !!LIC3 = {declaration(}&cvms(!!LIC2)&{)};
	ifcr !!LIC3 neq check!type(define)
	    thenc
		define !!error = !!LIC2;
	    elsec
		redefine !!LICS = cvms(!!LICS)& { }& cvms(!!LIC1);
		redefine !!LICM = !!LICM lor !!LIC2;
	    endc
    } endc
    require 13&10&"License required:"&cvms(!!LICS)&13&10 message;
    simple procedure !License!Check;
    begin "LicCheck"
	if ( !!LICM land calli(-'20,'41) ) neq !!LICM then usererr(0,0,
	    "?license required:"&cvms(!!LICS)&" by "&ban$file, "X");
    end "LicCheck";
    require !License!Check initialization[2];
    require unstack!delimiters
};
define ban$file = {scanc(compiler!banner[length(scanc(compiler!banner,":",null,"IA"))+4 to inf],'11," ","IS")};
require unstack!delimiters;
   