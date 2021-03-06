
require "{}()" delimiters;

define $op!number=0; 
forlc $opc=( jmp,cfm,rva,wva,saa,hlt,rpc,rep,vrm,vcl,cfh,dfh,clr,rsi,
    rar,rer,smf,chr,sva,hst,gft,con,gin,vch,car,cer,get,run,sav,ssa)
 doc { 	define !FO}&{$OPC=$op!number; 
	redefine $op!number=$op!number+1; }
endc

redefine $op!number=1; 
forlc $opc=( noh,bdi,nrt,bdf,nlc,und,nch,clp,bar,npr,bfd,bdh,bfn,adb,
    ioe,alr,alh,csj,fhe,fhu,nfh,bcn,btn,bht,cch,bsv,cii,ccf,cfj,rem)
 doc { 	define FE}&{$OPC}&{comment=$op!number; 
	redefine $op!number=$op!number+1; }
endc

require unstack!delimiters;
external integer simple procedure frmopV( integer frame, arg, opcode );
external integer simple procedure frmopR( integer frame; 
			reference integer arg; integer opcode );
 