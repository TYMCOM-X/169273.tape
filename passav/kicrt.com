;kicrt.com, last modified 5/31/83, zw
;create TYM-Pascal system on a KI10
:
:time 300
:
copy PKIINI.ENV, P10INI.ENV 
copy MKIDLP.LOW, MDLPRO.LOW 
copy MKIDLP.SHR, MDLPRO.SHR 
copy MKIDLP.EXE, MDLPRO.EXE 
copy NKICRT.COM, NLBCRT.COM 
copy IKICRT.COM, ILBCRT.COM 
copy RKICRT.COM, RDNCRT.COM 
copy MKIDLS.PAS, MMMDLS.PAS 
copy OKIMAC.COM, ODMMAC.COM 
copy OKIMAC.CMD, ODMMAC.CMD 
copy OKIBLD.COM, ODMBLD.COM 
copy OKILSD.COM, ODMLSD.COM 
copy PKICAL.BLD, PASCAL.BLD 
copy PKIINI.PAS, PASINI.PAS 
copy KICRT.OLD, PASCRT.OLD
copy KIRES, PASRES
:
:com nlbcrt.com
:
:com ilbcrt.com
:
:com rdncrt.com
:
:com odmcrt.com
:
:com pmfcrt.com
:
:com cmlcrt.com
    