;paslib.com, last modified 5/31/83, zw
;link the PASLIB.REL
:
r (upl)fudge2
paslib= debmon, debug, debio, deblex, debbrk, debref, debasm, 
 debdmp, debscp, debsym, debprt, debbol, pasmon, rtenv, 
 dbsup, mmmodf, ncqfit, mmqfit, rtcnv/a
paslib= paslib, rtstrs, rtsets, iofile, iocnnl, iontxt, iochar,
 iochfk, ioerro, tenio, pasdis, buf0, decode, pasppn, iofake,
 rtmath, rterrs, rtpfid, onesca, rtamsg, except, rtpsar, sovfak/a
paslib= paslib/x

   